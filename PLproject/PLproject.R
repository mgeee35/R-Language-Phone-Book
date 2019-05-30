

insert.function <- function() {
  
  repeat {
    
    surname <<- readline("surname: ")
    name <<- readline("name: ")
    phone <<- readline("phone: ")
    
    new.data <<- data.frame(surname, name, phone, stringsAsFactors = TRUE)
    
    if (is.null(first.data) == TRUE) {
      first.data <<- new.data
    } else {
      first.data <<- rbind(first.data, new.data)
    }
    print(new.data)
    cat(" *******************************\n")
    cat("Baska kayit eklemeden cikmak icin 9'a, devam icin baska tusa basin.\n")
    decision <<- readline("Seciminiz: ")
    if (decision == 9) {
      main.menu()
    } else {
      insert.function()
    }
  }
  main.menu()
}


search.function <- function() {
  search_no = readline("Soyad aramasi icin 1, tel aramasi icin 2'yi tuslayin: ")
  if (search_no == 1) {
    searching <<- readline("Aradiginiz soyadini giriniz: ")
    print(first.data[which(first.data$surname == searching),])
    cat(" *******************************\n")
  } else if (search_no == 2) {
    searching <<- readline("Aradiginiz telefon numarasini giriniz: ")
    print(first.data[which(first.data$phone == searching),])
    cat(" *******************************\n")
  } else {
    cat(" Yanlis secim yaptiniz. Tekrar deneyin.\n")
    search.function()
  }

}


remove.function <- function() {
  remove_no = readline("Soyad silme icin 1, tel silme icin 2'yi tuslayin butun kayitlari silmek icin 3'u tuslayin: ")
  if (remove_no == 1) {
    searching <<- readline("Sileceginiz soyadini giriniz: ")
    #print(first.data <<- first.data[!grepl(searching, first.data$surname),])
    print(first.data <<- first.data[which(!first.data$surname %in% searching),])
    cat(" *******************************\n")
  } else if (remove_no == 2) {
    searching <<- readline("Sileceginiz telefon numarasini giriniz: ")
    #print(first.data <<- first.data[!grepl(searching, first.data$surname),])
    print(first.data <<- first.data[which(!first.data$phone %in% searching),])
    cat(" *******************************\n")
  } else if (remove_no == 3) {
    #print(first.data <<- first.data[!grepl(first.data),])
    print(first.data <<- first.data[which(!first.data),])
    cat(" Tum kayitlar silindi.\n")
    cat(" *******************************\n")    
  } else {
    cat(" Yanlis secim yaptiniz. Tekrar deneyin.\n")
    remove.function()
  }
  main.menu()
}  


exit.function <- function() {
  cat(" Cikis yaptiniz.")
  stop()
}


main.menu <- function() {
  menu <- " Telefon Rehberi Menusu\n 1. Kisi Ekle\n 2. Kisi Ara
 3. Kisileri Listele\n 4. Kisi Sil\n 9. Cikis\n"
  cat(menu)
  number <<- readline(prompt = "Lutfen secim yapiniz: ")
  choice.function()
}


choice.function <- function () {
  if (number == 1) {
    insert.function()
  } else if (number == 2) {
    search.function()
  } else if (number == 3) {
    print(first.data)
    cat(" *******************************\n")
    View(first.data)
    main.menu()
  } else if (number == 4) {
    remove.function()
  } else if (number == 9) {
    exit.function()
  } else {
    cat(" Yanlis secim yaptiniz. Tekrar deneyin.\n")
    main.menu()
  }
}


first.data <- data.frame()
main.menu()
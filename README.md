Instalacao do R e R Studio

Siga os passos daqui. 

	https://linuxhint.com/install-r-and-rstudio-linux-mint/

Quando chegar no passo abaixo, execute o comando normalmente.

	sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

Após a execução, será exibido um Warning. Execute o comando abaixo que deve resolver

	sudo apt-key export E084DAB9 | sudo gpg --dearmour -o /etc/apt/trusted.gpg.d/cran.gpg

Pode continuar a executar todos os demais passos, exceto o último pois você não vai querer remover o que acabou de instalar.
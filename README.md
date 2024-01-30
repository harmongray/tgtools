:warning: **This is a public-facing development repository only. DO NOT use this package outside its own development!** :warning:

## 
This is an R package in development for seamless handling of Telegram data in R. It is intended with two data sources in mind:

1. The export function in Telegram Desktop, which supports:
- `.json`
- `.html`
2. The popular [telegram-tracker](https://github.com/estebanpdl/telegram-tracker) tool, which available on GitHub and has over 250 stars. It is by far the most popular tool available in the open-source software for downloading telegram-channel text data _en masse_. It has a sizeable audience in the OSINT world, especially among those following the Russian War of Aggression in Ukraine.

  The current project status is pre-alpha. It is a mostly-functional proof-of-concept, though neither the export method nor the telegram-tracker repository method are supported. A different, lesser known library, [telescrape](https://github.com/PeterWalchhofer/Telescrape) was originally used by the author of this package. However, due to discovered inconsistent regex captures of links it will not be supported in the future.

  There are many open-source libraries that use the Python Telethon library to access the official telegram API. If these third-party libraries are used by data scientists or OSINT researchers, this package is interested in developing a library that makes data from multiple sources easily combined with extraction of URLs, forwards, and other related data available in the Telegram API.

  Although it is technically installable via `remotes::install_github()` methods in R, **do not do this unless you are testing the package functionality as a developer**.

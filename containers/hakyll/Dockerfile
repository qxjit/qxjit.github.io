# flipstone/playground-haskell
FROM 600a8b289a40

USER vagrant
RUN HOME=/home/vagrant \
    bash -i -c "\
      cabal update && \
      cd /home/vagrant && \
      mkdir hakyll && \
      cd hakyll && \
      cabal sandbox init && \
      cabal install hakyll-4.5.5.0 \
                    pandoc-1.13.1 -fhttps \
      "

RUN HOME=/home/vagrant \
    bash -i -c "\
      cd /home/vagrant && \
      echo 'export PATH=/home/vagrant/hakyll/.cabal-sandbox/bin:\$PATH' \
           >> .bash_profile \
    "

WORKDIR /data
ENV HOME /home/vagrant
ENTRYPOINT ["/bin/bash", "-l", "-c"]


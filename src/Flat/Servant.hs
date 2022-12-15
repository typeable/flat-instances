{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Flat.Servant where

import Data.Bifunctor
import Data.ByteString.Lazy
import Flat as F
import Servant.API


data FlatBinary

instance Accept FlatBinary where
  contentType _ = "application/octet-stream"

instance Flat a => MimeRender FlatBinary a where
  mimeRender _ = fromStrict . F.flat

instance Flat a => MimeUnrender FlatBinary a where
  mimeUnrender _ = first show . F.unflat

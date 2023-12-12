{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Metadata.ProtocolSettings
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Metadata.ProtocolSettings where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct (Parser, field, record, text)
import Data.Text (Text)
import GHC.Generics (Generic)

$( passthroughBareB
     [d|
       data ProtocolSettings = ProtocolSettings {h2 :: Text}
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e ProtocolSettings
parse = record ProtocolSettings {h2 = field "h2" text}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.EndpointDiscovery
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.EndpointDiscovery where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    bool,
    field,
    optional,
    record,
  )
import GHC.Generics (Generic)

$( passthroughBareB
     [d|
       data EndpointDiscovery = EndpointDiscovery {required :: Maybe Bool}
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e EndpointDiscovery
parse =
  record
    EndpointDiscovery
      { required = optional $ field "required" bool
      }

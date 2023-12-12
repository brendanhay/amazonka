{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.Error
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.Error where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Service.Operation.Error.Info qualified as Error (Info)
import Botocore.Service.Operation.Error.Info qualified as ErrorInfo
import Botocore.Service.Types (ShapeName, shapeName)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    bool,
    field,
    optional,
    record,
    text,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (error)

$( passthroughBareB
     [d|
       data Error = Error
         { shape :: ShapeName,
           documentation :: Maybe Text,
           error :: Maybe Error.Info,
           exception :: Maybe Bool,
           fault :: Maybe Bool
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Error
parse =
  record
    Error
      { shape = field "shape" shapeName,
        documentation = optional $ field "documentation" text,
        exception = optional $ field "exception" bool,
        fault = optional $ field "fault" bool,
        error = optional . field "error" $ ErrorInfo.parse
      }

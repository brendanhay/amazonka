{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Service.Operation.StaticContextParams
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Service.Operation.StaticContextParams where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens (..))
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser (..),
    bool,
    enum,
    field,
    oneFieldObject,
    optional,
    record,
  )
import GHC.Generics (Generic)
import Prelude hiding (error)

data OperationType = Control | Data
  deriving (Bounded, Enum, Eq, Ord, Show, Generic)

$( passthroughBareB
     [d|
       data StaticContextParams = StaticContextParams
         { operationType :: Maybe OperationType,
           requiresAccountId :: Maybe Bool,
           disableAccessPoints :: Maybe Bool,
           useObjectLambdaEndpoint :: Maybe Bool
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e StaticContextParams
parse =
  record
    StaticContextParams
      { operationType =
          optional . field "OperationType" $
            oneFieldObject . field "value" . enum $ \case
              Control -> "control"
              Data -> "data",
        requiresAccountId =
          optional . field "RequiresAccountId" $
            oneFieldObject $
              field "value" bool,
        disableAccessPoints =
          optional . field "DisableAccessPoints" $
            oneFieldObject $
              field "value" bool,
        useObjectLambdaEndpoint =
          optional . field "UseObjectLambdaEndpoint" $
            oneFieldObject $
              field "value" bool
      }

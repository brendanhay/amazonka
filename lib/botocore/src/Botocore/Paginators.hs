{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Paginators
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Paginators where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Botocore.Paginators.Pagination (Pagination)
import Botocore.Paginators.Pagination qualified as Pagination
import Data.Aeson.Decoding.Tokens (Tokens)
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    field,
    map,
    optional,
    record,
    text,
  )
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (map)

$( passthroughBareB
     [d|
       data Paginators = Paginators
         { pagination :: Map Text Pagination,
           version :: Maybe Text
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Paginators
parse =
  record
    Paginators
      { pagination = field "pagination" $ map Pagination.parse,
        version = optional $ field "version" text
      }

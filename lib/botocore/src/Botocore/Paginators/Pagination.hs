{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Botocore.Paginators.Pagination
-- Copyright   : (c) 2023 Bellroy Pty Ltd
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Jack Kelly <jack@jackkelly.name>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Botocore.Paginators.Pagination where

import Barbies (Barbie (..))
import Barbies.TH (passthroughBareB)
import Data.Aeson.Decoding.Tokens (Tokens)
import Data.Aeson.Decoding.Tokens.Direct
  ( Parser,
    field,
    nonEmpty,
    optional,
    orElse,
    record,
    text,
  )
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (map)

$( passthroughBareB
     [d|
       data Pagination = Pagination
         { inputToken :: Either (NonEmpty Text) Text,
           outputToken :: Either (NonEmpty Text) Text,
           resultKey :: Either (NonEmpty Text) Text,
           limitKey :: Maybe Text,
           moreResults :: Maybe Text,
           nonAggregateKeys :: Maybe (NonEmpty Text)
         }
         deriving stock (Eq, Show, Generic)
       |]
 )

parse :: Parser Tokens k e Pagination
parse =
  record
    Pagination
      { inputToken =
          field "input_token" $
            (Left <$> nonEmpty text) `orElse` (Right <$> text),
        outputToken =
          field "output_token" $
            (Left <$> nonEmpty text) `orElse` (Right <$> text),
        resultKey =
          field "result_key" $
            (Left <$> nonEmpty text) `orElse` (Right <$> text),
        limitKey = optional $ field "limit_key" text,
        moreResults = optional $ field "more_results" text,
        nonAggregateKeys = optional . field "non_aggregate_keys" $ nonEmpty text
      }

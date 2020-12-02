{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode where

import Network.AWS.Prelude

data BatchDeleteImportDataErrorCode
  = InternalServerError
  | NotFound
  | OverLimit
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText BatchDeleteImportDataErrorCode where
  parser =
    takeLowerText >>= \case
      "internal_server_error" -> pure InternalServerError
      "not_found" -> pure NotFound
      "over_limit" -> pure OverLimit
      e ->
        fromTextError $
          "Failure parsing BatchDeleteImportDataErrorCode from value: '" <> e
            <> "'. Accepted values: internal_server_error, not_found, over_limit"

instance ToText BatchDeleteImportDataErrorCode where
  toText = \case
    InternalServerError -> "INTERNAL_SERVER_ERROR"
    NotFound -> "NOT_FOUND"
    OverLimit -> "OVER_LIMIT"

instance Hashable BatchDeleteImportDataErrorCode

instance NFData BatchDeleteImportDataErrorCode

instance ToByteString BatchDeleteImportDataErrorCode

instance ToQuery BatchDeleteImportDataErrorCode

instance ToHeader BatchDeleteImportDataErrorCode

instance FromJSON BatchDeleteImportDataErrorCode where
  parseJSON = parseJSONText "BatchDeleteImportDataErrorCode"

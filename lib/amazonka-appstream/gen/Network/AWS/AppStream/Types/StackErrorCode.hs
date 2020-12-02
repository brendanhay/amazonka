{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StackErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackErrorCode where

import Network.AWS.Prelude

data StackErrorCode
  = SECInternalServiceError
  | SECStorageConnectorError
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

instance FromText StackErrorCode where
  parser =
    takeLowerText >>= \case
      "internal_service_error" -> pure SECInternalServiceError
      "storage_connector_error" -> pure SECStorageConnectorError
      e ->
        fromTextError $
          "Failure parsing StackErrorCode from value: '" <> e
            <> "'. Accepted values: internal_service_error, storage_connector_error"

instance ToText StackErrorCode where
  toText = \case
    SECInternalServiceError -> "INTERNAL_SERVICE_ERROR"
    SECStorageConnectorError -> "STORAGE_CONNECTOR_ERROR"

instance Hashable StackErrorCode

instance NFData StackErrorCode

instance ToByteString StackErrorCode

instance ToQuery StackErrorCode

instance ToHeader StackErrorCode

instance FromJSON StackErrorCode where
  parseJSON = parseJSONText "StackErrorCode"

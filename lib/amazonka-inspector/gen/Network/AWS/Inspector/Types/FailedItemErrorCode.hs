{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FailedItemErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FailedItemErrorCode where

import Network.AWS.Prelude

data FailedItemErrorCode
  = FIECAccessDenied
  | FIECDuplicateARN
  | FIECInternalError
  | FIECInvalidARN
  | FIECItemDoesNotExist
  | FIECLimitExceeded
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

instance FromText FailedItemErrorCode where
  parser =
    takeLowerText >>= \case
      "access_denied" -> pure FIECAccessDenied
      "duplicate_arn" -> pure FIECDuplicateARN
      "internal_error" -> pure FIECInternalError
      "invalid_arn" -> pure FIECInvalidARN
      "item_does_not_exist" -> pure FIECItemDoesNotExist
      "limit_exceeded" -> pure FIECLimitExceeded
      e ->
        fromTextError $
          "Failure parsing FailedItemErrorCode from value: '" <> e
            <> "'. Accepted values: access_denied, duplicate_arn, internal_error, invalid_arn, item_does_not_exist, limit_exceeded"

instance ToText FailedItemErrorCode where
  toText = \case
    FIECAccessDenied -> "ACCESS_DENIED"
    FIECDuplicateARN -> "DUPLICATE_ARN"
    FIECInternalError -> "INTERNAL_ERROR"
    FIECInvalidARN -> "INVALID_ARN"
    FIECItemDoesNotExist -> "ITEM_DOES_NOT_EXIST"
    FIECLimitExceeded -> "LIMIT_EXCEEDED"

instance Hashable FailedItemErrorCode

instance NFData FailedItemErrorCode

instance ToByteString FailedItemErrorCode

instance ToQuery FailedItemErrorCode

instance ToHeader FailedItemErrorCode

instance FromJSON FailedItemErrorCode where
  parseJSON = parseJSONText "FailedItemErrorCode"

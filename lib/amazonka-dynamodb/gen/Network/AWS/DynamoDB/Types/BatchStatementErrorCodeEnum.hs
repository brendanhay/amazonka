{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum where

import Network.AWS.Prelude

data BatchStatementErrorCodeEnum
  = AccessDenied
  | ConditionalCheckFailed
  | DuplicateItem
  | InternalServerError
  | ItemCollectionSizeLimitExceeded
  | ProvisionedThroughputExceeded
  | RequestLimitExceeded
  | ResourceNotFound
  | ThrottlingError
  | TransactionConflict
  | ValidationError
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

instance FromText BatchStatementErrorCodeEnum where
  parser =
    takeLowerText >>= \case
      "accessdenied" -> pure AccessDenied
      "conditionalcheckfailed" -> pure ConditionalCheckFailed
      "duplicateitem" -> pure DuplicateItem
      "internalservererror" -> pure InternalServerError
      "itemcollectionsizelimitexceeded" -> pure ItemCollectionSizeLimitExceeded
      "provisionedthroughputexceeded" -> pure ProvisionedThroughputExceeded
      "requestlimitexceeded" -> pure RequestLimitExceeded
      "resourcenotfound" -> pure ResourceNotFound
      "throttlingerror" -> pure ThrottlingError
      "transactionconflict" -> pure TransactionConflict
      "validationerror" -> pure ValidationError
      e ->
        fromTextError $
          "Failure parsing BatchStatementErrorCodeEnum from value: '" <> e
            <> "'. Accepted values: accessdenied, conditionalcheckfailed, duplicateitem, internalservererror, itemcollectionsizelimitexceeded, provisionedthroughputexceeded, requestlimitexceeded, resourcenotfound, throttlingerror, transactionconflict, validationerror"

instance ToText BatchStatementErrorCodeEnum where
  toText = \case
    AccessDenied -> "AccessDenied"
    ConditionalCheckFailed -> "ConditionalCheckFailed"
    DuplicateItem -> "DuplicateItem"
    InternalServerError -> "InternalServerError"
    ItemCollectionSizeLimitExceeded -> "ItemCollectionSizeLimitExceeded"
    ProvisionedThroughputExceeded -> "ProvisionedThroughputExceeded"
    RequestLimitExceeded -> "RequestLimitExceeded"
    ResourceNotFound -> "ResourceNotFound"
    ThrottlingError -> "ThrottlingError"
    TransactionConflict -> "TransactionConflict"
    ValidationError -> "ValidationError"

instance Hashable BatchStatementErrorCodeEnum

instance NFData BatchStatementErrorCodeEnum

instance ToByteString BatchStatementErrorCodeEnum

instance ToQuery BatchStatementErrorCodeEnum

instance ToHeader BatchStatementErrorCodeEnum

instance FromJSON BatchStatementErrorCodeEnum where
  parseJSON = parseJSONText "BatchStatementErrorCodeEnum"

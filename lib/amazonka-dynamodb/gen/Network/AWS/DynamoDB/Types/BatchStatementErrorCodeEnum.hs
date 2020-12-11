-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementErrorCodeEnum
  ( BatchStatementErrorCodeEnum
      ( BatchStatementErrorCodeEnum',
        AccessDenied,
        ConditionalCheckFailed,
        DuplicateItem,
        InternalServerError,
        ItemCollectionSizeLimitExceeded,
        ProvisionedThroughputExceeded,
        RequestLimitExceeded,
        ResourceNotFound,
        ThrottlingError,
        TransactionConflict,
        ValidationError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BatchStatementErrorCodeEnum = BatchStatementErrorCodeEnum' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AccessDenied :: BatchStatementErrorCodeEnum
pattern AccessDenied = BatchStatementErrorCodeEnum' "AccessDenied"

pattern ConditionalCheckFailed :: BatchStatementErrorCodeEnum
pattern ConditionalCheckFailed = BatchStatementErrorCodeEnum' "ConditionalCheckFailed"

pattern DuplicateItem :: BatchStatementErrorCodeEnum
pattern DuplicateItem = BatchStatementErrorCodeEnum' "DuplicateItem"

pattern InternalServerError :: BatchStatementErrorCodeEnum
pattern InternalServerError = BatchStatementErrorCodeEnum' "InternalServerError"

pattern ItemCollectionSizeLimitExceeded :: BatchStatementErrorCodeEnum
pattern ItemCollectionSizeLimitExceeded = BatchStatementErrorCodeEnum' "ItemCollectionSizeLimitExceeded"

pattern ProvisionedThroughputExceeded :: BatchStatementErrorCodeEnum
pattern ProvisionedThroughputExceeded = BatchStatementErrorCodeEnum' "ProvisionedThroughputExceeded"

pattern RequestLimitExceeded :: BatchStatementErrorCodeEnum
pattern RequestLimitExceeded = BatchStatementErrorCodeEnum' "RequestLimitExceeded"

pattern ResourceNotFound :: BatchStatementErrorCodeEnum
pattern ResourceNotFound = BatchStatementErrorCodeEnum' "ResourceNotFound"

pattern ThrottlingError :: BatchStatementErrorCodeEnum
pattern ThrottlingError = BatchStatementErrorCodeEnum' "ThrottlingError"

pattern TransactionConflict :: BatchStatementErrorCodeEnum
pattern TransactionConflict = BatchStatementErrorCodeEnum' "TransactionConflict"

pattern ValidationError :: BatchStatementErrorCodeEnum
pattern ValidationError = BatchStatementErrorCodeEnum' "ValidationError"

{-# COMPLETE
  AccessDenied,
  ConditionalCheckFailed,
  DuplicateItem,
  InternalServerError,
  ItemCollectionSizeLimitExceeded,
  ProvisionedThroughputExceeded,
  RequestLimitExceeded,
  ResourceNotFound,
  ThrottlingError,
  TransactionConflict,
  ValidationError,
  BatchStatementErrorCodeEnum'
  #-}

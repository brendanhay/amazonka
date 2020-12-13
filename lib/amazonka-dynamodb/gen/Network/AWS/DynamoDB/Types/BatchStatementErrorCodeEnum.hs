{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ConditionalCheckFailed,
        ItemCollectionSizeLimitExceeded,
        RequestLimitExceeded,
        ValidationError,
        ProvisionedThroughputExceeded,
        TransactionConflict,
        ThrottlingError,
        InternalServerError,
        ResourceNotFound,
        AccessDenied,
        DuplicateItem
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

pattern ConditionalCheckFailed :: BatchStatementErrorCodeEnum
pattern ConditionalCheckFailed = BatchStatementErrorCodeEnum' "ConditionalCheckFailed"

pattern ItemCollectionSizeLimitExceeded :: BatchStatementErrorCodeEnum
pattern ItemCollectionSizeLimitExceeded = BatchStatementErrorCodeEnum' "ItemCollectionSizeLimitExceeded"

pattern RequestLimitExceeded :: BatchStatementErrorCodeEnum
pattern RequestLimitExceeded = BatchStatementErrorCodeEnum' "RequestLimitExceeded"

pattern ValidationError :: BatchStatementErrorCodeEnum
pattern ValidationError = BatchStatementErrorCodeEnum' "ValidationError"

pattern ProvisionedThroughputExceeded :: BatchStatementErrorCodeEnum
pattern ProvisionedThroughputExceeded = BatchStatementErrorCodeEnum' "ProvisionedThroughputExceeded"

pattern TransactionConflict :: BatchStatementErrorCodeEnum
pattern TransactionConflict = BatchStatementErrorCodeEnum' "TransactionConflict"

pattern ThrottlingError :: BatchStatementErrorCodeEnum
pattern ThrottlingError = BatchStatementErrorCodeEnum' "ThrottlingError"

pattern InternalServerError :: BatchStatementErrorCodeEnum
pattern InternalServerError = BatchStatementErrorCodeEnum' "InternalServerError"

pattern ResourceNotFound :: BatchStatementErrorCodeEnum
pattern ResourceNotFound = BatchStatementErrorCodeEnum' "ResourceNotFound"

pattern AccessDenied :: BatchStatementErrorCodeEnum
pattern AccessDenied = BatchStatementErrorCodeEnum' "AccessDenied"

pattern DuplicateItem :: BatchStatementErrorCodeEnum
pattern DuplicateItem = BatchStatementErrorCodeEnum' "DuplicateItem"

{-# COMPLETE
  ConditionalCheckFailed,
  ItemCollectionSizeLimitExceeded,
  RequestLimitExceeded,
  ValidationError,
  ProvisionedThroughputExceeded,
  TransactionConflict,
  ThrottlingError,
  InternalServerError,
  ResourceNotFound,
  AccessDenied,
  DuplicateItem,
  BatchStatementErrorCodeEnum'
  #-}

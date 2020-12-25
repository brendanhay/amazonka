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
        BatchStatementErrorCodeEnumConditionalCheckFailed,
        BatchStatementErrorCodeEnumItemCollectionSizeLimitExceeded,
        BatchStatementErrorCodeEnumRequestLimitExceeded,
        BatchStatementErrorCodeEnumValidationError,
        BatchStatementErrorCodeEnumProvisionedThroughputExceeded,
        BatchStatementErrorCodeEnumTransactionConflict,
        BatchStatementErrorCodeEnumThrottlingError,
        BatchStatementErrorCodeEnumInternalServerError,
        BatchStatementErrorCodeEnumResourceNotFound,
        BatchStatementErrorCodeEnumAccessDenied,
        BatchStatementErrorCodeEnumDuplicateItem,
        fromBatchStatementErrorCodeEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BatchStatementErrorCodeEnum = BatchStatementErrorCodeEnum'
  { fromBatchStatementErrorCodeEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern BatchStatementErrorCodeEnumConditionalCheckFailed :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumConditionalCheckFailed = BatchStatementErrorCodeEnum' "ConditionalCheckFailed"

pattern BatchStatementErrorCodeEnumItemCollectionSizeLimitExceeded :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumItemCollectionSizeLimitExceeded = BatchStatementErrorCodeEnum' "ItemCollectionSizeLimitExceeded"

pattern BatchStatementErrorCodeEnumRequestLimitExceeded :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumRequestLimitExceeded = BatchStatementErrorCodeEnum' "RequestLimitExceeded"

pattern BatchStatementErrorCodeEnumValidationError :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumValidationError = BatchStatementErrorCodeEnum' "ValidationError"

pattern BatchStatementErrorCodeEnumProvisionedThroughputExceeded :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumProvisionedThroughputExceeded = BatchStatementErrorCodeEnum' "ProvisionedThroughputExceeded"

pattern BatchStatementErrorCodeEnumTransactionConflict :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumTransactionConflict = BatchStatementErrorCodeEnum' "TransactionConflict"

pattern BatchStatementErrorCodeEnumThrottlingError :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumThrottlingError = BatchStatementErrorCodeEnum' "ThrottlingError"

pattern BatchStatementErrorCodeEnumInternalServerError :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumInternalServerError = BatchStatementErrorCodeEnum' "InternalServerError"

pattern BatchStatementErrorCodeEnumResourceNotFound :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumResourceNotFound = BatchStatementErrorCodeEnum' "ResourceNotFound"

pattern BatchStatementErrorCodeEnumAccessDenied :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumAccessDenied = BatchStatementErrorCodeEnum' "AccessDenied"

pattern BatchStatementErrorCodeEnumDuplicateItem :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnumDuplicateItem = BatchStatementErrorCodeEnum' "DuplicateItem"

{-# COMPLETE
  BatchStatementErrorCodeEnumConditionalCheckFailed,
  BatchStatementErrorCodeEnumItemCollectionSizeLimitExceeded,
  BatchStatementErrorCodeEnumRequestLimitExceeded,
  BatchStatementErrorCodeEnumValidationError,
  BatchStatementErrorCodeEnumProvisionedThroughputExceeded,
  BatchStatementErrorCodeEnumTransactionConflict,
  BatchStatementErrorCodeEnumThrottlingError,
  BatchStatementErrorCodeEnumInternalServerError,
  BatchStatementErrorCodeEnumResourceNotFound,
  BatchStatementErrorCodeEnumAccessDenied,
  BatchStatementErrorCodeEnumDuplicateItem,
  BatchStatementErrorCodeEnum'
  #-}

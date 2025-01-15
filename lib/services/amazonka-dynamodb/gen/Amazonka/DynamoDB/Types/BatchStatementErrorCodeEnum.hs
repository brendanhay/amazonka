{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Types.BatchStatementErrorCodeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BatchStatementErrorCodeEnum
  ( BatchStatementErrorCodeEnum
      ( ..,
        BatchStatementErrorCodeEnum_AccessDenied,
        BatchStatementErrorCodeEnum_ConditionalCheckFailed,
        BatchStatementErrorCodeEnum_DuplicateItem,
        BatchStatementErrorCodeEnum_InternalServerError,
        BatchStatementErrorCodeEnum_ItemCollectionSizeLimitExceeded,
        BatchStatementErrorCodeEnum_ProvisionedThroughputExceeded,
        BatchStatementErrorCodeEnum_RequestLimitExceeded,
        BatchStatementErrorCodeEnum_ResourceNotFound,
        BatchStatementErrorCodeEnum_ThrottlingError,
        BatchStatementErrorCodeEnum_TransactionConflict,
        BatchStatementErrorCodeEnum_ValidationError
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype BatchStatementErrorCodeEnum = BatchStatementErrorCodeEnum'
  { fromBatchStatementErrorCodeEnum ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern BatchStatementErrorCodeEnum_AccessDenied :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_AccessDenied = BatchStatementErrorCodeEnum' "AccessDenied"

pattern BatchStatementErrorCodeEnum_ConditionalCheckFailed :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_ConditionalCheckFailed = BatchStatementErrorCodeEnum' "ConditionalCheckFailed"

pattern BatchStatementErrorCodeEnum_DuplicateItem :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_DuplicateItem = BatchStatementErrorCodeEnum' "DuplicateItem"

pattern BatchStatementErrorCodeEnum_InternalServerError :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_InternalServerError = BatchStatementErrorCodeEnum' "InternalServerError"

pattern BatchStatementErrorCodeEnum_ItemCollectionSizeLimitExceeded :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_ItemCollectionSizeLimitExceeded = BatchStatementErrorCodeEnum' "ItemCollectionSizeLimitExceeded"

pattern BatchStatementErrorCodeEnum_ProvisionedThroughputExceeded :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_ProvisionedThroughputExceeded = BatchStatementErrorCodeEnum' "ProvisionedThroughputExceeded"

pattern BatchStatementErrorCodeEnum_RequestLimitExceeded :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_RequestLimitExceeded = BatchStatementErrorCodeEnum' "RequestLimitExceeded"

pattern BatchStatementErrorCodeEnum_ResourceNotFound :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_ResourceNotFound = BatchStatementErrorCodeEnum' "ResourceNotFound"

pattern BatchStatementErrorCodeEnum_ThrottlingError :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_ThrottlingError = BatchStatementErrorCodeEnum' "ThrottlingError"

pattern BatchStatementErrorCodeEnum_TransactionConflict :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_TransactionConflict = BatchStatementErrorCodeEnum' "TransactionConflict"

pattern BatchStatementErrorCodeEnum_ValidationError :: BatchStatementErrorCodeEnum
pattern BatchStatementErrorCodeEnum_ValidationError = BatchStatementErrorCodeEnum' "ValidationError"

{-# COMPLETE
  BatchStatementErrorCodeEnum_AccessDenied,
  BatchStatementErrorCodeEnum_ConditionalCheckFailed,
  BatchStatementErrorCodeEnum_DuplicateItem,
  BatchStatementErrorCodeEnum_InternalServerError,
  BatchStatementErrorCodeEnum_ItemCollectionSizeLimitExceeded,
  BatchStatementErrorCodeEnum_ProvisionedThroughputExceeded,
  BatchStatementErrorCodeEnum_RequestLimitExceeded,
  BatchStatementErrorCodeEnum_ResourceNotFound,
  BatchStatementErrorCodeEnum_ThrottlingError,
  BatchStatementErrorCodeEnum_TransactionConflict,
  BatchStatementErrorCodeEnum_ValidationError,
  BatchStatementErrorCodeEnum'
  #-}

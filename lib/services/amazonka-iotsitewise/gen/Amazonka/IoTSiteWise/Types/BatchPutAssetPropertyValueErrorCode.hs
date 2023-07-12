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
-- Module      : Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyValueErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.BatchPutAssetPropertyValueErrorCode
  ( BatchPutAssetPropertyValueErrorCode
      ( ..,
        BatchPutAssetPropertyValueErrorCode_AccessDeniedException,
        BatchPutAssetPropertyValueErrorCode_ConflictingOperationException,
        BatchPutAssetPropertyValueErrorCode_InternalFailureException,
        BatchPutAssetPropertyValueErrorCode_InvalidRequestException,
        BatchPutAssetPropertyValueErrorCode_LimitExceededException,
        BatchPutAssetPropertyValueErrorCode_ResourceNotFoundException,
        BatchPutAssetPropertyValueErrorCode_ServiceUnavailableException,
        BatchPutAssetPropertyValueErrorCode_ThrottlingException,
        BatchPutAssetPropertyValueErrorCode_TimestampOutOfRangeException
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BatchPutAssetPropertyValueErrorCode = BatchPutAssetPropertyValueErrorCode'
  { fromBatchPutAssetPropertyValueErrorCode ::
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

pattern BatchPutAssetPropertyValueErrorCode_AccessDeniedException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_AccessDeniedException = BatchPutAssetPropertyValueErrorCode' "AccessDeniedException"

pattern BatchPutAssetPropertyValueErrorCode_ConflictingOperationException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_ConflictingOperationException = BatchPutAssetPropertyValueErrorCode' "ConflictingOperationException"

pattern BatchPutAssetPropertyValueErrorCode_InternalFailureException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_InternalFailureException = BatchPutAssetPropertyValueErrorCode' "InternalFailureException"

pattern BatchPutAssetPropertyValueErrorCode_InvalidRequestException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_InvalidRequestException = BatchPutAssetPropertyValueErrorCode' "InvalidRequestException"

pattern BatchPutAssetPropertyValueErrorCode_LimitExceededException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_LimitExceededException = BatchPutAssetPropertyValueErrorCode' "LimitExceededException"

pattern BatchPutAssetPropertyValueErrorCode_ResourceNotFoundException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_ResourceNotFoundException = BatchPutAssetPropertyValueErrorCode' "ResourceNotFoundException"

pattern BatchPutAssetPropertyValueErrorCode_ServiceUnavailableException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_ServiceUnavailableException = BatchPutAssetPropertyValueErrorCode' "ServiceUnavailableException"

pattern BatchPutAssetPropertyValueErrorCode_ThrottlingException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_ThrottlingException = BatchPutAssetPropertyValueErrorCode' "ThrottlingException"

pattern BatchPutAssetPropertyValueErrorCode_TimestampOutOfRangeException :: BatchPutAssetPropertyValueErrorCode
pattern BatchPutAssetPropertyValueErrorCode_TimestampOutOfRangeException = BatchPutAssetPropertyValueErrorCode' "TimestampOutOfRangeException"

{-# COMPLETE
  BatchPutAssetPropertyValueErrorCode_AccessDeniedException,
  BatchPutAssetPropertyValueErrorCode_ConflictingOperationException,
  BatchPutAssetPropertyValueErrorCode_InternalFailureException,
  BatchPutAssetPropertyValueErrorCode_InvalidRequestException,
  BatchPutAssetPropertyValueErrorCode_LimitExceededException,
  BatchPutAssetPropertyValueErrorCode_ResourceNotFoundException,
  BatchPutAssetPropertyValueErrorCode_ServiceUnavailableException,
  BatchPutAssetPropertyValueErrorCode_ThrottlingException,
  BatchPutAssetPropertyValueErrorCode_TimestampOutOfRangeException,
  BatchPutAssetPropertyValueErrorCode'
  #-}

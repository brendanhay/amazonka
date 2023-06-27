{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerFeatureStoreRuntime.Lens
  ( -- * Operations

    -- ** BatchGetRecord
    batchGetRecord_identifiers,
    batchGetRecordResponse_httpStatus,
    batchGetRecordResponse_records,
    batchGetRecordResponse_errors,
    batchGetRecordResponse_unprocessedIdentifiers,

    -- ** DeleteRecord
    deleteRecord_deletionMode,
    deleteRecord_targetStores,
    deleteRecord_featureGroupName,
    deleteRecord_recordIdentifierValueAsString,
    deleteRecord_eventTime,

    -- ** GetRecord
    getRecord_featureNames,
    getRecord_featureGroupName,
    getRecord_recordIdentifierValueAsString,
    getRecordResponse_record,
    getRecordResponse_httpStatus,

    -- ** PutRecord
    putRecord_targetStores,
    putRecord_featureGroupName,
    putRecord_record,

    -- * Types

    -- ** BatchGetRecordError
    batchGetRecordError_featureGroupName,
    batchGetRecordError_recordIdentifierValueAsString,
    batchGetRecordError_errorCode,
    batchGetRecordError_errorMessage,

    -- ** BatchGetRecordIdentifier
    batchGetRecordIdentifier_featureNames,
    batchGetRecordIdentifier_featureGroupName,
    batchGetRecordIdentifier_recordIdentifiersValueAsString,

    -- ** BatchGetRecordResultDetail
    batchGetRecordResultDetail_featureGroupName,
    batchGetRecordResultDetail_recordIdentifierValueAsString,
    batchGetRecordResultDetail_record,

    -- ** FeatureValue
    featureValue_featureName,
    featureValue_valueAsString,
  )
where

import Amazonka.SageMakerFeatureStoreRuntime.BatchGetRecord
import Amazonka.SageMakerFeatureStoreRuntime.DeleteRecord
import Amazonka.SageMakerFeatureStoreRuntime.GetRecord
import Amazonka.SageMakerFeatureStoreRuntime.PutRecord
import Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordError
import Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordIdentifier
import Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordResultDetail
import Amazonka.SageMakerFeatureStoreRuntime.Types.FeatureValue

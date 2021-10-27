{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerFeatureStoreRuntime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMakerFeatureStoreRuntime.Lens
  ( -- * Operations

    -- ** PutRecord
    putRecord_featureGroupName,
    putRecord_record,

    -- ** DeleteRecord
    deleteRecord_featureGroupName,
    deleteRecord_recordIdentifierValueAsString,
    deleteRecord_eventTime,

    -- ** BatchGetRecord
    batchGetRecord_identifiers,
    batchGetRecordResponse_httpStatus,
    batchGetRecordResponse_records,
    batchGetRecordResponse_errors,
    batchGetRecordResponse_unprocessedIdentifiers,

    -- ** GetRecord
    getRecord_featureNames,
    getRecord_featureGroupName,
    getRecord_recordIdentifierValueAsString,
    getRecordResponse_record,
    getRecordResponse_httpStatus,

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

import Network.AWS.SageMakerFeatureStoreRuntime.BatchGetRecord
import Network.AWS.SageMakerFeatureStoreRuntime.DeleteRecord
import Network.AWS.SageMakerFeatureStoreRuntime.GetRecord
import Network.AWS.SageMakerFeatureStoreRuntime.PutRecord
import Network.AWS.SageMakerFeatureStoreRuntime.Types.BatchGetRecordError
import Network.AWS.SageMakerFeatureStoreRuntime.Types.BatchGetRecordIdentifier
import Network.AWS.SageMakerFeatureStoreRuntime.Types.BatchGetRecordResultDetail
import Network.AWS.SageMakerFeatureStoreRuntime.Types.FeatureValue

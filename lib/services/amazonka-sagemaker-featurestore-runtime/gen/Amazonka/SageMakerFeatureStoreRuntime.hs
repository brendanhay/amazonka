{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SageMakerFeatureStoreRuntime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Contains all data plane API operations and data types for the Amazon
-- SageMaker Feature Store. Use this API to put, delete, and retrieve (get)
-- features from a feature store.
--
-- Use the following operations to configure your @OnlineStore@ and
-- @OfflineStore@ features, and to create and manage feature groups:
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateFeatureGroup.html CreateFeatureGroup>
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DeleteFeatureGroup.html DeleteFeatureGroup>
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeFeatureGroup.html DescribeFeatureGroup>
--
-- -   <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_ListFeatureGroups.html ListFeatureGroups>
module Amazonka.SageMakerFeatureStoreRuntime
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessForbidden
    _AccessForbidden,

    -- ** InternalFailure
    _InternalFailure,

    -- ** ResourceNotFound
    _ResourceNotFound,

    -- ** ServiceUnavailable
    _ServiceUnavailable,

    -- ** ValidationError
    _ValidationError,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchGetRecord
    BatchGetRecord (BatchGetRecord'),
    newBatchGetRecord,
    BatchGetRecordResponse (BatchGetRecordResponse'),
    newBatchGetRecordResponse,

    -- ** DeleteRecord
    DeleteRecord (DeleteRecord'),
    newDeleteRecord,
    DeleteRecordResponse (DeleteRecordResponse'),
    newDeleteRecordResponse,

    -- ** GetRecord
    GetRecord (GetRecord'),
    newGetRecord,
    GetRecordResponse (GetRecordResponse'),
    newGetRecordResponse,

    -- ** PutRecord
    PutRecord (PutRecord'),
    newPutRecord,
    PutRecordResponse (PutRecordResponse'),
    newPutRecordResponse,

    -- * Types

    -- ** DeletionMode
    DeletionMode (..),

    -- ** TargetStore
    TargetStore (..),

    -- ** BatchGetRecordError
    BatchGetRecordError (BatchGetRecordError'),
    newBatchGetRecordError,

    -- ** BatchGetRecordIdentifier
    BatchGetRecordIdentifier (BatchGetRecordIdentifier'),
    newBatchGetRecordIdentifier,

    -- ** BatchGetRecordResultDetail
    BatchGetRecordResultDetail (BatchGetRecordResultDetail'),
    newBatchGetRecordResultDetail,

    -- ** FeatureValue
    FeatureValue (FeatureValue'),
    newFeatureValue,
  )
where

import Amazonka.SageMakerFeatureStoreRuntime.BatchGetRecord
import Amazonka.SageMakerFeatureStoreRuntime.DeleteRecord
import Amazonka.SageMakerFeatureStoreRuntime.GetRecord
import Amazonka.SageMakerFeatureStoreRuntime.Lens
import Amazonka.SageMakerFeatureStoreRuntime.PutRecord
import Amazonka.SageMakerFeatureStoreRuntime.Types
import Amazonka.SageMakerFeatureStoreRuntime.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SageMakerFeatureStoreRuntime'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

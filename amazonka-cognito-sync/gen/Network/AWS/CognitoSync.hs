{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Cognito Sync
--
-- Amazon Cognito Sync provides an AWS service and client library that
-- enable cross-device syncing of application-related user data. High-level
-- client libraries are available for both iOS and Android. You can use
-- these libraries to persist data locally so that it\'s available even if
-- the device is offline. Developer credentials don\'t need to be stored on
-- the mobile device to access the service. You can use Amazon Cognito to
-- obtain a normalized user ID and credentials. User data is persisted in a
-- dataset that can store up to 1 MB of key-value pairs, and you can have
-- up to 20 datasets per user identity.
--
-- With Amazon Cognito Sync, the data stored for each identity is
-- accessible only to credentials assigned to that identity. In order to
-- use the Cognito Sync service, you need to make API calls using
-- credentials retrieved with
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/Welcome.html Amazon Cognito Identity service>.
--
-- If you want to use Cognito Sync in an Android or iOS application, you
-- will probably want to make API calls via the AWS Mobile SDK. To learn
-- more, see the
-- <http://docs.aws.amazon.com/mobile/sdkforandroid/developerguide/cognito-sync.html Developer Guide for Android>
-- and the
-- <http://docs.aws.amazon.com/mobile/sdkforios/developerguide/cognito-sync.html Developer Guide for iOS>.
module Network.AWS.CognitoSync
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidConfigurationException
    _InvalidConfigurationException,

    -- ** ResourceConflictException
    _ResourceConflictException,

    -- ** InternalErrorException
    _InternalErrorException,

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** LambdaThrottledException
    _LambdaThrottledException,

    -- ** AlreadyStreamedException
    _AlreadyStreamedException,

    -- ** InvalidLambdaFunctionOutputException
    _InvalidLambdaFunctionOutputException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** DuplicateRequestException
    _DuplicateRequestException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** NotAuthorizedException
    _NotAuthorizedException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateRecords
    UpdateRecords (UpdateRecords'),
    newUpdateRecords,
    UpdateRecordsResponse (UpdateRecordsResponse'),
    newUpdateRecordsResponse,

    -- ** ListRecords
    ListRecords (ListRecords'),
    newListRecords,
    ListRecordsResponse (ListRecordsResponse'),
    newListRecordsResponse,

    -- ** RegisterDevice
    RegisterDevice (RegisterDevice'),
    newRegisterDevice,
    RegisterDeviceResponse (RegisterDeviceResponse'),
    newRegisterDeviceResponse,

    -- ** SubscribeToDataset
    SubscribeToDataset (SubscribeToDataset'),
    newSubscribeToDataset,
    SubscribeToDatasetResponse (SubscribeToDatasetResponse'),
    newSubscribeToDatasetResponse,

    -- ** BulkPublish
    BulkPublish (BulkPublish'),
    newBulkPublish,
    BulkPublishResponse (BulkPublishResponse'),
    newBulkPublishResponse,

    -- ** DescribeIdentityUsage
    DescribeIdentityUsage (DescribeIdentityUsage'),
    newDescribeIdentityUsage,
    DescribeIdentityUsageResponse (DescribeIdentityUsageResponse'),
    newDescribeIdentityUsageResponse,

    -- ** SetIdentityPoolConfiguration
    SetIdentityPoolConfiguration (SetIdentityPoolConfiguration'),
    newSetIdentityPoolConfiguration,
    SetIdentityPoolConfigurationResponse (SetIdentityPoolConfigurationResponse'),
    newSetIdentityPoolConfigurationResponse,

    -- ** GetBulkPublishDetails
    GetBulkPublishDetails (GetBulkPublishDetails'),
    newGetBulkPublishDetails,
    GetBulkPublishDetailsResponse (GetBulkPublishDetailsResponse'),
    newGetBulkPublishDetailsResponse,

    -- ** ListIdentityPoolUsage
    ListIdentityPoolUsage (ListIdentityPoolUsage'),
    newListIdentityPoolUsage,
    ListIdentityPoolUsageResponse (ListIdentityPoolUsageResponse'),
    newListIdentityPoolUsageResponse,

    -- ** SetCognitoEvents
    SetCognitoEvents (SetCognitoEvents'),
    newSetCognitoEvents,
    SetCognitoEventsResponse (SetCognitoEventsResponse'),
    newSetCognitoEventsResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

    -- ** UnsubscribeFromDataset
    UnsubscribeFromDataset (UnsubscribeFromDataset'),
    newUnsubscribeFromDataset,
    UnsubscribeFromDatasetResponse (UnsubscribeFromDatasetResponse'),
    newUnsubscribeFromDatasetResponse,

    -- ** GetIdentityPoolConfiguration
    GetIdentityPoolConfiguration (GetIdentityPoolConfiguration'),
    newGetIdentityPoolConfiguration,
    GetIdentityPoolConfigurationResponse (GetIdentityPoolConfigurationResponse'),
    newGetIdentityPoolConfigurationResponse,

    -- ** GetCognitoEvents
    GetCognitoEvents (GetCognitoEvents'),
    newGetCognitoEvents,
    GetCognitoEventsResponse (GetCognitoEventsResponse'),
    newGetCognitoEventsResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** ListDatasets
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** DescribeIdentityPoolUsage
    DescribeIdentityPoolUsage (DescribeIdentityPoolUsage'),
    newDescribeIdentityPoolUsage,
    DescribeIdentityPoolUsageResponse (DescribeIdentityPoolUsageResponse'),
    newDescribeIdentityPoolUsageResponse,

    -- * Types

    -- ** BulkPublishStatus
    BulkPublishStatus (..),

    -- ** Operation
    Operation (..),

    -- ** Platform
    Platform (..),

    -- ** StreamingStatus
    StreamingStatus (..),

    -- ** CognitoStreams
    CognitoStreams (CognitoStreams'),
    newCognitoStreams,

    -- ** Dataset
    Dataset (Dataset'),
    newDataset,

    -- ** IdentityPoolUsage
    IdentityPoolUsage (IdentityPoolUsage'),
    newIdentityPoolUsage,

    -- ** IdentityUsage
    IdentityUsage (IdentityUsage'),
    newIdentityUsage,

    -- ** PushSync
    PushSync (PushSync'),
    newPushSync,

    -- ** Record
    Record (Record'),
    newRecord,

    -- ** RecordPatch
    RecordPatch (RecordPatch'),
    newRecordPatch,
  )
where

import Network.AWS.CognitoSync.BulkPublish
import Network.AWS.CognitoSync.DeleteDataset
import Network.AWS.CognitoSync.DescribeDataset
import Network.AWS.CognitoSync.DescribeIdentityPoolUsage
import Network.AWS.CognitoSync.DescribeIdentityUsage
import Network.AWS.CognitoSync.GetBulkPublishDetails
import Network.AWS.CognitoSync.GetCognitoEvents
import Network.AWS.CognitoSync.GetIdentityPoolConfiguration
import Network.AWS.CognitoSync.Lens
import Network.AWS.CognitoSync.ListDatasets
import Network.AWS.CognitoSync.ListIdentityPoolUsage
import Network.AWS.CognitoSync.ListRecords
import Network.AWS.CognitoSync.RegisterDevice
import Network.AWS.CognitoSync.SetCognitoEvents
import Network.AWS.CognitoSync.SetIdentityPoolConfiguration
import Network.AWS.CognitoSync.SubscribeToDataset
import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.UnsubscribeFromDataset
import Network.AWS.CognitoSync.UpdateRecords
import Network.AWS.CognitoSync.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CognitoSync'.

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

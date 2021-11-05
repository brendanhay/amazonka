{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoSync.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Lens
  ( -- * Operations

    -- ** DescribeDataset
    describeDataset_identityPoolId,
    describeDataset_identityId,
    describeDataset_datasetName,
    describeDatasetResponse_dataset,
    describeDatasetResponse_httpStatus,

    -- ** SetCognitoEvents
    setCognitoEvents_identityPoolId,
    setCognitoEvents_events,

    -- ** DescribeIdentityPoolUsage
    describeIdentityPoolUsage_identityPoolId,
    describeIdentityPoolUsageResponse_identityPoolUsage,
    describeIdentityPoolUsageResponse_httpStatus,

    -- ** GetBulkPublishDetails
    getBulkPublishDetails_identityPoolId,
    getBulkPublishDetailsResponse_bulkPublishStartTime,
    getBulkPublishDetailsResponse_identityPoolId,
    getBulkPublishDetailsResponse_bulkPublishCompleteTime,
    getBulkPublishDetailsResponse_failureMessage,
    getBulkPublishDetailsResponse_bulkPublishStatus,
    getBulkPublishDetailsResponse_httpStatus,

    -- ** ListIdentityPoolUsage
    listIdentityPoolUsage_nextToken,
    listIdentityPoolUsage_maxResults,
    listIdentityPoolUsageResponse_identityPoolUsages,
    listIdentityPoolUsageResponse_count,
    listIdentityPoolUsageResponse_nextToken,
    listIdentityPoolUsageResponse_maxResults,
    listIdentityPoolUsageResponse_httpStatus,

    -- ** SetIdentityPoolConfiguration
    setIdentityPoolConfiguration_cognitoStreams,
    setIdentityPoolConfiguration_pushSync,
    setIdentityPoolConfiguration_identityPoolId,
    setIdentityPoolConfigurationResponse_identityPoolId,
    setIdentityPoolConfigurationResponse_cognitoStreams,
    setIdentityPoolConfigurationResponse_pushSync,
    setIdentityPoolConfigurationResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_identityPoolId,
    deleteDataset_identityId,
    deleteDataset_datasetName,
    deleteDatasetResponse_dataset,
    deleteDatasetResponse_httpStatus,

    -- ** GetCognitoEvents
    getCognitoEvents_identityPoolId,
    getCognitoEventsResponse_events,
    getCognitoEventsResponse_httpStatus,

    -- ** DescribeIdentityUsage
    describeIdentityUsage_identityPoolId,
    describeIdentityUsage_identityId,
    describeIdentityUsageResponse_identityUsage,
    describeIdentityUsageResponse_httpStatus,

    -- ** RegisterDevice
    registerDevice_identityPoolId,
    registerDevice_identityId,
    registerDevice_platform,
    registerDevice_token,
    registerDeviceResponse_deviceId,
    registerDeviceResponse_httpStatus,

    -- ** SubscribeToDataset
    subscribeToDataset_identityPoolId,
    subscribeToDataset_identityId,
    subscribeToDataset_datasetName,
    subscribeToDataset_deviceId,
    subscribeToDatasetResponse_httpStatus,

    -- ** GetIdentityPoolConfiguration
    getIdentityPoolConfiguration_identityPoolId,
    getIdentityPoolConfigurationResponse_identityPoolId,
    getIdentityPoolConfigurationResponse_cognitoStreams,
    getIdentityPoolConfigurationResponse_pushSync,
    getIdentityPoolConfigurationResponse_httpStatus,

    -- ** ListRecords
    listRecords_lastSyncCount,
    listRecords_nextToken,
    listRecords_syncSessionToken,
    listRecords_maxResults,
    listRecords_identityPoolId,
    listRecords_identityId,
    listRecords_datasetName,
    listRecordsResponse_datasetDeletedAfterRequestedSyncCount,
    listRecordsResponse_datasetExists,
    listRecordsResponse_count,
    listRecordsResponse_records,
    listRecordsResponse_nextToken,
    listRecordsResponse_mergedDatasetNames,
    listRecordsResponse_syncSessionToken,
    listRecordsResponse_lastModifiedBy,
    listRecordsResponse_datasetSyncCount,
    listRecordsResponse_httpStatus,

    -- ** UnsubscribeFromDataset
    unsubscribeFromDataset_identityPoolId,
    unsubscribeFromDataset_identityId,
    unsubscribeFromDataset_datasetName,
    unsubscribeFromDataset_deviceId,
    unsubscribeFromDatasetResponse_httpStatus,

    -- ** UpdateRecords
    updateRecords_recordPatches,
    updateRecords_deviceId,
    updateRecords_clientContext,
    updateRecords_identityPoolId,
    updateRecords_identityId,
    updateRecords_datasetName,
    updateRecords_syncSessionToken,
    updateRecordsResponse_records,
    updateRecordsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasets_identityId,
    listDatasets_identityPoolId,
    listDatasetsResponse_count,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_httpStatus,

    -- ** BulkPublish
    bulkPublish_identityPoolId,
    bulkPublishResponse_identityPoolId,
    bulkPublishResponse_httpStatus,

    -- * Types

    -- ** CognitoStreams
    cognitoStreams_streamingStatus,
    cognitoStreams_streamName,
    cognitoStreams_roleArn,

    -- ** Dataset
    dataset_lastModifiedDate,
    dataset_numRecords,
    dataset_dataStorage,
    dataset_datasetName,
    dataset_creationDate,
    dataset_lastModifiedBy,
    dataset_identityId,

    -- ** IdentityPoolUsage
    identityPoolUsage_lastModifiedDate,
    identityPoolUsage_identityPoolId,
    identityPoolUsage_dataStorage,
    identityPoolUsage_syncSessionsCount,

    -- ** IdentityUsage
    identityUsage_lastModifiedDate,
    identityUsage_identityPoolId,
    identityUsage_datasetCount,
    identityUsage_dataStorage,
    identityUsage_identityId,

    -- ** PushSync
    pushSync_applicationArns,
    pushSync_roleArn,

    -- ** Record
    record_syncCount,
    record_deviceLastModifiedDate,
    record_lastModifiedDate,
    record_value,
    record_key,
    record_lastModifiedBy,

    -- ** RecordPatch
    recordPatch_deviceLastModifiedDate,
    recordPatch_value,
    recordPatch_op,
    recordPatch_key,
    recordPatch_syncCount,
  )
where

import Amazonka.CognitoSync.BulkPublish
import Amazonka.CognitoSync.DeleteDataset
import Amazonka.CognitoSync.DescribeDataset
import Amazonka.CognitoSync.DescribeIdentityPoolUsage
import Amazonka.CognitoSync.DescribeIdentityUsage
import Amazonka.CognitoSync.GetBulkPublishDetails
import Amazonka.CognitoSync.GetCognitoEvents
import Amazonka.CognitoSync.GetIdentityPoolConfiguration
import Amazonka.CognitoSync.ListDatasets
import Amazonka.CognitoSync.ListIdentityPoolUsage
import Amazonka.CognitoSync.ListRecords
import Amazonka.CognitoSync.RegisterDevice
import Amazonka.CognitoSync.SetCognitoEvents
import Amazonka.CognitoSync.SetIdentityPoolConfiguration
import Amazonka.CognitoSync.SubscribeToDataset
import Amazonka.CognitoSync.Types.CognitoStreams
import Amazonka.CognitoSync.Types.Dataset
import Amazonka.CognitoSync.Types.IdentityPoolUsage
import Amazonka.CognitoSync.Types.IdentityUsage
import Amazonka.CognitoSync.Types.PushSync
import Amazonka.CognitoSync.Types.Record
import Amazonka.CognitoSync.Types.RecordPatch
import Amazonka.CognitoSync.UnsubscribeFromDataset
import Amazonka.CognitoSync.UpdateRecords

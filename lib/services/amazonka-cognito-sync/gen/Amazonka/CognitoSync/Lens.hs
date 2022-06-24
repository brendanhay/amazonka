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

    -- ** BulkPublish
    bulkPublish_identityPoolId,
    bulkPublishResponse_identityPoolId,
    bulkPublishResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_identityPoolId,
    deleteDataset_identityId,
    deleteDataset_datasetName,
    deleteDatasetResponse_dataset,
    deleteDatasetResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_identityPoolId,
    describeDataset_identityId,
    describeDataset_datasetName,
    describeDatasetResponse_dataset,
    describeDatasetResponse_httpStatus,

    -- ** DescribeIdentityPoolUsage
    describeIdentityPoolUsage_identityPoolId,
    describeIdentityPoolUsageResponse_identityPoolUsage,
    describeIdentityPoolUsageResponse_httpStatus,

    -- ** DescribeIdentityUsage
    describeIdentityUsage_identityPoolId,
    describeIdentityUsage_identityId,
    describeIdentityUsageResponse_identityUsage,
    describeIdentityUsageResponse_httpStatus,

    -- ** GetBulkPublishDetails
    getBulkPublishDetails_identityPoolId,
    getBulkPublishDetailsResponse_bulkPublishCompleteTime,
    getBulkPublishDetailsResponse_failureMessage,
    getBulkPublishDetailsResponse_identityPoolId,
    getBulkPublishDetailsResponse_bulkPublishStatus,
    getBulkPublishDetailsResponse_bulkPublishStartTime,
    getBulkPublishDetailsResponse_httpStatus,

    -- ** GetCognitoEvents
    getCognitoEvents_identityPoolId,
    getCognitoEventsResponse_events,
    getCognitoEventsResponse_httpStatus,

    -- ** GetIdentityPoolConfiguration
    getIdentityPoolConfiguration_identityPoolId,
    getIdentityPoolConfigurationResponse_cognitoStreams,
    getIdentityPoolConfigurationResponse_identityPoolId,
    getIdentityPoolConfigurationResponse_pushSync,
    getIdentityPoolConfigurationResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasets_identityId,
    listDatasets_identityPoolId,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_count,
    listDatasetsResponse_httpStatus,

    -- ** ListIdentityPoolUsage
    listIdentityPoolUsage_nextToken,
    listIdentityPoolUsage_maxResults,
    listIdentityPoolUsageResponse_nextToken,
    listIdentityPoolUsageResponse_count,
    listIdentityPoolUsageResponse_maxResults,
    listIdentityPoolUsageResponse_identityPoolUsages,
    listIdentityPoolUsageResponse_httpStatus,

    -- ** ListRecords
    listRecords_nextToken,
    listRecords_syncSessionToken,
    listRecords_lastSyncCount,
    listRecords_maxResults,
    listRecords_identityPoolId,
    listRecords_identityId,
    listRecords_datasetName,
    listRecordsResponse_records,
    listRecordsResponse_nextToken,
    listRecordsResponse_datasetExists,
    listRecordsResponse_syncSessionToken,
    listRecordsResponse_mergedDatasetNames,
    listRecordsResponse_datasetSyncCount,
    listRecordsResponse_count,
    listRecordsResponse_lastModifiedBy,
    listRecordsResponse_datasetDeletedAfterRequestedSyncCount,
    listRecordsResponse_httpStatus,

    -- ** RegisterDevice
    registerDevice_identityPoolId,
    registerDevice_identityId,
    registerDevice_platform,
    registerDevice_token,
    registerDeviceResponse_deviceId,
    registerDeviceResponse_httpStatus,

    -- ** SetCognitoEvents
    setCognitoEvents_identityPoolId,
    setCognitoEvents_events,

    -- ** SetIdentityPoolConfiguration
    setIdentityPoolConfiguration_cognitoStreams,
    setIdentityPoolConfiguration_pushSync,
    setIdentityPoolConfiguration_identityPoolId,
    setIdentityPoolConfigurationResponse_cognitoStreams,
    setIdentityPoolConfigurationResponse_identityPoolId,
    setIdentityPoolConfigurationResponse_pushSync,
    setIdentityPoolConfigurationResponse_httpStatus,

    -- ** SubscribeToDataset
    subscribeToDataset_identityPoolId,
    subscribeToDataset_identityId,
    subscribeToDataset_datasetName,
    subscribeToDataset_deviceId,
    subscribeToDatasetResponse_httpStatus,

    -- ** UnsubscribeFromDataset
    unsubscribeFromDataset_identityPoolId,
    unsubscribeFromDataset_identityId,
    unsubscribeFromDataset_datasetName,
    unsubscribeFromDataset_deviceId,
    unsubscribeFromDatasetResponse_httpStatus,

    -- ** UpdateRecords
    updateRecords_deviceId,
    updateRecords_clientContext,
    updateRecords_recordPatches,
    updateRecords_identityPoolId,
    updateRecords_identityId,
    updateRecords_datasetName,
    updateRecords_syncSessionToken,
    updateRecordsResponse_records,
    updateRecordsResponse_httpStatus,

    -- * Types

    -- ** CognitoStreams
    cognitoStreams_roleArn,
    cognitoStreams_streamingStatus,
    cognitoStreams_streamName,

    -- ** Dataset
    dataset_numRecords,
    dataset_lastModifiedDate,
    dataset_dataStorage,
    dataset_datasetName,
    dataset_creationDate,
    dataset_lastModifiedBy,
    dataset_identityId,

    -- ** IdentityPoolUsage
    identityPoolUsage_lastModifiedDate,
    identityPoolUsage_dataStorage,
    identityPoolUsage_syncSessionsCount,
    identityPoolUsage_identityPoolId,

    -- ** IdentityUsage
    identityUsage_datasetCount,
    identityUsage_lastModifiedDate,
    identityUsage_dataStorage,
    identityUsage_identityPoolId,
    identityUsage_identityId,

    -- ** PushSync
    pushSync_roleArn,
    pushSync_applicationArns,

    -- ** Record
    record_key,
    record_lastModifiedDate,
    record_lastModifiedBy,
    record_syncCount,
    record_deviceLastModifiedDate,
    record_value,

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

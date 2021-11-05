{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.HealthLake.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Lens
  ( -- * Operations

    -- ** StartFHIRImportJob
    startFHIRImportJob_jobName,
    startFHIRImportJob_inputDataConfig,
    startFHIRImportJob_jobOutputDataConfig,
    startFHIRImportJob_datastoreId,
    startFHIRImportJob_dataAccessRoleArn,
    startFHIRImportJob_clientToken,
    startFHIRImportJobResponse_datastoreId,
    startFHIRImportJobResponse_httpStatus,
    startFHIRImportJobResponse_jobId,
    startFHIRImportJobResponse_jobStatus,

    -- ** DescribeFHIRDatastore
    describeFHIRDatastore_datastoreId,
    describeFHIRDatastoreResponse_httpStatus,
    describeFHIRDatastoreResponse_datastoreProperties,

    -- ** DescribeFHIRImportJob
    describeFHIRImportJob_datastoreId,
    describeFHIRImportJob_jobId,
    describeFHIRImportJobResponse_httpStatus,
    describeFHIRImportJobResponse_importJobProperties,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteFHIRDatastore
    deleteFHIRDatastore_datastoreId,
    deleteFHIRDatastoreResponse_httpStatus,
    deleteFHIRDatastoreResponse_datastoreId,
    deleteFHIRDatastoreResponse_datastoreArn,
    deleteFHIRDatastoreResponse_datastoreStatus,
    deleteFHIRDatastoreResponse_datastoreEndpoint,

    -- ** DescribeFHIRExportJob
    describeFHIRExportJob_datastoreId,
    describeFHIRExportJob_jobId,
    describeFHIRExportJobResponse_httpStatus,
    describeFHIRExportJobResponse_exportJobProperties,

    -- ** CreateFHIRDatastore
    createFHIRDatastore_clientToken,
    createFHIRDatastore_sseConfiguration,
    createFHIRDatastore_datastoreName,
    createFHIRDatastore_preloadDataConfig,
    createFHIRDatastore_tags,
    createFHIRDatastore_datastoreTypeVersion,
    createFHIRDatastoreResponse_httpStatus,
    createFHIRDatastoreResponse_datastoreId,
    createFHIRDatastoreResponse_datastoreArn,
    createFHIRDatastoreResponse_datastoreStatus,
    createFHIRDatastoreResponse_datastoreEndpoint,

    -- ** ListFHIRExportJobs
    listFHIRExportJobs_submittedAfter,
    listFHIRExportJobs_jobName,
    listFHIRExportJobs_submittedBefore,
    listFHIRExportJobs_nextToken,
    listFHIRExportJobs_jobStatus,
    listFHIRExportJobs_maxResults,
    listFHIRExportJobs_datastoreId,
    listFHIRExportJobsResponse_nextToken,
    listFHIRExportJobsResponse_httpStatus,
    listFHIRExportJobsResponse_exportJobPropertiesList,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListFHIRDatastores
    listFHIRDatastores_nextToken,
    listFHIRDatastores_filter,
    listFHIRDatastores_maxResults,
    listFHIRDatastoresResponse_nextToken,
    listFHIRDatastoresResponse_httpStatus,
    listFHIRDatastoresResponse_datastorePropertiesList,

    -- ** StartFHIRExportJob
    startFHIRExportJob_jobName,
    startFHIRExportJob_outputDataConfig,
    startFHIRExportJob_datastoreId,
    startFHIRExportJob_dataAccessRoleArn,
    startFHIRExportJob_clientToken,
    startFHIRExportJobResponse_datastoreId,
    startFHIRExportJobResponse_httpStatus,
    startFHIRExportJobResponse_jobId,
    startFHIRExportJobResponse_jobStatus,

    -- ** ListFHIRImportJobs
    listFHIRImportJobs_submittedAfter,
    listFHIRImportJobs_jobName,
    listFHIRImportJobs_submittedBefore,
    listFHIRImportJobs_nextToken,
    listFHIRImportJobs_jobStatus,
    listFHIRImportJobs_maxResults,
    listFHIRImportJobs_datastoreId,
    listFHIRImportJobsResponse_nextToken,
    listFHIRImportJobsResponse_httpStatus,
    listFHIRImportJobsResponse_importJobPropertiesList,

    -- * Types

    -- ** DatastoreFilter
    datastoreFilter_createdAfter,
    datastoreFilter_datastoreName,
    datastoreFilter_datastoreStatus,
    datastoreFilter_createdBefore,

    -- ** DatastoreProperties
    datastoreProperties_sseConfiguration,
    datastoreProperties_createdAt,
    datastoreProperties_datastoreName,
    datastoreProperties_preloadDataConfig,
    datastoreProperties_datastoreId,
    datastoreProperties_datastoreArn,
    datastoreProperties_datastoreStatus,
    datastoreProperties_datastoreTypeVersion,
    datastoreProperties_datastoreEndpoint,

    -- ** ExportJobProperties
    exportJobProperties_jobName,
    exportJobProperties_endTime,
    exportJobProperties_dataAccessRoleArn,
    exportJobProperties_message,
    exportJobProperties_jobId,
    exportJobProperties_jobStatus,
    exportJobProperties_submitTime,
    exportJobProperties_datastoreId,
    exportJobProperties_outputDataConfig,

    -- ** ImportJobProperties
    importJobProperties_jobOutputDataConfig,
    importJobProperties_jobName,
    importJobProperties_endTime,
    importJobProperties_dataAccessRoleArn,
    importJobProperties_message,
    importJobProperties_jobId,
    importJobProperties_jobStatus,
    importJobProperties_submitTime,
    importJobProperties_datastoreId,
    importJobProperties_inputDataConfig,

    -- ** InputDataConfig
    inputDataConfig_s3Uri,

    -- ** KmsEncryptionConfig
    kmsEncryptionConfig_kmsKeyId,
    kmsEncryptionConfig_cmkType,

    -- ** OutputDataConfig
    outputDataConfig_s3Configuration,

    -- ** PreloadDataConfig
    preloadDataConfig_preloadDataType,

    -- ** S3Configuration
    s3Configuration_s3Uri,
    s3Configuration_kmsKeyId,

    -- ** SseConfiguration
    sseConfiguration_kmsEncryptionConfig,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.HealthLake.CreateFHIRDatastore
import Amazonka.HealthLake.DeleteFHIRDatastore
import Amazonka.HealthLake.DescribeFHIRDatastore
import Amazonka.HealthLake.DescribeFHIRExportJob
import Amazonka.HealthLake.DescribeFHIRImportJob
import Amazonka.HealthLake.ListFHIRDatastores
import Amazonka.HealthLake.ListFHIRExportJobs
import Amazonka.HealthLake.ListFHIRImportJobs
import Amazonka.HealthLake.ListTagsForResource
import Amazonka.HealthLake.StartFHIRExportJob
import Amazonka.HealthLake.StartFHIRImportJob
import Amazonka.HealthLake.TagResource
import Amazonka.HealthLake.Types.DatastoreFilter
import Amazonka.HealthLake.Types.DatastoreProperties
import Amazonka.HealthLake.Types.ExportJobProperties
import Amazonka.HealthLake.Types.ImportJobProperties
import Amazonka.HealthLake.Types.InputDataConfig
import Amazonka.HealthLake.Types.KmsEncryptionConfig
import Amazonka.HealthLake.Types.OutputDataConfig
import Amazonka.HealthLake.Types.PreloadDataConfig
import Amazonka.HealthLake.Types.S3Configuration
import Amazonka.HealthLake.Types.SseConfiguration
import Amazonka.HealthLake.Types.Tag
import Amazonka.HealthLake.UntagResource

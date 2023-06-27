{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TimeStreamWrite.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Lens
  ( -- * Operations

    -- ** CreateBatchLoadTask
    createBatchLoadTask_clientToken,
    createBatchLoadTask_dataModelConfiguration,
    createBatchLoadTask_recordVersion,
    createBatchLoadTask_dataSourceConfiguration,
    createBatchLoadTask_reportConfiguration,
    createBatchLoadTask_targetDatabaseName,
    createBatchLoadTask_targetTableName,
    createBatchLoadTaskResponse_httpStatus,
    createBatchLoadTaskResponse_taskId,

    -- ** CreateDatabase
    createDatabase_kmsKeyId,
    createDatabase_tags,
    createDatabase_databaseName,
    createDatabaseResponse_database,
    createDatabaseResponse_httpStatus,

    -- ** CreateTable
    createTable_magneticStoreWriteProperties,
    createTable_retentionProperties,
    createTable_schema,
    createTable_tags,
    createTable_databaseName,
    createTable_tableName,
    createTableResponse_table,
    createTableResponse_httpStatus,

    -- ** DeleteDatabase
    deleteDatabase_databaseName,

    -- ** DeleteTable
    deleteTable_databaseName,
    deleteTable_tableName,

    -- ** DescribeBatchLoadTask
    describeBatchLoadTask_taskId,
    describeBatchLoadTaskResponse_httpStatus,
    describeBatchLoadTaskResponse_batchLoadTaskDescription,

    -- ** DescribeDatabase
    describeDatabase_databaseName,
    describeDatabaseResponse_database,
    describeDatabaseResponse_httpStatus,

    -- ** DescribeEndpoints
    describeEndpointsResponse_httpStatus,
    describeEndpointsResponse_endpoints,

    -- ** DescribeTable
    describeTable_databaseName,
    describeTable_tableName,
    describeTableResponse_table,
    describeTableResponse_httpStatus,

    -- ** ListBatchLoadTasks
    listBatchLoadTasks_maxResults,
    listBatchLoadTasks_nextToken,
    listBatchLoadTasks_taskStatus,
    listBatchLoadTasksResponse_batchLoadTasks,
    listBatchLoadTasksResponse_nextToken,
    listBatchLoadTasksResponse_httpStatus,

    -- ** ListDatabases
    listDatabases_maxResults,
    listDatabases_nextToken,
    listDatabasesResponse_databases,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_httpStatus,

    -- ** ListTables
    listTables_databaseName,
    listTables_maxResults,
    listTables_nextToken,
    listTablesResponse_nextToken,
    listTablesResponse_tables,
    listTablesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ResumeBatchLoadTask
    resumeBatchLoadTask_taskId,
    resumeBatchLoadTaskResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDatabase
    updateDatabase_databaseName,
    updateDatabase_kmsKeyId,
    updateDatabaseResponse_database,
    updateDatabaseResponse_httpStatus,

    -- ** UpdateTable
    updateTable_magneticStoreWriteProperties,
    updateTable_retentionProperties,
    updateTable_schema,
    updateTable_databaseName,
    updateTable_tableName,
    updateTableResponse_table,
    updateTableResponse_httpStatus,

    -- ** WriteRecords
    writeRecords_commonAttributes,
    writeRecords_databaseName,
    writeRecords_tableName,
    writeRecords_records,
    writeRecordsResponse_recordsIngested,
    writeRecordsResponse_httpStatus,

    -- * Types

    -- ** BatchLoadProgressReport
    batchLoadProgressReport_bytesMetered,
    batchLoadProgressReport_fileFailures,
    batchLoadProgressReport_parseFailures,
    batchLoadProgressReport_recordIngestionFailures,
    batchLoadProgressReport_recordsIngested,
    batchLoadProgressReport_recordsProcessed,

    -- ** BatchLoadTask
    batchLoadTask_creationTime,
    batchLoadTask_databaseName,
    batchLoadTask_lastUpdatedTime,
    batchLoadTask_resumableUntil,
    batchLoadTask_tableName,
    batchLoadTask_taskId,
    batchLoadTask_taskStatus,

    -- ** BatchLoadTaskDescription
    batchLoadTaskDescription_creationTime,
    batchLoadTaskDescription_dataModelConfiguration,
    batchLoadTaskDescription_dataSourceConfiguration,
    batchLoadTaskDescription_errorMessage,
    batchLoadTaskDescription_lastUpdatedTime,
    batchLoadTaskDescription_progressReport,
    batchLoadTaskDescription_recordVersion,
    batchLoadTaskDescription_reportConfiguration,
    batchLoadTaskDescription_resumableUntil,
    batchLoadTaskDescription_targetDatabaseName,
    batchLoadTaskDescription_targetTableName,
    batchLoadTaskDescription_taskId,
    batchLoadTaskDescription_taskStatus,

    -- ** CsvConfiguration
    csvConfiguration_columnSeparator,
    csvConfiguration_escapeChar,
    csvConfiguration_nullValue,
    csvConfiguration_quoteChar,
    csvConfiguration_trimWhiteSpace,

    -- ** DataModel
    dataModel_measureNameColumn,
    dataModel_mixedMeasureMappings,
    dataModel_multiMeasureMappings,
    dataModel_timeColumn,
    dataModel_timeUnit,
    dataModel_dimensionMappings,

    -- ** DataModelConfiguration
    dataModelConfiguration_dataModel,
    dataModelConfiguration_dataModelS3Configuration,

    -- ** DataModelS3Configuration
    dataModelS3Configuration_bucketName,
    dataModelS3Configuration_objectKey,

    -- ** DataSourceConfiguration
    dataSourceConfiguration_csvConfiguration,
    dataSourceConfiguration_dataSourceS3Configuration,
    dataSourceConfiguration_dataFormat,

    -- ** DataSourceS3Configuration
    dataSourceS3Configuration_objectKeyPrefix,
    dataSourceS3Configuration_bucketName,

    -- ** Database
    database_arn,
    database_creationTime,
    database_databaseName,
    database_kmsKeyId,
    database_lastUpdatedTime,
    database_tableCount,

    -- ** Dimension
    dimension_dimensionValueType,
    dimension_name,
    dimension_value,

    -- ** DimensionMapping
    dimensionMapping_destinationColumn,
    dimensionMapping_sourceColumn,

    -- ** Endpoint
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- ** MagneticStoreRejectedDataLocation
    magneticStoreRejectedDataLocation_s3Configuration,

    -- ** MagneticStoreWriteProperties
    magneticStoreWriteProperties_magneticStoreRejectedDataLocation,
    magneticStoreWriteProperties_enableMagneticStoreWrites,

    -- ** MeasureValue
    measureValue_name,
    measureValue_value,
    measureValue_type,

    -- ** MixedMeasureMapping
    mixedMeasureMapping_measureName,
    mixedMeasureMapping_multiMeasureAttributeMappings,
    mixedMeasureMapping_sourceColumn,
    mixedMeasureMapping_targetMeasureName,
    mixedMeasureMapping_measureValueType,

    -- ** MultiMeasureAttributeMapping
    multiMeasureAttributeMapping_measureValueType,
    multiMeasureAttributeMapping_targetMultiMeasureAttributeName,
    multiMeasureAttributeMapping_sourceColumn,

    -- ** MultiMeasureMappings
    multiMeasureMappings_targetMultiMeasureName,
    multiMeasureMappings_multiMeasureAttributeMappings,

    -- ** PartitionKey
    partitionKey_enforcementInRecord,
    partitionKey_name,
    partitionKey_type,

    -- ** Record
    record_dimensions,
    record_measureName,
    record_measureValue,
    record_measureValueType,
    record_measureValues,
    record_time,
    record_timeUnit,
    record_version,

    -- ** RecordsIngested
    recordsIngested_magneticStore,
    recordsIngested_memoryStore,
    recordsIngested_total,

    -- ** ReportConfiguration
    reportConfiguration_reportS3Configuration,

    -- ** ReportS3Configuration
    reportS3Configuration_encryptionOption,
    reportS3Configuration_kmsKeyId,
    reportS3Configuration_objectKeyPrefix,
    reportS3Configuration_bucketName,

    -- ** RetentionProperties
    retentionProperties_memoryStoreRetentionPeriodInHours,
    retentionProperties_magneticStoreRetentionPeriodInDays,

    -- ** S3Configuration
    s3Configuration_bucketName,
    s3Configuration_encryptionOption,
    s3Configuration_kmsKeyId,
    s3Configuration_objectKeyPrefix,

    -- ** Schema
    schema_compositePartitionKey,

    -- ** Table
    table_arn,
    table_creationTime,
    table_databaseName,
    table_lastUpdatedTime,
    table_magneticStoreWriteProperties,
    table_retentionProperties,
    table_schema,
    table_tableName,
    table_tableStatus,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.TimeStreamWrite.CreateBatchLoadTask
import Amazonka.TimeStreamWrite.CreateDatabase
import Amazonka.TimeStreamWrite.CreateTable
import Amazonka.TimeStreamWrite.DeleteDatabase
import Amazonka.TimeStreamWrite.DeleteTable
import Amazonka.TimeStreamWrite.DescribeBatchLoadTask
import Amazonka.TimeStreamWrite.DescribeDatabase
import Amazonka.TimeStreamWrite.DescribeEndpoints
import Amazonka.TimeStreamWrite.DescribeTable
import Amazonka.TimeStreamWrite.ListBatchLoadTasks
import Amazonka.TimeStreamWrite.ListDatabases
import Amazonka.TimeStreamWrite.ListTables
import Amazonka.TimeStreamWrite.ListTagsForResource
import Amazonka.TimeStreamWrite.ResumeBatchLoadTask
import Amazonka.TimeStreamWrite.TagResource
import Amazonka.TimeStreamWrite.Types.BatchLoadProgressReport
import Amazonka.TimeStreamWrite.Types.BatchLoadTask
import Amazonka.TimeStreamWrite.Types.BatchLoadTaskDescription
import Amazonka.TimeStreamWrite.Types.CsvConfiguration
import Amazonka.TimeStreamWrite.Types.DataModel
import Amazonka.TimeStreamWrite.Types.DataModelConfiguration
import Amazonka.TimeStreamWrite.Types.DataModelS3Configuration
import Amazonka.TimeStreamWrite.Types.DataSourceConfiguration
import Amazonka.TimeStreamWrite.Types.DataSourceS3Configuration
import Amazonka.TimeStreamWrite.Types.Database
import Amazonka.TimeStreamWrite.Types.Dimension
import Amazonka.TimeStreamWrite.Types.DimensionMapping
import Amazonka.TimeStreamWrite.Types.Endpoint
import Amazonka.TimeStreamWrite.Types.MagneticStoreRejectedDataLocation
import Amazonka.TimeStreamWrite.Types.MagneticStoreWriteProperties
import Amazonka.TimeStreamWrite.Types.MeasureValue
import Amazonka.TimeStreamWrite.Types.MixedMeasureMapping
import Amazonka.TimeStreamWrite.Types.MultiMeasureAttributeMapping
import Amazonka.TimeStreamWrite.Types.MultiMeasureMappings
import Amazonka.TimeStreamWrite.Types.PartitionKey
import Amazonka.TimeStreamWrite.Types.Record
import Amazonka.TimeStreamWrite.Types.RecordsIngested
import Amazonka.TimeStreamWrite.Types.ReportConfiguration
import Amazonka.TimeStreamWrite.Types.ReportS3Configuration
import Amazonka.TimeStreamWrite.Types.RetentionProperties
import Amazonka.TimeStreamWrite.Types.S3Configuration
import Amazonka.TimeStreamWrite.Types.Schema
import Amazonka.TimeStreamWrite.Types.Table
import Amazonka.TimeStreamWrite.Types.Tag
import Amazonka.TimeStreamWrite.UntagResource
import Amazonka.TimeStreamWrite.UpdateDatabase
import Amazonka.TimeStreamWrite.UpdateTable
import Amazonka.TimeStreamWrite.WriteRecords

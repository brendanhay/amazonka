{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TimeStreamWrite.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Lens
  ( -- * Operations

    -- ** CreateDatabase
    createDatabase_tags,
    createDatabase_kmsKeyId,
    createDatabase_databaseName,
    createDatabaseResponse_database,
    createDatabaseResponse_httpStatus,

    -- ** CreateTable
    createTable_tags,
    createTable_retentionProperties,
    createTable_magneticStoreWriteProperties,
    createTable_databaseName,
    createTable_tableName,
    createTableResponse_table,
    createTableResponse_httpStatus,

    -- ** DeleteDatabase
    deleteDatabase_databaseName,

    -- ** DeleteTable
    deleteTable_databaseName,
    deleteTable_tableName,

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

    -- ** ListDatabases
    listDatabases_nextToken,
    listDatabases_maxResults,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_databases,
    listDatabasesResponse_httpStatus,

    -- ** ListTables
    listTables_nextToken,
    listTables_databaseName,
    listTables_maxResults,
    listTablesResponse_tables,
    listTablesResponse_nextToken,
    listTablesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

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
    updateTable_retentionProperties,
    updateTable_magneticStoreWriteProperties,
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

    -- ** Database
    database_databaseName,
    database_arn,
    database_lastUpdatedTime,
    database_kmsKeyId,
    database_creationTime,
    database_tableCount,

    -- ** Dimension
    dimension_dimensionValueType,
    dimension_name,
    dimension_value,

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

    -- ** Record
    record_timeUnit,
    record_measureValue,
    record_measureValues,
    record_time,
    record_dimensions,
    record_measureName,
    record_measureValueType,
    record_version,

    -- ** RecordsIngested
    recordsIngested_total,
    recordsIngested_magneticStore,
    recordsIngested_memoryStore,

    -- ** RetentionProperties
    retentionProperties_memoryStoreRetentionPeriodInHours,
    retentionProperties_magneticStoreRetentionPeriodInDays,

    -- ** S3Configuration
    s3Configuration_objectKeyPrefix,
    s3Configuration_encryptionOption,
    s3Configuration_bucketName,
    s3Configuration_kmsKeyId,

    -- ** Table
    table_tableName,
    table_tableStatus,
    table_databaseName,
    table_arn,
    table_lastUpdatedTime,
    table_retentionProperties,
    table_magneticStoreWriteProperties,
    table_creationTime,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.TimeStreamWrite.CreateDatabase
import Amazonka.TimeStreamWrite.CreateTable
import Amazonka.TimeStreamWrite.DeleteDatabase
import Amazonka.TimeStreamWrite.DeleteTable
import Amazonka.TimeStreamWrite.DescribeDatabase
import Amazonka.TimeStreamWrite.DescribeEndpoints
import Amazonka.TimeStreamWrite.DescribeTable
import Amazonka.TimeStreamWrite.ListDatabases
import Amazonka.TimeStreamWrite.ListTables
import Amazonka.TimeStreamWrite.ListTagsForResource
import Amazonka.TimeStreamWrite.TagResource
import Amazonka.TimeStreamWrite.Types.Database
import Amazonka.TimeStreamWrite.Types.Dimension
import Amazonka.TimeStreamWrite.Types.Endpoint
import Amazonka.TimeStreamWrite.Types.MagneticStoreRejectedDataLocation
import Amazonka.TimeStreamWrite.Types.MagneticStoreWriteProperties
import Amazonka.TimeStreamWrite.Types.MeasureValue
import Amazonka.TimeStreamWrite.Types.Record
import Amazonka.TimeStreamWrite.Types.RecordsIngested
import Amazonka.TimeStreamWrite.Types.RetentionProperties
import Amazonka.TimeStreamWrite.Types.S3Configuration
import Amazonka.TimeStreamWrite.Types.Table
import Amazonka.TimeStreamWrite.Types.Tag
import Amazonka.TimeStreamWrite.UntagResource
import Amazonka.TimeStreamWrite.UpdateDatabase
import Amazonka.TimeStreamWrite.UpdateTable
import Amazonka.TimeStreamWrite.WriteRecords

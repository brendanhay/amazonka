{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KeySpaces.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Lens
  ( -- * Operations

    -- ** CreateKeyspace
    createKeyspace_tags,
    createKeyspace_keyspaceName,
    createKeyspaceResponse_httpStatus,
    createKeyspaceResponse_resourceArn,

    -- ** CreateTable
    createTable_tags,
    createTable_ttl,
    createTable_pointInTimeRecovery,
    createTable_capacitySpecification,
    createTable_encryptionSpecification,
    createTable_comment,
    createTable_defaultTimeToLive,
    createTable_keyspaceName,
    createTable_tableName,
    createTable_schemaDefinition,
    createTableResponse_httpStatus,
    createTableResponse_resourceArn,

    -- ** DeleteKeyspace
    deleteKeyspace_keyspaceName,
    deleteKeyspaceResponse_httpStatus,

    -- ** DeleteTable
    deleteTable_keyspaceName,
    deleteTable_tableName,
    deleteTableResponse_httpStatus,

    -- ** GetKeyspace
    getKeyspace_keyspaceName,
    getKeyspaceResponse_httpStatus,
    getKeyspaceResponse_keyspaceName,
    getKeyspaceResponse_resourceArn,

    -- ** GetTable
    getTable_keyspaceName,
    getTable_tableName,
    getTableResponse_ttl,
    getTableResponse_pointInTimeRecovery,
    getTableResponse_capacitySpecification,
    getTableResponse_status,
    getTableResponse_encryptionSpecification,
    getTableResponse_creationTimestamp,
    getTableResponse_comment,
    getTableResponse_defaultTimeToLive,
    getTableResponse_schemaDefinition,
    getTableResponse_httpStatus,
    getTableResponse_keyspaceName,
    getTableResponse_tableName,
    getTableResponse_resourceArn,

    -- ** ListKeyspaces
    listKeyspaces_nextToken,
    listKeyspaces_maxResults,
    listKeyspacesResponse_nextToken,
    listKeyspacesResponse_httpStatus,
    listKeyspacesResponse_keyspaces,

    -- ** ListTables
    listTables_nextToken,
    listTables_maxResults,
    listTables_keyspaceName,
    listTablesResponse_tables,
    listTablesResponse_nextToken,
    listTablesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** RestoreTable
    restoreTable_encryptionSpecificationOverride,
    restoreTable_capacitySpecificationOverride,
    restoreTable_tagsOverride,
    restoreTable_restoreTimestamp,
    restoreTable_pointInTimeRecoveryOverride,
    restoreTable_sourceKeyspaceName,
    restoreTable_sourceTableName,
    restoreTable_targetKeyspaceName,
    restoreTable_targetTableName,
    restoreTableResponse_httpStatus,
    restoreTableResponse_restoredTableARN,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tags,
    untagResourceResponse_httpStatus,

    -- ** UpdateTable
    updateTable_ttl,
    updateTable_pointInTimeRecovery,
    updateTable_capacitySpecification,
    updateTable_encryptionSpecification,
    updateTable_addColumns,
    updateTable_defaultTimeToLive,
    updateTable_keyspaceName,
    updateTable_tableName,
    updateTableResponse_httpStatus,
    updateTableResponse_resourceArn,

    -- * Types

    -- ** CapacitySpecification
    capacitySpecification_readCapacityUnits,
    capacitySpecification_writeCapacityUnits,
    capacitySpecification_throughputMode,

    -- ** CapacitySpecificationSummary
    capacitySpecificationSummary_readCapacityUnits,
    capacitySpecificationSummary_lastUpdateToPayPerRequestTimestamp,
    capacitySpecificationSummary_writeCapacityUnits,
    capacitySpecificationSummary_throughputMode,

    -- ** ClusteringKey
    clusteringKey_name,
    clusteringKey_orderBy,

    -- ** ColumnDefinition
    columnDefinition_name,
    columnDefinition_type,

    -- ** Comment
    comment_message,

    -- ** EncryptionSpecification
    encryptionSpecification_kmsKeyIdentifier,
    encryptionSpecification_type,

    -- ** KeyspaceSummary
    keyspaceSummary_keyspaceName,
    keyspaceSummary_resourceArn,

    -- ** PartitionKey
    partitionKey_name,

    -- ** PointInTimeRecovery
    pointInTimeRecovery_status,

    -- ** PointInTimeRecoverySummary
    pointInTimeRecoverySummary_earliestRestorableTimestamp,
    pointInTimeRecoverySummary_status,

    -- ** SchemaDefinition
    schemaDefinition_clusteringKeys,
    schemaDefinition_staticColumns,
    schemaDefinition_allColumns,
    schemaDefinition_partitionKeys,

    -- ** StaticColumn
    staticColumn_name,

    -- ** TableSummary
    tableSummary_keyspaceName,
    tableSummary_tableName,
    tableSummary_resourceArn,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimeToLive
    timeToLive_status,
  )
where

import Amazonka.KeySpaces.CreateKeyspace
import Amazonka.KeySpaces.CreateTable
import Amazonka.KeySpaces.DeleteKeyspace
import Amazonka.KeySpaces.DeleteTable
import Amazonka.KeySpaces.GetKeyspace
import Amazonka.KeySpaces.GetTable
import Amazonka.KeySpaces.ListKeyspaces
import Amazonka.KeySpaces.ListTables
import Amazonka.KeySpaces.ListTagsForResource
import Amazonka.KeySpaces.RestoreTable
import Amazonka.KeySpaces.TagResource
import Amazonka.KeySpaces.Types.CapacitySpecification
import Amazonka.KeySpaces.Types.CapacitySpecificationSummary
import Amazonka.KeySpaces.Types.ClusteringKey
import Amazonka.KeySpaces.Types.ColumnDefinition
import Amazonka.KeySpaces.Types.Comment
import Amazonka.KeySpaces.Types.EncryptionSpecification
import Amazonka.KeySpaces.Types.KeyspaceSummary
import Amazonka.KeySpaces.Types.PartitionKey
import Amazonka.KeySpaces.Types.PointInTimeRecovery
import Amazonka.KeySpaces.Types.PointInTimeRecoverySummary
import Amazonka.KeySpaces.Types.SchemaDefinition
import Amazonka.KeySpaces.Types.StaticColumn
import Amazonka.KeySpaces.Types.TableSummary
import Amazonka.KeySpaces.Types.Tag
import Amazonka.KeySpaces.Types.TimeToLive
import Amazonka.KeySpaces.UntagResource
import Amazonka.KeySpaces.UpdateTable

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.KeySpaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-02-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Keyspaces (for Apache Cassandra) is a scalable, highly available,
-- and managed Apache Cassandra-compatible database service. Amazon
-- Keyspaces makes it easy to migrate, run, and scale Cassandra workloads
-- in the Amazon Web Services Cloud. With just a few clicks on the Amazon
-- Web Services Management Console or a few lines of code, you can create
-- keyspaces and tables in Amazon Keyspaces, without deploying any
-- infrastructure or installing software.
--
-- In addition to supporting Cassandra Query Language (CQL) requests via
-- open-source Cassandra drivers, Amazon Keyspaces supports data definition
-- language (DDL) operations to manage keyspaces and tables using the
-- Amazon Web Services SDK and CLI. This API reference describes the
-- supported DDL operations in detail.
--
-- For the list of all supported CQL APIs, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/cassandra-apis.html Supported Cassandra APIs, operations, and data types in Amazon Keyspaces>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- To learn how Amazon Keyspaces API actions are recorded with CloudTrail,
-- see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/logging-using-cloudtrail.html#service-name-info-in-cloudtrail Amazon Keyspaces information in CloudTrail>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- For more information about Amazon Web Services APIs, for example how to
-- implement retry logic or how to sign Amazon Web Services API requests,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-apis.html Amazon Web Services APIs>
-- in the /General Reference/.
module Amazonka.KeySpaces
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateKeyspace
    CreateKeyspace (CreateKeyspace'),
    newCreateKeyspace,
    CreateKeyspaceResponse (CreateKeyspaceResponse'),
    newCreateKeyspaceResponse,

    -- ** CreateTable
    CreateTable (CreateTable'),
    newCreateTable,
    CreateTableResponse (CreateTableResponse'),
    newCreateTableResponse,

    -- ** DeleteKeyspace
    DeleteKeyspace (DeleteKeyspace'),
    newDeleteKeyspace,
    DeleteKeyspaceResponse (DeleteKeyspaceResponse'),
    newDeleteKeyspaceResponse,

    -- ** DeleteTable
    DeleteTable (DeleteTable'),
    newDeleteTable,
    DeleteTableResponse (DeleteTableResponse'),
    newDeleteTableResponse,

    -- ** GetKeyspace
    GetKeyspace (GetKeyspace'),
    newGetKeyspace,
    GetKeyspaceResponse (GetKeyspaceResponse'),
    newGetKeyspaceResponse,

    -- ** GetTable
    GetTable (GetTable'),
    newGetTable,
    GetTableResponse (GetTableResponse'),
    newGetTableResponse,

    -- ** ListKeyspaces (Paginated)
    ListKeyspaces (ListKeyspaces'),
    newListKeyspaces,
    ListKeyspacesResponse (ListKeyspacesResponse'),
    newListKeyspacesResponse,

    -- ** ListTables (Paginated)
    ListTables (ListTables'),
    newListTables,
    ListTablesResponse (ListTablesResponse'),
    newListTablesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RestoreTable
    RestoreTable (RestoreTable'),
    newRestoreTable,
    RestoreTableResponse (RestoreTableResponse'),
    newRestoreTableResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateTable
    UpdateTable (UpdateTable'),
    newUpdateTable,
    UpdateTableResponse (UpdateTableResponse'),
    newUpdateTableResponse,

    -- * Types

    -- ** EncryptionType
    EncryptionType (..),

    -- ** PointInTimeRecoveryStatus
    PointInTimeRecoveryStatus (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** TableStatus
    TableStatus (..),

    -- ** ThroughputMode
    ThroughputMode (..),

    -- ** TimeToLiveStatus
    TimeToLiveStatus (..),

    -- ** CapacitySpecification
    CapacitySpecification (CapacitySpecification'),
    newCapacitySpecification,

    -- ** CapacitySpecificationSummary
    CapacitySpecificationSummary (CapacitySpecificationSummary'),
    newCapacitySpecificationSummary,

    -- ** ClusteringKey
    ClusteringKey (ClusteringKey'),
    newClusteringKey,

    -- ** ColumnDefinition
    ColumnDefinition (ColumnDefinition'),
    newColumnDefinition,

    -- ** Comment
    Comment (Comment'),
    newComment,

    -- ** EncryptionSpecification
    EncryptionSpecification (EncryptionSpecification'),
    newEncryptionSpecification,

    -- ** KeyspaceSummary
    KeyspaceSummary (KeyspaceSummary'),
    newKeyspaceSummary,

    -- ** PartitionKey
    PartitionKey (PartitionKey'),
    newPartitionKey,

    -- ** PointInTimeRecovery
    PointInTimeRecovery (PointInTimeRecovery'),
    newPointInTimeRecovery,

    -- ** PointInTimeRecoverySummary
    PointInTimeRecoverySummary (PointInTimeRecoverySummary'),
    newPointInTimeRecoverySummary,

    -- ** SchemaDefinition
    SchemaDefinition (SchemaDefinition'),
    newSchemaDefinition,

    -- ** StaticColumn
    StaticColumn (StaticColumn'),
    newStaticColumn,

    -- ** TableSummary
    TableSummary (TableSummary'),
    newTableSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimeToLive
    TimeToLive (TimeToLive'),
    newTimeToLive,
  )
where

import Amazonka.KeySpaces.CreateKeyspace
import Amazonka.KeySpaces.CreateTable
import Amazonka.KeySpaces.DeleteKeyspace
import Amazonka.KeySpaces.DeleteTable
import Amazonka.KeySpaces.GetKeyspace
import Amazonka.KeySpaces.GetTable
import Amazonka.KeySpaces.Lens
import Amazonka.KeySpaces.ListKeyspaces
import Amazonka.KeySpaces.ListTables
import Amazonka.KeySpaces.ListTagsForResource
import Amazonka.KeySpaces.RestoreTable
import Amazonka.KeySpaces.TagResource
import Amazonka.KeySpaces.Types
import Amazonka.KeySpaces.UntagResource
import Amazonka.KeySpaces.UpdateTable
import Amazonka.KeySpaces.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'KeySpaces'.

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

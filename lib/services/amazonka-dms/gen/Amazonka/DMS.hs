{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Database Migration Service
--
-- Database Migration Service (DMS) can migrate your data to and from the
-- most widely used commercial and open-source databases such as Oracle,
-- PostgreSQL, Microsoft SQL Server, Amazon Redshift, MariaDB, Amazon
-- Aurora, MySQL, and SAP Adaptive Server Enterprise (ASE). The service
-- supports homogeneous migrations such as Oracle to Oracle, as well as
-- heterogeneous migrations between different database platforms, such as
-- Oracle to MySQL or SQL Server to PostgreSQL.
--
-- For more information about DMS, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/Welcome.html What Is Database Migration Service?>
-- in the /Database Migration Service User Guide./
module Amazonka.DMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** InvalidCertificateFault
    _InvalidCertificateFault,

    -- ** KMSNotFoundFault
    _KMSNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** ResourceQuotaExceededFault
    _ResourceQuotaExceededFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** KMSThrottlingFault
    _KMSThrottlingFault,

    -- ** AccessDeniedFault
    _AccessDeniedFault,

    -- ** ResourceAlreadyExistsFault
    _ResourceAlreadyExistsFault,

    -- ** KMSFault
    _KMSFault,

    -- ** KMSAccessDeniedFault
    _KMSAccessDeniedFault,

    -- ** KMSInvalidStateFault
    _KMSInvalidStateFault,

    -- ** InsufficientResourceCapacityFault
    _InsufficientResourceCapacityFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** InvalidResourceStateFault
    _InvalidResourceStateFault,

    -- ** ReplicationSubnetGroupDoesNotCoverEnoughAZs
    _ReplicationSubnetGroupDoesNotCoverEnoughAZs,

    -- ** KMSDisabledFault
    _KMSDisabledFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** UpgradeDependencyFailureFault
    _UpgradeDependencyFailureFault,

    -- ** S3ResourceNotFoundFault
    _S3ResourceNotFoundFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** S3AccessDeniedFault
    _S3AccessDeniedFault,

    -- * Waiters
    -- $waiters

    -- ** ReplicationTaskStopped
    newReplicationTaskStopped,

    -- ** ReplicationInstanceAvailable
    newReplicationInstanceAvailable,

    -- ** EndpointDeleted
    newEndpointDeleted,

    -- ** ReplicationTaskDeleted
    newReplicationTaskDeleted,

    -- ** ReplicationTaskRunning
    newReplicationTaskRunning,

    -- ** ReplicationInstanceDeleted
    newReplicationInstanceDeleted,

    -- ** ReplicationTaskReady
    newReplicationTaskReady,

    -- ** TestConnectionSucceeds
    newTestConnectionSucceeds,

    -- * Operations
    -- $operations

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** ApplyPendingMaintenanceAction
    ApplyPendingMaintenanceAction (ApplyPendingMaintenanceAction'),
    newApplyPendingMaintenanceAction,
    ApplyPendingMaintenanceActionResponse (ApplyPendingMaintenanceActionResponse'),
    newApplyPendingMaintenanceActionResponse,

    -- ** CancelReplicationTaskAssessmentRun
    CancelReplicationTaskAssessmentRun (CancelReplicationTaskAssessmentRun'),
    newCancelReplicationTaskAssessmentRun,
    CancelReplicationTaskAssessmentRunResponse (CancelReplicationTaskAssessmentRunResponse'),
    newCancelReplicationTaskAssessmentRunResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** CreateReplicationInstance
    CreateReplicationInstance (CreateReplicationInstance'),
    newCreateReplicationInstance,
    CreateReplicationInstanceResponse (CreateReplicationInstanceResponse'),
    newCreateReplicationInstanceResponse,

    -- ** CreateReplicationSubnetGroup
    CreateReplicationSubnetGroup (CreateReplicationSubnetGroup'),
    newCreateReplicationSubnetGroup,
    CreateReplicationSubnetGroupResponse (CreateReplicationSubnetGroupResponse'),
    newCreateReplicationSubnetGroupResponse,

    -- ** CreateReplicationTask
    CreateReplicationTask (CreateReplicationTask'),
    newCreateReplicationTask,
    CreateReplicationTaskResponse (CreateReplicationTaskResponse'),
    newCreateReplicationTaskResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** DeleteReplicationInstance
    DeleteReplicationInstance (DeleteReplicationInstance'),
    newDeleteReplicationInstance,
    DeleteReplicationInstanceResponse (DeleteReplicationInstanceResponse'),
    newDeleteReplicationInstanceResponse,

    -- ** DeleteReplicationSubnetGroup
    DeleteReplicationSubnetGroup (DeleteReplicationSubnetGroup'),
    newDeleteReplicationSubnetGroup,
    DeleteReplicationSubnetGroupResponse (DeleteReplicationSubnetGroupResponse'),
    newDeleteReplicationSubnetGroupResponse,

    -- ** DeleteReplicationTask
    DeleteReplicationTask (DeleteReplicationTask'),
    newDeleteReplicationTask,
    DeleteReplicationTaskResponse (DeleteReplicationTaskResponse'),
    newDeleteReplicationTaskResponse,

    -- ** DeleteReplicationTaskAssessmentRun
    DeleteReplicationTaskAssessmentRun (DeleteReplicationTaskAssessmentRun'),
    newDeleteReplicationTaskAssessmentRun,
    DeleteReplicationTaskAssessmentRunResponse (DeleteReplicationTaskAssessmentRunResponse'),
    newDeleteReplicationTaskAssessmentRunResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeApplicableIndividualAssessments
    DescribeApplicableIndividualAssessments (DescribeApplicableIndividualAssessments'),
    newDescribeApplicableIndividualAssessments,
    DescribeApplicableIndividualAssessmentsResponse (DescribeApplicableIndividualAssessmentsResponse'),
    newDescribeApplicableIndividualAssessmentsResponse,

    -- ** DescribeCertificates (Paginated)
    DescribeCertificates (DescribeCertificates'),
    newDescribeCertificates,
    DescribeCertificatesResponse (DescribeCertificatesResponse'),
    newDescribeCertificatesResponse,

    -- ** DescribeConnections (Paginated)
    DescribeConnections (DescribeConnections'),
    newDescribeConnections,
    DescribeConnectionsResponse (DescribeConnectionsResponse'),
    newDescribeConnectionsResponse,

    -- ** DescribeEndpointSettings
    DescribeEndpointSettings (DescribeEndpointSettings'),
    newDescribeEndpointSettings,
    DescribeEndpointSettingsResponse (DescribeEndpointSettingsResponse'),
    newDescribeEndpointSettingsResponse,

    -- ** DescribeEndpointTypes (Paginated)
    DescribeEndpointTypes (DescribeEndpointTypes'),
    newDescribeEndpointTypes,
    DescribeEndpointTypesResponse (DescribeEndpointTypesResponse'),
    newDescribeEndpointTypesResponse,

    -- ** DescribeEndpoints (Paginated)
    DescribeEndpoints (DescribeEndpoints'),
    newDescribeEndpoints,
    DescribeEndpointsResponse (DescribeEndpointsResponse'),
    newDescribeEndpointsResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeOrderableReplicationInstances (Paginated)
    DescribeOrderableReplicationInstances (DescribeOrderableReplicationInstances'),
    newDescribeOrderableReplicationInstances,
    DescribeOrderableReplicationInstancesResponse (DescribeOrderableReplicationInstancesResponse'),
    newDescribeOrderableReplicationInstancesResponse,

    -- ** DescribePendingMaintenanceActions
    DescribePendingMaintenanceActions (DescribePendingMaintenanceActions'),
    newDescribePendingMaintenanceActions,
    DescribePendingMaintenanceActionsResponse (DescribePendingMaintenanceActionsResponse'),
    newDescribePendingMaintenanceActionsResponse,

    -- ** DescribeRefreshSchemasStatus
    DescribeRefreshSchemasStatus (DescribeRefreshSchemasStatus'),
    newDescribeRefreshSchemasStatus,
    DescribeRefreshSchemasStatusResponse (DescribeRefreshSchemasStatusResponse'),
    newDescribeRefreshSchemasStatusResponse,

    -- ** DescribeReplicationInstanceTaskLogs
    DescribeReplicationInstanceTaskLogs (DescribeReplicationInstanceTaskLogs'),
    newDescribeReplicationInstanceTaskLogs,
    DescribeReplicationInstanceTaskLogsResponse (DescribeReplicationInstanceTaskLogsResponse'),
    newDescribeReplicationInstanceTaskLogsResponse,

    -- ** DescribeReplicationInstances (Paginated)
    DescribeReplicationInstances (DescribeReplicationInstances'),
    newDescribeReplicationInstances,
    DescribeReplicationInstancesResponse (DescribeReplicationInstancesResponse'),
    newDescribeReplicationInstancesResponse,

    -- ** DescribeReplicationSubnetGroups (Paginated)
    DescribeReplicationSubnetGroups (DescribeReplicationSubnetGroups'),
    newDescribeReplicationSubnetGroups,
    DescribeReplicationSubnetGroupsResponse (DescribeReplicationSubnetGroupsResponse'),
    newDescribeReplicationSubnetGroupsResponse,

    -- ** DescribeReplicationTaskAssessmentResults (Paginated)
    DescribeReplicationTaskAssessmentResults (DescribeReplicationTaskAssessmentResults'),
    newDescribeReplicationTaskAssessmentResults,
    DescribeReplicationTaskAssessmentResultsResponse (DescribeReplicationTaskAssessmentResultsResponse'),
    newDescribeReplicationTaskAssessmentResultsResponse,

    -- ** DescribeReplicationTaskAssessmentRuns
    DescribeReplicationTaskAssessmentRuns (DescribeReplicationTaskAssessmentRuns'),
    newDescribeReplicationTaskAssessmentRuns,
    DescribeReplicationTaskAssessmentRunsResponse (DescribeReplicationTaskAssessmentRunsResponse'),
    newDescribeReplicationTaskAssessmentRunsResponse,

    -- ** DescribeReplicationTaskIndividualAssessments
    DescribeReplicationTaskIndividualAssessments (DescribeReplicationTaskIndividualAssessments'),
    newDescribeReplicationTaskIndividualAssessments,
    DescribeReplicationTaskIndividualAssessmentsResponse (DescribeReplicationTaskIndividualAssessmentsResponse'),
    newDescribeReplicationTaskIndividualAssessmentsResponse,

    -- ** DescribeReplicationTasks (Paginated)
    DescribeReplicationTasks (DescribeReplicationTasks'),
    newDescribeReplicationTasks,
    DescribeReplicationTasksResponse (DescribeReplicationTasksResponse'),
    newDescribeReplicationTasksResponse,

    -- ** DescribeSchemas (Paginated)
    DescribeSchemas (DescribeSchemas'),
    newDescribeSchemas,
    DescribeSchemasResponse (DescribeSchemasResponse'),
    newDescribeSchemasResponse,

    -- ** DescribeTableStatistics (Paginated)
    DescribeTableStatistics (DescribeTableStatistics'),
    newDescribeTableStatistics,
    DescribeTableStatisticsResponse (DescribeTableStatisticsResponse'),
    newDescribeTableStatisticsResponse,

    -- ** ImportCertificate
    ImportCertificate (ImportCertificate'),
    newImportCertificate,
    ImportCertificateResponse (ImportCertificateResponse'),
    newImportCertificateResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ModifyEndpoint
    ModifyEndpoint (ModifyEndpoint'),
    newModifyEndpoint,
    ModifyEndpointResponse (ModifyEndpointResponse'),
    newModifyEndpointResponse,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** ModifyReplicationInstance
    ModifyReplicationInstance (ModifyReplicationInstance'),
    newModifyReplicationInstance,
    ModifyReplicationInstanceResponse (ModifyReplicationInstanceResponse'),
    newModifyReplicationInstanceResponse,

    -- ** ModifyReplicationSubnetGroup
    ModifyReplicationSubnetGroup (ModifyReplicationSubnetGroup'),
    newModifyReplicationSubnetGroup,
    ModifyReplicationSubnetGroupResponse (ModifyReplicationSubnetGroupResponse'),
    newModifyReplicationSubnetGroupResponse,

    -- ** ModifyReplicationTask
    ModifyReplicationTask (ModifyReplicationTask'),
    newModifyReplicationTask,
    ModifyReplicationTaskResponse (ModifyReplicationTaskResponse'),
    newModifyReplicationTaskResponse,

    -- ** MoveReplicationTask
    MoveReplicationTask (MoveReplicationTask'),
    newMoveReplicationTask,
    MoveReplicationTaskResponse (MoveReplicationTaskResponse'),
    newMoveReplicationTaskResponse,

    -- ** RebootReplicationInstance
    RebootReplicationInstance (RebootReplicationInstance'),
    newRebootReplicationInstance,
    RebootReplicationInstanceResponse (RebootReplicationInstanceResponse'),
    newRebootReplicationInstanceResponse,

    -- ** RefreshSchemas
    RefreshSchemas (RefreshSchemas'),
    newRefreshSchemas,
    RefreshSchemasResponse (RefreshSchemasResponse'),
    newRefreshSchemasResponse,

    -- ** ReloadTables
    ReloadTables (ReloadTables'),
    newReloadTables,
    ReloadTablesResponse (ReloadTablesResponse'),
    newReloadTablesResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** StartReplicationTask
    StartReplicationTask (StartReplicationTask'),
    newStartReplicationTask,
    StartReplicationTaskResponse (StartReplicationTaskResponse'),
    newStartReplicationTaskResponse,

    -- ** StartReplicationTaskAssessment
    StartReplicationTaskAssessment (StartReplicationTaskAssessment'),
    newStartReplicationTaskAssessment,
    StartReplicationTaskAssessmentResponse (StartReplicationTaskAssessmentResponse'),
    newStartReplicationTaskAssessmentResponse,

    -- ** StartReplicationTaskAssessmentRun
    StartReplicationTaskAssessmentRun (StartReplicationTaskAssessmentRun'),
    newStartReplicationTaskAssessmentRun,
    StartReplicationTaskAssessmentRunResponse (StartReplicationTaskAssessmentRunResponse'),
    newStartReplicationTaskAssessmentRunResponse,

    -- ** StopReplicationTask
    StopReplicationTask (StopReplicationTask'),
    newStopReplicationTask,
    StopReplicationTaskResponse (StopReplicationTaskResponse'),
    newStopReplicationTaskResponse,

    -- ** TestConnection
    TestConnection (TestConnection'),
    newTestConnection,
    TestConnectionResponse (TestConnectionResponse'),
    newTestConnectionResponse,

    -- * Types

    -- ** AuthMechanismValue
    AuthMechanismValue (..),

    -- ** AuthTypeValue
    AuthTypeValue (..),

    -- ** CannedAclForObjectsValue
    CannedAclForObjectsValue (..),

    -- ** CharLengthSemantics
    CharLengthSemantics (..),

    -- ** CompressionTypeValue
    CompressionTypeValue (..),

    -- ** DataFormatValue
    DataFormatValue (..),

    -- ** DatePartitionDelimiterValue
    DatePartitionDelimiterValue (..),

    -- ** DatePartitionSequenceValue
    DatePartitionSequenceValue (..),

    -- ** DmsSslModeValue
    DmsSslModeValue (..),

    -- ** EncodingTypeValue
    EncodingTypeValue (..),

    -- ** EncryptionModeValue
    EncryptionModeValue (..),

    -- ** EndpointSettingTypeValue
    EndpointSettingTypeValue (..),

    -- ** KafkaSecurityProtocol
    KafkaSecurityProtocol (..),

    -- ** MessageFormatValue
    MessageFormatValue (..),

    -- ** MigrationTypeValue
    MigrationTypeValue (..),

    -- ** NestingLevelValue
    NestingLevelValue (..),

    -- ** ParquetVersionValue
    ParquetVersionValue (..),

    -- ** PluginNameValue
    PluginNameValue (..),

    -- ** RedisAuthTypeValue
    RedisAuthTypeValue (..),

    -- ** RefreshSchemasStatusTypeValue
    RefreshSchemasStatusTypeValue (..),

    -- ** ReleaseStatusValues
    ReleaseStatusValues (..),

    -- ** ReloadOptionValue
    ReloadOptionValue (..),

    -- ** ReplicationEndpointTypeValue
    ReplicationEndpointTypeValue (..),

    -- ** SafeguardPolicy
    SafeguardPolicy (..),

    -- ** SourceType
    SourceType (..),

    -- ** SslSecurityProtocolValue
    SslSecurityProtocolValue (..),

    -- ** StartReplicationTaskTypeValue
    StartReplicationTaskTypeValue (..),

    -- ** TargetDbType
    TargetDbType (..),

    -- ** AccountQuota
    AccountQuota (AccountQuota'),
    newAccountQuota,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** DmsTransferSettings
    DmsTransferSettings (DmsTransferSettings'),
    newDmsTransferSettings,

    -- ** DocDbSettings
    DocDbSettings (DocDbSettings'),
    newDocDbSettings,

    -- ** DynamoDbSettings
    DynamoDbSettings (DynamoDbSettings'),
    newDynamoDbSettings,

    -- ** ElasticsearchSettings
    ElasticsearchSettings (ElasticsearchSettings'),
    newElasticsearchSettings,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EndpointSetting
    EndpointSetting (EndpointSetting'),
    newEndpointSetting,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** EventCategoryGroup
    EventCategoryGroup (EventCategoryGroup'),
    newEventCategoryGroup,

    -- ** EventSubscription
    EventSubscription (EventSubscription'),
    newEventSubscription,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** IBMDb2Settings
    IBMDb2Settings (IBMDb2Settings'),
    newIBMDb2Settings,

    -- ** KafkaSettings
    KafkaSettings (KafkaSettings'),
    newKafkaSettings,

    -- ** KinesisSettings
    KinesisSettings (KinesisSettings'),
    newKinesisSettings,

    -- ** MicrosoftSQLServerSettings
    MicrosoftSQLServerSettings (MicrosoftSQLServerSettings'),
    newMicrosoftSQLServerSettings,

    -- ** MongoDbSettings
    MongoDbSettings (MongoDbSettings'),
    newMongoDbSettings,

    -- ** MySQLSettings
    MySQLSettings (MySQLSettings'),
    newMySQLSettings,

    -- ** NeptuneSettings
    NeptuneSettings (NeptuneSettings'),
    newNeptuneSettings,

    -- ** OracleSettings
    OracleSettings (OracleSettings'),
    newOracleSettings,

    -- ** OrderableReplicationInstance
    OrderableReplicationInstance (OrderableReplicationInstance'),
    newOrderableReplicationInstance,

    -- ** PendingMaintenanceAction
    PendingMaintenanceAction (PendingMaintenanceAction'),
    newPendingMaintenanceAction,

    -- ** PostgreSQLSettings
    PostgreSQLSettings (PostgreSQLSettings'),
    newPostgreSQLSettings,

    -- ** RedisSettings
    RedisSettings (RedisSettings'),
    newRedisSettings,

    -- ** RedshiftSettings
    RedshiftSettings (RedshiftSettings'),
    newRedshiftSettings,

    -- ** RefreshSchemasStatus
    RefreshSchemasStatus (RefreshSchemasStatus'),
    newRefreshSchemasStatus,

    -- ** ReplicationInstance
    ReplicationInstance (ReplicationInstance'),
    newReplicationInstance,

    -- ** ReplicationInstanceTaskLog
    ReplicationInstanceTaskLog (ReplicationInstanceTaskLog'),
    newReplicationInstanceTaskLog,

    -- ** ReplicationPendingModifiedValues
    ReplicationPendingModifiedValues (ReplicationPendingModifiedValues'),
    newReplicationPendingModifiedValues,

    -- ** ReplicationSubnetGroup
    ReplicationSubnetGroup (ReplicationSubnetGroup'),
    newReplicationSubnetGroup,

    -- ** ReplicationTask
    ReplicationTask (ReplicationTask'),
    newReplicationTask,

    -- ** ReplicationTaskAssessmentResult
    ReplicationTaskAssessmentResult (ReplicationTaskAssessmentResult'),
    newReplicationTaskAssessmentResult,

    -- ** ReplicationTaskAssessmentRun
    ReplicationTaskAssessmentRun (ReplicationTaskAssessmentRun'),
    newReplicationTaskAssessmentRun,

    -- ** ReplicationTaskAssessmentRunProgress
    ReplicationTaskAssessmentRunProgress (ReplicationTaskAssessmentRunProgress'),
    newReplicationTaskAssessmentRunProgress,

    -- ** ReplicationTaskIndividualAssessment
    ReplicationTaskIndividualAssessment (ReplicationTaskIndividualAssessment'),
    newReplicationTaskIndividualAssessment,

    -- ** ReplicationTaskStats
    ReplicationTaskStats (ReplicationTaskStats'),
    newReplicationTaskStats,

    -- ** ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (ResourcePendingMaintenanceActions'),
    newResourcePendingMaintenanceActions,

    -- ** S3Settings
    S3Settings (S3Settings'),
    newS3Settings,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SupportedEndpointType
    SupportedEndpointType (SupportedEndpointType'),
    newSupportedEndpointType,

    -- ** SybaseSettings
    SybaseSettings (SybaseSettings'),
    newSybaseSettings,

    -- ** TableStatistics
    TableStatistics (TableStatistics'),
    newTableStatistics,

    -- ** TableToReload
    TableToReload (TableToReload'),
    newTableToReload,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VpcSecurityGroupMembership
    VpcSecurityGroupMembership (VpcSecurityGroupMembership'),
    newVpcSecurityGroupMembership,
  )
where

import Amazonka.DMS.AddTagsToResource
import Amazonka.DMS.ApplyPendingMaintenanceAction
import Amazonka.DMS.CancelReplicationTaskAssessmentRun
import Amazonka.DMS.CreateEndpoint
import Amazonka.DMS.CreateEventSubscription
import Amazonka.DMS.CreateReplicationInstance
import Amazonka.DMS.CreateReplicationSubnetGroup
import Amazonka.DMS.CreateReplicationTask
import Amazonka.DMS.DeleteCertificate
import Amazonka.DMS.DeleteConnection
import Amazonka.DMS.DeleteEndpoint
import Amazonka.DMS.DeleteEventSubscription
import Amazonka.DMS.DeleteReplicationInstance
import Amazonka.DMS.DeleteReplicationSubnetGroup
import Amazonka.DMS.DeleteReplicationTask
import Amazonka.DMS.DeleteReplicationTaskAssessmentRun
import Amazonka.DMS.DescribeAccountAttributes
import Amazonka.DMS.DescribeApplicableIndividualAssessments
import Amazonka.DMS.DescribeCertificates
import Amazonka.DMS.DescribeConnections
import Amazonka.DMS.DescribeEndpointSettings
import Amazonka.DMS.DescribeEndpointTypes
import Amazonka.DMS.DescribeEndpoints
import Amazonka.DMS.DescribeEventCategories
import Amazonka.DMS.DescribeEventSubscriptions
import Amazonka.DMS.DescribeEvents
import Amazonka.DMS.DescribeOrderableReplicationInstances
import Amazonka.DMS.DescribePendingMaintenanceActions
import Amazonka.DMS.DescribeRefreshSchemasStatus
import Amazonka.DMS.DescribeReplicationInstanceTaskLogs
import Amazonka.DMS.DescribeReplicationInstances
import Amazonka.DMS.DescribeReplicationSubnetGroups
import Amazonka.DMS.DescribeReplicationTaskAssessmentResults
import Amazonka.DMS.DescribeReplicationTaskAssessmentRuns
import Amazonka.DMS.DescribeReplicationTaskIndividualAssessments
import Amazonka.DMS.DescribeReplicationTasks
import Amazonka.DMS.DescribeSchemas
import Amazonka.DMS.DescribeTableStatistics
import Amazonka.DMS.ImportCertificate
import Amazonka.DMS.Lens
import Amazonka.DMS.ListTagsForResource
import Amazonka.DMS.ModifyEndpoint
import Amazonka.DMS.ModifyEventSubscription
import Amazonka.DMS.ModifyReplicationInstance
import Amazonka.DMS.ModifyReplicationSubnetGroup
import Amazonka.DMS.ModifyReplicationTask
import Amazonka.DMS.MoveReplicationTask
import Amazonka.DMS.RebootReplicationInstance
import Amazonka.DMS.RefreshSchemas
import Amazonka.DMS.ReloadTables
import Amazonka.DMS.RemoveTagsFromResource
import Amazonka.DMS.StartReplicationTask
import Amazonka.DMS.StartReplicationTaskAssessment
import Amazonka.DMS.StartReplicationTaskAssessmentRun
import Amazonka.DMS.StopReplicationTask
import Amazonka.DMS.TestConnection
import Amazonka.DMS.Types
import Amazonka.DMS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DMS'.

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.DMS
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
module Network.AWS.DMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** KMSAccessDeniedFault
    _KMSAccessDeniedFault,

    -- ** KMSDisabledFault
    _KMSDisabledFault,

    -- ** KMSFault
    _KMSFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** ReplicationSubnetGroupDoesNotCoverEnoughAZs
    _ReplicationSubnetGroupDoesNotCoverEnoughAZs,

    -- ** S3ResourceNotFoundFault
    _S3ResourceNotFoundFault,

    -- ** InvalidResourceStateFault
    _InvalidResourceStateFault,

    -- ** InvalidCertificateFault
    _InvalidCertificateFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** ResourceAlreadyExistsFault
    _ResourceAlreadyExistsFault,

    -- ** InsufficientResourceCapacityFault
    _InsufficientResourceCapacityFault,

    -- ** S3AccessDeniedFault
    _S3AccessDeniedFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** KMSNotFoundFault
    _KMSNotFoundFault,

    -- ** KMSThrottlingFault
    _KMSThrottlingFault,

    -- ** ResourceQuotaExceededFault
    _ResourceQuotaExceededFault,

    -- ** UpgradeDependencyFailureFault
    _UpgradeDependencyFailureFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** AccessDeniedFault
    _AccessDeniedFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** KMSInvalidStateFault
    _KMSInvalidStateFault,

    -- * Waiters
    -- $waiters

    -- ** ReplicationInstanceAvailable
    newReplicationInstanceAvailable,

    -- ** ReplicationTaskDeleted
    newReplicationTaskDeleted,

    -- ** ReplicationTaskReady
    newReplicationTaskReady,

    -- ** ReplicationInstanceDeleted
    newReplicationInstanceDeleted,

    -- ** EndpointDeleted
    newEndpointDeleted,

    -- ** ReplicationTaskStopped
    newReplicationTaskStopped,

    -- ** ReplicationTaskRunning
    newReplicationTaskRunning,

    -- ** TestConnectionSucceeds
    newTestConnectionSucceeds,

    -- * Operations
    -- $operations

    -- ** DeleteReplicationInstance
    DeleteReplicationInstance (DeleteReplicationInstance'),
    newDeleteReplicationInstance,
    DeleteReplicationInstanceResponse (DeleteReplicationInstanceResponse'),
    newDeleteReplicationInstanceResponse,

    -- ** RebootReplicationInstance
    RebootReplicationInstance (RebootReplicationInstance'),
    newRebootReplicationInstance,
    RebootReplicationInstanceResponse (RebootReplicationInstanceResponse'),
    newRebootReplicationInstanceResponse,

    -- ** ReloadTables
    ReloadTables (ReloadTables'),
    newReloadTables,
    ReloadTablesResponse (ReloadTablesResponse'),
    newReloadTablesResponse,

    -- ** StartReplicationTaskAssessment
    StartReplicationTaskAssessment (StartReplicationTaskAssessment'),
    newStartReplicationTaskAssessment,
    StartReplicationTaskAssessmentResponse (StartReplicationTaskAssessmentResponse'),
    newStartReplicationTaskAssessmentResponse,

    -- ** DeleteReplicationTaskAssessmentRun
    DeleteReplicationTaskAssessmentRun (DeleteReplicationTaskAssessmentRun'),
    newDeleteReplicationTaskAssessmentRun,
    DeleteReplicationTaskAssessmentRunResponse (DeleteReplicationTaskAssessmentRunResponse'),
    newDeleteReplicationTaskAssessmentRunResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** DescribeSchemas (Paginated)
    DescribeSchemas (DescribeSchemas'),
    newDescribeSchemas,
    DescribeSchemasResponse (DescribeSchemasResponse'),
    newDescribeSchemasResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** DescribeReplicationInstanceTaskLogs
    DescribeReplicationInstanceTaskLogs (DescribeReplicationInstanceTaskLogs'),
    newDescribeReplicationInstanceTaskLogs,
    DescribeReplicationInstanceTaskLogsResponse (DescribeReplicationInstanceTaskLogsResponse'),
    newDescribeReplicationInstanceTaskLogsResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeEndpointTypes (Paginated)
    DescribeEndpointTypes (DescribeEndpointTypes'),
    newDescribeEndpointTypes,
    DescribeEndpointTypesResponse (DescribeEndpointTypesResponse'),
    newDescribeEndpointTypesResponse,

    -- ** DeleteReplicationTask
    DeleteReplicationTask (DeleteReplicationTask'),
    newDeleteReplicationTask,
    DeleteReplicationTaskResponse (DeleteReplicationTaskResponse'),
    newDeleteReplicationTaskResponse,

    -- ** DescribeReplicationTaskAssessmentRuns
    DescribeReplicationTaskAssessmentRuns (DescribeReplicationTaskAssessmentRuns'),
    newDescribeReplicationTaskAssessmentRuns,
    DescribeReplicationTaskAssessmentRunsResponse (DescribeReplicationTaskAssessmentRunsResponse'),
    newDescribeReplicationTaskAssessmentRunsResponse,

    -- ** DescribeReplicationTaskAssessmentResults (Paginated)
    DescribeReplicationTaskAssessmentResults (DescribeReplicationTaskAssessmentResults'),
    newDescribeReplicationTaskAssessmentResults,
    DescribeReplicationTaskAssessmentResultsResponse (DescribeReplicationTaskAssessmentResultsResponse'),
    newDescribeReplicationTaskAssessmentResultsResponse,

    -- ** TestConnection
    TestConnection (TestConnection'),
    newTestConnection,
    TestConnectionResponse (TestConnectionResponse'),
    newTestConnectionResponse,

    -- ** DescribeConnections (Paginated)
    DescribeConnections (DescribeConnections'),
    newDescribeConnections,
    DescribeConnectionsResponse (DescribeConnectionsResponse'),
    newDescribeConnectionsResponse,

    -- ** MoveReplicationTask
    MoveReplicationTask (MoveReplicationTask'),
    newMoveReplicationTask,
    MoveReplicationTaskResponse (MoveReplicationTaskResponse'),
    newMoveReplicationTaskResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** ModifyEndpoint
    ModifyEndpoint (ModifyEndpoint'),
    newModifyEndpoint,
    ModifyEndpointResponse (ModifyEndpointResponse'),
    newModifyEndpointResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** DescribeEndpointSettings
    DescribeEndpointSettings (DescribeEndpointSettings'),
    newDescribeEndpointSettings,
    DescribeEndpointSettingsResponse (DescribeEndpointSettingsResponse'),
    newDescribeEndpointSettingsResponse,

    -- ** DescribeCertificates (Paginated)
    DescribeCertificates (DescribeCertificates'),
    newDescribeCertificates,
    DescribeCertificatesResponse (DescribeCertificatesResponse'),
    newDescribeCertificatesResponse,

    -- ** StartReplicationTaskAssessmentRun
    StartReplicationTaskAssessmentRun (StartReplicationTaskAssessmentRun'),
    newStartReplicationTaskAssessmentRun,
    StartReplicationTaskAssessmentRunResponse (StartReplicationTaskAssessmentRunResponse'),
    newStartReplicationTaskAssessmentRunResponse,

    -- ** DeleteEventSubscription
    DeleteEventSubscription (DeleteEventSubscription'),
    newDeleteEventSubscription,
    DeleteEventSubscriptionResponse (DeleteEventSubscriptionResponse'),
    newDeleteEventSubscriptionResponse,

    -- ** DescribeTableStatistics (Paginated)
    DescribeTableStatistics (DescribeTableStatistics'),
    newDescribeTableStatistics,
    DescribeTableStatisticsResponse (DescribeTableStatisticsResponse'),
    newDescribeTableStatisticsResponse,

    -- ** DescribeReplicationSubnetGroups (Paginated)
    DescribeReplicationSubnetGroups (DescribeReplicationSubnetGroups'),
    newDescribeReplicationSubnetGroups,
    DescribeReplicationSubnetGroupsResponse (DescribeReplicationSubnetGroupsResponse'),
    newDescribeReplicationSubnetGroupsResponse,

    -- ** StartReplicationTask
    StartReplicationTask (StartReplicationTask'),
    newStartReplicationTask,
    StartReplicationTaskResponse (StartReplicationTaskResponse'),
    newStartReplicationTaskResponse,

    -- ** DescribeEventSubscriptions (Paginated)
    DescribeEventSubscriptions (DescribeEventSubscriptions'),
    newDescribeEventSubscriptions,
    DescribeEventSubscriptionsResponse (DescribeEventSubscriptionsResponse'),
    newDescribeEventSubscriptionsResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** CreateReplicationSubnetGroup
    CreateReplicationSubnetGroup (CreateReplicationSubnetGroup'),
    newCreateReplicationSubnetGroup,
    CreateReplicationSubnetGroupResponse (CreateReplicationSubnetGroupResponse'),
    newCreateReplicationSubnetGroupResponse,

    -- ** DescribeApplicableIndividualAssessments
    DescribeApplicableIndividualAssessments (DescribeApplicableIndividualAssessments'),
    newDescribeApplicableIndividualAssessments,
    DescribeApplicableIndividualAssessmentsResponse (DescribeApplicableIndividualAssessmentsResponse'),
    newDescribeApplicableIndividualAssessmentsResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** RefreshSchemas
    RefreshSchemas (RefreshSchemas'),
    newRefreshSchemas,
    RefreshSchemasResponse (RefreshSchemasResponse'),
    newRefreshSchemasResponse,

    -- ** DescribeReplicationTasks (Paginated)
    DescribeReplicationTasks (DescribeReplicationTasks'),
    newDescribeReplicationTasks,
    DescribeReplicationTasksResponse (DescribeReplicationTasksResponse'),
    newDescribeReplicationTasksResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

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

    -- ** CreateReplicationTask
    CreateReplicationTask (CreateReplicationTask'),
    newCreateReplicationTask,
    CreateReplicationTaskResponse (CreateReplicationTaskResponse'),
    newCreateReplicationTaskResponse,

    -- ** DescribeEndpoints (Paginated)
    DescribeEndpoints (DescribeEndpoints'),
    newDescribeEndpoints,
    DescribeEndpointsResponse (DescribeEndpointsResponse'),
    newDescribeEndpointsResponse,

    -- ** ModifyReplicationInstance
    ModifyReplicationInstance (ModifyReplicationInstance'),
    newModifyReplicationInstance,
    ModifyReplicationInstanceResponse (ModifyReplicationInstanceResponse'),
    newModifyReplicationInstanceResponse,

    -- ** ImportCertificate
    ImportCertificate (ImportCertificate'),
    newImportCertificate,
    ImportCertificateResponse (ImportCertificateResponse'),
    newImportCertificateResponse,

    -- ** CancelReplicationTaskAssessmentRun
    CancelReplicationTaskAssessmentRun (CancelReplicationTaskAssessmentRun'),
    newCancelReplicationTaskAssessmentRun,
    CancelReplicationTaskAssessmentRunResponse (CancelReplicationTaskAssessmentRunResponse'),
    newCancelReplicationTaskAssessmentRunResponse,

    -- ** ModifyReplicationSubnetGroup
    ModifyReplicationSubnetGroup (ModifyReplicationSubnetGroup'),
    newModifyReplicationSubnetGroup,
    ModifyReplicationSubnetGroupResponse (ModifyReplicationSubnetGroupResponse'),
    newModifyReplicationSubnetGroupResponse,

    -- ** DescribeReplicationTaskIndividualAssessments
    DescribeReplicationTaskIndividualAssessments (DescribeReplicationTaskIndividualAssessments'),
    newDescribeReplicationTaskIndividualAssessments,
    DescribeReplicationTaskIndividualAssessmentsResponse (DescribeReplicationTaskIndividualAssessmentsResponse'),
    newDescribeReplicationTaskIndividualAssessmentsResponse,

    -- ** ApplyPendingMaintenanceAction
    ApplyPendingMaintenanceAction (ApplyPendingMaintenanceAction'),
    newApplyPendingMaintenanceAction,
    ApplyPendingMaintenanceActionResponse (ApplyPendingMaintenanceActionResponse'),
    newApplyPendingMaintenanceActionResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** DescribeReplicationInstances (Paginated)
    DescribeReplicationInstances (DescribeReplicationInstances'),
    newDescribeReplicationInstances,
    DescribeReplicationInstancesResponse (DescribeReplicationInstancesResponse'),
    newDescribeReplicationInstancesResponse,

    -- ** DescribeRefreshSchemasStatus
    DescribeRefreshSchemasStatus (DescribeRefreshSchemasStatus'),
    newDescribeRefreshSchemasStatus,
    DescribeRefreshSchemasStatusResponse (DescribeRefreshSchemasStatusResponse'),
    newDescribeRefreshSchemasStatusResponse,

    -- ** StopReplicationTask
    StopReplicationTask (StopReplicationTask'),
    newStopReplicationTask,
    StopReplicationTaskResponse (StopReplicationTaskResponse'),
    newStopReplicationTaskResponse,

    -- ** ModifyReplicationTask
    ModifyReplicationTask (ModifyReplicationTask'),
    newModifyReplicationTask,
    ModifyReplicationTaskResponse (ModifyReplicationTaskResponse'),
    newModifyReplicationTaskResponse,

    -- ** CreateReplicationInstance
    CreateReplicationInstance (CreateReplicationInstance'),
    newCreateReplicationInstance,
    CreateReplicationInstanceResponse (CreateReplicationInstanceResponse'),
    newCreateReplicationInstanceResponse,

    -- ** DeleteReplicationSubnetGroup
    DeleteReplicationSubnetGroup (DeleteReplicationSubnetGroup'),
    newDeleteReplicationSubnetGroup,
    DeleteReplicationSubnetGroupResponse (DeleteReplicationSubnetGroupResponse'),
    newDeleteReplicationSubnetGroupResponse,

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

import Network.AWS.DMS.AddTagsToResource
import Network.AWS.DMS.ApplyPendingMaintenanceAction
import Network.AWS.DMS.CancelReplicationTaskAssessmentRun
import Network.AWS.DMS.CreateEndpoint
import Network.AWS.DMS.CreateEventSubscription
import Network.AWS.DMS.CreateReplicationInstance
import Network.AWS.DMS.CreateReplicationSubnetGroup
import Network.AWS.DMS.CreateReplicationTask
import Network.AWS.DMS.DeleteCertificate
import Network.AWS.DMS.DeleteConnection
import Network.AWS.DMS.DeleteEndpoint
import Network.AWS.DMS.DeleteEventSubscription
import Network.AWS.DMS.DeleteReplicationInstance
import Network.AWS.DMS.DeleteReplicationSubnetGroup
import Network.AWS.DMS.DeleteReplicationTask
import Network.AWS.DMS.DeleteReplicationTaskAssessmentRun
import Network.AWS.DMS.DescribeAccountAttributes
import Network.AWS.DMS.DescribeApplicableIndividualAssessments
import Network.AWS.DMS.DescribeCertificates
import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.DescribeEndpointSettings
import Network.AWS.DMS.DescribeEndpointTypes
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.DescribeEventCategories
import Network.AWS.DMS.DescribeEventSubscriptions
import Network.AWS.DMS.DescribeEvents
import Network.AWS.DMS.DescribeOrderableReplicationInstances
import Network.AWS.DMS.DescribePendingMaintenanceActions
import Network.AWS.DMS.DescribeRefreshSchemasStatus
import Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeReplicationSubnetGroups
import Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
import Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
import Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
import Network.AWS.DMS.DescribeReplicationTasks
import Network.AWS.DMS.DescribeSchemas
import Network.AWS.DMS.DescribeTableStatistics
import Network.AWS.DMS.ImportCertificate
import Network.AWS.DMS.Lens
import Network.AWS.DMS.ListTagsForResource
import Network.AWS.DMS.ModifyEndpoint
import Network.AWS.DMS.ModifyEventSubscription
import Network.AWS.DMS.ModifyReplicationInstance
import Network.AWS.DMS.ModifyReplicationSubnetGroup
import Network.AWS.DMS.ModifyReplicationTask
import Network.AWS.DMS.MoveReplicationTask
import Network.AWS.DMS.RebootReplicationInstance
import Network.AWS.DMS.RefreshSchemas
import Network.AWS.DMS.ReloadTables
import Network.AWS.DMS.RemoveTagsFromResource
import Network.AWS.DMS.StartReplicationTask
import Network.AWS.DMS.StartReplicationTaskAssessment
import Network.AWS.DMS.StartReplicationTaskAssessmentRun
import Network.AWS.DMS.StopReplicationTask
import Network.AWS.DMS.TestConnection
import Network.AWS.DMS.Types
import Network.AWS.DMS.Waiters

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Database Migration Service
--
-- AWS Database Migration Service (AWS DMS) can migrate your data to and
-- from the most widely used commercial and open-source databases such as
-- Oracle, PostgreSQL, Microsoft SQL Server, Amazon Redshift, MariaDB,
-- Amazon Aurora, MySQL, and SAP Adaptive Server Enterprise (ASE). The
-- service supports homogeneous migrations such as Oracle to Oracle, as
-- well as heterogeneous migrations between different database platforms,
-- such as Oracle to MySQL or SQL Server to PostgreSQL.
--
-- For more information about AWS DMS, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/Welcome.html What Is AWS Database Migration Service?>
-- in the /AWS Database Migration User Guide./
module Network.AWS.DMS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** KMSDisabledFault
    _KMSDisabledFault,

    -- ** KMSFault
    _KMSFault,

    -- ** KMSAccessDeniedFault
    _KMSAccessDeniedFault,

    -- ** AccessDeniedFault
    _AccessDeniedFault,

    -- ** InvalidCertificateFault
    _InvalidCertificateFault,

    -- ** SNSNoAuthorizationFault
    _SNSNoAuthorizationFault,

    -- ** InvalidResourceStateFault
    _InvalidResourceStateFault,

    -- ** ResourceNotFoundFault
    _ResourceNotFoundFault,

    -- ** KMSKeyNotAccessibleFault
    _KMSKeyNotAccessibleFault,

    -- ** ResourceQuotaExceededFault
    _ResourceQuotaExceededFault,

    -- ** SNSInvalidTopicFault
    _SNSInvalidTopicFault,

    -- ** KMSNotFoundFault
    _KMSNotFoundFault,

    -- ** KMSInvalidStateFault
    _KMSInvalidStateFault,

    -- ** SubnetAlreadyInUse
    _SubnetAlreadyInUse,

    -- ** InsufficientResourceCapacityFault
    _InsufficientResourceCapacityFault,

    -- ** S3AccessDeniedFault
    _S3AccessDeniedFault,

    -- ** StorageQuotaExceededFault
    _StorageQuotaExceededFault,

    -- ** ResourceAlreadyExistsFault
    _ResourceAlreadyExistsFault,

    -- ** ReplicationSubnetGroupDoesNotCoverEnoughAZs
    _ReplicationSubnetGroupDoesNotCoverEnoughAZs,

    -- ** S3ResourceNotFoundFault
    _S3ResourceNotFoundFault,

    -- ** UpgradeDependencyFailureFault
    _UpgradeDependencyFailureFault,

    -- ** KMSThrottlingFault
    _KMSThrottlingFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- * Waiters
    -- $waiters

    -- ** ReplicationInstanceAvailable
    newReplicationInstanceAvailable,

    -- ** ReplicationTaskDeleted
    newReplicationTaskDeleted,

    -- ** EndpointDeleted
    newEndpointDeleted,

    -- ** ReplicationTaskRunning
    newReplicationTaskRunning,

    -- ** ReplicationInstanceDeleted
    newReplicationInstanceDeleted,

    -- ** ReplicationTaskReady
    newReplicationTaskReady,

    -- ** TestConnectionSucceeds
    newTestConnectionSucceeds,

    -- ** ReplicationTaskStopped
    newReplicationTaskStopped,

    -- * Operations
    -- $operations

    -- ** DeleteReplicationTaskAssessmentRun
    DeleteReplicationTaskAssessmentRun (DeleteReplicationTaskAssessmentRun'),
    newDeleteReplicationTaskAssessmentRun,
    DeleteReplicationTaskAssessmentRunResponse (DeleteReplicationTaskAssessmentRunResponse'),
    newDeleteReplicationTaskAssessmentRunResponse,

    -- ** DescribeEventCategories
    DescribeEventCategories (DescribeEventCategories'),
    newDescribeEventCategories,
    DescribeEventCategoriesResponse (DescribeEventCategoriesResponse'),
    newDescribeEventCategoriesResponse,

    -- ** StartReplicationTaskAssessment
    StartReplicationTaskAssessment (StartReplicationTaskAssessment'),
    newStartReplicationTaskAssessment,
    StartReplicationTaskAssessmentResponse (StartReplicationTaskAssessmentResponse'),
    newStartReplicationTaskAssessmentResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** CreateEndpoint
    CreateEndpoint (CreateEndpoint'),
    newCreateEndpoint,
    CreateEndpointResponse (CreateEndpointResponse'),
    newCreateEndpointResponse,

    -- ** DescribeOrderableReplicationInstances (Paginated)
    DescribeOrderableReplicationInstances (DescribeOrderableReplicationInstances'),
    newDescribeOrderableReplicationInstances,
    DescribeOrderableReplicationInstancesResponse (DescribeOrderableReplicationInstancesResponse'),
    newDescribeOrderableReplicationInstancesResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** DescribeApplicableIndividualAssessments
    DescribeApplicableIndividualAssessments (DescribeApplicableIndividualAssessments'),
    newDescribeApplicableIndividualAssessments,
    DescribeApplicableIndividualAssessmentsResponse (DescribeApplicableIndividualAssessmentsResponse'),
    newDescribeApplicableIndividualAssessmentsResponse,

    -- ** ReloadTables
    ReloadTables (ReloadTables'),
    newReloadTables,
    ReloadTablesResponse (ReloadTablesResponse'),
    newReloadTablesResponse,

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

    -- ** ModifyReplicationTask
    ModifyReplicationTask (ModifyReplicationTask'),
    newModifyReplicationTask,
    ModifyReplicationTaskResponse (ModifyReplicationTaskResponse'),
    newModifyReplicationTaskResponse,

    -- ** StopReplicationTask
    StopReplicationTask (StopReplicationTask'),
    newStopReplicationTask,
    StopReplicationTaskResponse (StopReplicationTaskResponse'),
    newStopReplicationTaskResponse,

    -- ** CreateReplicationInstance
    CreateReplicationInstance (CreateReplicationInstance'),
    newCreateReplicationInstance,
    CreateReplicationInstanceResponse (CreateReplicationInstanceResponse'),
    newCreateReplicationInstanceResponse,

    -- ** DescribeReplicationSubnetGroups (Paginated)
    DescribeReplicationSubnetGroups (DescribeReplicationSubnetGroups'),
    newDescribeReplicationSubnetGroups,
    DescribeReplicationSubnetGroupsResponse (DescribeReplicationSubnetGroupsResponse'),
    newDescribeReplicationSubnetGroupsResponse,

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

    -- ** StartReplicationTaskAssessmentRun
    StartReplicationTaskAssessmentRun (StartReplicationTaskAssessmentRun'),
    newStartReplicationTaskAssessmentRun,
    StartReplicationTaskAssessmentRunResponse (StartReplicationTaskAssessmentRunResponse'),
    newStartReplicationTaskAssessmentRunResponse,

    -- ** DescribeRefreshSchemasStatus
    DescribeRefreshSchemasStatus (DescribeRefreshSchemasStatus'),
    newDescribeRefreshSchemasStatus,
    DescribeRefreshSchemasStatusResponse (DescribeRefreshSchemasStatusResponse'),
    newDescribeRefreshSchemasStatusResponse,

    -- ** DescribeAccountAttributes
    DescribeAccountAttributes (DescribeAccountAttributes'),
    newDescribeAccountAttributes,
    DescribeAccountAttributesResponse (DescribeAccountAttributesResponse'),
    newDescribeAccountAttributesResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** DescribeCertificates (Paginated)
    DescribeCertificates (DescribeCertificates'),
    newDescribeCertificates,
    DescribeCertificatesResponse (DescribeCertificatesResponse'),
    newDescribeCertificatesResponse,

    -- ** ModifyEndpoint
    ModifyEndpoint (ModifyEndpoint'),
    newModifyEndpoint,
    ModifyEndpointResponse (ModifyEndpointResponse'),
    newModifyEndpointResponse,

    -- ** TestConnection
    TestConnection (TestConnection'),
    newTestConnection,
    TestConnectionResponse (TestConnectionResponse'),
    newTestConnectionResponse,

    -- ** DescribeReplicationTaskAssessmentResults (Paginated)
    DescribeReplicationTaskAssessmentResults (DescribeReplicationTaskAssessmentResults'),
    newDescribeReplicationTaskAssessmentResults,
    DescribeReplicationTaskAssessmentResultsResponse (DescribeReplicationTaskAssessmentResultsResponse'),
    newDescribeReplicationTaskAssessmentResultsResponse,

    -- ** ApplyPendingMaintenanceAction
    ApplyPendingMaintenanceAction (ApplyPendingMaintenanceAction'),
    newApplyPendingMaintenanceAction,
    ApplyPendingMaintenanceActionResponse (ApplyPendingMaintenanceActionResponse'),
    newApplyPendingMaintenanceActionResponse,

    -- ** ImportCertificate
    ImportCertificate (ImportCertificate'),
    newImportCertificate,
    ImportCertificateResponse (ImportCertificateResponse'),
    newImportCertificateResponse,

    -- ** DescribeEndpointTypes (Paginated)
    DescribeEndpointTypes (DescribeEndpointTypes'),
    newDescribeEndpointTypes,
    DescribeEndpointTypesResponse (DescribeEndpointTypesResponse'),
    newDescribeEndpointTypesResponse,

    -- ** DescribePendingMaintenanceActions
    DescribePendingMaintenanceActions (DescribePendingMaintenanceActions'),
    newDescribePendingMaintenanceActions,
    DescribePendingMaintenanceActionsResponse (DescribePendingMaintenanceActionsResponse'),
    newDescribePendingMaintenanceActionsResponse,

    -- ** DeleteEndpoint
    DeleteEndpoint (DeleteEndpoint'),
    newDeleteEndpoint,
    DeleteEndpointResponse (DeleteEndpointResponse'),
    newDeleteEndpointResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeSchemas (Paginated)
    DescribeSchemas (DescribeSchemas'),
    newDescribeSchemas,
    DescribeSchemasResponse (DescribeSchemasResponse'),
    newDescribeSchemasResponse,

    -- ** ModifyEventSubscription
    ModifyEventSubscription (ModifyEventSubscription'),
    newModifyEventSubscription,
    ModifyEventSubscriptionResponse (ModifyEventSubscriptionResponse'),
    newModifyEventSubscriptionResponse,

    -- ** DescribeReplicationTasks (Paginated)
    DescribeReplicationTasks (DescribeReplicationTasks'),
    newDescribeReplicationTasks,
    DescribeReplicationTasksResponse (DescribeReplicationTasksResponse'),
    newDescribeReplicationTasksResponse,

    -- ** RefreshSchemas
    RefreshSchemas (RefreshSchemas'),
    newRefreshSchemas,
    RefreshSchemasResponse (RefreshSchemasResponse'),
    newRefreshSchemasResponse,

    -- ** CreateReplicationSubnetGroup
    CreateReplicationSubnetGroup (CreateReplicationSubnetGroup'),
    newCreateReplicationSubnetGroup,
    CreateReplicationSubnetGroupResponse (CreateReplicationSubnetGroupResponse'),
    newCreateReplicationSubnetGroupResponse,

    -- ** RebootReplicationInstance
    RebootReplicationInstance (RebootReplicationInstance'),
    newRebootReplicationInstance,
    RebootReplicationInstanceResponse (RebootReplicationInstanceResponse'),
    newRebootReplicationInstanceResponse,

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

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** CreateEventSubscription
    CreateEventSubscription (CreateEventSubscription'),
    newCreateEventSubscription,
    CreateEventSubscriptionResponse (CreateEventSubscriptionResponse'),
    newCreateEventSubscriptionResponse,

    -- ** DescribeReplicationInstances (Paginated)
    DescribeReplicationInstances (DescribeReplicationInstances'),
    newDescribeReplicationInstances,
    DescribeReplicationInstancesResponse (DescribeReplicationInstancesResponse'),
    newDescribeReplicationInstancesResponse,

    -- ** DescribeReplicationTaskAssessmentRuns
    DescribeReplicationTaskAssessmentRuns (DescribeReplicationTaskAssessmentRuns'),
    newDescribeReplicationTaskAssessmentRuns,
    DescribeReplicationTaskAssessmentRunsResponse (DescribeReplicationTaskAssessmentRunsResponse'),
    newDescribeReplicationTaskAssessmentRunsResponse,

    -- ** CancelReplicationTaskAssessmentRun
    CancelReplicationTaskAssessmentRun (CancelReplicationTaskAssessmentRun'),
    newCancelReplicationTaskAssessmentRun,
    CancelReplicationTaskAssessmentRunResponse (CancelReplicationTaskAssessmentRunResponse'),
    newCancelReplicationTaskAssessmentRunResponse,

    -- ** DescribeConnections (Paginated)
    DescribeConnections (DescribeConnections'),
    newDescribeConnections,
    DescribeConnectionsResponse (DescribeConnectionsResponse'),
    newDescribeConnectionsResponse,

    -- ** ModifyReplicationSubnetGroup
    ModifyReplicationSubnetGroup (ModifyReplicationSubnetGroup'),
    newModifyReplicationSubnetGroup,
    ModifyReplicationSubnetGroupResponse (ModifyReplicationSubnetGroupResponse'),
    newModifyReplicationSubnetGroupResponse,

    -- ** DeleteReplicationTask
    DeleteReplicationTask (DeleteReplicationTask'),
    newDeleteReplicationTask,
    DeleteReplicationTaskResponse (DeleteReplicationTaskResponse'),
    newDeleteReplicationTaskResponse,

    -- ** MoveReplicationTask
    MoveReplicationTask (MoveReplicationTask'),
    newMoveReplicationTask,
    MoveReplicationTaskResponse (MoveReplicationTaskResponse'),
    newMoveReplicationTaskResponse,

    -- ** DescribeReplicationTaskIndividualAssessments
    DescribeReplicationTaskIndividualAssessments (DescribeReplicationTaskIndividualAssessments'),
    newDescribeReplicationTaskIndividualAssessments,
    DescribeReplicationTaskIndividualAssessmentsResponse (DescribeReplicationTaskIndividualAssessmentsResponse'),
    newDescribeReplicationTaskIndividualAssessmentsResponse,

    -- ** ModifyReplicationInstance
    ModifyReplicationInstance (ModifyReplicationInstance'),
    newModifyReplicationInstance,
    ModifyReplicationInstanceResponse (ModifyReplicationInstanceResponse'),
    newModifyReplicationInstanceResponse,

    -- ** DescribeEndpoints (Paginated)
    DescribeEndpoints (DescribeEndpoints'),
    newDescribeEndpoints,
    DescribeEndpointsResponse (DescribeEndpointsResponse'),
    newDescribeEndpointsResponse,

    -- ** CreateReplicationTask
    CreateReplicationTask (CreateReplicationTask'),
    newCreateReplicationTask,
    CreateReplicationTaskResponse (CreateReplicationTaskResponse'),
    newCreateReplicationTaskResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeReplicationInstanceTaskLogs
    DescribeReplicationInstanceTaskLogs (DescribeReplicationInstanceTaskLogs'),
    newDescribeReplicationInstanceTaskLogs,
    DescribeReplicationInstanceTaskLogsResponse (DescribeReplicationInstanceTaskLogsResponse'),
    newDescribeReplicationInstanceTaskLogsResponse,

    -- * Types

    -- ** AuthMechanismValue
    AuthMechanismValue (..),

    -- ** AuthTypeValue
    AuthTypeValue (..),

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

    -- ** MessageFormatValue
    MessageFormatValue (..),

    -- ** MigrationTypeValue
    MigrationTypeValue (..),

    -- ** NestingLevelValue
    NestingLevelValue (..),

    -- ** ParquetVersionValue
    ParquetVersionValue (..),

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

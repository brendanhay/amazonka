{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Kafka
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The operations for managing an Amazon MSK cluster.
module Network.AWS.Kafka
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateConfiguration
    CreateConfiguration (CreateConfiguration'),
    newCreateConfiguration,
    CreateConfigurationResponse (CreateConfigurationResponse'),
    newCreateConfigurationResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** RebootBroker
    RebootBroker (RebootBroker'),
    newRebootBroker,
    RebootBrokerResponse (RebootBrokerResponse'),
    newRebootBrokerResponse,

    -- ** ListConfigurationRevisions (Paginated)
    ListConfigurationRevisions (ListConfigurationRevisions'),
    newListConfigurationRevisions,
    ListConfigurationRevisionsResponse (ListConfigurationRevisionsResponse'),
    newListConfigurationRevisionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListKafkaVersions (Paginated)
    ListKafkaVersions (ListKafkaVersions'),
    newListKafkaVersions,
    ListKafkaVersionsResponse (ListKafkaVersionsResponse'),
    newListKafkaVersionsResponse,

    -- ** UpdateMonitoring
    UpdateMonitoring (UpdateMonitoring'),
    newUpdateMonitoring,
    UpdateMonitoringResponse (UpdateMonitoringResponse'),
    newUpdateMonitoringResponse,

    -- ** BatchAssociateScramSecret
    BatchAssociateScramSecret (BatchAssociateScramSecret'),
    newBatchAssociateScramSecret,
    BatchAssociateScramSecretResponse (BatchAssociateScramSecretResponse'),
    newBatchAssociateScramSecretResponse,

    -- ** UpdateBrokerStorage
    UpdateBrokerStorage (UpdateBrokerStorage'),
    newUpdateBrokerStorage,
    UpdateBrokerStorageResponse (UpdateBrokerStorageResponse'),
    newUpdateBrokerStorageResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** UpdateClusterConfiguration
    UpdateClusterConfiguration (UpdateClusterConfiguration'),
    newUpdateClusterConfiguration,
    UpdateClusterConfigurationResponse (UpdateClusterConfigurationResponse'),
    newUpdateClusterConfigurationResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** UpdateBrokerCount
    UpdateBrokerCount (UpdateBrokerCount'),
    newUpdateBrokerCount,
    UpdateBrokerCountResponse (UpdateBrokerCountResponse'),
    newUpdateBrokerCountResponse,

    -- ** ListConfigurations (Paginated)
    ListConfigurations (ListConfigurations'),
    newListConfigurations,
    ListConfigurationsResponse (ListConfigurationsResponse'),
    newListConfigurationsResponse,

    -- ** GetBootstrapBrokers
    GetBootstrapBrokers (GetBootstrapBrokers'),
    newGetBootstrapBrokers,
    GetBootstrapBrokersResponse (GetBootstrapBrokersResponse'),
    newGetBootstrapBrokersResponse,

    -- ** UpdateClusterKafkaVersion
    UpdateClusterKafkaVersion (UpdateClusterKafkaVersion'),
    newUpdateClusterKafkaVersion,
    UpdateClusterKafkaVersionResponse (UpdateClusterKafkaVersionResponse'),
    newUpdateClusterKafkaVersionResponse,

    -- ** UpdateSecurity
    UpdateSecurity (UpdateSecurity'),
    newUpdateSecurity,
    UpdateSecurityResponse (UpdateSecurityResponse'),
    newUpdateSecurityResponse,

    -- ** GetCompatibleKafkaVersions
    GetCompatibleKafkaVersions (GetCompatibleKafkaVersions'),
    newGetCompatibleKafkaVersions,
    GetCompatibleKafkaVersionsResponse (GetCompatibleKafkaVersionsResponse'),
    newGetCompatibleKafkaVersionsResponse,

    -- ** DescribeClusterOperation
    DescribeClusterOperation (DescribeClusterOperation'),
    newDescribeClusterOperation,
    DescribeClusterOperationResponse (DescribeClusterOperationResponse'),
    newDescribeClusterOperationResponse,

    -- ** UpdateBrokerType
    UpdateBrokerType (UpdateBrokerType'),
    newUpdateBrokerType,
    UpdateBrokerTypeResponse (UpdateBrokerTypeResponse'),
    newUpdateBrokerTypeResponse,

    -- ** DescribeConfiguration
    DescribeConfiguration (DescribeConfiguration'),
    newDescribeConfiguration,
    DescribeConfigurationResponse (DescribeConfigurationResponse'),
    newDescribeConfigurationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListClusterOperations (Paginated)
    ListClusterOperations (ListClusterOperations'),
    newListClusterOperations,
    ListClusterOperationsResponse (ListClusterOperationsResponse'),
    newListClusterOperationsResponse,

    -- ** BatchDisassociateScramSecret
    BatchDisassociateScramSecret (BatchDisassociateScramSecret'),
    newBatchDisassociateScramSecret,
    BatchDisassociateScramSecretResponse (BatchDisassociateScramSecretResponse'),
    newBatchDisassociateScramSecretResponse,

    -- ** DescribeConfigurationRevision
    DescribeConfigurationRevision (DescribeConfigurationRevision'),
    newDescribeConfigurationRevision,
    DescribeConfigurationRevisionResponse (DescribeConfigurationRevisionResponse'),
    newDescribeConfigurationRevisionResponse,

    -- ** DeleteConfiguration
    DeleteConfiguration (DeleteConfiguration'),
    newDeleteConfiguration,
    DeleteConfigurationResponse (DeleteConfigurationResponse'),
    newDeleteConfigurationResponse,

    -- ** UpdateConfiguration
    UpdateConfiguration (UpdateConfiguration'),
    newUpdateConfiguration,
    UpdateConfigurationResponse (UpdateConfigurationResponse'),
    newUpdateConfigurationResponse,

    -- ** ListNodes (Paginated)
    ListNodes (ListNodes'),
    newListNodes,
    ListNodesResponse (ListNodesResponse'),
    newListNodesResponse,

    -- ** ListScramSecrets (Paginated)
    ListScramSecrets (ListScramSecrets'),
    newListScramSecrets,
    ListScramSecretsResponse (ListScramSecretsResponse'),
    newListScramSecretsResponse,

    -- * Types

    -- ** BrokerAZDistribution
    BrokerAZDistribution (..),

    -- ** ClientBroker
    ClientBroker (..),

    -- ** ClusterState
    ClusterState (..),

    -- ** ConfigurationState
    ConfigurationState (..),

    -- ** EnhancedMonitoring
    EnhancedMonitoring (..),

    -- ** KafkaVersionStatus
    KafkaVersionStatus (..),

    -- ** NodeType
    NodeType (..),

    -- ** BrokerEBSVolumeInfo
    BrokerEBSVolumeInfo (BrokerEBSVolumeInfo'),
    newBrokerEBSVolumeInfo,

    -- ** BrokerLogs
    BrokerLogs (BrokerLogs'),
    newBrokerLogs,

    -- ** BrokerNodeGroupInfo
    BrokerNodeGroupInfo (BrokerNodeGroupInfo'),
    newBrokerNodeGroupInfo,

    -- ** BrokerNodeInfo
    BrokerNodeInfo (BrokerNodeInfo'),
    newBrokerNodeInfo,

    -- ** BrokerSoftwareInfo
    BrokerSoftwareInfo (BrokerSoftwareInfo'),
    newBrokerSoftwareInfo,

    -- ** ClientAuthentication
    ClientAuthentication (ClientAuthentication'),
    newClientAuthentication,

    -- ** CloudWatchLogs
    CloudWatchLogs (CloudWatchLogs'),
    newCloudWatchLogs,

    -- ** ClusterInfo
    ClusterInfo (ClusterInfo'),
    newClusterInfo,

    -- ** ClusterOperationInfo
    ClusterOperationInfo (ClusterOperationInfo'),
    newClusterOperationInfo,

    -- ** ClusterOperationStep
    ClusterOperationStep (ClusterOperationStep'),
    newClusterOperationStep,

    -- ** ClusterOperationStepInfo
    ClusterOperationStepInfo (ClusterOperationStepInfo'),
    newClusterOperationStepInfo,

    -- ** CompatibleKafkaVersion
    CompatibleKafkaVersion (CompatibleKafkaVersion'),
    newCompatibleKafkaVersion,

    -- ** Configuration
    Configuration (Configuration'),
    newConfiguration,

    -- ** ConfigurationInfo
    ConfigurationInfo (ConfigurationInfo'),
    newConfigurationInfo,

    -- ** ConfigurationRevision
    ConfigurationRevision (ConfigurationRevision'),
    newConfigurationRevision,

    -- ** EBSStorageInfo
    EBSStorageInfo (EBSStorageInfo'),
    newEBSStorageInfo,

    -- ** EncryptionAtRest
    EncryptionAtRest (EncryptionAtRest'),
    newEncryptionAtRest,

    -- ** EncryptionInTransit
    EncryptionInTransit (EncryptionInTransit'),
    newEncryptionInTransit,

    -- ** EncryptionInfo
    EncryptionInfo (EncryptionInfo'),
    newEncryptionInfo,

    -- ** ErrorInfo
    ErrorInfo (ErrorInfo'),
    newErrorInfo,

    -- ** Firehose
    Firehose (Firehose'),
    newFirehose,

    -- ** Iam
    Iam (Iam'),
    newIam,

    -- ** JmxExporter
    JmxExporter (JmxExporter'),
    newJmxExporter,

    -- ** JmxExporterInfo
    JmxExporterInfo (JmxExporterInfo'),
    newJmxExporterInfo,

    -- ** KafkaVersion
    KafkaVersion (KafkaVersion'),
    newKafkaVersion,

    -- ** LoggingInfo
    LoggingInfo (LoggingInfo'),
    newLoggingInfo,

    -- ** MutableClusterInfo
    MutableClusterInfo (MutableClusterInfo'),
    newMutableClusterInfo,

    -- ** NodeExporter
    NodeExporter (NodeExporter'),
    newNodeExporter,

    -- ** NodeExporterInfo
    NodeExporterInfo (NodeExporterInfo'),
    newNodeExporterInfo,

    -- ** NodeInfo
    NodeInfo (NodeInfo'),
    newNodeInfo,

    -- ** OpenMonitoring
    OpenMonitoring (OpenMonitoring'),
    newOpenMonitoring,

    -- ** OpenMonitoringInfo
    OpenMonitoringInfo (OpenMonitoringInfo'),
    newOpenMonitoringInfo,

    -- ** Prometheus
    Prometheus (Prometheus'),
    newPrometheus,

    -- ** PrometheusInfo
    PrometheusInfo (PrometheusInfo'),
    newPrometheusInfo,

    -- ** S3
    S3 (S3'),
    newS3,

    -- ** Sasl
    Sasl (Sasl'),
    newSasl,

    -- ** Scram
    Scram (Scram'),
    newScram,

    -- ** StateInfo
    StateInfo (StateInfo'),
    newStateInfo,

    -- ** StorageInfo
    StorageInfo (StorageInfo'),
    newStorageInfo,

    -- ** Tls
    Tls (Tls'),
    newTls,

    -- ** Unauthenticated
    Unauthenticated (Unauthenticated'),
    newUnauthenticated,

    -- ** UnprocessedScramSecret
    UnprocessedScramSecret (UnprocessedScramSecret'),
    newUnprocessedScramSecret,

    -- ** ZookeeperNodeInfo
    ZookeeperNodeInfo (ZookeeperNodeInfo'),
    newZookeeperNodeInfo,
  )
where

import Network.AWS.Kafka.BatchAssociateScramSecret
import Network.AWS.Kafka.BatchDisassociateScramSecret
import Network.AWS.Kafka.CreateCluster
import Network.AWS.Kafka.CreateConfiguration
import Network.AWS.Kafka.DeleteCluster
import Network.AWS.Kafka.DeleteConfiguration
import Network.AWS.Kafka.DescribeCluster
import Network.AWS.Kafka.DescribeClusterOperation
import Network.AWS.Kafka.DescribeConfiguration
import Network.AWS.Kafka.DescribeConfigurationRevision
import Network.AWS.Kafka.GetBootstrapBrokers
import Network.AWS.Kafka.GetCompatibleKafkaVersions
import Network.AWS.Kafka.Lens
import Network.AWS.Kafka.ListClusterOperations
import Network.AWS.Kafka.ListClusters
import Network.AWS.Kafka.ListConfigurationRevisions
import Network.AWS.Kafka.ListConfigurations
import Network.AWS.Kafka.ListKafkaVersions
import Network.AWS.Kafka.ListNodes
import Network.AWS.Kafka.ListScramSecrets
import Network.AWS.Kafka.ListTagsForResource
import Network.AWS.Kafka.RebootBroker
import Network.AWS.Kafka.TagResource
import Network.AWS.Kafka.Types
import Network.AWS.Kafka.UntagResource
import Network.AWS.Kafka.UpdateBrokerCount
import Network.AWS.Kafka.UpdateBrokerStorage
import Network.AWS.Kafka.UpdateBrokerType
import Network.AWS.Kafka.UpdateClusterConfiguration
import Network.AWS.Kafka.UpdateClusterKafkaVersion
import Network.AWS.Kafka.UpdateConfiguration
import Network.AWS.Kafka.UpdateMonitoring
import Network.AWS.Kafka.UpdateSecurity
import Network.AWS.Kafka.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Kafka'.

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

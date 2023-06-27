{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Kafka
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The operations for managing an Amazon MSK cluster.
module Amazonka.Kafka
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchAssociateScramSecret
    BatchAssociateScramSecret (BatchAssociateScramSecret'),
    newBatchAssociateScramSecret,
    BatchAssociateScramSecretResponse (BatchAssociateScramSecretResponse'),
    newBatchAssociateScramSecretResponse,

    -- ** BatchDisassociateScramSecret
    BatchDisassociateScramSecret (BatchDisassociateScramSecret'),
    newBatchDisassociateScramSecret,
    BatchDisassociateScramSecretResponse (BatchDisassociateScramSecretResponse'),
    newBatchDisassociateScramSecretResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateClusterV2
    CreateClusterV2 (CreateClusterV2'),
    newCreateClusterV2,
    CreateClusterV2Response (CreateClusterV2Response'),
    newCreateClusterV2Response,

    -- ** CreateConfiguration
    CreateConfiguration (CreateConfiguration'),
    newCreateConfiguration,
    CreateConfigurationResponse (CreateConfigurationResponse'),
    newCreateConfigurationResponse,

    -- ** CreateVpcConnection
    CreateVpcConnection (CreateVpcConnection'),
    newCreateVpcConnection,
    CreateVpcConnectionResponse (CreateVpcConnectionResponse'),
    newCreateVpcConnectionResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteClusterPolicy
    DeleteClusterPolicy (DeleteClusterPolicy'),
    newDeleteClusterPolicy,
    DeleteClusterPolicyResponse (DeleteClusterPolicyResponse'),
    newDeleteClusterPolicyResponse,

    -- ** DeleteConfiguration
    DeleteConfiguration (DeleteConfiguration'),
    newDeleteConfiguration,
    DeleteConfigurationResponse (DeleteConfigurationResponse'),
    newDeleteConfigurationResponse,

    -- ** DeleteVpcConnection
    DeleteVpcConnection (DeleteVpcConnection'),
    newDeleteVpcConnection,
    DeleteVpcConnectionResponse (DeleteVpcConnectionResponse'),
    newDeleteVpcConnectionResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** DescribeClusterOperation
    DescribeClusterOperation (DescribeClusterOperation'),
    newDescribeClusterOperation,
    DescribeClusterOperationResponse (DescribeClusterOperationResponse'),
    newDescribeClusterOperationResponse,

    -- ** DescribeClusterV2
    DescribeClusterV2 (DescribeClusterV2'),
    newDescribeClusterV2,
    DescribeClusterV2Response (DescribeClusterV2Response'),
    newDescribeClusterV2Response,

    -- ** DescribeConfiguration
    DescribeConfiguration (DescribeConfiguration'),
    newDescribeConfiguration,
    DescribeConfigurationResponse (DescribeConfigurationResponse'),
    newDescribeConfigurationResponse,

    -- ** DescribeConfigurationRevision
    DescribeConfigurationRevision (DescribeConfigurationRevision'),
    newDescribeConfigurationRevision,
    DescribeConfigurationRevisionResponse (DescribeConfigurationRevisionResponse'),
    newDescribeConfigurationRevisionResponse,

    -- ** DescribeVpcConnection
    DescribeVpcConnection (DescribeVpcConnection'),
    newDescribeVpcConnection,
    DescribeVpcConnectionResponse (DescribeVpcConnectionResponse'),
    newDescribeVpcConnectionResponse,

    -- ** GetBootstrapBrokers
    GetBootstrapBrokers (GetBootstrapBrokers'),
    newGetBootstrapBrokers,
    GetBootstrapBrokersResponse (GetBootstrapBrokersResponse'),
    newGetBootstrapBrokersResponse,

    -- ** GetClusterPolicy
    GetClusterPolicy (GetClusterPolicy'),
    newGetClusterPolicy,
    GetClusterPolicyResponse (GetClusterPolicyResponse'),
    newGetClusterPolicyResponse,

    -- ** GetCompatibleKafkaVersions
    GetCompatibleKafkaVersions (GetCompatibleKafkaVersions'),
    newGetCompatibleKafkaVersions,
    GetCompatibleKafkaVersionsResponse (GetCompatibleKafkaVersionsResponse'),
    newGetCompatibleKafkaVersionsResponse,

    -- ** ListClientVpcConnections (Paginated)
    ListClientVpcConnections (ListClientVpcConnections'),
    newListClientVpcConnections,
    ListClientVpcConnectionsResponse (ListClientVpcConnectionsResponse'),
    newListClientVpcConnectionsResponse,

    -- ** ListClusterOperations (Paginated)
    ListClusterOperations (ListClusterOperations'),
    newListClusterOperations,
    ListClusterOperationsResponse (ListClusterOperationsResponse'),
    newListClusterOperationsResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** ListClustersV2 (Paginated)
    ListClustersV2 (ListClustersV2'),
    newListClustersV2,
    ListClustersV2Response (ListClustersV2Response'),
    newListClustersV2Response,

    -- ** ListConfigurationRevisions (Paginated)
    ListConfigurationRevisions (ListConfigurationRevisions'),
    newListConfigurationRevisions,
    ListConfigurationRevisionsResponse (ListConfigurationRevisionsResponse'),
    newListConfigurationRevisionsResponse,

    -- ** ListConfigurations (Paginated)
    ListConfigurations (ListConfigurations'),
    newListConfigurations,
    ListConfigurationsResponse (ListConfigurationsResponse'),
    newListConfigurationsResponse,

    -- ** ListKafkaVersions (Paginated)
    ListKafkaVersions (ListKafkaVersions'),
    newListKafkaVersions,
    ListKafkaVersionsResponse (ListKafkaVersionsResponse'),
    newListKafkaVersionsResponse,

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

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListVpcConnections (Paginated)
    ListVpcConnections (ListVpcConnections'),
    newListVpcConnections,
    ListVpcConnectionsResponse (ListVpcConnectionsResponse'),
    newListVpcConnectionsResponse,

    -- ** PutClusterPolicy
    PutClusterPolicy (PutClusterPolicy'),
    newPutClusterPolicy,
    PutClusterPolicyResponse (PutClusterPolicyResponse'),
    newPutClusterPolicyResponse,

    -- ** RebootBroker
    RebootBroker (RebootBroker'),
    newRebootBroker,
    RebootBrokerResponse (RebootBrokerResponse'),
    newRebootBrokerResponse,

    -- ** RejectClientVpcConnection
    RejectClientVpcConnection (RejectClientVpcConnection'),
    newRejectClientVpcConnection,
    RejectClientVpcConnectionResponse (RejectClientVpcConnectionResponse'),
    newRejectClientVpcConnectionResponse,

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

    -- ** UpdateBrokerCount
    UpdateBrokerCount (UpdateBrokerCount'),
    newUpdateBrokerCount,
    UpdateBrokerCountResponse (UpdateBrokerCountResponse'),
    newUpdateBrokerCountResponse,

    -- ** UpdateBrokerStorage
    UpdateBrokerStorage (UpdateBrokerStorage'),
    newUpdateBrokerStorage,
    UpdateBrokerStorageResponse (UpdateBrokerStorageResponse'),
    newUpdateBrokerStorageResponse,

    -- ** UpdateBrokerType
    UpdateBrokerType (UpdateBrokerType'),
    newUpdateBrokerType,
    UpdateBrokerTypeResponse (UpdateBrokerTypeResponse'),
    newUpdateBrokerTypeResponse,

    -- ** UpdateClusterConfiguration
    UpdateClusterConfiguration (UpdateClusterConfiguration'),
    newUpdateClusterConfiguration,
    UpdateClusterConfigurationResponse (UpdateClusterConfigurationResponse'),
    newUpdateClusterConfigurationResponse,

    -- ** UpdateClusterKafkaVersion
    UpdateClusterKafkaVersion (UpdateClusterKafkaVersion'),
    newUpdateClusterKafkaVersion,
    UpdateClusterKafkaVersionResponse (UpdateClusterKafkaVersionResponse'),
    newUpdateClusterKafkaVersionResponse,

    -- ** UpdateConfiguration
    UpdateConfiguration (UpdateConfiguration'),
    newUpdateConfiguration,
    UpdateConfigurationResponse (UpdateConfigurationResponse'),
    newUpdateConfigurationResponse,

    -- ** UpdateConnectivity
    UpdateConnectivity (UpdateConnectivity'),
    newUpdateConnectivity,
    UpdateConnectivityResponse (UpdateConnectivityResponse'),
    newUpdateConnectivityResponse,

    -- ** UpdateMonitoring
    UpdateMonitoring (UpdateMonitoring'),
    newUpdateMonitoring,
    UpdateMonitoringResponse (UpdateMonitoringResponse'),
    newUpdateMonitoringResponse,

    -- ** UpdateSecurity
    UpdateSecurity (UpdateSecurity'),
    newUpdateSecurity,
    UpdateSecurityResponse (UpdateSecurityResponse'),
    newUpdateSecurityResponse,

    -- ** UpdateStorage
    UpdateStorage (UpdateStorage'),
    newUpdateStorage,
    UpdateStorageResponse (UpdateStorageResponse'),
    newUpdateStorageResponse,

    -- * Types

    -- ** BrokerAZDistribution
    BrokerAZDistribution (..),

    -- ** ClientBroker
    ClientBroker (..),

    -- ** ClusterState
    ClusterState (..),

    -- ** ClusterType
    ClusterType (..),

    -- ** ConfigurationState
    ConfigurationState (..),

    -- ** EnhancedMonitoring
    EnhancedMonitoring (..),

    -- ** KafkaVersionStatus
    KafkaVersionStatus (..),

    -- ** NodeType
    NodeType (..),

    -- ** StorageMode
    StorageMode (..),

    -- ** UserIdentityType
    UserIdentityType (..),

    -- ** VpcConnectionState
    VpcConnectionState (..),

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

    -- ** ClientVpcConnection
    ClientVpcConnection (ClientVpcConnection'),
    newClientVpcConnection,

    -- ** CloudWatchLogs
    CloudWatchLogs (CloudWatchLogs'),
    newCloudWatchLogs,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

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

    -- ** ConnectivityInfo
    ConnectivityInfo (ConnectivityInfo'),
    newConnectivityInfo,

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

    -- ** Provisioned
    Provisioned (Provisioned'),
    newProvisioned,

    -- ** ProvisionedRequest
    ProvisionedRequest (ProvisionedRequest'),
    newProvisionedRequest,

    -- ** ProvisionedThroughput
    ProvisionedThroughput (ProvisionedThroughput'),
    newProvisionedThroughput,

    -- ** PublicAccess
    PublicAccess (PublicAccess'),
    newPublicAccess,

    -- ** S3
    S3 (S3'),
    newS3,

    -- ** Sasl
    Sasl (Sasl'),
    newSasl,

    -- ** Scram
    Scram (Scram'),
    newScram,

    -- ** Serverless
    Serverless (Serverless'),
    newServerless,

    -- ** ServerlessClientAuthentication
    ServerlessClientAuthentication (ServerlessClientAuthentication'),
    newServerlessClientAuthentication,

    -- ** ServerlessRequest
    ServerlessRequest (ServerlessRequest'),
    newServerlessRequest,

    -- ** ServerlessSasl
    ServerlessSasl (ServerlessSasl'),
    newServerlessSasl,

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

    -- ** UserIdentity
    UserIdentity (UserIdentity'),
    newUserIdentity,

    -- ** VpcConfig
    VpcConfig (VpcConfig'),
    newVpcConfig,

    -- ** VpcConnection
    VpcConnection (VpcConnection'),
    newVpcConnection,

    -- ** VpcConnectionInfo
    VpcConnectionInfo (VpcConnectionInfo'),
    newVpcConnectionInfo,

    -- ** VpcConnectivity
    VpcConnectivity (VpcConnectivity'),
    newVpcConnectivity,

    -- ** VpcConnectivityClientAuthentication
    VpcConnectivityClientAuthentication (VpcConnectivityClientAuthentication'),
    newVpcConnectivityClientAuthentication,

    -- ** VpcConnectivityIam
    VpcConnectivityIam (VpcConnectivityIam'),
    newVpcConnectivityIam,

    -- ** VpcConnectivitySasl
    VpcConnectivitySasl (VpcConnectivitySasl'),
    newVpcConnectivitySasl,

    -- ** VpcConnectivityScram
    VpcConnectivityScram (VpcConnectivityScram'),
    newVpcConnectivityScram,

    -- ** VpcConnectivityTls
    VpcConnectivityTls (VpcConnectivityTls'),
    newVpcConnectivityTls,

    -- ** ZookeeperNodeInfo
    ZookeeperNodeInfo (ZookeeperNodeInfo'),
    newZookeeperNodeInfo,
  )
where

import Amazonka.Kafka.BatchAssociateScramSecret
import Amazonka.Kafka.BatchDisassociateScramSecret
import Amazonka.Kafka.CreateCluster
import Amazonka.Kafka.CreateClusterV2
import Amazonka.Kafka.CreateConfiguration
import Amazonka.Kafka.CreateVpcConnection
import Amazonka.Kafka.DeleteCluster
import Amazonka.Kafka.DeleteClusterPolicy
import Amazonka.Kafka.DeleteConfiguration
import Amazonka.Kafka.DeleteVpcConnection
import Amazonka.Kafka.DescribeCluster
import Amazonka.Kafka.DescribeClusterOperation
import Amazonka.Kafka.DescribeClusterV2
import Amazonka.Kafka.DescribeConfiguration
import Amazonka.Kafka.DescribeConfigurationRevision
import Amazonka.Kafka.DescribeVpcConnection
import Amazonka.Kafka.GetBootstrapBrokers
import Amazonka.Kafka.GetClusterPolicy
import Amazonka.Kafka.GetCompatibleKafkaVersions
import Amazonka.Kafka.Lens
import Amazonka.Kafka.ListClientVpcConnections
import Amazonka.Kafka.ListClusterOperations
import Amazonka.Kafka.ListClusters
import Amazonka.Kafka.ListClustersV2
import Amazonka.Kafka.ListConfigurationRevisions
import Amazonka.Kafka.ListConfigurations
import Amazonka.Kafka.ListKafkaVersions
import Amazonka.Kafka.ListNodes
import Amazonka.Kafka.ListScramSecrets
import Amazonka.Kafka.ListTagsForResource
import Amazonka.Kafka.ListVpcConnections
import Amazonka.Kafka.PutClusterPolicy
import Amazonka.Kafka.RebootBroker
import Amazonka.Kafka.RejectClientVpcConnection
import Amazonka.Kafka.TagResource
import Amazonka.Kafka.Types
import Amazonka.Kafka.UntagResource
import Amazonka.Kafka.UpdateBrokerCount
import Amazonka.Kafka.UpdateBrokerStorage
import Amazonka.Kafka.UpdateBrokerType
import Amazonka.Kafka.UpdateClusterConfiguration
import Amazonka.Kafka.UpdateClusterKafkaVersion
import Amazonka.Kafka.UpdateConfiguration
import Amazonka.Kafka.UpdateConnectivity
import Amazonka.Kafka.UpdateMonitoring
import Amazonka.Kafka.UpdateSecurity
import Amazonka.Kafka.UpdateStorage
import Amazonka.Kafka.Waiters

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

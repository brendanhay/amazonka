{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Kafka
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Kafka where

import Amazonka.Kafka
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Kafka.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchAssociateScramSecret $
--             newBatchAssociateScramSecret
--
--         , requestBatchDisassociateScramSecret $
--             newBatchDisassociateScramSecret
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateClusterV2 $
--             newCreateClusterV2
--
--         , requestCreateConfiguration $
--             newCreateConfiguration
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteConfiguration $
--             newDeleteConfiguration
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestDescribeClusterOperation $
--             newDescribeClusterOperation
--
--         , requestDescribeClusterV2 $
--             newDescribeClusterV2
--
--         , requestDescribeConfiguration $
--             newDescribeConfiguration
--
--         , requestDescribeConfigurationRevision $
--             newDescribeConfigurationRevision
--
--         , requestGetBootstrapBrokers $
--             newGetBootstrapBrokers
--
--         , requestGetCompatibleKafkaVersions $
--             newGetCompatibleKafkaVersions
--
--         , requestListClusterOperations $
--             newListClusterOperations
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListClustersV2 $
--             newListClustersV2
--
--         , requestListConfigurationRevisions $
--             newListConfigurationRevisions
--
--         , requestListConfigurations $
--             newListConfigurations
--
--         , requestListKafkaVersions $
--             newListKafkaVersions
--
--         , requestListNodes $
--             newListNodes
--
--         , requestListScramSecrets $
--             newListScramSecrets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRebootBroker $
--             newRebootBroker
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateBrokerCount $
--             newUpdateBrokerCount
--
--         , requestUpdateBrokerStorage $
--             newUpdateBrokerStorage
--
--         , requestUpdateBrokerType $
--             newUpdateBrokerType
--
--         , requestUpdateClusterConfiguration $
--             newUpdateClusterConfiguration
--
--         , requestUpdateClusterKafkaVersion $
--             newUpdateClusterKafkaVersion
--
--         , requestUpdateConfiguration $
--             newUpdateConfiguration
--
--         , requestUpdateConnectivity $
--             newUpdateConnectivity
--
--         , requestUpdateMonitoring $
--             newUpdateMonitoring
--
--         , requestUpdateSecurity $
--             newUpdateSecurity
--
--         , requestUpdateStorage $
--             newUpdateStorage
--
--           ]

--     , testGroup "response"
--         [ responseBatchAssociateScramSecret $
--             newBatchAssociateScramSecretResponse
--
--         , responseBatchDisassociateScramSecret $
--             newBatchDisassociateScramSecretResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateClusterV2 $
--             newCreateClusterV2Response
--
--         , responseCreateConfiguration $
--             newCreateConfigurationResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteConfiguration $
--             newDeleteConfigurationResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseDescribeClusterOperation $
--             newDescribeClusterOperationResponse
--
--         , responseDescribeClusterV2 $
--             newDescribeClusterV2Response
--
--         , responseDescribeConfiguration $
--             newDescribeConfigurationResponse
--
--         , responseDescribeConfigurationRevision $
--             newDescribeConfigurationRevisionResponse
--
--         , responseGetBootstrapBrokers $
--             newGetBootstrapBrokersResponse
--
--         , responseGetCompatibleKafkaVersions $
--             newGetCompatibleKafkaVersionsResponse
--
--         , responseListClusterOperations $
--             newListClusterOperationsResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListClustersV2 $
--             newListClustersV2Response
--
--         , responseListConfigurationRevisions $
--             newListConfigurationRevisionsResponse
--
--         , responseListConfigurations $
--             newListConfigurationsResponse
--
--         , responseListKafkaVersions $
--             newListKafkaVersionsResponse
--
--         , responseListNodes $
--             newListNodesResponse
--
--         , responseListScramSecrets $
--             newListScramSecretsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRebootBroker $
--             newRebootBrokerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateBrokerCount $
--             newUpdateBrokerCountResponse
--
--         , responseUpdateBrokerStorage $
--             newUpdateBrokerStorageResponse
--
--         , responseUpdateBrokerType $
--             newUpdateBrokerTypeResponse
--
--         , responseUpdateClusterConfiguration $
--             newUpdateClusterConfigurationResponse
--
--         , responseUpdateClusterKafkaVersion $
--             newUpdateClusterKafkaVersionResponse
--
--         , responseUpdateConfiguration $
--             newUpdateConfigurationResponse
--
--         , responseUpdateConnectivity $
--             newUpdateConnectivityResponse
--
--         , responseUpdateMonitoring $
--             newUpdateMonitoringResponse
--
--         , responseUpdateSecurity $
--             newUpdateSecurityResponse
--
--         , responseUpdateStorage $
--             newUpdateStorageResponse
--
--           ]
--     ]

-- Requests

requestBatchAssociateScramSecret :: BatchAssociateScramSecret -> TestTree
requestBatchAssociateScramSecret =
  req
    "BatchAssociateScramSecret"
    "fixture/BatchAssociateScramSecret.yaml"

requestBatchDisassociateScramSecret :: BatchDisassociateScramSecret -> TestTree
requestBatchDisassociateScramSecret =
  req
    "BatchDisassociateScramSecret"
    "fixture/BatchDisassociateScramSecret.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateClusterV2 :: CreateClusterV2 -> TestTree
requestCreateClusterV2 =
  req
    "CreateClusterV2"
    "fixture/CreateClusterV2.yaml"

requestCreateConfiguration :: CreateConfiguration -> TestTree
requestCreateConfiguration =
  req
    "CreateConfiguration"
    "fixture/CreateConfiguration.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteConfiguration :: DeleteConfiguration -> TestTree
requestDeleteConfiguration =
  req
    "DeleteConfiguration"
    "fixture/DeleteConfiguration.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestDescribeClusterOperation :: DescribeClusterOperation -> TestTree
requestDescribeClusterOperation =
  req
    "DescribeClusterOperation"
    "fixture/DescribeClusterOperation.yaml"

requestDescribeClusterV2 :: DescribeClusterV2 -> TestTree
requestDescribeClusterV2 =
  req
    "DescribeClusterV2"
    "fixture/DescribeClusterV2.yaml"

requestDescribeConfiguration :: DescribeConfiguration -> TestTree
requestDescribeConfiguration =
  req
    "DescribeConfiguration"
    "fixture/DescribeConfiguration.yaml"

requestDescribeConfigurationRevision :: DescribeConfigurationRevision -> TestTree
requestDescribeConfigurationRevision =
  req
    "DescribeConfigurationRevision"
    "fixture/DescribeConfigurationRevision.yaml"

requestGetBootstrapBrokers :: GetBootstrapBrokers -> TestTree
requestGetBootstrapBrokers =
  req
    "GetBootstrapBrokers"
    "fixture/GetBootstrapBrokers.yaml"

requestGetCompatibleKafkaVersions :: GetCompatibleKafkaVersions -> TestTree
requestGetCompatibleKafkaVersions =
  req
    "GetCompatibleKafkaVersions"
    "fixture/GetCompatibleKafkaVersions.yaml"

requestListClusterOperations :: ListClusterOperations -> TestTree
requestListClusterOperations =
  req
    "ListClusterOperations"
    "fixture/ListClusterOperations.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListClustersV2 :: ListClustersV2 -> TestTree
requestListClustersV2 =
  req
    "ListClustersV2"
    "fixture/ListClustersV2.yaml"

requestListConfigurationRevisions :: ListConfigurationRevisions -> TestTree
requestListConfigurationRevisions =
  req
    "ListConfigurationRevisions"
    "fixture/ListConfigurationRevisions.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations =
  req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestListKafkaVersions :: ListKafkaVersions -> TestTree
requestListKafkaVersions =
  req
    "ListKafkaVersions"
    "fixture/ListKafkaVersions.yaml"

requestListNodes :: ListNodes -> TestTree
requestListNodes =
  req
    "ListNodes"
    "fixture/ListNodes.yaml"

requestListScramSecrets :: ListScramSecrets -> TestTree
requestListScramSecrets =
  req
    "ListScramSecrets"
    "fixture/ListScramSecrets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRebootBroker :: RebootBroker -> TestTree
requestRebootBroker =
  req
    "RebootBroker"
    "fixture/RebootBroker.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateBrokerCount :: UpdateBrokerCount -> TestTree
requestUpdateBrokerCount =
  req
    "UpdateBrokerCount"
    "fixture/UpdateBrokerCount.yaml"

requestUpdateBrokerStorage :: UpdateBrokerStorage -> TestTree
requestUpdateBrokerStorage =
  req
    "UpdateBrokerStorage"
    "fixture/UpdateBrokerStorage.yaml"

requestUpdateBrokerType :: UpdateBrokerType -> TestTree
requestUpdateBrokerType =
  req
    "UpdateBrokerType"
    "fixture/UpdateBrokerType.yaml"

requestUpdateClusterConfiguration :: UpdateClusterConfiguration -> TestTree
requestUpdateClusterConfiguration =
  req
    "UpdateClusterConfiguration"
    "fixture/UpdateClusterConfiguration.yaml"

requestUpdateClusterKafkaVersion :: UpdateClusterKafkaVersion -> TestTree
requestUpdateClusterKafkaVersion =
  req
    "UpdateClusterKafkaVersion"
    "fixture/UpdateClusterKafkaVersion.yaml"

requestUpdateConfiguration :: UpdateConfiguration -> TestTree
requestUpdateConfiguration =
  req
    "UpdateConfiguration"
    "fixture/UpdateConfiguration.yaml"

requestUpdateConnectivity :: UpdateConnectivity -> TestTree
requestUpdateConnectivity =
  req
    "UpdateConnectivity"
    "fixture/UpdateConnectivity.yaml"

requestUpdateMonitoring :: UpdateMonitoring -> TestTree
requestUpdateMonitoring =
  req
    "UpdateMonitoring"
    "fixture/UpdateMonitoring.yaml"

requestUpdateSecurity :: UpdateSecurity -> TestTree
requestUpdateSecurity =
  req
    "UpdateSecurity"
    "fixture/UpdateSecurity.yaml"

requestUpdateStorage :: UpdateStorage -> TestTree
requestUpdateStorage =
  req
    "UpdateStorage"
    "fixture/UpdateStorage.yaml"

-- Responses

responseBatchAssociateScramSecret :: BatchAssociateScramSecretResponse -> TestTree
responseBatchAssociateScramSecret =
  res
    "BatchAssociateScramSecretResponse"
    "fixture/BatchAssociateScramSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateScramSecret)

responseBatchDisassociateScramSecret :: BatchDisassociateScramSecretResponse -> TestTree
responseBatchDisassociateScramSecret =
  res
    "BatchDisassociateScramSecretResponse"
    "fixture/BatchDisassociateScramSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateScramSecret)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateClusterV2 :: CreateClusterV2Response -> TestTree
responseCreateClusterV2 =
  res
    "CreateClusterV2Response"
    "fixture/CreateClusterV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateClusterV2)

responseCreateConfiguration :: CreateConfigurationResponse -> TestTree
responseCreateConfiguration =
  res
    "CreateConfigurationResponse"
    "fixture/CreateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfiguration)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteConfiguration :: DeleteConfigurationResponse -> TestTree
responseDeleteConfiguration =
  res
    "DeleteConfigurationResponse"
    "fixture/DeleteConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfiguration)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseDescribeClusterOperation :: DescribeClusterOperationResponse -> TestTree
responseDescribeClusterOperation =
  res
    "DescribeClusterOperationResponse"
    "fixture/DescribeClusterOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterOperation)

responseDescribeClusterV2 :: DescribeClusterV2Response -> TestTree
responseDescribeClusterV2 =
  res
    "DescribeClusterV2Response"
    "fixture/DescribeClusterV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterV2)

responseDescribeConfiguration :: DescribeConfigurationResponse -> TestTree
responseDescribeConfiguration =
  res
    "DescribeConfigurationResponse"
    "fixture/DescribeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfiguration)

responseDescribeConfigurationRevision :: DescribeConfigurationRevisionResponse -> TestTree
responseDescribeConfigurationRevision =
  res
    "DescribeConfigurationRevisionResponse"
    "fixture/DescribeConfigurationRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationRevision)

responseGetBootstrapBrokers :: GetBootstrapBrokersResponse -> TestTree
responseGetBootstrapBrokers =
  res
    "GetBootstrapBrokersResponse"
    "fixture/GetBootstrapBrokersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBootstrapBrokers)

responseGetCompatibleKafkaVersions :: GetCompatibleKafkaVersionsResponse -> TestTree
responseGetCompatibleKafkaVersions =
  res
    "GetCompatibleKafkaVersionsResponse"
    "fixture/GetCompatibleKafkaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCompatibleKafkaVersions)

responseListClusterOperations :: ListClusterOperationsResponse -> TestTree
responseListClusterOperations =
  res
    "ListClusterOperationsResponse"
    "fixture/ListClusterOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusterOperations)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListClustersV2 :: ListClustersV2Response -> TestTree
responseListClustersV2 =
  res
    "ListClustersV2Response"
    "fixture/ListClustersV2Response.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClustersV2)

responseListConfigurationRevisions :: ListConfigurationRevisionsResponse -> TestTree
responseListConfigurationRevisions =
  res
    "ListConfigurationRevisionsResponse"
    "fixture/ListConfigurationRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationRevisions)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations =
  res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurations)

responseListKafkaVersions :: ListKafkaVersionsResponse -> TestTree
responseListKafkaVersions =
  res
    "ListKafkaVersionsResponse"
    "fixture/ListKafkaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKafkaVersions)

responseListNodes :: ListNodesResponse -> TestTree
responseListNodes =
  res
    "ListNodesResponse"
    "fixture/ListNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNodes)

responseListScramSecrets :: ListScramSecretsResponse -> TestTree
responseListScramSecrets =
  res
    "ListScramSecretsResponse"
    "fixture/ListScramSecretsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListScramSecrets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRebootBroker :: RebootBrokerResponse -> TestTree
responseRebootBroker =
  res
    "RebootBrokerResponse"
    "fixture/RebootBrokerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootBroker)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateBrokerCount :: UpdateBrokerCountResponse -> TestTree
responseUpdateBrokerCount =
  res
    "UpdateBrokerCountResponse"
    "fixture/UpdateBrokerCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBrokerCount)

responseUpdateBrokerStorage :: UpdateBrokerStorageResponse -> TestTree
responseUpdateBrokerStorage =
  res
    "UpdateBrokerStorageResponse"
    "fixture/UpdateBrokerStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBrokerStorage)

responseUpdateBrokerType :: UpdateBrokerTypeResponse -> TestTree
responseUpdateBrokerType =
  res
    "UpdateBrokerTypeResponse"
    "fixture/UpdateBrokerTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBrokerType)

responseUpdateClusterConfiguration :: UpdateClusterConfigurationResponse -> TestTree
responseUpdateClusterConfiguration =
  res
    "UpdateClusterConfigurationResponse"
    "fixture/UpdateClusterConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterConfiguration)

responseUpdateClusterKafkaVersion :: UpdateClusterKafkaVersionResponse -> TestTree
responseUpdateClusterKafkaVersion =
  res
    "UpdateClusterKafkaVersionResponse"
    "fixture/UpdateClusterKafkaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterKafkaVersion)

responseUpdateConfiguration :: UpdateConfigurationResponse -> TestTree
responseUpdateConfiguration =
  res
    "UpdateConfigurationResponse"
    "fixture/UpdateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfiguration)

responseUpdateConnectivity :: UpdateConnectivityResponse -> TestTree
responseUpdateConnectivity =
  res
    "UpdateConnectivityResponse"
    "fixture/UpdateConnectivityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectivity)

responseUpdateMonitoring :: UpdateMonitoringResponse -> TestTree
responseUpdateMonitoring =
  res
    "UpdateMonitoringResponse"
    "fixture/UpdateMonitoringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMonitoring)

responseUpdateSecurity :: UpdateSecurityResponse -> TestTree
responseUpdateSecurity =
  res
    "UpdateSecurityResponse"
    "fixture/UpdateSecurityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurity)

responseUpdateStorage :: UpdateStorageResponse -> TestTree
responseUpdateStorage =
  res
    "UpdateStorageResponse"
    "fixture/UpdateStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStorage)

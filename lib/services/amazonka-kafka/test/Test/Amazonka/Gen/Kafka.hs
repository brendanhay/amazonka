{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Kafka
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateConfiguration $
--             newCreateConfiguration
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestRebootBroker $
--             newRebootBroker
--
--         , requestListConfigurationRevisions $
--             newListConfigurationRevisions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListKafkaVersions $
--             newListKafkaVersions
--
--         , requestUpdateMonitoring $
--             newUpdateMonitoring
--
--         , requestBatchAssociateScramSecret $
--             newBatchAssociateScramSecret
--
--         , requestUpdateBrokerStorage $
--             newUpdateBrokerStorage
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestUpdateClusterConfiguration $
--             newUpdateClusterConfiguration
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestUpdateBrokerCount $
--             newUpdateBrokerCount
--
--         , requestListConfigurations $
--             newListConfigurations
--
--         , requestGetBootstrapBrokers $
--             newGetBootstrapBrokers
--
--         , requestUpdateClusterKafkaVersion $
--             newUpdateClusterKafkaVersion
--
--         , requestUpdateSecurity $
--             newUpdateSecurity
--
--         , requestGetCompatibleKafkaVersions $
--             newGetCompatibleKafkaVersions
--
--         , requestDescribeClusterOperation $
--             newDescribeClusterOperation
--
--         , requestUpdateBrokerType $
--             newUpdateBrokerType
--
--         , requestDescribeConfiguration $
--             newDescribeConfiguration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListClusters $
--             newListClusters
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListClusterOperations $
--             newListClusterOperations
--
--         , requestBatchDisassociateScramSecret $
--             newBatchDisassociateScramSecret
--
--         , requestDescribeConfigurationRevision $
--             newDescribeConfigurationRevision
--
--         , requestDeleteConfiguration $
--             newDeleteConfiguration
--
--         , requestUpdateConfiguration $
--             newUpdateConfiguration
--
--         , requestListNodes $
--             newListNodes
--
--         , requestListScramSecrets $
--             newListScramSecrets
--
--           ]

--     , testGroup "response"
--         [ responseCreateConfiguration $
--             newCreateConfigurationResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseRebootBroker $
--             newRebootBrokerResponse
--
--         , responseListConfigurationRevisions $
--             newListConfigurationRevisionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListKafkaVersions $
--             newListKafkaVersionsResponse
--
--         , responseUpdateMonitoring $
--             newUpdateMonitoringResponse
--
--         , responseBatchAssociateScramSecret $
--             newBatchAssociateScramSecretResponse
--
--         , responseUpdateBrokerStorage $
--             newUpdateBrokerStorageResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseUpdateClusterConfiguration $
--             newUpdateClusterConfigurationResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseUpdateBrokerCount $
--             newUpdateBrokerCountResponse
--
--         , responseListConfigurations $
--             newListConfigurationsResponse
--
--         , responseGetBootstrapBrokers $
--             newGetBootstrapBrokersResponse
--
--         , responseUpdateClusterKafkaVersion $
--             newUpdateClusterKafkaVersionResponse
--
--         , responseUpdateSecurity $
--             newUpdateSecurityResponse
--
--         , responseGetCompatibleKafkaVersions $
--             newGetCompatibleKafkaVersionsResponse
--
--         , responseDescribeClusterOperation $
--             newDescribeClusterOperationResponse
--
--         , responseUpdateBrokerType $
--             newUpdateBrokerTypeResponse
--
--         , responseDescribeConfiguration $
--             newDescribeConfigurationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListClusterOperations $
--             newListClusterOperationsResponse
--
--         , responseBatchDisassociateScramSecret $
--             newBatchDisassociateScramSecretResponse
--
--         , responseDescribeConfigurationRevision $
--             newDescribeConfigurationRevisionResponse
--
--         , responseDeleteConfiguration $
--             newDeleteConfigurationResponse
--
--         , responseUpdateConfiguration $
--             newUpdateConfigurationResponse
--
--         , responseListNodes $
--             newListNodesResponse
--
--         , responseListScramSecrets $
--             newListScramSecretsResponse
--
--           ]
--     ]

-- Requests

requestCreateConfiguration :: CreateConfiguration -> TestTree
requestCreateConfiguration =
  req
    "CreateConfiguration"
    "fixture/CreateConfiguration.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestRebootBroker :: RebootBroker -> TestTree
requestRebootBroker =
  req
    "RebootBroker"
    "fixture/RebootBroker.yaml"

requestListConfigurationRevisions :: ListConfigurationRevisions -> TestTree
requestListConfigurationRevisions =
  req
    "ListConfigurationRevisions"
    "fixture/ListConfigurationRevisions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListKafkaVersions :: ListKafkaVersions -> TestTree
requestListKafkaVersions =
  req
    "ListKafkaVersions"
    "fixture/ListKafkaVersions.yaml"

requestUpdateMonitoring :: UpdateMonitoring -> TestTree
requestUpdateMonitoring =
  req
    "UpdateMonitoring"
    "fixture/UpdateMonitoring.yaml"

requestBatchAssociateScramSecret :: BatchAssociateScramSecret -> TestTree
requestBatchAssociateScramSecret =
  req
    "BatchAssociateScramSecret"
    "fixture/BatchAssociateScramSecret.yaml"

requestUpdateBrokerStorage :: UpdateBrokerStorage -> TestTree
requestUpdateBrokerStorage =
  req
    "UpdateBrokerStorage"
    "fixture/UpdateBrokerStorage.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestUpdateClusterConfiguration :: UpdateClusterConfiguration -> TestTree
requestUpdateClusterConfiguration =
  req
    "UpdateClusterConfiguration"
    "fixture/UpdateClusterConfiguration.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestUpdateBrokerCount :: UpdateBrokerCount -> TestTree
requestUpdateBrokerCount =
  req
    "UpdateBrokerCount"
    "fixture/UpdateBrokerCount.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations =
  req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestGetBootstrapBrokers :: GetBootstrapBrokers -> TestTree
requestGetBootstrapBrokers =
  req
    "GetBootstrapBrokers"
    "fixture/GetBootstrapBrokers.yaml"

requestUpdateClusterKafkaVersion :: UpdateClusterKafkaVersion -> TestTree
requestUpdateClusterKafkaVersion =
  req
    "UpdateClusterKafkaVersion"
    "fixture/UpdateClusterKafkaVersion.yaml"

requestUpdateSecurity :: UpdateSecurity -> TestTree
requestUpdateSecurity =
  req
    "UpdateSecurity"
    "fixture/UpdateSecurity.yaml"

requestGetCompatibleKafkaVersions :: GetCompatibleKafkaVersions -> TestTree
requestGetCompatibleKafkaVersions =
  req
    "GetCompatibleKafkaVersions"
    "fixture/GetCompatibleKafkaVersions.yaml"

requestDescribeClusterOperation :: DescribeClusterOperation -> TestTree
requestDescribeClusterOperation =
  req
    "DescribeClusterOperation"
    "fixture/DescribeClusterOperation.yaml"

requestUpdateBrokerType :: UpdateBrokerType -> TestTree
requestUpdateBrokerType =
  req
    "UpdateBrokerType"
    "fixture/UpdateBrokerType.yaml"

requestDescribeConfiguration :: DescribeConfiguration -> TestTree
requestDescribeConfiguration =
  req
    "DescribeConfiguration"
    "fixture/DescribeConfiguration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListClusterOperations :: ListClusterOperations -> TestTree
requestListClusterOperations =
  req
    "ListClusterOperations"
    "fixture/ListClusterOperations.yaml"

requestBatchDisassociateScramSecret :: BatchDisassociateScramSecret -> TestTree
requestBatchDisassociateScramSecret =
  req
    "BatchDisassociateScramSecret"
    "fixture/BatchDisassociateScramSecret.yaml"

requestDescribeConfigurationRevision :: DescribeConfigurationRevision -> TestTree
requestDescribeConfigurationRevision =
  req
    "DescribeConfigurationRevision"
    "fixture/DescribeConfigurationRevision.yaml"

requestDeleteConfiguration :: DeleteConfiguration -> TestTree
requestDeleteConfiguration =
  req
    "DeleteConfiguration"
    "fixture/DeleteConfiguration.yaml"

requestUpdateConfiguration :: UpdateConfiguration -> TestTree
requestUpdateConfiguration =
  req
    "UpdateConfiguration"
    "fixture/UpdateConfiguration.yaml"

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

-- Responses

responseCreateConfiguration :: CreateConfigurationResponse -> TestTree
responseCreateConfiguration =
  res
    "CreateConfigurationResponse"
    "fixture/CreateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfiguration)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseRebootBroker :: RebootBrokerResponse -> TestTree
responseRebootBroker =
  res
    "RebootBrokerResponse"
    "fixture/RebootBrokerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootBroker)

responseListConfigurationRevisions :: ListConfigurationRevisionsResponse -> TestTree
responseListConfigurationRevisions =
  res
    "ListConfigurationRevisionsResponse"
    "fixture/ListConfigurationRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationRevisions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListKafkaVersions :: ListKafkaVersionsResponse -> TestTree
responseListKafkaVersions =
  res
    "ListKafkaVersionsResponse"
    "fixture/ListKafkaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKafkaVersions)

responseUpdateMonitoring :: UpdateMonitoringResponse -> TestTree
responseUpdateMonitoring =
  res
    "UpdateMonitoringResponse"
    "fixture/UpdateMonitoringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMonitoring)

responseBatchAssociateScramSecret :: BatchAssociateScramSecretResponse -> TestTree
responseBatchAssociateScramSecret =
  res
    "BatchAssociateScramSecretResponse"
    "fixture/BatchAssociateScramSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateScramSecret)

responseUpdateBrokerStorage :: UpdateBrokerStorageResponse -> TestTree
responseUpdateBrokerStorage =
  res
    "UpdateBrokerStorageResponse"
    "fixture/UpdateBrokerStorageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBrokerStorage)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseUpdateClusterConfiguration :: UpdateClusterConfigurationResponse -> TestTree
responseUpdateClusterConfiguration =
  res
    "UpdateClusterConfigurationResponse"
    "fixture/UpdateClusterConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterConfiguration)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseUpdateBrokerCount :: UpdateBrokerCountResponse -> TestTree
responseUpdateBrokerCount =
  res
    "UpdateBrokerCountResponse"
    "fixture/UpdateBrokerCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBrokerCount)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations =
  res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurations)

responseGetBootstrapBrokers :: GetBootstrapBrokersResponse -> TestTree
responseGetBootstrapBrokers =
  res
    "GetBootstrapBrokersResponse"
    "fixture/GetBootstrapBrokersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBootstrapBrokers)

responseUpdateClusterKafkaVersion :: UpdateClusterKafkaVersionResponse -> TestTree
responseUpdateClusterKafkaVersion =
  res
    "UpdateClusterKafkaVersionResponse"
    "fixture/UpdateClusterKafkaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateClusterKafkaVersion)

responseUpdateSecurity :: UpdateSecurityResponse -> TestTree
responseUpdateSecurity =
  res
    "UpdateSecurityResponse"
    "fixture/UpdateSecurityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSecurity)

responseGetCompatibleKafkaVersions :: GetCompatibleKafkaVersionsResponse -> TestTree
responseGetCompatibleKafkaVersions =
  res
    "GetCompatibleKafkaVersionsResponse"
    "fixture/GetCompatibleKafkaVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCompatibleKafkaVersions)

responseDescribeClusterOperation :: DescribeClusterOperationResponse -> TestTree
responseDescribeClusterOperation =
  res
    "DescribeClusterOperationResponse"
    "fixture/DescribeClusterOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeClusterOperation)

responseUpdateBrokerType :: UpdateBrokerTypeResponse -> TestTree
responseUpdateBrokerType =
  res
    "UpdateBrokerTypeResponse"
    "fixture/UpdateBrokerTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBrokerType)

responseDescribeConfiguration :: DescribeConfigurationResponse -> TestTree
responseDescribeConfiguration =
  res
    "DescribeConfigurationResponse"
    "fixture/DescribeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfiguration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListClusterOperations :: ListClusterOperationsResponse -> TestTree
responseListClusterOperations =
  res
    "ListClusterOperationsResponse"
    "fixture/ListClusterOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusterOperations)

responseBatchDisassociateScramSecret :: BatchDisassociateScramSecretResponse -> TestTree
responseBatchDisassociateScramSecret =
  res
    "BatchDisassociateScramSecretResponse"
    "fixture/BatchDisassociateScramSecretResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateScramSecret)

responseDescribeConfigurationRevision :: DescribeConfigurationRevisionResponse -> TestTree
responseDescribeConfigurationRevision =
  res
    "DescribeConfigurationRevisionResponse"
    "fixture/DescribeConfigurationRevisionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationRevision)

responseDeleteConfiguration :: DeleteConfigurationResponse -> TestTree
responseDeleteConfiguration =
  res
    "DeleteConfigurationResponse"
    "fixture/DeleteConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfiguration)

responseUpdateConfiguration :: UpdateConfigurationResponse -> TestTree
responseUpdateConfiguration =
  res
    "UpdateConfigurationResponse"
    "fixture/UpdateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfiguration)

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

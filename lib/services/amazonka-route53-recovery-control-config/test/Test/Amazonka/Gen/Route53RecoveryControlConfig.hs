{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53RecoveryControlConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Route53RecoveryControlConfig where

import Amazonka.Route53RecoveryControlConfig
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Route53RecoveryControlConfig.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateCluster $
--             newCreateCluster
--
--         , requestCreateControlPanel $
--             newCreateControlPanel
--
--         , requestCreateRoutingControl $
--             newCreateRoutingControl
--
--         , requestCreateSafetyRule $
--             newCreateSafetyRule
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestDeleteControlPanel $
--             newDeleteControlPanel
--
--         , requestDeleteRoutingControl $
--             newDeleteRoutingControl
--
--         , requestDeleteSafetyRule $
--             newDeleteSafetyRule
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestDescribeControlPanel $
--             newDescribeControlPanel
--
--         , requestDescribeRoutingControl $
--             newDescribeRoutingControl
--
--         , requestDescribeSafetyRule $
--             newDescribeSafetyRule
--
--         , requestListAssociatedRoute53HealthChecks $
--             newListAssociatedRoute53HealthChecks
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListControlPanels $
--             newListControlPanels
--
--         , requestListRoutingControls $
--             newListRoutingControls
--
--         , requestListSafetyRules $
--             newListSafetyRules
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateControlPanel $
--             newUpdateControlPanel
--
--         , requestUpdateRoutingControl $
--             newUpdateRoutingControl
--
--         , requestUpdateSafetyRule $
--             newUpdateSafetyRule
--
--           ]

--     , testGroup "response"
--         [ responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseCreateControlPanel $
--             newCreateControlPanelResponse
--
--         , responseCreateRoutingControl $
--             newCreateRoutingControlResponse
--
--         , responseCreateSafetyRule $
--             newCreateSafetyRuleResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseDeleteControlPanel $
--             newDeleteControlPanelResponse
--
--         , responseDeleteRoutingControl $
--             newDeleteRoutingControlResponse
--
--         , responseDeleteSafetyRule $
--             newDeleteSafetyRuleResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseDescribeControlPanel $
--             newDescribeControlPanelResponse
--
--         , responseDescribeRoutingControl $
--             newDescribeRoutingControlResponse
--
--         , responseDescribeSafetyRule $
--             newDescribeSafetyRuleResponse
--
--         , responseListAssociatedRoute53HealthChecks $
--             newListAssociatedRoute53HealthChecksResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListControlPanels $
--             newListControlPanelsResponse
--
--         , responseListRoutingControls $
--             newListRoutingControlsResponse
--
--         , responseListSafetyRules $
--             newListSafetyRulesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateControlPanel $
--             newUpdateControlPanelResponse
--
--         , responseUpdateRoutingControl $
--             newUpdateRoutingControlResponse
--
--         , responseUpdateSafetyRule $
--             newUpdateSafetyRuleResponse
--
--           ]
--     ]

-- Requests

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestCreateControlPanel :: CreateControlPanel -> TestTree
requestCreateControlPanel =
  req
    "CreateControlPanel"
    "fixture/CreateControlPanel.yaml"

requestCreateRoutingControl :: CreateRoutingControl -> TestTree
requestCreateRoutingControl =
  req
    "CreateRoutingControl"
    "fixture/CreateRoutingControl.yaml"

requestCreateSafetyRule :: CreateSafetyRule -> TestTree
requestCreateSafetyRule =
  req
    "CreateSafetyRule"
    "fixture/CreateSafetyRule.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestDeleteControlPanel :: DeleteControlPanel -> TestTree
requestDeleteControlPanel =
  req
    "DeleteControlPanel"
    "fixture/DeleteControlPanel.yaml"

requestDeleteRoutingControl :: DeleteRoutingControl -> TestTree
requestDeleteRoutingControl =
  req
    "DeleteRoutingControl"
    "fixture/DeleteRoutingControl.yaml"

requestDeleteSafetyRule :: DeleteSafetyRule -> TestTree
requestDeleteSafetyRule =
  req
    "DeleteSafetyRule"
    "fixture/DeleteSafetyRule.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestDescribeControlPanel :: DescribeControlPanel -> TestTree
requestDescribeControlPanel =
  req
    "DescribeControlPanel"
    "fixture/DescribeControlPanel.yaml"

requestDescribeRoutingControl :: DescribeRoutingControl -> TestTree
requestDescribeRoutingControl =
  req
    "DescribeRoutingControl"
    "fixture/DescribeRoutingControl.yaml"

requestDescribeSafetyRule :: DescribeSafetyRule -> TestTree
requestDescribeSafetyRule =
  req
    "DescribeSafetyRule"
    "fixture/DescribeSafetyRule.yaml"

requestListAssociatedRoute53HealthChecks :: ListAssociatedRoute53HealthChecks -> TestTree
requestListAssociatedRoute53HealthChecks =
  req
    "ListAssociatedRoute53HealthChecks"
    "fixture/ListAssociatedRoute53HealthChecks.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListControlPanels :: ListControlPanels -> TestTree
requestListControlPanels =
  req
    "ListControlPanels"
    "fixture/ListControlPanels.yaml"

requestListRoutingControls :: ListRoutingControls -> TestTree
requestListRoutingControls =
  req
    "ListRoutingControls"
    "fixture/ListRoutingControls.yaml"

requestListSafetyRules :: ListSafetyRules -> TestTree
requestListSafetyRules =
  req
    "ListSafetyRules"
    "fixture/ListSafetyRules.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestUpdateControlPanel :: UpdateControlPanel -> TestTree
requestUpdateControlPanel =
  req
    "UpdateControlPanel"
    "fixture/UpdateControlPanel.yaml"

requestUpdateRoutingControl :: UpdateRoutingControl -> TestTree
requestUpdateRoutingControl =
  req
    "UpdateRoutingControl"
    "fixture/UpdateRoutingControl.yaml"

requestUpdateSafetyRule :: UpdateSafetyRule -> TestTree
requestUpdateSafetyRule =
  req
    "UpdateSafetyRule"
    "fixture/UpdateSafetyRule.yaml"

-- Responses

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseCreateControlPanel :: CreateControlPanelResponse -> TestTree
responseCreateControlPanel =
  res
    "CreateControlPanelResponse"
    "fixture/CreateControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateControlPanel)

responseCreateRoutingControl :: CreateRoutingControlResponse -> TestTree
responseCreateRoutingControl =
  res
    "CreateRoutingControlResponse"
    "fixture/CreateRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoutingControl)

responseCreateSafetyRule :: CreateSafetyRuleResponse -> TestTree
responseCreateSafetyRule =
  res
    "CreateSafetyRuleResponse"
    "fixture/CreateSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSafetyRule)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseDeleteControlPanel :: DeleteControlPanelResponse -> TestTree
responseDeleteControlPanel =
  res
    "DeleteControlPanelResponse"
    "fixture/DeleteControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteControlPanel)

responseDeleteRoutingControl :: DeleteRoutingControlResponse -> TestTree
responseDeleteRoutingControl =
  res
    "DeleteRoutingControlResponse"
    "fixture/DeleteRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoutingControl)

responseDeleteSafetyRule :: DeleteSafetyRuleResponse -> TestTree
responseDeleteSafetyRule =
  res
    "DeleteSafetyRuleResponse"
    "fixture/DeleteSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSafetyRule)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseDescribeControlPanel :: DescribeControlPanelResponse -> TestTree
responseDescribeControlPanel =
  res
    "DescribeControlPanelResponse"
    "fixture/DescribeControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeControlPanel)

responseDescribeRoutingControl :: DescribeRoutingControlResponse -> TestTree
responseDescribeRoutingControl =
  res
    "DescribeRoutingControlResponse"
    "fixture/DescribeRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoutingControl)

responseDescribeSafetyRule :: DescribeSafetyRuleResponse -> TestTree
responseDescribeSafetyRule =
  res
    "DescribeSafetyRuleResponse"
    "fixture/DescribeSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSafetyRule)

responseListAssociatedRoute53HealthChecks :: ListAssociatedRoute53HealthChecksResponse -> TestTree
responseListAssociatedRoute53HealthChecks =
  res
    "ListAssociatedRoute53HealthChecksResponse"
    "fixture/ListAssociatedRoute53HealthChecksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedRoute53HealthChecks)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListControlPanels :: ListControlPanelsResponse -> TestTree
responseListControlPanels =
  res
    "ListControlPanelsResponse"
    "fixture/ListControlPanelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListControlPanels)

responseListRoutingControls :: ListRoutingControlsResponse -> TestTree
responseListRoutingControls =
  res
    "ListRoutingControlsResponse"
    "fixture/ListRoutingControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutingControls)

responseListSafetyRules :: ListSafetyRulesResponse -> TestTree
responseListSafetyRules =
  res
    "ListSafetyRulesResponse"
    "fixture/ListSafetyRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSafetyRules)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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

responseUpdateControlPanel :: UpdateControlPanelResponse -> TestTree
responseUpdateControlPanel =
  res
    "UpdateControlPanelResponse"
    "fixture/UpdateControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateControlPanel)

responseUpdateRoutingControl :: UpdateRoutingControlResponse -> TestTree
responseUpdateRoutingControl =
  res
    "UpdateRoutingControlResponse"
    "fixture/UpdateRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingControl)

responseUpdateSafetyRule :: UpdateSafetyRuleResponse -> TestTree
responseUpdateSafetyRule =
  res
    "UpdateSafetyRuleResponse"
    "fixture/UpdateSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSafetyRule)

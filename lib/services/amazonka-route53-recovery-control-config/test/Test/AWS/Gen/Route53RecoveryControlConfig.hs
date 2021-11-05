{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53RecoveryControlConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Route53RecoveryControlConfig where

import qualified Data.Proxy as Proxy
import Network.AWS.Route53RecoveryControlConfig
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Route53RecoveryControlConfig.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeControlPanel $
--             newDescribeControlPanel
--
--         , requestCreateRoutingControl $
--             newCreateRoutingControl
--
--         , requestDescribeCluster $
--             newDescribeCluster
--
--         , requestDeleteRoutingControl $
--             newDeleteRoutingControl
--
--         , requestUpdateRoutingControl $
--             newUpdateRoutingControl
--
--         , requestCreateControlPanel $
--             newCreateControlPanel
--
--         , requestUpdateControlPanel $
--             newUpdateControlPanel
--
--         , requestDeleteControlPanel $
--             newDeleteControlPanel
--
--         , requestDeleteCluster $
--             newDeleteCluster
--
--         , requestCreateSafetyRule $
--             newCreateSafetyRule
--
--         , requestCreateCluster $
--             newCreateCluster
--
--         , requestListAssociatedRoute53HealthChecks $
--             newListAssociatedRoute53HealthChecks
--
--         , requestDescribeSafetyRule $
--             newDescribeSafetyRule
--
--         , requestListRoutingControls $
--             newListRoutingControls
--
--         , requestListControlPanels $
--             newListControlPanels
--
--         , requestUpdateSafetyRule $
--             newUpdateSafetyRule
--
--         , requestDeleteSafetyRule $
--             newDeleteSafetyRule
--
--         , requestListClusters $
--             newListClusters
--
--         , requestListSafetyRules $
--             newListSafetyRules
--
--         , requestDescribeRoutingControl $
--             newDescribeRoutingControl
--
--           ]

--     , testGroup "response"
--         [ responseDescribeControlPanel $
--             newDescribeControlPanelResponse
--
--         , responseCreateRoutingControl $
--             newCreateRoutingControlResponse
--
--         , responseDescribeCluster $
--             newDescribeClusterResponse
--
--         , responseDeleteRoutingControl $
--             newDeleteRoutingControlResponse
--
--         , responseUpdateRoutingControl $
--             newUpdateRoutingControlResponse
--
--         , responseCreateControlPanel $
--             newCreateControlPanelResponse
--
--         , responseUpdateControlPanel $
--             newUpdateControlPanelResponse
--
--         , responseDeleteControlPanel $
--             newDeleteControlPanelResponse
--
--         , responseDeleteCluster $
--             newDeleteClusterResponse
--
--         , responseCreateSafetyRule $
--             newCreateSafetyRuleResponse
--
--         , responseCreateCluster $
--             newCreateClusterResponse
--
--         , responseListAssociatedRoute53HealthChecks $
--             newListAssociatedRoute53HealthChecksResponse
--
--         , responseDescribeSafetyRule $
--             newDescribeSafetyRuleResponse
--
--         , responseListRoutingControls $
--             newListRoutingControlsResponse
--
--         , responseListControlPanels $
--             newListControlPanelsResponse
--
--         , responseUpdateSafetyRule $
--             newUpdateSafetyRuleResponse
--
--         , responseDeleteSafetyRule $
--             newDeleteSafetyRuleResponse
--
--         , responseListClusters $
--             newListClustersResponse
--
--         , responseListSafetyRules $
--             newListSafetyRulesResponse
--
--         , responseDescribeRoutingControl $
--             newDescribeRoutingControlResponse
--
--           ]
--     ]

-- Requests

requestDescribeControlPanel :: DescribeControlPanel -> TestTree
requestDescribeControlPanel =
  req
    "DescribeControlPanel"
    "fixture/DescribeControlPanel.yaml"

requestCreateRoutingControl :: CreateRoutingControl -> TestTree
requestCreateRoutingControl =
  req
    "CreateRoutingControl"
    "fixture/CreateRoutingControl.yaml"

requestDescribeCluster :: DescribeCluster -> TestTree
requestDescribeCluster =
  req
    "DescribeCluster"
    "fixture/DescribeCluster.yaml"

requestDeleteRoutingControl :: DeleteRoutingControl -> TestTree
requestDeleteRoutingControl =
  req
    "DeleteRoutingControl"
    "fixture/DeleteRoutingControl.yaml"

requestUpdateRoutingControl :: UpdateRoutingControl -> TestTree
requestUpdateRoutingControl =
  req
    "UpdateRoutingControl"
    "fixture/UpdateRoutingControl.yaml"

requestCreateControlPanel :: CreateControlPanel -> TestTree
requestCreateControlPanel =
  req
    "CreateControlPanel"
    "fixture/CreateControlPanel.yaml"

requestUpdateControlPanel :: UpdateControlPanel -> TestTree
requestUpdateControlPanel =
  req
    "UpdateControlPanel"
    "fixture/UpdateControlPanel.yaml"

requestDeleteControlPanel :: DeleteControlPanel -> TestTree
requestDeleteControlPanel =
  req
    "DeleteControlPanel"
    "fixture/DeleteControlPanel.yaml"

requestDeleteCluster :: DeleteCluster -> TestTree
requestDeleteCluster =
  req
    "DeleteCluster"
    "fixture/DeleteCluster.yaml"

requestCreateSafetyRule :: CreateSafetyRule -> TestTree
requestCreateSafetyRule =
  req
    "CreateSafetyRule"
    "fixture/CreateSafetyRule.yaml"

requestCreateCluster :: CreateCluster -> TestTree
requestCreateCluster =
  req
    "CreateCluster"
    "fixture/CreateCluster.yaml"

requestListAssociatedRoute53HealthChecks :: ListAssociatedRoute53HealthChecks -> TestTree
requestListAssociatedRoute53HealthChecks =
  req
    "ListAssociatedRoute53HealthChecks"
    "fixture/ListAssociatedRoute53HealthChecks.yaml"

requestDescribeSafetyRule :: DescribeSafetyRule -> TestTree
requestDescribeSafetyRule =
  req
    "DescribeSafetyRule"
    "fixture/DescribeSafetyRule.yaml"

requestListRoutingControls :: ListRoutingControls -> TestTree
requestListRoutingControls =
  req
    "ListRoutingControls"
    "fixture/ListRoutingControls.yaml"

requestListControlPanels :: ListControlPanels -> TestTree
requestListControlPanels =
  req
    "ListControlPanels"
    "fixture/ListControlPanels.yaml"

requestUpdateSafetyRule :: UpdateSafetyRule -> TestTree
requestUpdateSafetyRule =
  req
    "UpdateSafetyRule"
    "fixture/UpdateSafetyRule.yaml"

requestDeleteSafetyRule :: DeleteSafetyRule -> TestTree
requestDeleteSafetyRule =
  req
    "DeleteSafetyRule"
    "fixture/DeleteSafetyRule.yaml"

requestListClusters :: ListClusters -> TestTree
requestListClusters =
  req
    "ListClusters"
    "fixture/ListClusters.yaml"

requestListSafetyRules :: ListSafetyRules -> TestTree
requestListSafetyRules =
  req
    "ListSafetyRules"
    "fixture/ListSafetyRules.yaml"

requestDescribeRoutingControl :: DescribeRoutingControl -> TestTree
requestDescribeRoutingControl =
  req
    "DescribeRoutingControl"
    "fixture/DescribeRoutingControl.yaml"

-- Responses

responseDescribeControlPanel :: DescribeControlPanelResponse -> TestTree
responseDescribeControlPanel =
  res
    "DescribeControlPanelResponse"
    "fixture/DescribeControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeControlPanel)

responseCreateRoutingControl :: CreateRoutingControlResponse -> TestTree
responseCreateRoutingControl =
  res
    "CreateRoutingControlResponse"
    "fixture/CreateRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRoutingControl)

responseDescribeCluster :: DescribeClusterResponse -> TestTree
responseDescribeCluster =
  res
    "DescribeClusterResponse"
    "fixture/DescribeClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCluster)

responseDeleteRoutingControl :: DeleteRoutingControlResponse -> TestTree
responseDeleteRoutingControl =
  res
    "DeleteRoutingControlResponse"
    "fixture/DeleteRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRoutingControl)

responseUpdateRoutingControl :: UpdateRoutingControlResponse -> TestTree
responseUpdateRoutingControl =
  res
    "UpdateRoutingControlResponse"
    "fixture/UpdateRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRoutingControl)

responseCreateControlPanel :: CreateControlPanelResponse -> TestTree
responseCreateControlPanel =
  res
    "CreateControlPanelResponse"
    "fixture/CreateControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateControlPanel)

responseUpdateControlPanel :: UpdateControlPanelResponse -> TestTree
responseUpdateControlPanel =
  res
    "UpdateControlPanelResponse"
    "fixture/UpdateControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateControlPanel)

responseDeleteControlPanel :: DeleteControlPanelResponse -> TestTree
responseDeleteControlPanel =
  res
    "DeleteControlPanelResponse"
    "fixture/DeleteControlPanelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteControlPanel)

responseDeleteCluster :: DeleteClusterResponse -> TestTree
responseDeleteCluster =
  res
    "DeleteClusterResponse"
    "fixture/DeleteClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCluster)

responseCreateSafetyRule :: CreateSafetyRuleResponse -> TestTree
responseCreateSafetyRule =
  res
    "CreateSafetyRuleResponse"
    "fixture/CreateSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSafetyRule)

responseCreateCluster :: CreateClusterResponse -> TestTree
responseCreateCluster =
  res
    "CreateClusterResponse"
    "fixture/CreateClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCluster)

responseListAssociatedRoute53HealthChecks :: ListAssociatedRoute53HealthChecksResponse -> TestTree
responseListAssociatedRoute53HealthChecks =
  res
    "ListAssociatedRoute53HealthChecksResponse"
    "fixture/ListAssociatedRoute53HealthChecksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedRoute53HealthChecks)

responseDescribeSafetyRule :: DescribeSafetyRuleResponse -> TestTree
responseDescribeSafetyRule =
  res
    "DescribeSafetyRuleResponse"
    "fixture/DescribeSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSafetyRule)

responseListRoutingControls :: ListRoutingControlsResponse -> TestTree
responseListRoutingControls =
  res
    "ListRoutingControlsResponse"
    "fixture/ListRoutingControlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoutingControls)

responseListControlPanels :: ListControlPanelsResponse -> TestTree
responseListControlPanels =
  res
    "ListControlPanelsResponse"
    "fixture/ListControlPanelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListControlPanels)

responseUpdateSafetyRule :: UpdateSafetyRuleResponse -> TestTree
responseUpdateSafetyRule =
  res
    "UpdateSafetyRuleResponse"
    "fixture/UpdateSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSafetyRule)

responseDeleteSafetyRule :: DeleteSafetyRuleResponse -> TestTree
responseDeleteSafetyRule =
  res
    "DeleteSafetyRuleResponse"
    "fixture/DeleteSafetyRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSafetyRule)

responseListClusters :: ListClustersResponse -> TestTree
responseListClusters =
  res
    "ListClustersResponse"
    "fixture/ListClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListClusters)

responseListSafetyRules :: ListSafetyRulesResponse -> TestTree
responseListSafetyRules =
  res
    "ListSafetyRulesResponse"
    "fixture/ListSafetyRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSafetyRules)

responseDescribeRoutingControl :: DescribeRoutingControlResponse -> TestTree
responseDescribeRoutingControl =
  res
    "DescribeRoutingControlResponse"
    "fixture/DescribeRoutingControlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRoutingControl)

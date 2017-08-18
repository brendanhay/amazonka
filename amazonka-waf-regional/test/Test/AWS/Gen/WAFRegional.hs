{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WAFRegional
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WAFRegional where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.WAFRegional
import Test.AWS.WAFRegional.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListRateBasedRules $
--             listRateBasedRules
--
--         , requestGetSizeConstraintSet $
--             getSizeConstraintSet
--
--         , requestDeleteRateBasedRule $
--             deleteRateBasedRule
--
--         , requestUpdateRateBasedRule $
--             updateRateBasedRule
--
--         , requestUpdateRule $
--             updateRule
--
--         , requestDeleteRule $
--             deleteRule
--
--         , requestCreateIPSet $
--             createIPSet
--
--         , requestGetChangeTokenStatus $
--             getChangeTokenStatus
--
--         , requestDeleteWebACL $
--             deleteWebACL
--
--         , requestUpdateWebACL $
--             updateWebACL
--
--         , requestListWebACLs $
--             listWebACLs
--
--         , requestListRules $
--             listRules
--
--         , requestCreateRule $
--             createRule
--
--         , requestCreateWebACL $
--             createWebACL
--
--         , requestListByteMatchSets $
--             listByteMatchSets
--
--         , requestGetIPSet $
--             getIPSet
--
--         , requestGetWebACL $
--             getWebACL
--
--         , requestGetRule $
--             getRule
--
--         , requestDeleteXSSMatchSet $
--             deleteXSSMatchSet
--
--         , requestUpdateXSSMatchSet $
--             updateXSSMatchSet
--
--         , requestListXSSMatchSets $
--             listXSSMatchSets
--
--         , requestGetChangeToken $
--             getChangeToken
--
--         , requestListSizeConstraintSets $
--             listSizeConstraintSets
--
--         , requestListResourcesForWebACL $
--             listResourcesForWebACL
--
--         , requestGetSampledRequests $
--             getSampledRequests
--
--         , requestGetSqlInjectionMatchSet $
--             getSqlInjectionMatchSet
--
--         , requestGetWebACLForResource $
--             getWebACLForResource
--
--         , requestDisassociateWebACL $
--             disassociateWebACL
--
--         , requestCreateSqlInjectionMatchSet $
--             createSqlInjectionMatchSet
--
--         , requestGetXSSMatchSet $
--             getXSSMatchSet
--
--         , requestCreateByteMatchSet $
--             createByteMatchSet
--
--         , requestUpdateByteMatchSet $
--             updateByteMatchSet
--
--         , requestDeleteByteMatchSet $
--             deleteByteMatchSet
--
--         , requestGetRateBasedRuleManagedKeys $
--             getRateBasedRuleManagedKeys
--
--         , requestAssociateWebACL $
--             associateWebACL
--
--         , requestDeleteIPSet $
--             deleteIPSet
--
--         , requestUpdateIPSet $
--             updateIPSet
--
--         , requestListIPSets $
--             listIPSets
--
--         , requestCreateXSSMatchSet $
--             createXSSMatchSet
--
--         , requestGetByteMatchSet $
--             getByteMatchSet
--
--         , requestGetRateBasedRule $
--             getRateBasedRule
--
--         , requestDeleteSizeConstraintSet $
--             deleteSizeConstraintSet
--
--         , requestUpdateSizeConstraintSet $
--             updateSizeConstraintSet
--
--         , requestCreateSizeConstraintSet $
--             createSizeConstraintSet
--
--         , requestListSqlInjectionMatchSets $
--             listSqlInjectionMatchSets
--
--         , requestCreateRateBasedRule $
--             createRateBasedRule
--
--         , requestDeleteSqlInjectionMatchSet $
--             deleteSqlInjectionMatchSet
--
--         , requestUpdateSqlInjectionMatchSet $
--             updateSqlInjectionMatchSet
--
--           ]

--     , testGroup "response"
--         [ responseListRateBasedRules $
--             listRateBasedRulesResponse
--
--         , responseGetSizeConstraintSet $
--             getSizeConstraintSetResponse
--
--         , responseDeleteRateBasedRule $
--             deleteRateBasedRuleResponse
--
--         , responseUpdateRateBasedRule $
--             updateRateBasedRuleResponse
--
--         , responseUpdateRule $
--             updateRuleResponse
--
--         , responseDeleteRule $
--             deleteRuleResponse
--
--         , responseCreateIPSet $
--             createIPSetResponse
--
--         , responseGetChangeTokenStatus $
--             getChangeTokenStatusResponse
--
--         , responseDeleteWebACL $
--             deleteWebACLResponse
--
--         , responseUpdateWebACL $
--             updateWebACLResponse
--
--         , responseListWebACLs $
--             listWebACLsResponse
--
--         , responseListRules $
--             listRulesResponse
--
--         , responseCreateRule $
--             createRuleResponse
--
--         , responseCreateWebACL $
--             createWebACLResponse
--
--         , responseListByteMatchSets $
--             listByteMatchSetsResponse
--
--         , responseGetIPSet $
--             getIPSetResponse
--
--         , responseGetWebACL $
--             getWebACLResponse
--
--         , responseGetRule $
--             getRuleResponse
--
--         , responseDeleteXSSMatchSet $
--             deleteXSSMatchSetResponse
--
--         , responseUpdateXSSMatchSet $
--             updateXSSMatchSetResponse
--
--         , responseListXSSMatchSets $
--             listXSSMatchSetsResponse
--
--         , responseGetChangeToken $
--             getChangeTokenResponse
--
--         , responseListSizeConstraintSets $
--             listSizeConstraintSetsResponse
--
--         , responseListResourcesForWebACL $
--             listResourcesForWebACLResponse
--
--         , responseGetSampledRequests $
--             getSampledRequestsResponse
--
--         , responseGetSqlInjectionMatchSet $
--             getSqlInjectionMatchSetResponse
--
--         , responseGetWebACLForResource $
--             getWebACLForResourceResponse
--
--         , responseDisassociateWebACL $
--             disassociateWebACLResponse
--
--         , responseCreateSqlInjectionMatchSet $
--             createSqlInjectionMatchSetResponse
--
--         , responseGetXSSMatchSet $
--             getXSSMatchSetResponse
--
--         , responseCreateByteMatchSet $
--             createByteMatchSetResponse
--
--         , responseUpdateByteMatchSet $
--             updateByteMatchSetResponse
--
--         , responseDeleteByteMatchSet $
--             deleteByteMatchSetResponse
--
--         , responseGetRateBasedRuleManagedKeys $
--             getRateBasedRuleManagedKeysResponse
--
--         , responseAssociateWebACL $
--             associateWebACLResponse
--
--         , responseDeleteIPSet $
--             deleteIPSetResponse
--
--         , responseUpdateIPSet $
--             updateIPSetResponse
--
--         , responseListIPSets $
--             listIPSetsResponse
--
--         , responseCreateXSSMatchSet $
--             createXSSMatchSetResponse
--
--         , responseGetByteMatchSet $
--             getByteMatchSetResponse
--
--         , responseGetRateBasedRule $
--             getRateBasedRuleResponse
--
--         , responseDeleteSizeConstraintSet $
--             deleteSizeConstraintSetResponse
--
--         , responseUpdateSizeConstraintSet $
--             updateSizeConstraintSetResponse
--
--         , responseCreateSizeConstraintSet $
--             createSizeConstraintSetResponse
--
--         , responseListSqlInjectionMatchSets $
--             listSqlInjectionMatchSetsResponse
--
--         , responseCreateRateBasedRule $
--             createRateBasedRuleResponse
--
--         , responseDeleteSqlInjectionMatchSet $
--             deleteSqlInjectionMatchSetResponse
--
--         , responseUpdateSqlInjectionMatchSet $
--             updateSqlInjectionMatchSetResponse
--
--           ]
--     ]

-- Requests

requestListRateBasedRules :: ListRateBasedRules -> TestTree
requestListRateBasedRules = req
    "ListRateBasedRules"
    "fixture/ListRateBasedRules.yaml"

requestGetSizeConstraintSet :: GetSizeConstraintSet -> TestTree
requestGetSizeConstraintSet = req
    "GetSizeConstraintSet"
    "fixture/GetSizeConstraintSet.yaml"

requestDeleteRateBasedRule :: DeleteRateBasedRule -> TestTree
requestDeleteRateBasedRule = req
    "DeleteRateBasedRule"
    "fixture/DeleteRateBasedRule.yaml"

requestUpdateRateBasedRule :: UpdateRateBasedRule -> TestTree
requestUpdateRateBasedRule = req
    "UpdateRateBasedRule"
    "fixture/UpdateRateBasedRule.yaml"

requestUpdateRule :: UpdateRule -> TestTree
requestUpdateRule = req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule = req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet = req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestGetChangeTokenStatus :: GetChangeTokenStatus -> TestTree
requestGetChangeTokenStatus = req
    "GetChangeTokenStatus"
    "fixture/GetChangeTokenStatus.yaml"

requestDeleteWebACL :: DeleteWebACL -> TestTree
requestDeleteWebACL = req
    "DeleteWebACL"
    "fixture/DeleteWebACL.yaml"

requestUpdateWebACL :: UpdateWebACL -> TestTree
requestUpdateWebACL = req
    "UpdateWebACL"
    "fixture/UpdateWebACL.yaml"

requestListWebACLs :: ListWebACLs -> TestTree
requestListWebACLs = req
    "ListWebACLs"
    "fixture/ListWebACLs.yaml"

requestListRules :: ListRules -> TestTree
requestListRules = req
    "ListRules"
    "fixture/ListRules.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule = req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestCreateWebACL :: CreateWebACL -> TestTree
requestCreateWebACL = req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

requestListByteMatchSets :: ListByteMatchSets -> TestTree
requestListByteMatchSets = req
    "ListByteMatchSets"
    "fixture/ListByteMatchSets.yaml"

requestGetIPSet :: GetIPSet -> TestTree
requestGetIPSet = req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

requestGetWebACL :: GetWebACL -> TestTree
requestGetWebACL = req
    "GetWebACL"
    "fixture/GetWebACL.yaml"

requestGetRule :: GetRule -> TestTree
requestGetRule = req
    "GetRule"
    "fixture/GetRule.yaml"

requestDeleteXSSMatchSet :: DeleteXSSMatchSet -> TestTree
requestDeleteXSSMatchSet = req
    "DeleteXSSMatchSet"
    "fixture/DeleteXSSMatchSet.yaml"

requestUpdateXSSMatchSet :: UpdateXSSMatchSet -> TestTree
requestUpdateXSSMatchSet = req
    "UpdateXSSMatchSet"
    "fixture/UpdateXSSMatchSet.yaml"

requestListXSSMatchSets :: ListXSSMatchSets -> TestTree
requestListXSSMatchSets = req
    "ListXSSMatchSets"
    "fixture/ListXSSMatchSets.yaml"

requestGetChangeToken :: GetChangeToken -> TestTree
requestGetChangeToken = req
    "GetChangeToken"
    "fixture/GetChangeToken.yaml"

requestListSizeConstraintSets :: ListSizeConstraintSets -> TestTree
requestListSizeConstraintSets = req
    "ListSizeConstraintSets"
    "fixture/ListSizeConstraintSets.yaml"

requestListResourcesForWebACL :: ListResourcesForWebACL -> TestTree
requestListResourcesForWebACL = req
    "ListResourcesForWebACL"
    "fixture/ListResourcesForWebACL.yaml"

requestGetSampledRequests :: GetSampledRequests -> TestTree
requestGetSampledRequests = req
    "GetSampledRequests"
    "fixture/GetSampledRequests.yaml"

requestGetSqlInjectionMatchSet :: GetSqlInjectionMatchSet -> TestTree
requestGetSqlInjectionMatchSet = req
    "GetSqlInjectionMatchSet"
    "fixture/GetSqlInjectionMatchSet.yaml"

requestGetWebACLForResource :: GetWebACLForResource -> TestTree
requestGetWebACLForResource = req
    "GetWebACLForResource"
    "fixture/GetWebACLForResource.yaml"

requestDisassociateWebACL :: DisassociateWebACL -> TestTree
requestDisassociateWebACL = req
    "DisassociateWebACL"
    "fixture/DisassociateWebACL.yaml"

requestCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSet -> TestTree
requestCreateSqlInjectionMatchSet = req
    "CreateSqlInjectionMatchSet"
    "fixture/CreateSqlInjectionMatchSet.yaml"

requestGetXSSMatchSet :: GetXSSMatchSet -> TestTree
requestGetXSSMatchSet = req
    "GetXSSMatchSet"
    "fixture/GetXSSMatchSet.yaml"

requestCreateByteMatchSet :: CreateByteMatchSet -> TestTree
requestCreateByteMatchSet = req
    "CreateByteMatchSet"
    "fixture/CreateByteMatchSet.yaml"

requestUpdateByteMatchSet :: UpdateByteMatchSet -> TestTree
requestUpdateByteMatchSet = req
    "UpdateByteMatchSet"
    "fixture/UpdateByteMatchSet.yaml"

requestDeleteByteMatchSet :: DeleteByteMatchSet -> TestTree
requestDeleteByteMatchSet = req
    "DeleteByteMatchSet"
    "fixture/DeleteByteMatchSet.yaml"

requestGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeys -> TestTree
requestGetRateBasedRuleManagedKeys = req
    "GetRateBasedRuleManagedKeys"
    "fixture/GetRateBasedRuleManagedKeys.yaml"

requestAssociateWebACL :: AssociateWebACL -> TestTree
requestAssociateWebACL = req
    "AssociateWebACL"
    "fixture/AssociateWebACL.yaml"

requestDeleteIPSet :: DeleteIPSet -> TestTree
requestDeleteIPSet = req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet = req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets = req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestCreateXSSMatchSet :: CreateXSSMatchSet -> TestTree
requestCreateXSSMatchSet = req
    "CreateXSSMatchSet"
    "fixture/CreateXSSMatchSet.yaml"

requestGetByteMatchSet :: GetByteMatchSet -> TestTree
requestGetByteMatchSet = req
    "GetByteMatchSet"
    "fixture/GetByteMatchSet.yaml"

requestGetRateBasedRule :: GetRateBasedRule -> TestTree
requestGetRateBasedRule = req
    "GetRateBasedRule"
    "fixture/GetRateBasedRule.yaml"

requestDeleteSizeConstraintSet :: DeleteSizeConstraintSet -> TestTree
requestDeleteSizeConstraintSet = req
    "DeleteSizeConstraintSet"
    "fixture/DeleteSizeConstraintSet.yaml"

requestUpdateSizeConstraintSet :: UpdateSizeConstraintSet -> TestTree
requestUpdateSizeConstraintSet = req
    "UpdateSizeConstraintSet"
    "fixture/UpdateSizeConstraintSet.yaml"

requestCreateSizeConstraintSet :: CreateSizeConstraintSet -> TestTree
requestCreateSizeConstraintSet = req
    "CreateSizeConstraintSet"
    "fixture/CreateSizeConstraintSet.yaml"

requestListSqlInjectionMatchSets :: ListSqlInjectionMatchSets -> TestTree
requestListSqlInjectionMatchSets = req
    "ListSqlInjectionMatchSets"
    "fixture/ListSqlInjectionMatchSets.yaml"

requestCreateRateBasedRule :: CreateRateBasedRule -> TestTree
requestCreateRateBasedRule = req
    "CreateRateBasedRule"
    "fixture/CreateRateBasedRule.yaml"

requestDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSet -> TestTree
requestDeleteSqlInjectionMatchSet = req
    "DeleteSqlInjectionMatchSet"
    "fixture/DeleteSqlInjectionMatchSet.yaml"

requestUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSet -> TestTree
requestUpdateSqlInjectionMatchSet = req
    "UpdateSqlInjectionMatchSet"
    "fixture/UpdateSqlInjectionMatchSet.yaml"

-- Responses

responseListRateBasedRules :: ListRateBasedRulesResponse -> TestTree
responseListRateBasedRules = res
    "ListRateBasedRulesResponse"
    "fixture/ListRateBasedRulesResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListRateBasedRules)

responseGetSizeConstraintSet :: GetSizeConstraintSetResponse -> TestTree
responseGetSizeConstraintSet = res
    "GetSizeConstraintSetResponse"
    "fixture/GetSizeConstraintSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetSizeConstraintSet)

responseDeleteRateBasedRule :: DeleteRateBasedRuleResponse -> TestTree
responseDeleteRateBasedRule = res
    "DeleteRateBasedRuleResponse"
    "fixture/DeleteRateBasedRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteRateBasedRule)

responseUpdateRateBasedRule :: UpdateRateBasedRuleResponse -> TestTree
responseUpdateRateBasedRule = res
    "UpdateRateBasedRuleResponse"
    "fixture/UpdateRateBasedRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateRateBasedRule)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule = res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateRule)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule = res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteRule)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet = res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateIPSet)

responseGetChangeTokenStatus :: GetChangeTokenStatusResponse -> TestTree
responseGetChangeTokenStatus = res
    "GetChangeTokenStatusResponse"
    "fixture/GetChangeTokenStatusResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetChangeTokenStatus)

responseDeleteWebACL :: DeleteWebACLResponse -> TestTree
responseDeleteWebACL = res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteWebACL)

responseUpdateWebACL :: UpdateWebACLResponse -> TestTree
responseUpdateWebACL = res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateWebACL)

responseListWebACLs :: ListWebACLsResponse -> TestTree
responseListWebACLs = res
    "ListWebACLsResponse"
    "fixture/ListWebACLsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListWebACLs)

responseListRules :: ListRulesResponse -> TestTree
responseListRules = res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListRules)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule = res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateRule)

responseCreateWebACL :: CreateWebACLResponse -> TestTree
responseCreateWebACL = res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateWebACL)

responseListByteMatchSets :: ListByteMatchSetsResponse -> TestTree
responseListByteMatchSets = res
    "ListByteMatchSetsResponse"
    "fixture/ListByteMatchSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListByteMatchSets)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet = res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetIPSet)

responseGetWebACL :: GetWebACLResponse -> TestTree
responseGetWebACL = res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetWebACL)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule = res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetRule)

responseDeleteXSSMatchSet :: DeleteXSSMatchSetResponse -> TestTree
responseDeleteXSSMatchSet = res
    "DeleteXSSMatchSetResponse"
    "fixture/DeleteXSSMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteXSSMatchSet)

responseUpdateXSSMatchSet :: UpdateXSSMatchSetResponse -> TestTree
responseUpdateXSSMatchSet = res
    "UpdateXSSMatchSetResponse"
    "fixture/UpdateXSSMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateXSSMatchSet)

responseListXSSMatchSets :: ListXSSMatchSetsResponse -> TestTree
responseListXSSMatchSets = res
    "ListXSSMatchSetsResponse"
    "fixture/ListXSSMatchSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListXSSMatchSets)

responseGetChangeToken :: GetChangeTokenResponse -> TestTree
responseGetChangeToken = res
    "GetChangeTokenResponse"
    "fixture/GetChangeTokenResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetChangeToken)

responseListSizeConstraintSets :: ListSizeConstraintSetsResponse -> TestTree
responseListSizeConstraintSets = res
    "ListSizeConstraintSetsResponse"
    "fixture/ListSizeConstraintSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListSizeConstraintSets)

responseListResourcesForWebACL :: ListResourcesForWebACLResponse -> TestTree
responseListResourcesForWebACL = res
    "ListResourcesForWebACLResponse"
    "fixture/ListResourcesForWebACLResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListResourcesForWebACL)

responseGetSampledRequests :: GetSampledRequestsResponse -> TestTree
responseGetSampledRequests = res
    "GetSampledRequestsResponse"
    "fixture/GetSampledRequestsResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetSampledRequests)

responseGetSqlInjectionMatchSet :: GetSqlInjectionMatchSetResponse -> TestTree
responseGetSqlInjectionMatchSet = res
    "GetSqlInjectionMatchSetResponse"
    "fixture/GetSqlInjectionMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetSqlInjectionMatchSet)

responseGetWebACLForResource :: GetWebACLForResourceResponse -> TestTree
responseGetWebACLForResource = res
    "GetWebACLForResourceResponse"
    "fixture/GetWebACLForResourceResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetWebACLForResource)

responseDisassociateWebACL :: DisassociateWebACLResponse -> TestTree
responseDisassociateWebACL = res
    "DisassociateWebACLResponse"
    "fixture/DisassociateWebACLResponse.proto"
    wAFRegional
    (Proxy :: Proxy DisassociateWebACL)

responseCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSetResponse -> TestTree
responseCreateSqlInjectionMatchSet = res
    "CreateSqlInjectionMatchSetResponse"
    "fixture/CreateSqlInjectionMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateSqlInjectionMatchSet)

responseGetXSSMatchSet :: GetXSSMatchSetResponse -> TestTree
responseGetXSSMatchSet = res
    "GetXSSMatchSetResponse"
    "fixture/GetXSSMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetXSSMatchSet)

responseCreateByteMatchSet :: CreateByteMatchSetResponse -> TestTree
responseCreateByteMatchSet = res
    "CreateByteMatchSetResponse"
    "fixture/CreateByteMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateByteMatchSet)

responseUpdateByteMatchSet :: UpdateByteMatchSetResponse -> TestTree
responseUpdateByteMatchSet = res
    "UpdateByteMatchSetResponse"
    "fixture/UpdateByteMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateByteMatchSet)

responseDeleteByteMatchSet :: DeleteByteMatchSetResponse -> TestTree
responseDeleteByteMatchSet = res
    "DeleteByteMatchSetResponse"
    "fixture/DeleteByteMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteByteMatchSet)

responseGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeysResponse -> TestTree
responseGetRateBasedRuleManagedKeys = res
    "GetRateBasedRuleManagedKeysResponse"
    "fixture/GetRateBasedRuleManagedKeysResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetRateBasedRuleManagedKeys)

responseAssociateWebACL :: AssociateWebACLResponse -> TestTree
responseAssociateWebACL = res
    "AssociateWebACLResponse"
    "fixture/AssociateWebACLResponse.proto"
    wAFRegional
    (Proxy :: Proxy AssociateWebACL)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet = res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteIPSet)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet = res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateIPSet)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets = res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListIPSets)

responseCreateXSSMatchSet :: CreateXSSMatchSetResponse -> TestTree
responseCreateXSSMatchSet = res
    "CreateXSSMatchSetResponse"
    "fixture/CreateXSSMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateXSSMatchSet)

responseGetByteMatchSet :: GetByteMatchSetResponse -> TestTree
responseGetByteMatchSet = res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetByteMatchSet)

responseGetRateBasedRule :: GetRateBasedRuleResponse -> TestTree
responseGetRateBasedRule = res
    "GetRateBasedRuleResponse"
    "fixture/GetRateBasedRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetRateBasedRule)

responseDeleteSizeConstraintSet :: DeleteSizeConstraintSetResponse -> TestTree
responseDeleteSizeConstraintSet = res
    "DeleteSizeConstraintSetResponse"
    "fixture/DeleteSizeConstraintSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteSizeConstraintSet)

responseUpdateSizeConstraintSet :: UpdateSizeConstraintSetResponse -> TestTree
responseUpdateSizeConstraintSet = res
    "UpdateSizeConstraintSetResponse"
    "fixture/UpdateSizeConstraintSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateSizeConstraintSet)

responseCreateSizeConstraintSet :: CreateSizeConstraintSetResponse -> TestTree
responseCreateSizeConstraintSet = res
    "CreateSizeConstraintSetResponse"
    "fixture/CreateSizeConstraintSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateSizeConstraintSet)

responseListSqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> TestTree
responseListSqlInjectionMatchSets = res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListSqlInjectionMatchSets)

responseCreateRateBasedRule :: CreateRateBasedRuleResponse -> TestTree
responseCreateRateBasedRule = res
    "CreateRateBasedRuleResponse"
    "fixture/CreateRateBasedRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateRateBasedRule)

responseDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSetResponse -> TestTree
responseDeleteSqlInjectionMatchSet = res
    "DeleteSqlInjectionMatchSetResponse"
    "fixture/DeleteSqlInjectionMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteSqlInjectionMatchSet)

responseUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSetResponse -> TestTree
responseUpdateSqlInjectionMatchSet = res
    "UpdateSqlInjectionMatchSetResponse"
    "fixture/UpdateSqlInjectionMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateSqlInjectionMatchSet)

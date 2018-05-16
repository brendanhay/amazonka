{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WAFRegional
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WAFRegional where

import Data.Proxy
import Network.AWS.WAFRegional
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WAFRegional.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListActivatedRulesInRuleGroup $
--             listActivatedRulesInRuleGroup
--
--         , requestListRateBasedRules $
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
--         , requestGetRuleGroup $
--             getRuleGroup
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
--         , requestGetGeoMatchSet $
--             getGeoMatchSet
--
--         , requestListByteMatchSets $
--             listByteMatchSets
--
--         , requestListGeoMatchSets $
--             listGeoMatchSets
--
--         , requestCreateRuleGroup $
--             createRuleGroup
--
--         , requestDeleteRegexMatchSet $
--             deleteRegexMatchSet
--
--         , requestUpdateRegexMatchSet $
--             updateRegexMatchSet
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
--         , requestCreateGeoMatchSet $
--             createGeoMatchSet
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
--         , requestListSubscribedRuleGroups $
--             listSubscribedRuleGroups
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
--         , requestPutPermissionPolicy $
--             putPermissionPolicy
--
--         , requestGetRateBasedRuleManagedKeys $
--             getRateBasedRuleManagedKeys
--
--         , requestAssociateWebACL $
--             associateWebACL
--
--         , requestDeletePermissionPolicy $
--             deletePermissionPolicy
--
--         , requestGetRegexMatchSet $
--             getRegexMatchSet
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
--         , requestListRegexMatchSets $
--             listRegexMatchSets
--
--         , requestCreateXSSMatchSet $
--             createXSSMatchSet
--
--         , requestDeleteGeoMatchSet $
--             deleteGeoMatchSet
--
--         , requestUpdateGeoMatchSet $
--             updateGeoMatchSet
--
--         , requestGetByteMatchSet $
--             getByteMatchSet
--
--         , requestGetPermissionPolicy $
--             getPermissionPolicy
--
--         , requestListRuleGroups $
--             listRuleGroups
--
--         , requestDeleteRuleGroup $
--             deleteRuleGroup
--
--         , requestUpdateRuleGroup $
--             updateRuleGroup
--
--         , requestCreateRegexMatchSet $
--             createRegexMatchSet
--
--         , requestGetRateBasedRule $
--             getRateBasedRule
--
--         , requestCreateRegexPatternSet $
--             createRegexPatternSet
--
--         , requestDeleteSizeConstraintSet $
--             deleteSizeConstraintSet
--
--         , requestUpdateSizeConstraintSet $
--             updateSizeConstraintSet
--
--         , requestDeleteRegexPatternSet $
--             deleteRegexPatternSet
--
--         , requestUpdateRegexPatternSet $
--             updateRegexPatternSet
--
--         , requestCreateSizeConstraintSet $
--             createSizeConstraintSet
--
--         , requestListRegexPatternSets $
--             listRegexPatternSets
--
--         , requestListSqlInjectionMatchSets $
--             listSqlInjectionMatchSets
--
--         , requestGetRegexPatternSet $
--             getRegexPatternSet
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
--         [ responseListActivatedRulesInRuleGroup $
--             listActivatedRulesInRuleGroupResponse
--
--         , responseListRateBasedRules $
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
--         , responseGetRuleGroup $
--             getRuleGroupResponse
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
--         , responseGetGeoMatchSet $
--             getGeoMatchSetResponse
--
--         , responseListByteMatchSets $
--             listByteMatchSetsResponse
--
--         , responseListGeoMatchSets $
--             listGeoMatchSetsResponse
--
--         , responseCreateRuleGroup $
--             createRuleGroupResponse
--
--         , responseDeleteRegexMatchSet $
--             deleteRegexMatchSetResponse
--
--         , responseUpdateRegexMatchSet $
--             updateRegexMatchSetResponse
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
--         , responseCreateGeoMatchSet $
--             createGeoMatchSetResponse
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
--         , responseListSubscribedRuleGroups $
--             listSubscribedRuleGroupsResponse
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
--         , responsePutPermissionPolicy $
--             putPermissionPolicyResponse
--
--         , responseGetRateBasedRuleManagedKeys $
--             getRateBasedRuleManagedKeysResponse
--
--         , responseAssociateWebACL $
--             associateWebACLResponse
--
--         , responseDeletePermissionPolicy $
--             deletePermissionPolicyResponse
--
--         , responseGetRegexMatchSet $
--             getRegexMatchSetResponse
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
--         , responseListRegexMatchSets $
--             listRegexMatchSetsResponse
--
--         , responseCreateXSSMatchSet $
--             createXSSMatchSetResponse
--
--         , responseDeleteGeoMatchSet $
--             deleteGeoMatchSetResponse
--
--         , responseUpdateGeoMatchSet $
--             updateGeoMatchSetResponse
--
--         , responseGetByteMatchSet $
--             getByteMatchSetResponse
--
--         , responseGetPermissionPolicy $
--             getPermissionPolicyResponse
--
--         , responseListRuleGroups $
--             listRuleGroupsResponse
--
--         , responseDeleteRuleGroup $
--             deleteRuleGroupResponse
--
--         , responseUpdateRuleGroup $
--             updateRuleGroupResponse
--
--         , responseCreateRegexMatchSet $
--             createRegexMatchSetResponse
--
--         , responseGetRateBasedRule $
--             getRateBasedRuleResponse
--
--         , responseCreateRegexPatternSet $
--             createRegexPatternSetResponse
--
--         , responseDeleteSizeConstraintSet $
--             deleteSizeConstraintSetResponse
--
--         , responseUpdateSizeConstraintSet $
--             updateSizeConstraintSetResponse
--
--         , responseDeleteRegexPatternSet $
--             deleteRegexPatternSetResponse
--
--         , responseUpdateRegexPatternSet $
--             updateRegexPatternSetResponse
--
--         , responseCreateSizeConstraintSet $
--             createSizeConstraintSetResponse
--
--         , responseListRegexPatternSets $
--             listRegexPatternSetsResponse
--
--         , responseListSqlInjectionMatchSets $
--             listSqlInjectionMatchSetsResponse
--
--         , responseGetRegexPatternSet $
--             getRegexPatternSetResponse
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

requestListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroup -> TestTree
requestListActivatedRulesInRuleGroup = req
    "ListActivatedRulesInRuleGroup"
    "fixture/ListActivatedRulesInRuleGroup.yaml"

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

requestGetRuleGroup :: GetRuleGroup -> TestTree
requestGetRuleGroup = req
    "GetRuleGroup"
    "fixture/GetRuleGroup.yaml"

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

requestGetGeoMatchSet :: GetGeoMatchSet -> TestTree
requestGetGeoMatchSet = req
    "GetGeoMatchSet"
    "fixture/GetGeoMatchSet.yaml"

requestListByteMatchSets :: ListByteMatchSets -> TestTree
requestListByteMatchSets = req
    "ListByteMatchSets"
    "fixture/ListByteMatchSets.yaml"

requestListGeoMatchSets :: ListGeoMatchSets -> TestTree
requestListGeoMatchSets = req
    "ListGeoMatchSets"
    "fixture/ListGeoMatchSets.yaml"

requestCreateRuleGroup :: CreateRuleGroup -> TestTree
requestCreateRuleGroup = req
    "CreateRuleGroup"
    "fixture/CreateRuleGroup.yaml"

requestDeleteRegexMatchSet :: DeleteRegexMatchSet -> TestTree
requestDeleteRegexMatchSet = req
    "DeleteRegexMatchSet"
    "fixture/DeleteRegexMatchSet.yaml"

requestUpdateRegexMatchSet :: UpdateRegexMatchSet -> TestTree
requestUpdateRegexMatchSet = req
    "UpdateRegexMatchSet"
    "fixture/UpdateRegexMatchSet.yaml"

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

requestCreateGeoMatchSet :: CreateGeoMatchSet -> TestTree
requestCreateGeoMatchSet = req
    "CreateGeoMatchSet"
    "fixture/CreateGeoMatchSet.yaml"

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

requestListSubscribedRuleGroups :: ListSubscribedRuleGroups -> TestTree
requestListSubscribedRuleGroups = req
    "ListSubscribedRuleGroups"
    "fixture/ListSubscribedRuleGroups.yaml"

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

requestPutPermissionPolicy :: PutPermissionPolicy -> TestTree
requestPutPermissionPolicy = req
    "PutPermissionPolicy"
    "fixture/PutPermissionPolicy.yaml"

requestGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeys -> TestTree
requestGetRateBasedRuleManagedKeys = req
    "GetRateBasedRuleManagedKeys"
    "fixture/GetRateBasedRuleManagedKeys.yaml"

requestAssociateWebACL :: AssociateWebACL -> TestTree
requestAssociateWebACL = req
    "AssociateWebACL"
    "fixture/AssociateWebACL.yaml"

requestDeletePermissionPolicy :: DeletePermissionPolicy -> TestTree
requestDeletePermissionPolicy = req
    "DeletePermissionPolicy"
    "fixture/DeletePermissionPolicy.yaml"

requestGetRegexMatchSet :: GetRegexMatchSet -> TestTree
requestGetRegexMatchSet = req
    "GetRegexMatchSet"
    "fixture/GetRegexMatchSet.yaml"

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

requestListRegexMatchSets :: ListRegexMatchSets -> TestTree
requestListRegexMatchSets = req
    "ListRegexMatchSets"
    "fixture/ListRegexMatchSets.yaml"

requestCreateXSSMatchSet :: CreateXSSMatchSet -> TestTree
requestCreateXSSMatchSet = req
    "CreateXSSMatchSet"
    "fixture/CreateXSSMatchSet.yaml"

requestDeleteGeoMatchSet :: DeleteGeoMatchSet -> TestTree
requestDeleteGeoMatchSet = req
    "DeleteGeoMatchSet"
    "fixture/DeleteGeoMatchSet.yaml"

requestUpdateGeoMatchSet :: UpdateGeoMatchSet -> TestTree
requestUpdateGeoMatchSet = req
    "UpdateGeoMatchSet"
    "fixture/UpdateGeoMatchSet.yaml"

requestGetByteMatchSet :: GetByteMatchSet -> TestTree
requestGetByteMatchSet = req
    "GetByteMatchSet"
    "fixture/GetByteMatchSet.yaml"

requestGetPermissionPolicy :: GetPermissionPolicy -> TestTree
requestGetPermissionPolicy = req
    "GetPermissionPolicy"
    "fixture/GetPermissionPolicy.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups = req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

requestDeleteRuleGroup :: DeleteRuleGroup -> TestTree
requestDeleteRuleGroup = req
    "DeleteRuleGroup"
    "fixture/DeleteRuleGroup.yaml"

requestUpdateRuleGroup :: UpdateRuleGroup -> TestTree
requestUpdateRuleGroup = req
    "UpdateRuleGroup"
    "fixture/UpdateRuleGroup.yaml"

requestCreateRegexMatchSet :: CreateRegexMatchSet -> TestTree
requestCreateRegexMatchSet = req
    "CreateRegexMatchSet"
    "fixture/CreateRegexMatchSet.yaml"

requestGetRateBasedRule :: GetRateBasedRule -> TestTree
requestGetRateBasedRule = req
    "GetRateBasedRule"
    "fixture/GetRateBasedRule.yaml"

requestCreateRegexPatternSet :: CreateRegexPatternSet -> TestTree
requestCreateRegexPatternSet = req
    "CreateRegexPatternSet"
    "fixture/CreateRegexPatternSet.yaml"

requestDeleteSizeConstraintSet :: DeleteSizeConstraintSet -> TestTree
requestDeleteSizeConstraintSet = req
    "DeleteSizeConstraintSet"
    "fixture/DeleteSizeConstraintSet.yaml"

requestUpdateSizeConstraintSet :: UpdateSizeConstraintSet -> TestTree
requestUpdateSizeConstraintSet = req
    "UpdateSizeConstraintSet"
    "fixture/UpdateSizeConstraintSet.yaml"

requestDeleteRegexPatternSet :: DeleteRegexPatternSet -> TestTree
requestDeleteRegexPatternSet = req
    "DeleteRegexPatternSet"
    "fixture/DeleteRegexPatternSet.yaml"

requestUpdateRegexPatternSet :: UpdateRegexPatternSet -> TestTree
requestUpdateRegexPatternSet = req
    "UpdateRegexPatternSet"
    "fixture/UpdateRegexPatternSet.yaml"

requestCreateSizeConstraintSet :: CreateSizeConstraintSet -> TestTree
requestCreateSizeConstraintSet = req
    "CreateSizeConstraintSet"
    "fixture/CreateSizeConstraintSet.yaml"

requestListRegexPatternSets :: ListRegexPatternSets -> TestTree
requestListRegexPatternSets = req
    "ListRegexPatternSets"
    "fixture/ListRegexPatternSets.yaml"

requestListSqlInjectionMatchSets :: ListSqlInjectionMatchSets -> TestTree
requestListSqlInjectionMatchSets = req
    "ListSqlInjectionMatchSets"
    "fixture/ListSqlInjectionMatchSets.yaml"

requestGetRegexPatternSet :: GetRegexPatternSet -> TestTree
requestGetRegexPatternSet = req
    "GetRegexPatternSet"
    "fixture/GetRegexPatternSet.yaml"

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

responseListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroupResponse -> TestTree
responseListActivatedRulesInRuleGroup = res
    "ListActivatedRulesInRuleGroupResponse"
    "fixture/ListActivatedRulesInRuleGroupResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListActivatedRulesInRuleGroup)

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

responseGetRuleGroup :: GetRuleGroupResponse -> TestTree
responseGetRuleGroup = res
    "GetRuleGroupResponse"
    "fixture/GetRuleGroupResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetRuleGroup)

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

responseGetGeoMatchSet :: GetGeoMatchSetResponse -> TestTree
responseGetGeoMatchSet = res
    "GetGeoMatchSetResponse"
    "fixture/GetGeoMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetGeoMatchSet)

responseListByteMatchSets :: ListByteMatchSetsResponse -> TestTree
responseListByteMatchSets = res
    "ListByteMatchSetsResponse"
    "fixture/ListByteMatchSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListByteMatchSets)

responseListGeoMatchSets :: ListGeoMatchSetsResponse -> TestTree
responseListGeoMatchSets = res
    "ListGeoMatchSetsResponse"
    "fixture/ListGeoMatchSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListGeoMatchSets)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup = res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateRuleGroup)

responseDeleteRegexMatchSet :: DeleteRegexMatchSetResponse -> TestTree
responseDeleteRegexMatchSet = res
    "DeleteRegexMatchSetResponse"
    "fixture/DeleteRegexMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteRegexMatchSet)

responseUpdateRegexMatchSet :: UpdateRegexMatchSetResponse -> TestTree
responseUpdateRegexMatchSet = res
    "UpdateRegexMatchSetResponse"
    "fixture/UpdateRegexMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateRegexMatchSet)

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

responseCreateGeoMatchSet :: CreateGeoMatchSetResponse -> TestTree
responseCreateGeoMatchSet = res
    "CreateGeoMatchSetResponse"
    "fixture/CreateGeoMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateGeoMatchSet)

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

responseListSubscribedRuleGroups :: ListSubscribedRuleGroupsResponse -> TestTree
responseListSubscribedRuleGroups = res
    "ListSubscribedRuleGroupsResponse"
    "fixture/ListSubscribedRuleGroupsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListSubscribedRuleGroups)

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

responsePutPermissionPolicy :: PutPermissionPolicyResponse -> TestTree
responsePutPermissionPolicy = res
    "PutPermissionPolicyResponse"
    "fixture/PutPermissionPolicyResponse.proto"
    wAFRegional
    (Proxy :: Proxy PutPermissionPolicy)

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

responseDeletePermissionPolicy :: DeletePermissionPolicyResponse -> TestTree
responseDeletePermissionPolicy = res
    "DeletePermissionPolicyResponse"
    "fixture/DeletePermissionPolicyResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeletePermissionPolicy)

responseGetRegexMatchSet :: GetRegexMatchSetResponse -> TestTree
responseGetRegexMatchSet = res
    "GetRegexMatchSetResponse"
    "fixture/GetRegexMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetRegexMatchSet)

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

responseListRegexMatchSets :: ListRegexMatchSetsResponse -> TestTree
responseListRegexMatchSets = res
    "ListRegexMatchSetsResponse"
    "fixture/ListRegexMatchSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListRegexMatchSets)

responseCreateXSSMatchSet :: CreateXSSMatchSetResponse -> TestTree
responseCreateXSSMatchSet = res
    "CreateXSSMatchSetResponse"
    "fixture/CreateXSSMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateXSSMatchSet)

responseDeleteGeoMatchSet :: DeleteGeoMatchSetResponse -> TestTree
responseDeleteGeoMatchSet = res
    "DeleteGeoMatchSetResponse"
    "fixture/DeleteGeoMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteGeoMatchSet)

responseUpdateGeoMatchSet :: UpdateGeoMatchSetResponse -> TestTree
responseUpdateGeoMatchSet = res
    "UpdateGeoMatchSetResponse"
    "fixture/UpdateGeoMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateGeoMatchSet)

responseGetByteMatchSet :: GetByteMatchSetResponse -> TestTree
responseGetByteMatchSet = res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetByteMatchSet)

responseGetPermissionPolicy :: GetPermissionPolicyResponse -> TestTree
responseGetPermissionPolicy = res
    "GetPermissionPolicyResponse"
    "fixture/GetPermissionPolicyResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetPermissionPolicy)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups = res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListRuleGroups)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup = res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteRuleGroup)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup = res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateRuleGroup)

responseCreateRegexMatchSet :: CreateRegexMatchSetResponse -> TestTree
responseCreateRegexMatchSet = res
    "CreateRegexMatchSetResponse"
    "fixture/CreateRegexMatchSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateRegexMatchSet)

responseGetRateBasedRule :: GetRateBasedRuleResponse -> TestTree
responseGetRateBasedRule = res
    "GetRateBasedRuleResponse"
    "fixture/GetRateBasedRuleResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetRateBasedRule)

responseCreateRegexPatternSet :: CreateRegexPatternSetResponse -> TestTree
responseCreateRegexPatternSet = res
    "CreateRegexPatternSetResponse"
    "fixture/CreateRegexPatternSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateRegexPatternSet)

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

responseDeleteRegexPatternSet :: DeleteRegexPatternSetResponse -> TestTree
responseDeleteRegexPatternSet = res
    "DeleteRegexPatternSetResponse"
    "fixture/DeleteRegexPatternSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy DeleteRegexPatternSet)

responseUpdateRegexPatternSet :: UpdateRegexPatternSetResponse -> TestTree
responseUpdateRegexPatternSet = res
    "UpdateRegexPatternSetResponse"
    "fixture/UpdateRegexPatternSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy UpdateRegexPatternSet)

responseCreateSizeConstraintSet :: CreateSizeConstraintSetResponse -> TestTree
responseCreateSizeConstraintSet = res
    "CreateSizeConstraintSetResponse"
    "fixture/CreateSizeConstraintSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy CreateSizeConstraintSet)

responseListRegexPatternSets :: ListRegexPatternSetsResponse -> TestTree
responseListRegexPatternSets = res
    "ListRegexPatternSetsResponse"
    "fixture/ListRegexPatternSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListRegexPatternSets)

responseListSqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> TestTree
responseListSqlInjectionMatchSets = res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    wAFRegional
    (Proxy :: Proxy ListSqlInjectionMatchSets)

responseGetRegexPatternSet :: GetRegexPatternSetResponse -> TestTree
responseGetRegexPatternSet = res
    "GetRegexPatternSetResponse"
    "fixture/GetRegexPatternSetResponse.proto"
    wAFRegional
    (Proxy :: Proxy GetRegexPatternSet)

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

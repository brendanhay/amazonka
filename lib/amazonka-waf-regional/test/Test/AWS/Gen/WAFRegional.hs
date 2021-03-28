{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WAFRegional
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestListActivatedRulesInRuleGroup $
--             mkListActivatedRulesInRuleGroup
--
--         , requestListRateBasedRules $
--             mkListRateBasedRules
--
--         , requestGetSizeConstraintSet $
--             mkGetSizeConstraintSet
--
--         , requestDeleteRateBasedRule $
--             mkDeleteRateBasedRule
--
--         , requestUpdateRateBasedRule $
--             mkUpdateRateBasedRule
--
--         , requestUpdateRule $
--             mkUpdateRule
--
--         , requestDeleteRule $
--             mkDeleteRule
--
--         , requestCreateIPSet $
--             mkCreateIPSet
--
--         , requestGetRuleGroup $
--             mkGetRuleGroup
--
--         , requestGetChangeTokenStatus $
--             mkGetChangeTokenStatus
--
--         , requestDeleteWebACL $
--             mkDeleteWebACL
--
--         , requestUpdateWebACL $
--             mkUpdateWebACL
--
--         , requestListWebACLs $
--             mkListWebACLs
--
--         , requestListRules $
--             mkListRules
--
--         , requestCreateRule $
--             mkCreateRule
--
--         , requestDeleteLoggingConfiguration $
--             mkDeleteLoggingConfiguration
--
--         , requestCreateWebACL $
--             mkCreateWebACL
--
--         , requestGetGeoMatchSet $
--             mkGetGeoMatchSet
--
--         , requestPutLoggingConfiguration $
--             mkPutLoggingConfiguration
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestListByteMatchSets $
--             mkListByteMatchSets
--
--         , requestListGeoMatchSets $
--             mkListGeoMatchSets
--
--         , requestGetLoggingConfiguration $
--             mkGetLoggingConfiguration
--
--         , requestCreateRuleGroup $
--             mkCreateRuleGroup
--
--         , requestDeleteRegexMatchSet $
--             mkDeleteRegexMatchSet
--
--         , requestUpdateRegexMatchSet $
--             mkUpdateRegexMatchSet
--
--         , requestGetIPSet $
--             mkGetIPSet
--
--         , requestGetWebACL $
--             mkGetWebACL
--
--         , requestGetRule $
--             mkGetRule
--
--         , requestDeleteXssMatchSet $
--             mkDeleteXssMatchSet
--
--         , requestUpdateXssMatchSet $
--             mkUpdateXssMatchSet
--
--         , requestCreateWebACLMigrationStack $
--             mkCreateWebACLMigrationStack
--
--         , requestListXssMatchSets $
--             mkListXssMatchSets
--
--         , requestCreateGeoMatchSet $
--             mkCreateGeoMatchSet
--
--         , requestGetChangeToken $
--             mkGetChangeToken
--
--         , requestListSizeConstraintSets $
--             mkListSizeConstraintSets
--
--         , requestListResourcesForWebACL $
--             mkListResourcesForWebACL
--
--         , requestGetSampledRequests $
--             mkGetSampledRequests
--
--         , requestGetSqlInjectionMatchSet $
--             mkGetSqlInjectionMatchSet
--
--         , requestGetWebACLForResource $
--             mkGetWebACLForResource
--
--         , requestDisassociateWebACL $
--             mkDisassociateWebACL
--
--         , requestListSubscribedRuleGroups $
--             mkListSubscribedRuleGroups
--
--         , requestCreateSqlInjectionMatchSet $
--             mkCreateSqlInjectionMatchSet
--
--         , requestGetXssMatchSet $
--             mkGetXssMatchSet
--
--         , requestCreateByteMatchSet $
--             mkCreateByteMatchSet
--
--         , requestUpdateByteMatchSet $
--             mkUpdateByteMatchSet
--
--         , requestDeleteByteMatchSet $
--             mkDeleteByteMatchSet
--
--         , requestPutPermissionPolicy $
--             mkPutPermissionPolicy
--
--         , requestListLoggingConfigurations $
--             mkListLoggingConfigurations
--
--         , requestGetRateBasedRuleManagedKeys $
--             mkGetRateBasedRuleManagedKeys
--
--         , requestAssociateWebACL $
--             mkAssociateWebACL
--
--         , requestDeletePermissionPolicy $
--             mkDeletePermissionPolicy
--
--         , requestGetRegexMatchSet $
--             mkGetRegexMatchSet
--
--         , requestDeleteIPSet $
--             mkDeleteIPSet
--
--         , requestUpdateIPSet $
--             mkUpdateIPSet
--
--         , requestListIPSets $
--             mkListIPSets
--
--         , requestListRegexMatchSets $
--             mkListRegexMatchSets
--
--         , requestCreateXssMatchSet $
--             mkCreateXssMatchSet
--
--         , requestDeleteGeoMatchSet $
--             mkDeleteGeoMatchSet
--
--         , requestUpdateGeoMatchSet $
--             mkUpdateGeoMatchSet
--
--         , requestGetByteMatchSet $
--             mkGetByteMatchSet
--
--         , requestGetPermissionPolicy $
--             mkGetPermissionPolicy
--
--         , requestListRuleGroups $
--             mkListRuleGroups
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDeleteRuleGroup $
--             mkDeleteRuleGroup
--
--         , requestUpdateRuleGroup $
--             mkUpdateRuleGroup
--
--         , requestCreateRegexMatchSet $
--             mkCreateRegexMatchSet
--
--         , requestGetRateBasedRule $
--             mkGetRateBasedRule
--
--         , requestCreateRegexPatternSet $
--             mkCreateRegexPatternSet
--
--         , requestDeleteSizeConstraintSet $
--             mkDeleteSizeConstraintSet
--
--         , requestUpdateSizeConstraintSet $
--             mkUpdateSizeConstraintSet
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteRegexPatternSet $
--             mkDeleteRegexPatternSet
--
--         , requestUpdateRegexPatternSet $
--             mkUpdateRegexPatternSet
--
--         , requestCreateSizeConstraintSet $
--             mkCreateSizeConstraintSet
--
--         , requestListRegexPatternSets $
--             mkListRegexPatternSets
--
--         , requestListSqlInjectionMatchSets $
--             mkListSqlInjectionMatchSets
--
--         , requestGetRegexPatternSet $
--             mkGetRegexPatternSet
--
--         , requestCreateRateBasedRule $
--             mkCreateRateBasedRule
--
--         , requestDeleteSqlInjectionMatchSet $
--             mkDeleteSqlInjectionMatchSet
--
--         , requestUpdateSqlInjectionMatchSet $
--             mkUpdateSqlInjectionMatchSet
--
--           ]

--     , testGroup "response"
--         [ responseListActivatedRulesInRuleGroup $
--             mkListActivatedRulesInRuleGroupResponse
--
--         , responseListRateBasedRules $
--             mkListRateBasedRulesResponse
--
--         , responseGetSizeConstraintSet $
--             mkGetSizeConstraintSetResponse
--
--         , responseDeleteRateBasedRule $
--             mkDeleteRateBasedRuleResponse
--
--         , responseUpdateRateBasedRule $
--             mkUpdateRateBasedRuleResponse
--
--         , responseUpdateRule $
--             mkUpdateRuleResponse
--
--         , responseDeleteRule $
--             mkDeleteRuleResponse
--
--         , responseCreateIPSet $
--             mkCreateIPSetResponse
--
--         , responseGetRuleGroup $
--             mkGetRuleGroupResponse
--
--         , responseGetChangeTokenStatus $
--             mkGetChangeTokenStatusResponse
--
--         , responseDeleteWebACL $
--             mkDeleteWebACLResponse
--
--         , responseUpdateWebACL $
--             mkUpdateWebACLResponse
--
--         , responseListWebACLs $
--             mkListWebACLsResponse
--
--         , responseListRules $
--             mkListRulesResponse
--
--         , responseCreateRule $
--             mkCreateRuleResponse
--
--         , responseDeleteLoggingConfiguration $
--             mkDeleteLoggingConfigurationResponse
--
--         , responseCreateWebACL $
--             mkCreateWebACLResponse
--
--         , responseGetGeoMatchSet $
--             mkGetGeoMatchSetResponse
--
--         , responsePutLoggingConfiguration $
--             mkPutLoggingConfigurationResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseListByteMatchSets $
--             mkListByteMatchSetsResponse
--
--         , responseListGeoMatchSets $
--             mkListGeoMatchSetsResponse
--
--         , responseGetLoggingConfiguration $
--             mkGetLoggingConfigurationResponse
--
--         , responseCreateRuleGroup $
--             mkCreateRuleGroupResponse
--
--         , responseDeleteRegexMatchSet $
--             mkDeleteRegexMatchSetResponse
--
--         , responseUpdateRegexMatchSet $
--             mkUpdateRegexMatchSetResponse
--
--         , responseGetIPSet $
--             mkGetIPSetResponse
--
--         , responseGetWebACL $
--             mkGetWebACLResponse
--
--         , responseGetRule $
--             mkGetRuleResponse
--
--         , responseDeleteXssMatchSet $
--             mkDeleteXssMatchSetResponse
--
--         , responseUpdateXssMatchSet $
--             mkUpdateXssMatchSetResponse
--
--         , responseCreateWebACLMigrationStack $
--             mkCreateWebACLMigrationStackResponse
--
--         , responseListXssMatchSets $
--             mkListXssMatchSetsResponse
--
--         , responseCreateGeoMatchSet $
--             mkCreateGeoMatchSetResponse
--
--         , responseGetChangeToken $
--             mkGetChangeTokenResponse
--
--         , responseListSizeConstraintSets $
--             mkListSizeConstraintSetsResponse
--
--         , responseListResourcesForWebACL $
--             mkListResourcesForWebACLResponse
--
--         , responseGetSampledRequests $
--             mkGetSampledRequestsResponse
--
--         , responseGetSqlInjectionMatchSet $
--             mkGetSqlInjectionMatchSetResponse
--
--         , responseGetWebACLForResource $
--             mkGetWebACLForResourceResponse
--
--         , responseDisassociateWebACL $
--             mkDisassociateWebACLResponse
--
--         , responseListSubscribedRuleGroups $
--             mkListSubscribedRuleGroupsResponse
--
--         , responseCreateSqlInjectionMatchSet $
--             mkCreateSqlInjectionMatchSetResponse
--
--         , responseGetXssMatchSet $
--             mkGetXssMatchSetResponse
--
--         , responseCreateByteMatchSet $
--             mkCreateByteMatchSetResponse
--
--         , responseUpdateByteMatchSet $
--             mkUpdateByteMatchSetResponse
--
--         , responseDeleteByteMatchSet $
--             mkDeleteByteMatchSetResponse
--
--         , responsePutPermissionPolicy $
--             mkPutPermissionPolicyResponse
--
--         , responseListLoggingConfigurations $
--             mkListLoggingConfigurationsResponse
--
--         , responseGetRateBasedRuleManagedKeys $
--             mkGetRateBasedRuleManagedKeysResponse
--
--         , responseAssociateWebACL $
--             mkAssociateWebACLResponse
--
--         , responseDeletePermissionPolicy $
--             mkDeletePermissionPolicyResponse
--
--         , responseGetRegexMatchSet $
--             mkGetRegexMatchSetResponse
--
--         , responseDeleteIPSet $
--             mkDeleteIPSetResponse
--
--         , responseUpdateIPSet $
--             mkUpdateIPSetResponse
--
--         , responseListIPSets $
--             mkListIPSetsResponse
--
--         , responseListRegexMatchSets $
--             mkListRegexMatchSetsResponse
--
--         , responseCreateXssMatchSet $
--             mkCreateXssMatchSetResponse
--
--         , responseDeleteGeoMatchSet $
--             mkDeleteGeoMatchSetResponse
--
--         , responseUpdateGeoMatchSet $
--             mkUpdateGeoMatchSetResponse
--
--         , responseGetByteMatchSet $
--             mkGetByteMatchSetResponse
--
--         , responseGetPermissionPolicy $
--             mkGetPermissionPolicyResponse
--
--         , responseListRuleGroups $
--             mkListRuleGroupsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDeleteRuleGroup $
--             mkDeleteRuleGroupResponse
--
--         , responseUpdateRuleGroup $
--             mkUpdateRuleGroupResponse
--
--         , responseCreateRegexMatchSet $
--             mkCreateRegexMatchSetResponse
--
--         , responseGetRateBasedRule $
--             mkGetRateBasedRuleResponse
--
--         , responseCreateRegexPatternSet $
--             mkCreateRegexPatternSetResponse
--
--         , responseDeleteSizeConstraintSet $
--             mkDeleteSizeConstraintSetResponse
--
--         , responseUpdateSizeConstraintSet $
--             mkUpdateSizeConstraintSetResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteRegexPatternSet $
--             mkDeleteRegexPatternSetResponse
--
--         , responseUpdateRegexPatternSet $
--             mkUpdateRegexPatternSetResponse
--
--         , responseCreateSizeConstraintSet $
--             mkCreateSizeConstraintSetResponse
--
--         , responseListRegexPatternSets $
--             mkListRegexPatternSetsResponse
--
--         , responseListSqlInjectionMatchSets $
--             mkListSqlInjectionMatchSetsResponse
--
--         , responseGetRegexPatternSet $
--             mkGetRegexPatternSetResponse
--
--         , responseCreateRateBasedRule $
--             mkCreateRateBasedRuleResponse
--
--         , responseDeleteSqlInjectionMatchSet $
--             mkDeleteSqlInjectionMatchSetResponse
--
--         , responseUpdateSqlInjectionMatchSet $
--             mkUpdateSqlInjectionMatchSetResponse
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

requestDeleteLoggingConfiguration :: DeleteLoggingConfiguration -> TestTree
requestDeleteLoggingConfiguration = req
    "DeleteLoggingConfiguration"
    "fixture/DeleteLoggingConfiguration.yaml"

requestCreateWebACL :: CreateWebACL -> TestTree
requestCreateWebACL = req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

requestGetGeoMatchSet :: GetGeoMatchSet -> TestTree
requestGetGeoMatchSet = req
    "GetGeoMatchSet"
    "fixture/GetGeoMatchSet.yaml"

requestPutLoggingConfiguration :: PutLoggingConfiguration -> TestTree
requestPutLoggingConfiguration = req
    "PutLoggingConfiguration"
    "fixture/PutLoggingConfiguration.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListByteMatchSets :: ListByteMatchSets -> TestTree
requestListByteMatchSets = req
    "ListByteMatchSets"
    "fixture/ListByteMatchSets.yaml"

requestListGeoMatchSets :: ListGeoMatchSets -> TestTree
requestListGeoMatchSets = req
    "ListGeoMatchSets"
    "fixture/ListGeoMatchSets.yaml"

requestGetLoggingConfiguration :: GetLoggingConfiguration -> TestTree
requestGetLoggingConfiguration = req
    "GetLoggingConfiguration"
    "fixture/GetLoggingConfiguration.yaml"

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

requestDeleteXssMatchSet :: DeleteXssMatchSet -> TestTree
requestDeleteXssMatchSet = req
    "DeleteXssMatchSet"
    "fixture/DeleteXssMatchSet.yaml"

requestUpdateXssMatchSet :: UpdateXssMatchSet -> TestTree
requestUpdateXssMatchSet = req
    "UpdateXssMatchSet"
    "fixture/UpdateXssMatchSet.yaml"

requestCreateWebACLMigrationStack :: CreateWebACLMigrationStack -> TestTree
requestCreateWebACLMigrationStack = req
    "CreateWebACLMigrationStack"
    "fixture/CreateWebACLMigrationStack.yaml"

requestListXssMatchSets :: ListXssMatchSets -> TestTree
requestListXssMatchSets = req
    "ListXssMatchSets"
    "fixture/ListXssMatchSets.yaml"

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

requestGetXssMatchSet :: GetXssMatchSet -> TestTree
requestGetXssMatchSet = req
    "GetXssMatchSet"
    "fixture/GetXssMatchSet.yaml"

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

requestListLoggingConfigurations :: ListLoggingConfigurations -> TestTree
requestListLoggingConfigurations = req
    "ListLoggingConfigurations"
    "fixture/ListLoggingConfigurations.yaml"

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

requestCreateXssMatchSet :: CreateXssMatchSet -> TestTree
requestCreateXssMatchSet = req
    "CreateXssMatchSet"
    "fixture/CreateXssMatchSet.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

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
    mkServiceConfig
    (Proxy :: Proxy ListActivatedRulesInRuleGroup)

responseListRateBasedRules :: ListRateBasedRulesResponse -> TestTree
responseListRateBasedRules = res
    "ListRateBasedRulesResponse"
    "fixture/ListRateBasedRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRateBasedRules)

responseGetSizeConstraintSet :: GetSizeConstraintSetResponse -> TestTree
responseGetSizeConstraintSet = res
    "GetSizeConstraintSetResponse"
    "fixture/GetSizeConstraintSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSizeConstraintSet)

responseDeleteRateBasedRule :: DeleteRateBasedRuleResponse -> TestTree
responseDeleteRateBasedRule = res
    "DeleteRateBasedRuleResponse"
    "fixture/DeleteRateBasedRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRateBasedRule)

responseUpdateRateBasedRule :: UpdateRateBasedRuleResponse -> TestTree
responseUpdateRateBasedRule = res
    "UpdateRateBasedRuleResponse"
    "fixture/UpdateRateBasedRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRateBasedRule)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule = res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRule)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule = res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRule)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet = res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateIPSet)

responseGetRuleGroup :: GetRuleGroupResponse -> TestTree
responseGetRuleGroup = res
    "GetRuleGroupResponse"
    "fixture/GetRuleGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRuleGroup)

responseGetChangeTokenStatus :: GetChangeTokenStatusResponse -> TestTree
responseGetChangeTokenStatus = res
    "GetChangeTokenStatusResponse"
    "fixture/GetChangeTokenStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetChangeTokenStatus)

responseDeleteWebACL :: DeleteWebACLResponse -> TestTree
responseDeleteWebACL = res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteWebACL)

responseUpdateWebACL :: UpdateWebACLResponse -> TestTree
responseUpdateWebACL = res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateWebACL)

responseListWebACLs :: ListWebACLsResponse -> TestTree
responseListWebACLs = res
    "ListWebACLsResponse"
    "fixture/ListWebACLsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListWebACLs)

responseListRules :: ListRulesResponse -> TestTree
responseListRules = res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRules)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule = res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRule)

responseDeleteLoggingConfiguration :: DeleteLoggingConfigurationResponse -> TestTree
responseDeleteLoggingConfiguration = res
    "DeleteLoggingConfigurationResponse"
    "fixture/DeleteLoggingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLoggingConfiguration)

responseCreateWebACL :: CreateWebACLResponse -> TestTree
responseCreateWebACL = res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateWebACL)

responseGetGeoMatchSet :: GetGeoMatchSetResponse -> TestTree
responseGetGeoMatchSet = res
    "GetGeoMatchSetResponse"
    "fixture/GetGeoMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGeoMatchSet)

responsePutLoggingConfiguration :: PutLoggingConfigurationResponse -> TestTree
responsePutLoggingConfiguration = res
    "PutLoggingConfigurationResponse"
    "fixture/PutLoggingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutLoggingConfiguration)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseListByteMatchSets :: ListByteMatchSetsResponse -> TestTree
responseListByteMatchSets = res
    "ListByteMatchSetsResponse"
    "fixture/ListByteMatchSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListByteMatchSets)

responseListGeoMatchSets :: ListGeoMatchSetsResponse -> TestTree
responseListGeoMatchSets = res
    "ListGeoMatchSetsResponse"
    "fixture/ListGeoMatchSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGeoMatchSets)

responseGetLoggingConfiguration :: GetLoggingConfigurationResponse -> TestTree
responseGetLoggingConfiguration = res
    "GetLoggingConfigurationResponse"
    "fixture/GetLoggingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoggingConfiguration)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup = res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRuleGroup)

responseDeleteRegexMatchSet :: DeleteRegexMatchSetResponse -> TestTree
responseDeleteRegexMatchSet = res
    "DeleteRegexMatchSetResponse"
    "fixture/DeleteRegexMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRegexMatchSet)

responseUpdateRegexMatchSet :: UpdateRegexMatchSetResponse -> TestTree
responseUpdateRegexMatchSet = res
    "UpdateRegexMatchSetResponse"
    "fixture/UpdateRegexMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRegexMatchSet)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet = res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIPSet)

responseGetWebACL :: GetWebACLResponse -> TestTree
responseGetWebACL = res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetWebACL)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule = res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRule)

responseDeleteXssMatchSet :: DeleteXssMatchSetResponse -> TestTree
responseDeleteXssMatchSet = res
    "DeleteXssMatchSetResponse"
    "fixture/DeleteXssMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteXssMatchSet)

responseUpdateXssMatchSet :: UpdateXssMatchSetResponse -> TestTree
responseUpdateXssMatchSet = res
    "UpdateXssMatchSetResponse"
    "fixture/UpdateXssMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateXssMatchSet)

responseCreateWebACLMigrationStack :: CreateWebACLMigrationStackResponse -> TestTree
responseCreateWebACLMigrationStack = res
    "CreateWebACLMigrationStackResponse"
    "fixture/CreateWebACLMigrationStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateWebACLMigrationStack)

responseListXssMatchSets :: ListXssMatchSetsResponse -> TestTree
responseListXssMatchSets = res
    "ListXssMatchSetsResponse"
    "fixture/ListXssMatchSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListXssMatchSets)

responseCreateGeoMatchSet :: CreateGeoMatchSetResponse -> TestTree
responseCreateGeoMatchSet = res
    "CreateGeoMatchSetResponse"
    "fixture/CreateGeoMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGeoMatchSet)

responseGetChangeToken :: GetChangeTokenResponse -> TestTree
responseGetChangeToken = res
    "GetChangeTokenResponse"
    "fixture/GetChangeTokenResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetChangeToken)

responseListSizeConstraintSets :: ListSizeConstraintSetsResponse -> TestTree
responseListSizeConstraintSets = res
    "ListSizeConstraintSetsResponse"
    "fixture/ListSizeConstraintSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSizeConstraintSets)

responseListResourcesForWebACL :: ListResourcesForWebACLResponse -> TestTree
responseListResourcesForWebACL = res
    "ListResourcesForWebACLResponse"
    "fixture/ListResourcesForWebACLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListResourcesForWebACL)

responseGetSampledRequests :: GetSampledRequestsResponse -> TestTree
responseGetSampledRequests = res
    "GetSampledRequestsResponse"
    "fixture/GetSampledRequestsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSampledRequests)

responseGetSqlInjectionMatchSet :: GetSqlInjectionMatchSetResponse -> TestTree
responseGetSqlInjectionMatchSet = res
    "GetSqlInjectionMatchSetResponse"
    "fixture/GetSqlInjectionMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSqlInjectionMatchSet)

responseGetWebACLForResource :: GetWebACLForResourceResponse -> TestTree
responseGetWebACLForResource = res
    "GetWebACLForResourceResponse"
    "fixture/GetWebACLForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetWebACLForResource)

responseDisassociateWebACL :: DisassociateWebACLResponse -> TestTree
responseDisassociateWebACL = res
    "DisassociateWebACLResponse"
    "fixture/DisassociateWebACLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateWebACL)

responseListSubscribedRuleGroups :: ListSubscribedRuleGroupsResponse -> TestTree
responseListSubscribedRuleGroups = res
    "ListSubscribedRuleGroupsResponse"
    "fixture/ListSubscribedRuleGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSubscribedRuleGroups)

responseCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSetResponse -> TestTree
responseCreateSqlInjectionMatchSet = res
    "CreateSqlInjectionMatchSetResponse"
    "fixture/CreateSqlInjectionMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSqlInjectionMatchSet)

responseGetXssMatchSet :: GetXssMatchSetResponse -> TestTree
responseGetXssMatchSet = res
    "GetXssMatchSetResponse"
    "fixture/GetXssMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetXssMatchSet)

responseCreateByteMatchSet :: CreateByteMatchSetResponse -> TestTree
responseCreateByteMatchSet = res
    "CreateByteMatchSetResponse"
    "fixture/CreateByteMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateByteMatchSet)

responseUpdateByteMatchSet :: UpdateByteMatchSetResponse -> TestTree
responseUpdateByteMatchSet = res
    "UpdateByteMatchSetResponse"
    "fixture/UpdateByteMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateByteMatchSet)

responseDeleteByteMatchSet :: DeleteByteMatchSetResponse -> TestTree
responseDeleteByteMatchSet = res
    "DeleteByteMatchSetResponse"
    "fixture/DeleteByteMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteByteMatchSet)

responsePutPermissionPolicy :: PutPermissionPolicyResponse -> TestTree
responsePutPermissionPolicy = res
    "PutPermissionPolicyResponse"
    "fixture/PutPermissionPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutPermissionPolicy)

responseListLoggingConfigurations :: ListLoggingConfigurationsResponse -> TestTree
responseListLoggingConfigurations = res
    "ListLoggingConfigurationsResponse"
    "fixture/ListLoggingConfigurationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListLoggingConfigurations)

responseGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeysResponse -> TestTree
responseGetRateBasedRuleManagedKeys = res
    "GetRateBasedRuleManagedKeysResponse"
    "fixture/GetRateBasedRuleManagedKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRateBasedRuleManagedKeys)

responseAssociateWebACL :: AssociateWebACLResponse -> TestTree
responseAssociateWebACL = res
    "AssociateWebACLResponse"
    "fixture/AssociateWebACLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateWebACL)

responseDeletePermissionPolicy :: DeletePermissionPolicyResponse -> TestTree
responseDeletePermissionPolicy = res
    "DeletePermissionPolicyResponse"
    "fixture/DeletePermissionPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePermissionPolicy)

responseGetRegexMatchSet :: GetRegexMatchSetResponse -> TestTree
responseGetRegexMatchSet = res
    "GetRegexMatchSetResponse"
    "fixture/GetRegexMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRegexMatchSet)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet = res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIPSet)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet = res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateIPSet)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets = res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIPSets)

responseListRegexMatchSets :: ListRegexMatchSetsResponse -> TestTree
responseListRegexMatchSets = res
    "ListRegexMatchSetsResponse"
    "fixture/ListRegexMatchSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRegexMatchSets)

responseCreateXssMatchSet :: CreateXssMatchSetResponse -> TestTree
responseCreateXssMatchSet = res
    "CreateXssMatchSetResponse"
    "fixture/CreateXssMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateXssMatchSet)

responseDeleteGeoMatchSet :: DeleteGeoMatchSetResponse -> TestTree
responseDeleteGeoMatchSet = res
    "DeleteGeoMatchSetResponse"
    "fixture/DeleteGeoMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteGeoMatchSet)

responseUpdateGeoMatchSet :: UpdateGeoMatchSetResponse -> TestTree
responseUpdateGeoMatchSet = res
    "UpdateGeoMatchSetResponse"
    "fixture/UpdateGeoMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateGeoMatchSet)

responseGetByteMatchSet :: GetByteMatchSetResponse -> TestTree
responseGetByteMatchSet = res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetByteMatchSet)

responseGetPermissionPolicy :: GetPermissionPolicyResponse -> TestTree
responseGetPermissionPolicy = res
    "GetPermissionPolicyResponse"
    "fixture/GetPermissionPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPermissionPolicy)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups = res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRuleGroups)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup = res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRuleGroup)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup = res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRuleGroup)

responseCreateRegexMatchSet :: CreateRegexMatchSetResponse -> TestTree
responseCreateRegexMatchSet = res
    "CreateRegexMatchSetResponse"
    "fixture/CreateRegexMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRegexMatchSet)

responseGetRateBasedRule :: GetRateBasedRuleResponse -> TestTree
responseGetRateBasedRule = res
    "GetRateBasedRuleResponse"
    "fixture/GetRateBasedRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRateBasedRule)

responseCreateRegexPatternSet :: CreateRegexPatternSetResponse -> TestTree
responseCreateRegexPatternSet = res
    "CreateRegexPatternSetResponse"
    "fixture/CreateRegexPatternSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRegexPatternSet)

responseDeleteSizeConstraintSet :: DeleteSizeConstraintSetResponse -> TestTree
responseDeleteSizeConstraintSet = res
    "DeleteSizeConstraintSetResponse"
    "fixture/DeleteSizeConstraintSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSizeConstraintSet)

responseUpdateSizeConstraintSet :: UpdateSizeConstraintSetResponse -> TestTree
responseUpdateSizeConstraintSet = res
    "UpdateSizeConstraintSetResponse"
    "fixture/UpdateSizeConstraintSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSizeConstraintSet)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDeleteRegexPatternSet :: DeleteRegexPatternSetResponse -> TestTree
responseDeleteRegexPatternSet = res
    "DeleteRegexPatternSetResponse"
    "fixture/DeleteRegexPatternSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRegexPatternSet)

responseUpdateRegexPatternSet :: UpdateRegexPatternSetResponse -> TestTree
responseUpdateRegexPatternSet = res
    "UpdateRegexPatternSetResponse"
    "fixture/UpdateRegexPatternSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRegexPatternSet)

responseCreateSizeConstraintSet :: CreateSizeConstraintSetResponse -> TestTree
responseCreateSizeConstraintSet = res
    "CreateSizeConstraintSetResponse"
    "fixture/CreateSizeConstraintSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSizeConstraintSet)

responseListRegexPatternSets :: ListRegexPatternSetsResponse -> TestTree
responseListRegexPatternSets = res
    "ListRegexPatternSetsResponse"
    "fixture/ListRegexPatternSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRegexPatternSets)

responseListSqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> TestTree
responseListSqlInjectionMatchSets = res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSqlInjectionMatchSets)

responseGetRegexPatternSet :: GetRegexPatternSetResponse -> TestTree
responseGetRegexPatternSet = res
    "GetRegexPatternSetResponse"
    "fixture/GetRegexPatternSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRegexPatternSet)

responseCreateRateBasedRule :: CreateRateBasedRuleResponse -> TestTree
responseCreateRateBasedRule = res
    "CreateRateBasedRuleResponse"
    "fixture/CreateRateBasedRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRateBasedRule)

responseDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSetResponse -> TestTree
responseDeleteSqlInjectionMatchSet = res
    "DeleteSqlInjectionMatchSetResponse"
    "fixture/DeleteSqlInjectionMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSqlInjectionMatchSet)

responseUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSetResponse -> TestTree
responseUpdateSqlInjectionMatchSet = res
    "UpdateSqlInjectionMatchSetResponse"
    "fixture/UpdateSqlInjectionMatchSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSqlInjectionMatchSet)

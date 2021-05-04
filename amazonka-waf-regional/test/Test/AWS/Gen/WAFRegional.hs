{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WAFRegional
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestDeleteWebACL $
--             newDeleteWebACL
--
--         , requestGetChangeTokenStatus $
--             newGetChangeTokenStatus
--
--         , requestUpdateRule $
--             newUpdateRule
--
--         , requestGetRuleGroup $
--             newGetRuleGroup
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestUpdateWebACL $
--             newUpdateWebACL
--
--         , requestListRateBasedRules $
--             newListRateBasedRules
--
--         , requestGetSizeConstraintSet $
--             newGetSizeConstraintSet
--
--         , requestGetWebACLForResource $
--             newGetWebACLForResource
--
--         , requestListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSets
--
--         , requestCreateRateBasedRule $
--             newCreateRateBasedRule
--
--         , requestListRegexPatternSets $
--             newListRegexPatternSets
--
--         , requestGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSet
--
--         , requestCreateRegexPatternSet $
--             newCreateRegexPatternSet
--
--         , requestUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSet
--
--         , requestGetChangeToken $
--             newGetChangeToken
--
--         , requestListSizeConstraintSets $
--             newListSizeConstraintSets
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSet
--
--         , requestListXssMatchSets $
--             newListXssMatchSets
--
--         , requestDeleteRuleGroup $
--             newDeleteRuleGroup
--
--         , requestUpdateRuleGroup $
--             newUpdateRuleGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStack
--
--         , requestCreateRegexMatchSet $
--             newCreateRegexMatchSet
--
--         , requestCreateRuleGroup $
--             newCreateRuleGroup
--
--         , requestListRegexMatchSets $
--             newListRegexMatchSets
--
--         , requestUpdateRegexMatchSet $
--             newUpdateRegexMatchSet
--
--         , requestDeleteRegexMatchSet $
--             newDeleteRegexMatchSet
--
--         , requestGetLoggingConfiguration $
--             newGetLoggingConfiguration
--
--         , requestAssociateWebACL $
--             newAssociateWebACL
--
--         , requestDeleteLoggingConfiguration $
--             newDeleteLoggingConfiguration
--
--         , requestPutPermissionPolicy $
--             newPutPermissionPolicy
--
--         , requestDeleteIPSet $
--             newDeleteIPSet
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestListLoggingConfigurations $
--             newListLoggingConfigurations
--
--         , requestUpdateIPSet $
--             newUpdateIPSet
--
--         , requestGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeys
--
--         , requestGetGeoMatchSet $
--             newGetGeoMatchSet
--
--         , requestCreateWebACL $
--             newCreateWebACL
--
--         , requestListWebACLs $
--             newListWebACLs
--
--         , requestListRules $
--             newListRules
--
--         , requestCreateByteMatchSet $
--             newCreateByteMatchSet
--
--         , requestGetXssMatchSet $
--             newGetXssMatchSet
--
--         , requestCreateIPSet $
--             newCreateIPSet
--
--         , requestListSubscribedRuleGroups $
--             newListSubscribedRuleGroups
--
--         , requestListActivatedRulesInRuleGroup $
--             newListActivatedRulesInRuleGroup
--
--         , requestDisassociateWebACL $
--             newDisassociateWebACL
--
--         , requestDeleteRateBasedRule $
--             newDeleteRateBasedRule
--
--         , requestUpdateRateBasedRule $
--             newUpdateRateBasedRule
--
--         , requestCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSet
--
--         , requestGetRegexPatternSet $
--             newGetRegexPatternSet
--
--         , requestUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSet
--
--         , requestDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSet
--
--         , requestUpdateRegexPatternSet $
--             newUpdateRegexPatternSet
--
--         , requestDeleteRegexPatternSet $
--             newDeleteRegexPatternSet
--
--         , requestGetSampledRequests $
--             newGetSampledRequests
--
--         , requestListResourcesForWebACL $
--             newListResourcesForWebACL
--
--         , requestCreateSizeConstraintSet $
--             newCreateSizeConstraintSet
--
--         , requestGetRateBasedRule $
--             newGetRateBasedRule
--
--         , requestCreateGeoMatchSet $
--             newCreateGeoMatchSet
--
--         , requestDeleteXssMatchSet $
--             newDeleteXssMatchSet
--
--         , requestGetRule $
--             newGetRule
--
--         , requestListRuleGroups $
--             newListRuleGroups
--
--         , requestUpdateXssMatchSet $
--             newUpdateXssMatchSet
--
--         , requestGetWebACL $
--             newGetWebACL
--
--         , requestUpdateGeoMatchSet $
--             newUpdateGeoMatchSet
--
--         , requestGetPermissionPolicy $
--             newGetPermissionPolicy
--
--         , requestListGeoMatchSets $
--             newListGeoMatchSets
--
--         , requestGetByteMatchSet $
--             newGetByteMatchSet
--
--         , requestCreateXssMatchSet $
--             newCreateXssMatchSet
--
--         , requestGetIPSet $
--             newGetIPSet
--
--         , requestDeleteGeoMatchSet $
--             newDeleteGeoMatchSet
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestUpdateByteMatchSet $
--             newUpdateByteMatchSet
--
--         , requestDeleteByteMatchSet $
--             newDeleteByteMatchSet
--
--         , requestGetRegexMatchSet $
--             newGetRegexMatchSet
--
--         , requestListByteMatchSets $
--             newListByteMatchSets
--
--         , requestDeletePermissionPolicy $
--             newDeletePermissionPolicy
--
--         , requestListIPSets $
--             newListIPSets
--
--         , requestPutLoggingConfiguration $
--             newPutLoggingConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseDeleteWebACL $
--             newDeleteWebACLResponse
--
--         , responseGetChangeTokenStatus $
--             newGetChangeTokenStatusResponse
--
--         , responseUpdateRule $
--             newUpdateRuleResponse
--
--         , responseGetRuleGroup $
--             newGetRuleGroupResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseUpdateWebACL $
--             newUpdateWebACLResponse
--
--         , responseListRateBasedRules $
--             newListRateBasedRulesResponse
--
--         , responseGetSizeConstraintSet $
--             newGetSizeConstraintSetResponse
--
--         , responseGetWebACLForResource $
--             newGetWebACLForResourceResponse
--
--         , responseListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSetsResponse
--
--         , responseCreateRateBasedRule $
--             newCreateRateBasedRuleResponse
--
--         , responseListRegexPatternSets $
--             newListRegexPatternSetsResponse
--
--         , responseGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSetResponse
--
--         , responseCreateRegexPatternSet $
--             newCreateRegexPatternSetResponse
--
--         , responseUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSetResponse
--
--         , responseGetChangeToken $
--             newGetChangeTokenResponse
--
--         , responseListSizeConstraintSets $
--             newListSizeConstraintSetsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSetResponse
--
--         , responseListXssMatchSets $
--             newListXssMatchSetsResponse
--
--         , responseDeleteRuleGroup $
--             newDeleteRuleGroupResponse
--
--         , responseUpdateRuleGroup $
--             newUpdateRuleGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStackResponse
--
--         , responseCreateRegexMatchSet $
--             newCreateRegexMatchSetResponse
--
--         , responseCreateRuleGroup $
--             newCreateRuleGroupResponse
--
--         , responseListRegexMatchSets $
--             newListRegexMatchSetsResponse
--
--         , responseUpdateRegexMatchSet $
--             newUpdateRegexMatchSetResponse
--
--         , responseDeleteRegexMatchSet $
--             newDeleteRegexMatchSetResponse
--
--         , responseGetLoggingConfiguration $
--             newGetLoggingConfigurationResponse
--
--         , responseAssociateWebACL $
--             newAssociateWebACLResponse
--
--         , responseDeleteLoggingConfiguration $
--             newDeleteLoggingConfigurationResponse
--
--         , responsePutPermissionPolicy $
--             newPutPermissionPolicyResponse
--
--         , responseDeleteIPSet $
--             newDeleteIPSetResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseListLoggingConfigurations $
--             newListLoggingConfigurationsResponse
--
--         , responseUpdateIPSet $
--             newUpdateIPSetResponse
--
--         , responseGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeysResponse
--
--         , responseGetGeoMatchSet $
--             newGetGeoMatchSetResponse
--
--         , responseCreateWebACL $
--             newCreateWebACLResponse
--
--         , responseListWebACLs $
--             newListWebACLsResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseCreateByteMatchSet $
--             newCreateByteMatchSetResponse
--
--         , responseGetXssMatchSet $
--             newGetXssMatchSetResponse
--
--         , responseCreateIPSet $
--             newCreateIPSetResponse
--
--         , responseListSubscribedRuleGroups $
--             newListSubscribedRuleGroupsResponse
--
--         , responseListActivatedRulesInRuleGroup $
--             newListActivatedRulesInRuleGroupResponse
--
--         , responseDisassociateWebACL $
--             newDisassociateWebACLResponse
--
--         , responseDeleteRateBasedRule $
--             newDeleteRateBasedRuleResponse
--
--         , responseUpdateRateBasedRule $
--             newUpdateRateBasedRuleResponse
--
--         , responseCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSetResponse
--
--         , responseGetRegexPatternSet $
--             newGetRegexPatternSetResponse
--
--         , responseUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSetResponse
--
--         , responseDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSetResponse
--
--         , responseUpdateRegexPatternSet $
--             newUpdateRegexPatternSetResponse
--
--         , responseDeleteRegexPatternSet $
--             newDeleteRegexPatternSetResponse
--
--         , responseGetSampledRequests $
--             newGetSampledRequestsResponse
--
--         , responseListResourcesForWebACL $
--             newListResourcesForWebACLResponse
--
--         , responseCreateSizeConstraintSet $
--             newCreateSizeConstraintSetResponse
--
--         , responseGetRateBasedRule $
--             newGetRateBasedRuleResponse
--
--         , responseCreateGeoMatchSet $
--             newCreateGeoMatchSetResponse
--
--         , responseDeleteXssMatchSet $
--             newDeleteXssMatchSetResponse
--
--         , responseGetRule $
--             newGetRuleResponse
--
--         , responseListRuleGroups $
--             newListRuleGroupsResponse
--
--         , responseUpdateXssMatchSet $
--             newUpdateXssMatchSetResponse
--
--         , responseGetWebACL $
--             newGetWebACLResponse
--
--         , responseUpdateGeoMatchSet $
--             newUpdateGeoMatchSetResponse
--
--         , responseGetPermissionPolicy $
--             newGetPermissionPolicyResponse
--
--         , responseListGeoMatchSets $
--             newListGeoMatchSetsResponse
--
--         , responseGetByteMatchSet $
--             newGetByteMatchSetResponse
--
--         , responseCreateXssMatchSet $
--             newCreateXssMatchSetResponse
--
--         , responseGetIPSet $
--             newGetIPSetResponse
--
--         , responseDeleteGeoMatchSet $
--             newDeleteGeoMatchSetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseUpdateByteMatchSet $
--             newUpdateByteMatchSetResponse
--
--         , responseDeleteByteMatchSet $
--             newDeleteByteMatchSetResponse
--
--         , responseGetRegexMatchSet $
--             newGetRegexMatchSetResponse
--
--         , responseListByteMatchSets $
--             newListByteMatchSetsResponse
--
--         , responseDeletePermissionPolicy $
--             newDeletePermissionPolicyResponse
--
--         , responseListIPSets $
--             newListIPSetsResponse
--
--         , responsePutLoggingConfiguration $
--             newPutLoggingConfigurationResponse
--
--           ]
--     ]

-- Requests

requestDeleteWebACL :: DeleteWebACL -> TestTree
requestDeleteWebACL =
  req
    "DeleteWebACL"
    "fixture/DeleteWebACL.yaml"

requestGetChangeTokenStatus :: GetChangeTokenStatus -> TestTree
requestGetChangeTokenStatus =
  req
    "GetChangeTokenStatus"
    "fixture/GetChangeTokenStatus.yaml"

requestUpdateRule :: UpdateRule -> TestTree
requestUpdateRule =
  req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

requestGetRuleGroup :: GetRuleGroup -> TestTree
requestGetRuleGroup =
  req
    "GetRuleGroup"
    "fixture/GetRuleGroup.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestUpdateWebACL :: UpdateWebACL -> TestTree
requestUpdateWebACL =
  req
    "UpdateWebACL"
    "fixture/UpdateWebACL.yaml"

requestListRateBasedRules :: ListRateBasedRules -> TestTree
requestListRateBasedRules =
  req
    "ListRateBasedRules"
    "fixture/ListRateBasedRules.yaml"

requestGetSizeConstraintSet :: GetSizeConstraintSet -> TestTree
requestGetSizeConstraintSet =
  req
    "GetSizeConstraintSet"
    "fixture/GetSizeConstraintSet.yaml"

requestGetWebACLForResource :: GetWebACLForResource -> TestTree
requestGetWebACLForResource =
  req
    "GetWebACLForResource"
    "fixture/GetWebACLForResource.yaml"

requestListSqlInjectionMatchSets :: ListSqlInjectionMatchSets -> TestTree
requestListSqlInjectionMatchSets =
  req
    "ListSqlInjectionMatchSets"
    "fixture/ListSqlInjectionMatchSets.yaml"

requestCreateRateBasedRule :: CreateRateBasedRule -> TestTree
requestCreateRateBasedRule =
  req
    "CreateRateBasedRule"
    "fixture/CreateRateBasedRule.yaml"

requestListRegexPatternSets :: ListRegexPatternSets -> TestTree
requestListRegexPatternSets =
  req
    "ListRegexPatternSets"
    "fixture/ListRegexPatternSets.yaml"

requestGetSqlInjectionMatchSet :: GetSqlInjectionMatchSet -> TestTree
requestGetSqlInjectionMatchSet =
  req
    "GetSqlInjectionMatchSet"
    "fixture/GetSqlInjectionMatchSet.yaml"

requestCreateRegexPatternSet :: CreateRegexPatternSet -> TestTree
requestCreateRegexPatternSet =
  req
    "CreateRegexPatternSet"
    "fixture/CreateRegexPatternSet.yaml"

requestUpdateSizeConstraintSet :: UpdateSizeConstraintSet -> TestTree
requestUpdateSizeConstraintSet =
  req
    "UpdateSizeConstraintSet"
    "fixture/UpdateSizeConstraintSet.yaml"

requestGetChangeToken :: GetChangeToken -> TestTree
requestGetChangeToken =
  req
    "GetChangeToken"
    "fixture/GetChangeToken.yaml"

requestListSizeConstraintSets :: ListSizeConstraintSets -> TestTree
requestListSizeConstraintSets =
  req
    "ListSizeConstraintSets"
    "fixture/ListSizeConstraintSets.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteSizeConstraintSet :: DeleteSizeConstraintSet -> TestTree
requestDeleteSizeConstraintSet =
  req
    "DeleteSizeConstraintSet"
    "fixture/DeleteSizeConstraintSet.yaml"

requestListXssMatchSets :: ListXssMatchSets -> TestTree
requestListXssMatchSets =
  req
    "ListXssMatchSets"
    "fixture/ListXssMatchSets.yaml"

requestDeleteRuleGroup :: DeleteRuleGroup -> TestTree
requestDeleteRuleGroup =
  req
    "DeleteRuleGroup"
    "fixture/DeleteRuleGroup.yaml"

requestUpdateRuleGroup :: UpdateRuleGroup -> TestTree
requestUpdateRuleGroup =
  req
    "UpdateRuleGroup"
    "fixture/UpdateRuleGroup.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateWebACLMigrationStack :: CreateWebACLMigrationStack -> TestTree
requestCreateWebACLMigrationStack =
  req
    "CreateWebACLMigrationStack"
    "fixture/CreateWebACLMigrationStack.yaml"

requestCreateRegexMatchSet :: CreateRegexMatchSet -> TestTree
requestCreateRegexMatchSet =
  req
    "CreateRegexMatchSet"
    "fixture/CreateRegexMatchSet.yaml"

requestCreateRuleGroup :: CreateRuleGroup -> TestTree
requestCreateRuleGroup =
  req
    "CreateRuleGroup"
    "fixture/CreateRuleGroup.yaml"

requestListRegexMatchSets :: ListRegexMatchSets -> TestTree
requestListRegexMatchSets =
  req
    "ListRegexMatchSets"
    "fixture/ListRegexMatchSets.yaml"

requestUpdateRegexMatchSet :: UpdateRegexMatchSet -> TestTree
requestUpdateRegexMatchSet =
  req
    "UpdateRegexMatchSet"
    "fixture/UpdateRegexMatchSet.yaml"

requestDeleteRegexMatchSet :: DeleteRegexMatchSet -> TestTree
requestDeleteRegexMatchSet =
  req
    "DeleteRegexMatchSet"
    "fixture/DeleteRegexMatchSet.yaml"

requestGetLoggingConfiguration :: GetLoggingConfiguration -> TestTree
requestGetLoggingConfiguration =
  req
    "GetLoggingConfiguration"
    "fixture/GetLoggingConfiguration.yaml"

requestAssociateWebACL :: AssociateWebACL -> TestTree
requestAssociateWebACL =
  req
    "AssociateWebACL"
    "fixture/AssociateWebACL.yaml"

requestDeleteLoggingConfiguration :: DeleteLoggingConfiguration -> TestTree
requestDeleteLoggingConfiguration =
  req
    "DeleteLoggingConfiguration"
    "fixture/DeleteLoggingConfiguration.yaml"

requestPutPermissionPolicy :: PutPermissionPolicy -> TestTree
requestPutPermissionPolicy =
  req
    "PutPermissionPolicy"
    "fixture/PutPermissionPolicy.yaml"

requestDeleteIPSet :: DeleteIPSet -> TestTree
requestDeleteIPSet =
  req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestListLoggingConfigurations :: ListLoggingConfigurations -> TestTree
requestListLoggingConfigurations =
  req
    "ListLoggingConfigurations"
    "fixture/ListLoggingConfigurations.yaml"

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet =
  req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeys -> TestTree
requestGetRateBasedRuleManagedKeys =
  req
    "GetRateBasedRuleManagedKeys"
    "fixture/GetRateBasedRuleManagedKeys.yaml"

requestGetGeoMatchSet :: GetGeoMatchSet -> TestTree
requestGetGeoMatchSet =
  req
    "GetGeoMatchSet"
    "fixture/GetGeoMatchSet.yaml"

requestCreateWebACL :: CreateWebACL -> TestTree
requestCreateWebACL =
  req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

requestListWebACLs :: ListWebACLs -> TestTree
requestListWebACLs =
  req
    "ListWebACLs"
    "fixture/ListWebACLs.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestCreateByteMatchSet :: CreateByteMatchSet -> TestTree
requestCreateByteMatchSet =
  req
    "CreateByteMatchSet"
    "fixture/CreateByteMatchSet.yaml"

requestGetXssMatchSet :: GetXssMatchSet -> TestTree
requestGetXssMatchSet =
  req
    "GetXssMatchSet"
    "fixture/GetXssMatchSet.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet =
  req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestListSubscribedRuleGroups :: ListSubscribedRuleGroups -> TestTree
requestListSubscribedRuleGroups =
  req
    "ListSubscribedRuleGroups"
    "fixture/ListSubscribedRuleGroups.yaml"

requestListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroup -> TestTree
requestListActivatedRulesInRuleGroup =
  req
    "ListActivatedRulesInRuleGroup"
    "fixture/ListActivatedRulesInRuleGroup.yaml"

requestDisassociateWebACL :: DisassociateWebACL -> TestTree
requestDisassociateWebACL =
  req
    "DisassociateWebACL"
    "fixture/DisassociateWebACL.yaml"

requestDeleteRateBasedRule :: DeleteRateBasedRule -> TestTree
requestDeleteRateBasedRule =
  req
    "DeleteRateBasedRule"
    "fixture/DeleteRateBasedRule.yaml"

requestUpdateRateBasedRule :: UpdateRateBasedRule -> TestTree
requestUpdateRateBasedRule =
  req
    "UpdateRateBasedRule"
    "fixture/UpdateRateBasedRule.yaml"

requestCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSet -> TestTree
requestCreateSqlInjectionMatchSet =
  req
    "CreateSqlInjectionMatchSet"
    "fixture/CreateSqlInjectionMatchSet.yaml"

requestGetRegexPatternSet :: GetRegexPatternSet -> TestTree
requestGetRegexPatternSet =
  req
    "GetRegexPatternSet"
    "fixture/GetRegexPatternSet.yaml"

requestUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSet -> TestTree
requestUpdateSqlInjectionMatchSet =
  req
    "UpdateSqlInjectionMatchSet"
    "fixture/UpdateSqlInjectionMatchSet.yaml"

requestDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSet -> TestTree
requestDeleteSqlInjectionMatchSet =
  req
    "DeleteSqlInjectionMatchSet"
    "fixture/DeleteSqlInjectionMatchSet.yaml"

requestUpdateRegexPatternSet :: UpdateRegexPatternSet -> TestTree
requestUpdateRegexPatternSet =
  req
    "UpdateRegexPatternSet"
    "fixture/UpdateRegexPatternSet.yaml"

requestDeleteRegexPatternSet :: DeleteRegexPatternSet -> TestTree
requestDeleteRegexPatternSet =
  req
    "DeleteRegexPatternSet"
    "fixture/DeleteRegexPatternSet.yaml"

requestGetSampledRequests :: GetSampledRequests -> TestTree
requestGetSampledRequests =
  req
    "GetSampledRequests"
    "fixture/GetSampledRequests.yaml"

requestListResourcesForWebACL :: ListResourcesForWebACL -> TestTree
requestListResourcesForWebACL =
  req
    "ListResourcesForWebACL"
    "fixture/ListResourcesForWebACL.yaml"

requestCreateSizeConstraintSet :: CreateSizeConstraintSet -> TestTree
requestCreateSizeConstraintSet =
  req
    "CreateSizeConstraintSet"
    "fixture/CreateSizeConstraintSet.yaml"

requestGetRateBasedRule :: GetRateBasedRule -> TestTree
requestGetRateBasedRule =
  req
    "GetRateBasedRule"
    "fixture/GetRateBasedRule.yaml"

requestCreateGeoMatchSet :: CreateGeoMatchSet -> TestTree
requestCreateGeoMatchSet =
  req
    "CreateGeoMatchSet"
    "fixture/CreateGeoMatchSet.yaml"

requestDeleteXssMatchSet :: DeleteXssMatchSet -> TestTree
requestDeleteXssMatchSet =
  req
    "DeleteXssMatchSet"
    "fixture/DeleteXssMatchSet.yaml"

requestGetRule :: GetRule -> TestTree
requestGetRule =
  req
    "GetRule"
    "fixture/GetRule.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups =
  req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

requestUpdateXssMatchSet :: UpdateXssMatchSet -> TestTree
requestUpdateXssMatchSet =
  req
    "UpdateXssMatchSet"
    "fixture/UpdateXssMatchSet.yaml"

requestGetWebACL :: GetWebACL -> TestTree
requestGetWebACL =
  req
    "GetWebACL"
    "fixture/GetWebACL.yaml"

requestUpdateGeoMatchSet :: UpdateGeoMatchSet -> TestTree
requestUpdateGeoMatchSet =
  req
    "UpdateGeoMatchSet"
    "fixture/UpdateGeoMatchSet.yaml"

requestGetPermissionPolicy :: GetPermissionPolicy -> TestTree
requestGetPermissionPolicy =
  req
    "GetPermissionPolicy"
    "fixture/GetPermissionPolicy.yaml"

requestListGeoMatchSets :: ListGeoMatchSets -> TestTree
requestListGeoMatchSets =
  req
    "ListGeoMatchSets"
    "fixture/ListGeoMatchSets.yaml"

requestGetByteMatchSet :: GetByteMatchSet -> TestTree
requestGetByteMatchSet =
  req
    "GetByteMatchSet"
    "fixture/GetByteMatchSet.yaml"

requestCreateXssMatchSet :: CreateXssMatchSet -> TestTree
requestCreateXssMatchSet =
  req
    "CreateXssMatchSet"
    "fixture/CreateXssMatchSet.yaml"

requestGetIPSet :: GetIPSet -> TestTree
requestGetIPSet =
  req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

requestDeleteGeoMatchSet :: DeleteGeoMatchSet -> TestTree
requestDeleteGeoMatchSet =
  req
    "DeleteGeoMatchSet"
    "fixture/DeleteGeoMatchSet.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestUpdateByteMatchSet :: UpdateByteMatchSet -> TestTree
requestUpdateByteMatchSet =
  req
    "UpdateByteMatchSet"
    "fixture/UpdateByteMatchSet.yaml"

requestDeleteByteMatchSet :: DeleteByteMatchSet -> TestTree
requestDeleteByteMatchSet =
  req
    "DeleteByteMatchSet"
    "fixture/DeleteByteMatchSet.yaml"

requestGetRegexMatchSet :: GetRegexMatchSet -> TestTree
requestGetRegexMatchSet =
  req
    "GetRegexMatchSet"
    "fixture/GetRegexMatchSet.yaml"

requestListByteMatchSets :: ListByteMatchSets -> TestTree
requestListByteMatchSets =
  req
    "ListByteMatchSets"
    "fixture/ListByteMatchSets.yaml"

requestDeletePermissionPolicy :: DeletePermissionPolicy -> TestTree
requestDeletePermissionPolicy =
  req
    "DeletePermissionPolicy"
    "fixture/DeletePermissionPolicy.yaml"

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets =
  req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestPutLoggingConfiguration :: PutLoggingConfiguration -> TestTree
requestPutLoggingConfiguration =
  req
    "PutLoggingConfiguration"
    "fixture/PutLoggingConfiguration.yaml"

-- Responses

responseDeleteWebACL :: DeleteWebACLResponse -> TestTree
responseDeleteWebACL =
  res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWebACL)

responseGetChangeTokenStatus :: GetChangeTokenStatusResponse -> TestTree
responseGetChangeTokenStatus =
  res
    "GetChangeTokenStatusResponse"
    "fixture/GetChangeTokenStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetChangeTokenStatus)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule =
  res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRule)

responseGetRuleGroup :: GetRuleGroupResponse -> TestTree
responseGetRuleGroup =
  res
    "GetRuleGroupResponse"
    "fixture/GetRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetRuleGroup)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRule)

responseUpdateWebACL :: UpdateWebACLResponse -> TestTree
responseUpdateWebACL =
  res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWebACL)

responseListRateBasedRules :: ListRateBasedRulesResponse -> TestTree
responseListRateBasedRules =
  res
    "ListRateBasedRulesResponse"
    "fixture/ListRateBasedRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRateBasedRules)

responseGetSizeConstraintSet :: GetSizeConstraintSetResponse -> TestTree
responseGetSizeConstraintSet =
  res
    "GetSizeConstraintSetResponse"
    "fixture/GetSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetSizeConstraintSet)

responseGetWebACLForResource :: GetWebACLForResourceResponse -> TestTree
responseGetWebACLForResource =
  res
    "GetWebACLForResourceResponse"
    "fixture/GetWebACLForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetWebACLForResource)

responseListSqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> TestTree
responseListSqlInjectionMatchSets =
  res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSqlInjectionMatchSets)

responseCreateRateBasedRule :: CreateRateBasedRuleResponse -> TestTree
responseCreateRateBasedRule =
  res
    "CreateRateBasedRuleResponse"
    "fixture/CreateRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRateBasedRule)

responseListRegexPatternSets :: ListRegexPatternSetsResponse -> TestTree
responseListRegexPatternSets =
  res
    "ListRegexPatternSetsResponse"
    "fixture/ListRegexPatternSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRegexPatternSets)

responseGetSqlInjectionMatchSet :: GetSqlInjectionMatchSetResponse -> TestTree
responseGetSqlInjectionMatchSet =
  res
    "GetSqlInjectionMatchSetResponse"
    "fixture/GetSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetSqlInjectionMatchSet)

responseCreateRegexPatternSet :: CreateRegexPatternSetResponse -> TestTree
responseCreateRegexPatternSet =
  res
    "CreateRegexPatternSetResponse"
    "fixture/CreateRegexPatternSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRegexPatternSet)

responseUpdateSizeConstraintSet :: UpdateSizeConstraintSetResponse -> TestTree
responseUpdateSizeConstraintSet =
  res
    "UpdateSizeConstraintSetResponse"
    "fixture/UpdateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSizeConstraintSet)

responseGetChangeToken :: GetChangeTokenResponse -> TestTree
responseGetChangeToken =
  res
    "GetChangeTokenResponse"
    "fixture/GetChangeTokenResponse.proto"
    defaultService
    (Proxy :: Proxy GetChangeToken)

responseListSizeConstraintSets :: ListSizeConstraintSetsResponse -> TestTree
responseListSizeConstraintSets =
  res
    "ListSizeConstraintSetsResponse"
    "fixture/ListSizeConstraintSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSizeConstraintSets)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteSizeConstraintSet :: DeleteSizeConstraintSetResponse -> TestTree
responseDeleteSizeConstraintSet =
  res
    "DeleteSizeConstraintSetResponse"
    "fixture/DeleteSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSizeConstraintSet)

responseListXssMatchSets :: ListXssMatchSetsResponse -> TestTree
responseListXssMatchSets =
  res
    "ListXssMatchSetsResponse"
    "fixture/ListXssMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListXssMatchSets)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup =
  res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRuleGroup)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup =
  res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRuleGroup)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateWebACLMigrationStack :: CreateWebACLMigrationStackResponse -> TestTree
responseCreateWebACLMigrationStack =
  res
    "CreateWebACLMigrationStackResponse"
    "fixture/CreateWebACLMigrationStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWebACLMigrationStack)

responseCreateRegexMatchSet :: CreateRegexMatchSetResponse -> TestTree
responseCreateRegexMatchSet =
  res
    "CreateRegexMatchSetResponse"
    "fixture/CreateRegexMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRegexMatchSet)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup =
  res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRuleGroup)

responseListRegexMatchSets :: ListRegexMatchSetsResponse -> TestTree
responseListRegexMatchSets =
  res
    "ListRegexMatchSetsResponse"
    "fixture/ListRegexMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRegexMatchSets)

responseUpdateRegexMatchSet :: UpdateRegexMatchSetResponse -> TestTree
responseUpdateRegexMatchSet =
  res
    "UpdateRegexMatchSetResponse"
    "fixture/UpdateRegexMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRegexMatchSet)

responseDeleteRegexMatchSet :: DeleteRegexMatchSetResponse -> TestTree
responseDeleteRegexMatchSet =
  res
    "DeleteRegexMatchSetResponse"
    "fixture/DeleteRegexMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegexMatchSet)

responseGetLoggingConfiguration :: GetLoggingConfigurationResponse -> TestTree
responseGetLoggingConfiguration =
  res
    "GetLoggingConfigurationResponse"
    "fixture/GetLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoggingConfiguration)

responseAssociateWebACL :: AssociateWebACLResponse -> TestTree
responseAssociateWebACL =
  res
    "AssociateWebACLResponse"
    "fixture/AssociateWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateWebACL)

responseDeleteLoggingConfiguration :: DeleteLoggingConfigurationResponse -> TestTree
responseDeleteLoggingConfiguration =
  res
    "DeleteLoggingConfigurationResponse"
    "fixture/DeleteLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoggingConfiguration)

responsePutPermissionPolicy :: PutPermissionPolicyResponse -> TestTree
responsePutPermissionPolicy =
  res
    "PutPermissionPolicyResponse"
    "fixture/PutPermissionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutPermissionPolicy)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet =
  res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIPSet)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRule)

responseListLoggingConfigurations :: ListLoggingConfigurationsResponse -> TestTree
responseListLoggingConfigurations =
  res
    "ListLoggingConfigurationsResponse"
    "fixture/ListLoggingConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLoggingConfigurations)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet =
  res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIPSet)

responseGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeysResponse -> TestTree
responseGetRateBasedRuleManagedKeys =
  res
    "GetRateBasedRuleManagedKeysResponse"
    "fixture/GetRateBasedRuleManagedKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetRateBasedRuleManagedKeys)

responseGetGeoMatchSet :: GetGeoMatchSetResponse -> TestTree
responseGetGeoMatchSet =
  res
    "GetGeoMatchSetResponse"
    "fixture/GetGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetGeoMatchSet)

responseCreateWebACL :: CreateWebACLResponse -> TestTree
responseCreateWebACL =
  res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWebACL)

responseListWebACLs :: ListWebACLsResponse -> TestTree
responseListWebACLs =
  res
    "ListWebACLsResponse"
    "fixture/ListWebACLsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWebACLs)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRules)

responseCreateByteMatchSet :: CreateByteMatchSetResponse -> TestTree
responseCreateByteMatchSet =
  res
    "CreateByteMatchSetResponse"
    "fixture/CreateByteMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateByteMatchSet)

responseGetXssMatchSet :: GetXssMatchSetResponse -> TestTree
responseGetXssMatchSet =
  res
    "GetXssMatchSetResponse"
    "fixture/GetXssMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetXssMatchSet)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet =
  res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIPSet)

responseListSubscribedRuleGroups :: ListSubscribedRuleGroupsResponse -> TestTree
responseListSubscribedRuleGroups =
  res
    "ListSubscribedRuleGroupsResponse"
    "fixture/ListSubscribedRuleGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscribedRuleGroups)

responseListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroupResponse -> TestTree
responseListActivatedRulesInRuleGroup =
  res
    "ListActivatedRulesInRuleGroupResponse"
    "fixture/ListActivatedRulesInRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListActivatedRulesInRuleGroup)

responseDisassociateWebACL :: DisassociateWebACLResponse -> TestTree
responseDisassociateWebACL =
  res
    "DisassociateWebACLResponse"
    "fixture/DisassociateWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateWebACL)

responseDeleteRateBasedRule :: DeleteRateBasedRuleResponse -> TestTree
responseDeleteRateBasedRule =
  res
    "DeleteRateBasedRuleResponse"
    "fixture/DeleteRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRateBasedRule)

responseUpdateRateBasedRule :: UpdateRateBasedRuleResponse -> TestTree
responseUpdateRateBasedRule =
  res
    "UpdateRateBasedRuleResponse"
    "fixture/UpdateRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRateBasedRule)

responseCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSetResponse -> TestTree
responseCreateSqlInjectionMatchSet =
  res
    "CreateSqlInjectionMatchSetResponse"
    "fixture/CreateSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSqlInjectionMatchSet)

responseGetRegexPatternSet :: GetRegexPatternSetResponse -> TestTree
responseGetRegexPatternSet =
  res
    "GetRegexPatternSetResponse"
    "fixture/GetRegexPatternSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegexPatternSet)

responseUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSetResponse -> TestTree
responseUpdateSqlInjectionMatchSet =
  res
    "UpdateSqlInjectionMatchSetResponse"
    "fixture/UpdateSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSqlInjectionMatchSet)

responseDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSetResponse -> TestTree
responseDeleteSqlInjectionMatchSet =
  res
    "DeleteSqlInjectionMatchSetResponse"
    "fixture/DeleteSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSqlInjectionMatchSet)

responseUpdateRegexPatternSet :: UpdateRegexPatternSetResponse -> TestTree
responseUpdateRegexPatternSet =
  res
    "UpdateRegexPatternSetResponse"
    "fixture/UpdateRegexPatternSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRegexPatternSet)

responseDeleteRegexPatternSet :: DeleteRegexPatternSetResponse -> TestTree
responseDeleteRegexPatternSet =
  res
    "DeleteRegexPatternSetResponse"
    "fixture/DeleteRegexPatternSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegexPatternSet)

responseGetSampledRequests :: GetSampledRequestsResponse -> TestTree
responseGetSampledRequests =
  res
    "GetSampledRequestsResponse"
    "fixture/GetSampledRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSampledRequests)

responseListResourcesForWebACL :: ListResourcesForWebACLResponse -> TestTree
responseListResourcesForWebACL =
  res
    "ListResourcesForWebACLResponse"
    "fixture/ListResourcesForWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourcesForWebACL)

responseCreateSizeConstraintSet :: CreateSizeConstraintSetResponse -> TestTree
responseCreateSizeConstraintSet =
  res
    "CreateSizeConstraintSetResponse"
    "fixture/CreateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSizeConstraintSet)

responseGetRateBasedRule :: GetRateBasedRuleResponse -> TestTree
responseGetRateBasedRule =
  res
    "GetRateBasedRuleResponse"
    "fixture/GetRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetRateBasedRule)

responseCreateGeoMatchSet :: CreateGeoMatchSetResponse -> TestTree
responseCreateGeoMatchSet =
  res
    "CreateGeoMatchSetResponse"
    "fixture/CreateGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGeoMatchSet)

responseDeleteXssMatchSet :: DeleteXssMatchSetResponse -> TestTree
responseDeleteXssMatchSet =
  res
    "DeleteXssMatchSetResponse"
    "fixture/DeleteXssMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteXssMatchSet)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule =
  res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetRule)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups =
  res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRuleGroups)

responseUpdateXssMatchSet :: UpdateXssMatchSetResponse -> TestTree
responseUpdateXssMatchSet =
  res
    "UpdateXssMatchSetResponse"
    "fixture/UpdateXssMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateXssMatchSet)

responseGetWebACL :: GetWebACLResponse -> TestTree
responseGetWebACL =
  res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy GetWebACL)

responseUpdateGeoMatchSet :: UpdateGeoMatchSetResponse -> TestTree
responseUpdateGeoMatchSet =
  res
    "UpdateGeoMatchSetResponse"
    "fixture/UpdateGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGeoMatchSet)

responseGetPermissionPolicy :: GetPermissionPolicyResponse -> TestTree
responseGetPermissionPolicy =
  res
    "GetPermissionPolicyResponse"
    "fixture/GetPermissionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPermissionPolicy)

responseListGeoMatchSets :: ListGeoMatchSetsResponse -> TestTree
responseListGeoMatchSets =
  res
    "ListGeoMatchSetsResponse"
    "fixture/ListGeoMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGeoMatchSets)

responseGetByteMatchSet :: GetByteMatchSetResponse -> TestTree
responseGetByteMatchSet =
  res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetByteMatchSet)

responseCreateXssMatchSet :: CreateXssMatchSetResponse -> TestTree
responseCreateXssMatchSet =
  res
    "CreateXssMatchSetResponse"
    "fixture/CreateXssMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateXssMatchSet)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet =
  res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetIPSet)

responseDeleteGeoMatchSet :: DeleteGeoMatchSetResponse -> TestTree
responseDeleteGeoMatchSet =
  res
    "DeleteGeoMatchSetResponse"
    "fixture/DeleteGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGeoMatchSet)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseUpdateByteMatchSet :: UpdateByteMatchSetResponse -> TestTree
responseUpdateByteMatchSet =
  res
    "UpdateByteMatchSetResponse"
    "fixture/UpdateByteMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateByteMatchSet)

responseDeleteByteMatchSet :: DeleteByteMatchSetResponse -> TestTree
responseDeleteByteMatchSet =
  res
    "DeleteByteMatchSetResponse"
    "fixture/DeleteByteMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteByteMatchSet)

responseGetRegexMatchSet :: GetRegexMatchSetResponse -> TestTree
responseGetRegexMatchSet =
  res
    "GetRegexMatchSetResponse"
    "fixture/GetRegexMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegexMatchSet)

responseListByteMatchSets :: ListByteMatchSetsResponse -> TestTree
responseListByteMatchSets =
  res
    "ListByteMatchSetsResponse"
    "fixture/ListByteMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListByteMatchSets)

responseDeletePermissionPolicy :: DeletePermissionPolicyResponse -> TestTree
responseDeletePermissionPolicy =
  res
    "DeletePermissionPolicyResponse"
    "fixture/DeletePermissionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePermissionPolicy)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets =
  res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIPSets)

responsePutLoggingConfiguration :: PutLoggingConfigurationResponse -> TestTree
responsePutLoggingConfiguration =
  res
    "PutLoggingConfigurationResponse"
    "fixture/PutLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutLoggingConfiguration)

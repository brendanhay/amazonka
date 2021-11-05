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

import qualified Data.Proxy as Proxy
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
--             newListActivatedRulesInRuleGroup
--
--         , requestListRateBasedRules $
--             newListRateBasedRules
--
--         , requestGetSizeConstraintSet $
--             newGetSizeConstraintSet
--
--         , requestDeleteRateBasedRule $
--             newDeleteRateBasedRule
--
--         , requestUpdateRateBasedRule $
--             newUpdateRateBasedRule
--
--         , requestUpdateRule $
--             newUpdateRule
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestCreateIPSet $
--             newCreateIPSet
--
--         , requestGetRuleGroup $
--             newGetRuleGroup
--
--         , requestGetChangeTokenStatus $
--             newGetChangeTokenStatus
--
--         , requestDeleteWebACL $
--             newDeleteWebACL
--
--         , requestUpdateWebACL $
--             newUpdateWebACL
--
--         , requestListWebACLs $
--             newListWebACLs
--
--         , requestListRules $
--             newListRules
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestDeleteLoggingConfiguration $
--             newDeleteLoggingConfiguration
--
--         , requestCreateWebACL $
--             newCreateWebACL
--
--         , requestGetGeoMatchSet $
--             newGetGeoMatchSet
--
--         , requestPutLoggingConfiguration $
--             newPutLoggingConfiguration
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListByteMatchSets $
--             newListByteMatchSets
--
--         , requestListGeoMatchSets $
--             newListGeoMatchSets
--
--         , requestGetLoggingConfiguration $
--             newGetLoggingConfiguration
--
--         , requestCreateRuleGroup $
--             newCreateRuleGroup
--
--         , requestDeleteRegexMatchSet $
--             newDeleteRegexMatchSet
--
--         , requestUpdateRegexMatchSet $
--             newUpdateRegexMatchSet
--
--         , requestGetIPSet $
--             newGetIPSet
--
--         , requestGetWebACL $
--             newGetWebACL
--
--         , requestGetRule $
--             newGetRule
--
--         , requestDeleteXssMatchSet $
--             newDeleteXssMatchSet
--
--         , requestUpdateXssMatchSet $
--             newUpdateXssMatchSet
--
--         , requestCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStack
--
--         , requestListXssMatchSets $
--             newListXssMatchSets
--
--         , requestCreateGeoMatchSet $
--             newCreateGeoMatchSet
--
--         , requestGetChangeToken $
--             newGetChangeToken
--
--         , requestListSizeConstraintSets $
--             newListSizeConstraintSets
--
--         , requestListResourcesForWebACL $
--             newListResourcesForWebACL
--
--         , requestGetSampledRequests $
--             newGetSampledRequests
--
--         , requestGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSet
--
--         , requestGetWebACLForResource $
--             newGetWebACLForResource
--
--         , requestDisassociateWebACL $
--             newDisassociateWebACL
--
--         , requestListSubscribedRuleGroups $
--             newListSubscribedRuleGroups
--
--         , requestCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSet
--
--         , requestGetXssMatchSet $
--             newGetXssMatchSet
--
--         , requestCreateByteMatchSet $
--             newCreateByteMatchSet
--
--         , requestUpdateByteMatchSet $
--             newUpdateByteMatchSet
--
--         , requestDeleteByteMatchSet $
--             newDeleteByteMatchSet
--
--         , requestPutPermissionPolicy $
--             newPutPermissionPolicy
--
--         , requestListLoggingConfigurations $
--             newListLoggingConfigurations
--
--         , requestGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeys
--
--         , requestAssociateWebACL $
--             newAssociateWebACL
--
--         , requestDeletePermissionPolicy $
--             newDeletePermissionPolicy
--
--         , requestGetRegexMatchSet $
--             newGetRegexMatchSet
--
--         , requestDeleteIPSet $
--             newDeleteIPSet
--
--         , requestUpdateIPSet $
--             newUpdateIPSet
--
--         , requestListIPSets $
--             newListIPSets
--
--         , requestListRegexMatchSets $
--             newListRegexMatchSets
--
--         , requestCreateXssMatchSet $
--             newCreateXssMatchSet
--
--         , requestDeleteGeoMatchSet $
--             newDeleteGeoMatchSet
--
--         , requestUpdateGeoMatchSet $
--             newUpdateGeoMatchSet
--
--         , requestGetByteMatchSet $
--             newGetByteMatchSet
--
--         , requestGetPermissionPolicy $
--             newGetPermissionPolicy
--
--         , requestListRuleGroups $
--             newListRuleGroups
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteRuleGroup $
--             newDeleteRuleGroup
--
--         , requestUpdateRuleGroup $
--             newUpdateRuleGroup
--
--         , requestCreateRegexMatchSet $
--             newCreateRegexMatchSet
--
--         , requestGetRateBasedRule $
--             newGetRateBasedRule
--
--         , requestCreateRegexPatternSet $
--             newCreateRegexPatternSet
--
--         , requestDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSet
--
--         , requestUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSet
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteRegexPatternSet $
--             newDeleteRegexPatternSet
--
--         , requestUpdateRegexPatternSet $
--             newUpdateRegexPatternSet
--
--         , requestCreateSizeConstraintSet $
--             newCreateSizeConstraintSet
--
--         , requestListRegexPatternSets $
--             newListRegexPatternSets
--
--         , requestListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSets
--
--         , requestGetRegexPatternSet $
--             newGetRegexPatternSet
--
--         , requestCreateRateBasedRule $
--             newCreateRateBasedRule
--
--         , requestDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSet
--
--         , requestUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSet
--
--           ]

--     , testGroup "response"
--         [ responseListActivatedRulesInRuleGroup $
--             newListActivatedRulesInRuleGroupResponse
--
--         , responseListRateBasedRules $
--             newListRateBasedRulesResponse
--
--         , responseGetSizeConstraintSet $
--             newGetSizeConstraintSetResponse
--
--         , responseDeleteRateBasedRule $
--             newDeleteRateBasedRuleResponse
--
--         , responseUpdateRateBasedRule $
--             newUpdateRateBasedRuleResponse
--
--         , responseUpdateRule $
--             newUpdateRuleResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseCreateIPSet $
--             newCreateIPSetResponse
--
--         , responseGetRuleGroup $
--             newGetRuleGroupResponse
--
--         , responseGetChangeTokenStatus $
--             newGetChangeTokenStatusResponse
--
--         , responseDeleteWebACL $
--             newDeleteWebACLResponse
--
--         , responseUpdateWebACL $
--             newUpdateWebACLResponse
--
--         , responseListWebACLs $
--             newListWebACLsResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseDeleteLoggingConfiguration $
--             newDeleteLoggingConfigurationResponse
--
--         , responseCreateWebACL $
--             newCreateWebACLResponse
--
--         , responseGetGeoMatchSet $
--             newGetGeoMatchSetResponse
--
--         , responsePutLoggingConfiguration $
--             newPutLoggingConfigurationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListByteMatchSets $
--             newListByteMatchSetsResponse
--
--         , responseListGeoMatchSets $
--             newListGeoMatchSetsResponse
--
--         , responseGetLoggingConfiguration $
--             newGetLoggingConfigurationResponse
--
--         , responseCreateRuleGroup $
--             newCreateRuleGroupResponse
--
--         , responseDeleteRegexMatchSet $
--             newDeleteRegexMatchSetResponse
--
--         , responseUpdateRegexMatchSet $
--             newUpdateRegexMatchSetResponse
--
--         , responseGetIPSet $
--             newGetIPSetResponse
--
--         , responseGetWebACL $
--             newGetWebACLResponse
--
--         , responseGetRule $
--             newGetRuleResponse
--
--         , responseDeleteXssMatchSet $
--             newDeleteXssMatchSetResponse
--
--         , responseUpdateXssMatchSet $
--             newUpdateXssMatchSetResponse
--
--         , responseCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStackResponse
--
--         , responseListXssMatchSets $
--             newListXssMatchSetsResponse
--
--         , responseCreateGeoMatchSet $
--             newCreateGeoMatchSetResponse
--
--         , responseGetChangeToken $
--             newGetChangeTokenResponse
--
--         , responseListSizeConstraintSets $
--             newListSizeConstraintSetsResponse
--
--         , responseListResourcesForWebACL $
--             newListResourcesForWebACLResponse
--
--         , responseGetSampledRequests $
--             newGetSampledRequestsResponse
--
--         , responseGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSetResponse
--
--         , responseGetWebACLForResource $
--             newGetWebACLForResourceResponse
--
--         , responseDisassociateWebACL $
--             newDisassociateWebACLResponse
--
--         , responseListSubscribedRuleGroups $
--             newListSubscribedRuleGroupsResponse
--
--         , responseCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSetResponse
--
--         , responseGetXssMatchSet $
--             newGetXssMatchSetResponse
--
--         , responseCreateByteMatchSet $
--             newCreateByteMatchSetResponse
--
--         , responseUpdateByteMatchSet $
--             newUpdateByteMatchSetResponse
--
--         , responseDeleteByteMatchSet $
--             newDeleteByteMatchSetResponse
--
--         , responsePutPermissionPolicy $
--             newPutPermissionPolicyResponse
--
--         , responseListLoggingConfigurations $
--             newListLoggingConfigurationsResponse
--
--         , responseGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeysResponse
--
--         , responseAssociateWebACL $
--             newAssociateWebACLResponse
--
--         , responseDeletePermissionPolicy $
--             newDeletePermissionPolicyResponse
--
--         , responseGetRegexMatchSet $
--             newGetRegexMatchSetResponse
--
--         , responseDeleteIPSet $
--             newDeleteIPSetResponse
--
--         , responseUpdateIPSet $
--             newUpdateIPSetResponse
--
--         , responseListIPSets $
--             newListIPSetsResponse
--
--         , responseListRegexMatchSets $
--             newListRegexMatchSetsResponse
--
--         , responseCreateXssMatchSet $
--             newCreateXssMatchSetResponse
--
--         , responseDeleteGeoMatchSet $
--             newDeleteGeoMatchSetResponse
--
--         , responseUpdateGeoMatchSet $
--             newUpdateGeoMatchSetResponse
--
--         , responseGetByteMatchSet $
--             newGetByteMatchSetResponse
--
--         , responseGetPermissionPolicy $
--             newGetPermissionPolicyResponse
--
--         , responseListRuleGroups $
--             newListRuleGroupsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteRuleGroup $
--             newDeleteRuleGroupResponse
--
--         , responseUpdateRuleGroup $
--             newUpdateRuleGroupResponse
--
--         , responseCreateRegexMatchSet $
--             newCreateRegexMatchSetResponse
--
--         , responseGetRateBasedRule $
--             newGetRateBasedRuleResponse
--
--         , responseCreateRegexPatternSet $
--             newCreateRegexPatternSetResponse
--
--         , responseDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSetResponse
--
--         , responseUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSetResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteRegexPatternSet $
--             newDeleteRegexPatternSetResponse
--
--         , responseUpdateRegexPatternSet $
--             newUpdateRegexPatternSetResponse
--
--         , responseCreateSizeConstraintSet $
--             newCreateSizeConstraintSetResponse
--
--         , responseListRegexPatternSets $
--             newListRegexPatternSetsResponse
--
--         , responseListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSetsResponse
--
--         , responseGetRegexPatternSet $
--             newGetRegexPatternSetResponse
--
--         , responseCreateRateBasedRule $
--             newCreateRateBasedRuleResponse
--
--         , responseDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSetResponse
--
--         , responseUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSetResponse
--
--           ]
--     ]

-- Requests

requestListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroup -> TestTree
requestListActivatedRulesInRuleGroup =
  req
    "ListActivatedRulesInRuleGroup"
    "fixture/ListActivatedRulesInRuleGroup.yaml"

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

requestUpdateRule :: UpdateRule -> TestTree
requestUpdateRule =
  req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet =
  req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestGetRuleGroup :: GetRuleGroup -> TestTree
requestGetRuleGroup =
  req
    "GetRuleGroup"
    "fixture/GetRuleGroup.yaml"

requestGetChangeTokenStatus :: GetChangeTokenStatus -> TestTree
requestGetChangeTokenStatus =
  req
    "GetChangeTokenStatus"
    "fixture/GetChangeTokenStatus.yaml"

requestDeleteWebACL :: DeleteWebACL -> TestTree
requestDeleteWebACL =
  req
    "DeleteWebACL"
    "fixture/DeleteWebACL.yaml"

requestUpdateWebACL :: UpdateWebACL -> TestTree
requestUpdateWebACL =
  req
    "UpdateWebACL"
    "fixture/UpdateWebACL.yaml"

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

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestDeleteLoggingConfiguration :: DeleteLoggingConfiguration -> TestTree
requestDeleteLoggingConfiguration =
  req
    "DeleteLoggingConfiguration"
    "fixture/DeleteLoggingConfiguration.yaml"

requestCreateWebACL :: CreateWebACL -> TestTree
requestCreateWebACL =
  req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

requestGetGeoMatchSet :: GetGeoMatchSet -> TestTree
requestGetGeoMatchSet =
  req
    "GetGeoMatchSet"
    "fixture/GetGeoMatchSet.yaml"

requestPutLoggingConfiguration :: PutLoggingConfiguration -> TestTree
requestPutLoggingConfiguration =
  req
    "PutLoggingConfiguration"
    "fixture/PutLoggingConfiguration.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListByteMatchSets :: ListByteMatchSets -> TestTree
requestListByteMatchSets =
  req
    "ListByteMatchSets"
    "fixture/ListByteMatchSets.yaml"

requestListGeoMatchSets :: ListGeoMatchSets -> TestTree
requestListGeoMatchSets =
  req
    "ListGeoMatchSets"
    "fixture/ListGeoMatchSets.yaml"

requestGetLoggingConfiguration :: GetLoggingConfiguration -> TestTree
requestGetLoggingConfiguration =
  req
    "GetLoggingConfiguration"
    "fixture/GetLoggingConfiguration.yaml"

requestCreateRuleGroup :: CreateRuleGroup -> TestTree
requestCreateRuleGroup =
  req
    "CreateRuleGroup"
    "fixture/CreateRuleGroup.yaml"

requestDeleteRegexMatchSet :: DeleteRegexMatchSet -> TestTree
requestDeleteRegexMatchSet =
  req
    "DeleteRegexMatchSet"
    "fixture/DeleteRegexMatchSet.yaml"

requestUpdateRegexMatchSet :: UpdateRegexMatchSet -> TestTree
requestUpdateRegexMatchSet =
  req
    "UpdateRegexMatchSet"
    "fixture/UpdateRegexMatchSet.yaml"

requestGetIPSet :: GetIPSet -> TestTree
requestGetIPSet =
  req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

requestGetWebACL :: GetWebACL -> TestTree
requestGetWebACL =
  req
    "GetWebACL"
    "fixture/GetWebACL.yaml"

requestGetRule :: GetRule -> TestTree
requestGetRule =
  req
    "GetRule"
    "fixture/GetRule.yaml"

requestDeleteXssMatchSet :: DeleteXssMatchSet -> TestTree
requestDeleteXssMatchSet =
  req
    "DeleteXssMatchSet"
    "fixture/DeleteXssMatchSet.yaml"

requestUpdateXssMatchSet :: UpdateXssMatchSet -> TestTree
requestUpdateXssMatchSet =
  req
    "UpdateXssMatchSet"
    "fixture/UpdateXssMatchSet.yaml"

requestCreateWebACLMigrationStack :: CreateWebACLMigrationStack -> TestTree
requestCreateWebACLMigrationStack =
  req
    "CreateWebACLMigrationStack"
    "fixture/CreateWebACLMigrationStack.yaml"

requestListXssMatchSets :: ListXssMatchSets -> TestTree
requestListXssMatchSets =
  req
    "ListXssMatchSets"
    "fixture/ListXssMatchSets.yaml"

requestCreateGeoMatchSet :: CreateGeoMatchSet -> TestTree
requestCreateGeoMatchSet =
  req
    "CreateGeoMatchSet"
    "fixture/CreateGeoMatchSet.yaml"

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

requestListResourcesForWebACL :: ListResourcesForWebACL -> TestTree
requestListResourcesForWebACL =
  req
    "ListResourcesForWebACL"
    "fixture/ListResourcesForWebACL.yaml"

requestGetSampledRequests :: GetSampledRequests -> TestTree
requestGetSampledRequests =
  req
    "GetSampledRequests"
    "fixture/GetSampledRequests.yaml"

requestGetSqlInjectionMatchSet :: GetSqlInjectionMatchSet -> TestTree
requestGetSqlInjectionMatchSet =
  req
    "GetSqlInjectionMatchSet"
    "fixture/GetSqlInjectionMatchSet.yaml"

requestGetWebACLForResource :: GetWebACLForResource -> TestTree
requestGetWebACLForResource =
  req
    "GetWebACLForResource"
    "fixture/GetWebACLForResource.yaml"

requestDisassociateWebACL :: DisassociateWebACL -> TestTree
requestDisassociateWebACL =
  req
    "DisassociateWebACL"
    "fixture/DisassociateWebACL.yaml"

requestListSubscribedRuleGroups :: ListSubscribedRuleGroups -> TestTree
requestListSubscribedRuleGroups =
  req
    "ListSubscribedRuleGroups"
    "fixture/ListSubscribedRuleGroups.yaml"

requestCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSet -> TestTree
requestCreateSqlInjectionMatchSet =
  req
    "CreateSqlInjectionMatchSet"
    "fixture/CreateSqlInjectionMatchSet.yaml"

requestGetXssMatchSet :: GetXssMatchSet -> TestTree
requestGetXssMatchSet =
  req
    "GetXssMatchSet"
    "fixture/GetXssMatchSet.yaml"

requestCreateByteMatchSet :: CreateByteMatchSet -> TestTree
requestCreateByteMatchSet =
  req
    "CreateByteMatchSet"
    "fixture/CreateByteMatchSet.yaml"

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

requestPutPermissionPolicy :: PutPermissionPolicy -> TestTree
requestPutPermissionPolicy =
  req
    "PutPermissionPolicy"
    "fixture/PutPermissionPolicy.yaml"

requestListLoggingConfigurations :: ListLoggingConfigurations -> TestTree
requestListLoggingConfigurations =
  req
    "ListLoggingConfigurations"
    "fixture/ListLoggingConfigurations.yaml"

requestGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeys -> TestTree
requestGetRateBasedRuleManagedKeys =
  req
    "GetRateBasedRuleManagedKeys"
    "fixture/GetRateBasedRuleManagedKeys.yaml"

requestAssociateWebACL :: AssociateWebACL -> TestTree
requestAssociateWebACL =
  req
    "AssociateWebACL"
    "fixture/AssociateWebACL.yaml"

requestDeletePermissionPolicy :: DeletePermissionPolicy -> TestTree
requestDeletePermissionPolicy =
  req
    "DeletePermissionPolicy"
    "fixture/DeletePermissionPolicy.yaml"

requestGetRegexMatchSet :: GetRegexMatchSet -> TestTree
requestGetRegexMatchSet =
  req
    "GetRegexMatchSet"
    "fixture/GetRegexMatchSet.yaml"

requestDeleteIPSet :: DeleteIPSet -> TestTree
requestDeleteIPSet =
  req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet =
  req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets =
  req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestListRegexMatchSets :: ListRegexMatchSets -> TestTree
requestListRegexMatchSets =
  req
    "ListRegexMatchSets"
    "fixture/ListRegexMatchSets.yaml"

requestCreateXssMatchSet :: CreateXssMatchSet -> TestTree
requestCreateXssMatchSet =
  req
    "CreateXssMatchSet"
    "fixture/CreateXssMatchSet.yaml"

requestDeleteGeoMatchSet :: DeleteGeoMatchSet -> TestTree
requestDeleteGeoMatchSet =
  req
    "DeleteGeoMatchSet"
    "fixture/DeleteGeoMatchSet.yaml"

requestUpdateGeoMatchSet :: UpdateGeoMatchSet -> TestTree
requestUpdateGeoMatchSet =
  req
    "UpdateGeoMatchSet"
    "fixture/UpdateGeoMatchSet.yaml"

requestGetByteMatchSet :: GetByteMatchSet -> TestTree
requestGetByteMatchSet =
  req
    "GetByteMatchSet"
    "fixture/GetByteMatchSet.yaml"

requestGetPermissionPolicy :: GetPermissionPolicy -> TestTree
requestGetPermissionPolicy =
  req
    "GetPermissionPolicy"
    "fixture/GetPermissionPolicy.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups =
  req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestCreateRegexMatchSet :: CreateRegexMatchSet -> TestTree
requestCreateRegexMatchSet =
  req
    "CreateRegexMatchSet"
    "fixture/CreateRegexMatchSet.yaml"

requestGetRateBasedRule :: GetRateBasedRule -> TestTree
requestGetRateBasedRule =
  req
    "GetRateBasedRule"
    "fixture/GetRateBasedRule.yaml"

requestCreateRegexPatternSet :: CreateRegexPatternSet -> TestTree
requestCreateRegexPatternSet =
  req
    "CreateRegexPatternSet"
    "fixture/CreateRegexPatternSet.yaml"

requestDeleteSizeConstraintSet :: DeleteSizeConstraintSet -> TestTree
requestDeleteSizeConstraintSet =
  req
    "DeleteSizeConstraintSet"
    "fixture/DeleteSizeConstraintSet.yaml"

requestUpdateSizeConstraintSet :: UpdateSizeConstraintSet -> TestTree
requestUpdateSizeConstraintSet =
  req
    "UpdateSizeConstraintSet"
    "fixture/UpdateSizeConstraintSet.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteRegexPatternSet :: DeleteRegexPatternSet -> TestTree
requestDeleteRegexPatternSet =
  req
    "DeleteRegexPatternSet"
    "fixture/DeleteRegexPatternSet.yaml"

requestUpdateRegexPatternSet :: UpdateRegexPatternSet -> TestTree
requestUpdateRegexPatternSet =
  req
    "UpdateRegexPatternSet"
    "fixture/UpdateRegexPatternSet.yaml"

requestCreateSizeConstraintSet :: CreateSizeConstraintSet -> TestTree
requestCreateSizeConstraintSet =
  req
    "CreateSizeConstraintSet"
    "fixture/CreateSizeConstraintSet.yaml"

requestListRegexPatternSets :: ListRegexPatternSets -> TestTree
requestListRegexPatternSets =
  req
    "ListRegexPatternSets"
    "fixture/ListRegexPatternSets.yaml"

requestListSqlInjectionMatchSets :: ListSqlInjectionMatchSets -> TestTree
requestListSqlInjectionMatchSets =
  req
    "ListSqlInjectionMatchSets"
    "fixture/ListSqlInjectionMatchSets.yaml"

requestGetRegexPatternSet :: GetRegexPatternSet -> TestTree
requestGetRegexPatternSet =
  req
    "GetRegexPatternSet"
    "fixture/GetRegexPatternSet.yaml"

requestCreateRateBasedRule :: CreateRateBasedRule -> TestTree
requestCreateRateBasedRule =
  req
    "CreateRateBasedRule"
    "fixture/CreateRateBasedRule.yaml"

requestDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSet -> TestTree
requestDeleteSqlInjectionMatchSet =
  req
    "DeleteSqlInjectionMatchSet"
    "fixture/DeleteSqlInjectionMatchSet.yaml"

requestUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSet -> TestTree
requestUpdateSqlInjectionMatchSet =
  req
    "UpdateSqlInjectionMatchSet"
    "fixture/UpdateSqlInjectionMatchSet.yaml"

-- Responses

responseListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroupResponse -> TestTree
responseListActivatedRulesInRuleGroup =
  res
    "ListActivatedRulesInRuleGroupResponse"
    "fixture/ListActivatedRulesInRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActivatedRulesInRuleGroup)

responseListRateBasedRules :: ListRateBasedRulesResponse -> TestTree
responseListRateBasedRules =
  res
    "ListRateBasedRulesResponse"
    "fixture/ListRateBasedRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRateBasedRules)

responseGetSizeConstraintSet :: GetSizeConstraintSetResponse -> TestTree
responseGetSizeConstraintSet =
  res
    "GetSizeConstraintSetResponse"
    "fixture/GetSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSizeConstraintSet)

responseDeleteRateBasedRule :: DeleteRateBasedRuleResponse -> TestTree
responseDeleteRateBasedRule =
  res
    "DeleteRateBasedRuleResponse"
    "fixture/DeleteRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRateBasedRule)

responseUpdateRateBasedRule :: UpdateRateBasedRuleResponse -> TestTree
responseUpdateRateBasedRule =
  res
    "UpdateRateBasedRuleResponse"
    "fixture/UpdateRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRateBasedRule)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule =
  res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRule)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet =
  res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIPSet)

responseGetRuleGroup :: GetRuleGroupResponse -> TestTree
responseGetRuleGroup =
  res
    "GetRuleGroupResponse"
    "fixture/GetRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRuleGroup)

responseGetChangeTokenStatus :: GetChangeTokenStatusResponse -> TestTree
responseGetChangeTokenStatus =
  res
    "GetChangeTokenStatusResponse"
    "fixture/GetChangeTokenStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChangeTokenStatus)

responseDeleteWebACL :: DeleteWebACLResponse -> TestTree
responseDeleteWebACL =
  res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebACL)

responseUpdateWebACL :: UpdateWebACLResponse -> TestTree
responseUpdateWebACL =
  res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWebACL)

responseListWebACLs :: ListWebACLsResponse -> TestTree
responseListWebACLs =
  res
    "ListWebACLsResponse"
    "fixture/ListWebACLsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebACLs)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseDeleteLoggingConfiguration :: DeleteLoggingConfigurationResponse -> TestTree
responseDeleteLoggingConfiguration =
  res
    "DeleteLoggingConfigurationResponse"
    "fixture/DeleteLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoggingConfiguration)

responseCreateWebACL :: CreateWebACLResponse -> TestTree
responseCreateWebACL =
  res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebACL)

responseGetGeoMatchSet :: GetGeoMatchSetResponse -> TestTree
responseGetGeoMatchSet =
  res
    "GetGeoMatchSetResponse"
    "fixture/GetGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeoMatchSet)

responsePutLoggingConfiguration :: PutLoggingConfigurationResponse -> TestTree
responsePutLoggingConfiguration =
  res
    "PutLoggingConfigurationResponse"
    "fixture/PutLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingConfiguration)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListByteMatchSets :: ListByteMatchSetsResponse -> TestTree
responseListByteMatchSets =
  res
    "ListByteMatchSetsResponse"
    "fixture/ListByteMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListByteMatchSets)

responseListGeoMatchSets :: ListGeoMatchSetsResponse -> TestTree
responseListGeoMatchSets =
  res
    "ListGeoMatchSetsResponse"
    "fixture/ListGeoMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeoMatchSets)

responseGetLoggingConfiguration :: GetLoggingConfigurationResponse -> TestTree
responseGetLoggingConfiguration =
  res
    "GetLoggingConfigurationResponse"
    "fixture/GetLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggingConfiguration)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup =
  res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleGroup)

responseDeleteRegexMatchSet :: DeleteRegexMatchSetResponse -> TestTree
responseDeleteRegexMatchSet =
  res
    "DeleteRegexMatchSetResponse"
    "fixture/DeleteRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegexMatchSet)

responseUpdateRegexMatchSet :: UpdateRegexMatchSetResponse -> TestTree
responseUpdateRegexMatchSet =
  res
    "UpdateRegexMatchSetResponse"
    "fixture/UpdateRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegexMatchSet)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet =
  res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIPSet)

responseGetWebACL :: GetWebACLResponse -> TestTree
responseGetWebACL =
  res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWebACL)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule =
  res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRule)

responseDeleteXssMatchSet :: DeleteXssMatchSetResponse -> TestTree
responseDeleteXssMatchSet =
  res
    "DeleteXssMatchSetResponse"
    "fixture/DeleteXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteXssMatchSet)

responseUpdateXssMatchSet :: UpdateXssMatchSetResponse -> TestTree
responseUpdateXssMatchSet =
  res
    "UpdateXssMatchSetResponse"
    "fixture/UpdateXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateXssMatchSet)

responseCreateWebACLMigrationStack :: CreateWebACLMigrationStackResponse -> TestTree
responseCreateWebACLMigrationStack =
  res
    "CreateWebACLMigrationStackResponse"
    "fixture/CreateWebACLMigrationStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebACLMigrationStack)

responseListXssMatchSets :: ListXssMatchSetsResponse -> TestTree
responseListXssMatchSets =
  res
    "ListXssMatchSetsResponse"
    "fixture/ListXssMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListXssMatchSets)

responseCreateGeoMatchSet :: CreateGeoMatchSetResponse -> TestTree
responseCreateGeoMatchSet =
  res
    "CreateGeoMatchSetResponse"
    "fixture/CreateGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGeoMatchSet)

responseGetChangeToken :: GetChangeTokenResponse -> TestTree
responseGetChangeToken =
  res
    "GetChangeTokenResponse"
    "fixture/GetChangeTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChangeToken)

responseListSizeConstraintSets :: ListSizeConstraintSetsResponse -> TestTree
responseListSizeConstraintSets =
  res
    "ListSizeConstraintSetsResponse"
    "fixture/ListSizeConstraintSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSizeConstraintSets)

responseListResourcesForWebACL :: ListResourcesForWebACLResponse -> TestTree
responseListResourcesForWebACL =
  res
    "ListResourcesForWebACLResponse"
    "fixture/ListResourcesForWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcesForWebACL)

responseGetSampledRequests :: GetSampledRequestsResponse -> TestTree
responseGetSampledRequests =
  res
    "GetSampledRequestsResponse"
    "fixture/GetSampledRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSampledRequests)

responseGetSqlInjectionMatchSet :: GetSqlInjectionMatchSetResponse -> TestTree
responseGetSqlInjectionMatchSet =
  res
    "GetSqlInjectionMatchSetResponse"
    "fixture/GetSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSqlInjectionMatchSet)

responseGetWebACLForResource :: GetWebACLForResourceResponse -> TestTree
responseGetWebACLForResource =
  res
    "GetWebACLForResourceResponse"
    "fixture/GetWebACLForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWebACLForResource)

responseDisassociateWebACL :: DisassociateWebACLResponse -> TestTree
responseDisassociateWebACL =
  res
    "DisassociateWebACLResponse"
    "fixture/DisassociateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWebACL)

responseListSubscribedRuleGroups :: ListSubscribedRuleGroupsResponse -> TestTree
responseListSubscribedRuleGroups =
  res
    "ListSubscribedRuleGroupsResponse"
    "fixture/ListSubscribedRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscribedRuleGroups)

responseCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSetResponse -> TestTree
responseCreateSqlInjectionMatchSet =
  res
    "CreateSqlInjectionMatchSetResponse"
    "fixture/CreateSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSqlInjectionMatchSet)

responseGetXssMatchSet :: GetXssMatchSetResponse -> TestTree
responseGetXssMatchSet =
  res
    "GetXssMatchSetResponse"
    "fixture/GetXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetXssMatchSet)

responseCreateByteMatchSet :: CreateByteMatchSetResponse -> TestTree
responseCreateByteMatchSet =
  res
    "CreateByteMatchSetResponse"
    "fixture/CreateByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateByteMatchSet)

responseUpdateByteMatchSet :: UpdateByteMatchSetResponse -> TestTree
responseUpdateByteMatchSet =
  res
    "UpdateByteMatchSetResponse"
    "fixture/UpdateByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateByteMatchSet)

responseDeleteByteMatchSet :: DeleteByteMatchSetResponse -> TestTree
responseDeleteByteMatchSet =
  res
    "DeleteByteMatchSetResponse"
    "fixture/DeleteByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteByteMatchSet)

responsePutPermissionPolicy :: PutPermissionPolicyResponse -> TestTree
responsePutPermissionPolicy =
  res
    "PutPermissionPolicyResponse"
    "fixture/PutPermissionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPermissionPolicy)

responseListLoggingConfigurations :: ListLoggingConfigurationsResponse -> TestTree
responseListLoggingConfigurations =
  res
    "ListLoggingConfigurationsResponse"
    "fixture/ListLoggingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLoggingConfigurations)

responseGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeysResponse -> TestTree
responseGetRateBasedRuleManagedKeys =
  res
    "GetRateBasedRuleManagedKeysResponse"
    "fixture/GetRateBasedRuleManagedKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRateBasedRuleManagedKeys)

responseAssociateWebACL :: AssociateWebACLResponse -> TestTree
responseAssociateWebACL =
  res
    "AssociateWebACLResponse"
    "fixture/AssociateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWebACL)

responseDeletePermissionPolicy :: DeletePermissionPolicyResponse -> TestTree
responseDeletePermissionPolicy =
  res
    "DeletePermissionPolicyResponse"
    "fixture/DeletePermissionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermissionPolicy)

responseGetRegexMatchSet :: GetRegexMatchSetResponse -> TestTree
responseGetRegexMatchSet =
  res
    "GetRegexMatchSetResponse"
    "fixture/GetRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegexMatchSet)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet =
  res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIPSet)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet =
  res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIPSet)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets =
  res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIPSets)

responseListRegexMatchSets :: ListRegexMatchSetsResponse -> TestTree
responseListRegexMatchSets =
  res
    "ListRegexMatchSetsResponse"
    "fixture/ListRegexMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegexMatchSets)

responseCreateXssMatchSet :: CreateXssMatchSetResponse -> TestTree
responseCreateXssMatchSet =
  res
    "CreateXssMatchSetResponse"
    "fixture/CreateXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateXssMatchSet)

responseDeleteGeoMatchSet :: DeleteGeoMatchSetResponse -> TestTree
responseDeleteGeoMatchSet =
  res
    "DeleteGeoMatchSetResponse"
    "fixture/DeleteGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGeoMatchSet)

responseUpdateGeoMatchSet :: UpdateGeoMatchSetResponse -> TestTree
responseUpdateGeoMatchSet =
  res
    "UpdateGeoMatchSetResponse"
    "fixture/UpdateGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGeoMatchSet)

responseGetByteMatchSet :: GetByteMatchSetResponse -> TestTree
responseGetByteMatchSet =
  res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetByteMatchSet)

responseGetPermissionPolicy :: GetPermissionPolicyResponse -> TestTree
responseGetPermissionPolicy =
  res
    "GetPermissionPolicyResponse"
    "fixture/GetPermissionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPermissionPolicy)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups =
  res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleGroups)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup =
  res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleGroup)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup =
  res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleGroup)

responseCreateRegexMatchSet :: CreateRegexMatchSetResponse -> TestTree
responseCreateRegexMatchSet =
  res
    "CreateRegexMatchSetResponse"
    "fixture/CreateRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegexMatchSet)

responseGetRateBasedRule :: GetRateBasedRuleResponse -> TestTree
responseGetRateBasedRule =
  res
    "GetRateBasedRuleResponse"
    "fixture/GetRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRateBasedRule)

responseCreateRegexPatternSet :: CreateRegexPatternSetResponse -> TestTree
responseCreateRegexPatternSet =
  res
    "CreateRegexPatternSetResponse"
    "fixture/CreateRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegexPatternSet)

responseDeleteSizeConstraintSet :: DeleteSizeConstraintSetResponse -> TestTree
responseDeleteSizeConstraintSet =
  res
    "DeleteSizeConstraintSetResponse"
    "fixture/DeleteSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSizeConstraintSet)

responseUpdateSizeConstraintSet :: UpdateSizeConstraintSetResponse -> TestTree
responseUpdateSizeConstraintSet =
  res
    "UpdateSizeConstraintSetResponse"
    "fixture/UpdateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSizeConstraintSet)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteRegexPatternSet :: DeleteRegexPatternSetResponse -> TestTree
responseDeleteRegexPatternSet =
  res
    "DeleteRegexPatternSetResponse"
    "fixture/DeleteRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegexPatternSet)

responseUpdateRegexPatternSet :: UpdateRegexPatternSetResponse -> TestTree
responseUpdateRegexPatternSet =
  res
    "UpdateRegexPatternSetResponse"
    "fixture/UpdateRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegexPatternSet)

responseCreateSizeConstraintSet :: CreateSizeConstraintSetResponse -> TestTree
responseCreateSizeConstraintSet =
  res
    "CreateSizeConstraintSetResponse"
    "fixture/CreateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSizeConstraintSet)

responseListRegexPatternSets :: ListRegexPatternSetsResponse -> TestTree
responseListRegexPatternSets =
  res
    "ListRegexPatternSetsResponse"
    "fixture/ListRegexPatternSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegexPatternSets)

responseListSqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> TestTree
responseListSqlInjectionMatchSets =
  res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSqlInjectionMatchSets)

responseGetRegexPatternSet :: GetRegexPatternSetResponse -> TestTree
responseGetRegexPatternSet =
  res
    "GetRegexPatternSetResponse"
    "fixture/GetRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegexPatternSet)

responseCreateRateBasedRule :: CreateRateBasedRuleResponse -> TestTree
responseCreateRateBasedRule =
  res
    "CreateRateBasedRuleResponse"
    "fixture/CreateRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRateBasedRule)

responseDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSetResponse -> TestTree
responseDeleteSqlInjectionMatchSet =
  res
    "DeleteSqlInjectionMatchSetResponse"
    "fixture/DeleteSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSqlInjectionMatchSet)

responseUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSetResponse -> TestTree
responseUpdateSqlInjectionMatchSet =
  res
    "UpdateSqlInjectionMatchSetResponse"
    "fixture/UpdateSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSqlInjectionMatchSet)

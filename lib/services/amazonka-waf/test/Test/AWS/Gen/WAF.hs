{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WAF
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.WAF where

import Data.Proxy
import Network.AWS.WAF
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WAF.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetChangeTokenStatus $
--             newGetChangeTokenStatus
--
--         , requestUpdateRule $
--             newUpdateRule
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestGetRuleGroup $
--             newGetRuleGroup
--
--         , requestDeleteWebACL $
--             newDeleteWebACL
--
--         , requestUpdateWebACL $
--             newUpdateWebACL
--
--         , requestGetSizeConstraintSet $
--             newGetSizeConstraintSet
--
--         , requestListRateBasedRules $
--             newListRateBasedRules
--
--         , requestCreateRateBasedRule $
--             newCreateRateBasedRule
--
--         , requestListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSets
--
--         , requestListRegexPatternSets $
--             newListRegexPatternSets
--
--         , requestGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSet
--
--         , requestListSizeConstraintSets $
--             newListSizeConstraintSets
--
--         , requestUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSet
--
--         , requestCreateRegexPatternSet $
--             newCreateRegexPatternSet
--
--         , requestDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSet
--
--         , requestGetChangeToken $
--             newGetChangeToken
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStack
--
--         , requestUpdateRuleGroup $
--             newUpdateRuleGroup
--
--         , requestListXssMatchSets $
--             newListXssMatchSets
--
--         , requestDeleteRuleGroup $
--             newDeleteRuleGroup
--
--         , requestCreateRegexMatchSet $
--             newCreateRegexMatchSet
--
--         , requestCreateRuleGroup $
--             newCreateRuleGroup
--
--         , requestDeleteRegexMatchSet $
--             newDeleteRegexMatchSet
--
--         , requestListRegexMatchSets $
--             newListRegexMatchSets
--
--         , requestGetLoggingConfiguration $
--             newGetLoggingConfiguration
--
--         , requestUpdateRegexMatchSet $
--             newUpdateRegexMatchSet
--
--         , requestDeleteLoggingConfiguration $
--             newDeleteLoggingConfiguration
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
--         , requestCreateWebACL $
--             newCreateWebACL
--
--         , requestPutPermissionPolicy $
--             newPutPermissionPolicy
--
--         , requestDeleteIPSet $
--             newDeleteIPSet
--
--         , requestGetGeoMatchSet $
--             newGetGeoMatchSet
--
--         , requestGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeys
--
--         , requestCreateIPSet $
--             newCreateIPSet
--
--         , requestListRules $
--             newListRules
--
--         , requestListWebACLs $
--             newListWebACLs
--
--         , requestCreateByteMatchSet $
--             newCreateByteMatchSet
--
--         , requestGetXssMatchSet $
--             newGetXssMatchSet
--
--         , requestUpdateRateBasedRule $
--             newUpdateRateBasedRule
--
--         , requestDeleteRateBasedRule $
--             newDeleteRateBasedRule
--
--         , requestListActivatedRulesInRuleGroup $
--             newListActivatedRulesInRuleGroup
--
--         , requestListSubscribedRuleGroups $
--             newListSubscribedRuleGroups
--
--         , requestCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSet
--
--         , requestDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSet
--
--         , requestGetRegexPatternSet $
--             newGetRegexPatternSet
--
--         , requestUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSet
--
--         , requestDeleteRegexPatternSet $
--             newDeleteRegexPatternSet
--
--         , requestCreateSizeConstraintSet $
--             newCreateSizeConstraintSet
--
--         , requestUpdateRegexPatternSet $
--             newUpdateRegexPatternSet
--
--         , requestGetSampledRequests $
--             newGetSampledRequests
--
--         , requestGetRateBasedRule $
--             newGetRateBasedRule
--
--         , requestDeleteXssMatchSet $
--             newDeleteXssMatchSet
--
--         , requestListRuleGroups $
--             newListRuleGroups
--
--         , requestGetWebACL $
--             newGetWebACL
--
--         , requestGetRule $
--             newGetRule
--
--         , requestCreateGeoMatchSet $
--             newCreateGeoMatchSet
--
--         , requestUpdateXssMatchSet $
--             newUpdateXssMatchSet
--
--         , requestGetIPSet $
--             newGetIPSet
--
--         , requestCreateXssMatchSet $
--             newCreateXssMatchSet
--
--         , requestGetByteMatchSet $
--             newGetByteMatchSet
--
--         , requestUpdateGeoMatchSet $
--             newUpdateGeoMatchSet
--
--         , requestListGeoMatchSets $
--             newListGeoMatchSets
--
--         , requestDeleteGeoMatchSet $
--             newDeleteGeoMatchSet
--
--         , requestGetPermissionPolicy $
--             newGetPermissionPolicy
--
--         , requestDeleteByteMatchSet $
--             newDeleteByteMatchSet
--
--         , requestDeletePermissionPolicy $
--             newDeletePermissionPolicy
--
--         , requestGetRegexMatchSet $
--             newGetRegexMatchSet
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestUpdateByteMatchSet $
--             newUpdateByteMatchSet
--
--         , requestPutLoggingConfiguration $
--             newPutLoggingConfiguration
--
--         , requestListIPSets $
--             newListIPSets
--
--         , requestListByteMatchSets $
--             newListByteMatchSets
--
--           ]

--     , testGroup "response"
--         [ responseGetChangeTokenStatus $
--             newGetChangeTokenStatusResponse
--
--         , responseUpdateRule $
--             newUpdateRuleResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseGetRuleGroup $
--             newGetRuleGroupResponse
--
--         , responseDeleteWebACL $
--             newDeleteWebACLResponse
--
--         , responseUpdateWebACL $
--             newUpdateWebACLResponse
--
--         , responseGetSizeConstraintSet $
--             newGetSizeConstraintSetResponse
--
--         , responseListRateBasedRules $
--             newListRateBasedRulesResponse
--
--         , responseCreateRateBasedRule $
--             newCreateRateBasedRuleResponse
--
--         , responseListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSetsResponse
--
--         , responseListRegexPatternSets $
--             newListRegexPatternSetsResponse
--
--         , responseGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSetResponse
--
--         , responseListSizeConstraintSets $
--             newListSizeConstraintSetsResponse
--
--         , responseUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSetResponse
--
--         , responseCreateRegexPatternSet $
--             newCreateRegexPatternSetResponse
--
--         , responseDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSetResponse
--
--         , responseGetChangeToken $
--             newGetChangeTokenResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStackResponse
--
--         , responseUpdateRuleGroup $
--             newUpdateRuleGroupResponse
--
--         , responseListXssMatchSets $
--             newListXssMatchSetsResponse
--
--         , responseDeleteRuleGroup $
--             newDeleteRuleGroupResponse
--
--         , responseCreateRegexMatchSet $
--             newCreateRegexMatchSetResponse
--
--         , responseCreateRuleGroup $
--             newCreateRuleGroupResponse
--
--         , responseDeleteRegexMatchSet $
--             newDeleteRegexMatchSetResponse
--
--         , responseListRegexMatchSets $
--             newListRegexMatchSetsResponse
--
--         , responseGetLoggingConfiguration $
--             newGetLoggingConfigurationResponse
--
--         , responseUpdateRegexMatchSet $
--             newUpdateRegexMatchSetResponse
--
--         , responseDeleteLoggingConfiguration $
--             newDeleteLoggingConfigurationResponse
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
--         , responseCreateWebACL $
--             newCreateWebACLResponse
--
--         , responsePutPermissionPolicy $
--             newPutPermissionPolicyResponse
--
--         , responseDeleteIPSet $
--             newDeleteIPSetResponse
--
--         , responseGetGeoMatchSet $
--             newGetGeoMatchSetResponse
--
--         , responseGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeysResponse
--
--         , responseCreateIPSet $
--             newCreateIPSetResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListWebACLs $
--             newListWebACLsResponse
--
--         , responseCreateByteMatchSet $
--             newCreateByteMatchSetResponse
--
--         , responseGetXssMatchSet $
--             newGetXssMatchSetResponse
--
--         , responseUpdateRateBasedRule $
--             newUpdateRateBasedRuleResponse
--
--         , responseDeleteRateBasedRule $
--             newDeleteRateBasedRuleResponse
--
--         , responseListActivatedRulesInRuleGroup $
--             newListActivatedRulesInRuleGroupResponse
--
--         , responseListSubscribedRuleGroups $
--             newListSubscribedRuleGroupsResponse
--
--         , responseCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSetResponse
--
--         , responseDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSetResponse
--
--         , responseGetRegexPatternSet $
--             newGetRegexPatternSetResponse
--
--         , responseUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSetResponse
--
--         , responseDeleteRegexPatternSet $
--             newDeleteRegexPatternSetResponse
--
--         , responseCreateSizeConstraintSet $
--             newCreateSizeConstraintSetResponse
--
--         , responseUpdateRegexPatternSet $
--             newUpdateRegexPatternSetResponse
--
--         , responseGetSampledRequests $
--             newGetSampledRequestsResponse
--
--         , responseGetRateBasedRule $
--             newGetRateBasedRuleResponse
--
--         , responseDeleteXssMatchSet $
--             newDeleteXssMatchSetResponse
--
--         , responseListRuleGroups $
--             newListRuleGroupsResponse
--
--         , responseGetWebACL $
--             newGetWebACLResponse
--
--         , responseGetRule $
--             newGetRuleResponse
--
--         , responseCreateGeoMatchSet $
--             newCreateGeoMatchSetResponse
--
--         , responseUpdateXssMatchSet $
--             newUpdateXssMatchSetResponse
--
--         , responseGetIPSet $
--             newGetIPSetResponse
--
--         , responseCreateXssMatchSet $
--             newCreateXssMatchSetResponse
--
--         , responseGetByteMatchSet $
--             newGetByteMatchSetResponse
--
--         , responseUpdateGeoMatchSet $
--             newUpdateGeoMatchSetResponse
--
--         , responseListGeoMatchSets $
--             newListGeoMatchSetsResponse
--
--         , responseDeleteGeoMatchSet $
--             newDeleteGeoMatchSetResponse
--
--         , responseGetPermissionPolicy $
--             newGetPermissionPolicyResponse
--
--         , responseDeleteByteMatchSet $
--             newDeleteByteMatchSetResponse
--
--         , responseDeletePermissionPolicy $
--             newDeletePermissionPolicyResponse
--
--         , responseGetRegexMatchSet $
--             newGetRegexMatchSetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseUpdateByteMatchSet $
--             newUpdateByteMatchSetResponse
--
--         , responsePutLoggingConfiguration $
--             newPutLoggingConfigurationResponse
--
--         , responseListIPSets $
--             newListIPSetsResponse
--
--         , responseListByteMatchSets $
--             newListByteMatchSetsResponse
--
--           ]
--     ]

-- Requests

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

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestGetRuleGroup :: GetRuleGroup -> TestTree
requestGetRuleGroup =
  req
    "GetRuleGroup"
    "fixture/GetRuleGroup.yaml"

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

requestGetSizeConstraintSet :: GetSizeConstraintSet -> TestTree
requestGetSizeConstraintSet =
  req
    "GetSizeConstraintSet"
    "fixture/GetSizeConstraintSet.yaml"

requestListRateBasedRules :: ListRateBasedRules -> TestTree
requestListRateBasedRules =
  req
    "ListRateBasedRules"
    "fixture/ListRateBasedRules.yaml"

requestCreateRateBasedRule :: CreateRateBasedRule -> TestTree
requestCreateRateBasedRule =
  req
    "CreateRateBasedRule"
    "fixture/CreateRateBasedRule.yaml"

requestListSqlInjectionMatchSets :: ListSqlInjectionMatchSets -> TestTree
requestListSqlInjectionMatchSets =
  req
    "ListSqlInjectionMatchSets"
    "fixture/ListSqlInjectionMatchSets.yaml"

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

requestListSizeConstraintSets :: ListSizeConstraintSets -> TestTree
requestListSizeConstraintSets =
  req
    "ListSizeConstraintSets"
    "fixture/ListSizeConstraintSets.yaml"

requestUpdateSizeConstraintSet :: UpdateSizeConstraintSet -> TestTree
requestUpdateSizeConstraintSet =
  req
    "UpdateSizeConstraintSet"
    "fixture/UpdateSizeConstraintSet.yaml"

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

requestGetChangeToken :: GetChangeToken -> TestTree
requestGetChangeToken =
  req
    "GetChangeToken"
    "fixture/GetChangeToken.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestUpdateRuleGroup :: UpdateRuleGroup -> TestTree
requestUpdateRuleGroup =
  req
    "UpdateRuleGroup"
    "fixture/UpdateRuleGroup.yaml"

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

requestDeleteRegexMatchSet :: DeleteRegexMatchSet -> TestTree
requestDeleteRegexMatchSet =
  req
    "DeleteRegexMatchSet"
    "fixture/DeleteRegexMatchSet.yaml"

requestListRegexMatchSets :: ListRegexMatchSets -> TestTree
requestListRegexMatchSets =
  req
    "ListRegexMatchSets"
    "fixture/ListRegexMatchSets.yaml"

requestGetLoggingConfiguration :: GetLoggingConfiguration -> TestTree
requestGetLoggingConfiguration =
  req
    "GetLoggingConfiguration"
    "fixture/GetLoggingConfiguration.yaml"

requestUpdateRegexMatchSet :: UpdateRegexMatchSet -> TestTree
requestUpdateRegexMatchSet =
  req
    "UpdateRegexMatchSet"
    "fixture/UpdateRegexMatchSet.yaml"

requestDeleteLoggingConfiguration :: DeleteLoggingConfiguration -> TestTree
requestDeleteLoggingConfiguration =
  req
    "DeleteLoggingConfiguration"
    "fixture/DeleteLoggingConfiguration.yaml"

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

requestCreateWebACL :: CreateWebACL -> TestTree
requestCreateWebACL =
  req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

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

requestGetGeoMatchSet :: GetGeoMatchSet -> TestTree
requestGetGeoMatchSet =
  req
    "GetGeoMatchSet"
    "fixture/GetGeoMatchSet.yaml"

requestGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeys -> TestTree
requestGetRateBasedRuleManagedKeys =
  req
    "GetRateBasedRuleManagedKeys"
    "fixture/GetRateBasedRuleManagedKeys.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet =
  req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestListWebACLs :: ListWebACLs -> TestTree
requestListWebACLs =
  req
    "ListWebACLs"
    "fixture/ListWebACLs.yaml"

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

requestUpdateRateBasedRule :: UpdateRateBasedRule -> TestTree
requestUpdateRateBasedRule =
  req
    "UpdateRateBasedRule"
    "fixture/UpdateRateBasedRule.yaml"

requestDeleteRateBasedRule :: DeleteRateBasedRule -> TestTree
requestDeleteRateBasedRule =
  req
    "DeleteRateBasedRule"
    "fixture/DeleteRateBasedRule.yaml"

requestListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroup -> TestTree
requestListActivatedRulesInRuleGroup =
  req
    "ListActivatedRulesInRuleGroup"
    "fixture/ListActivatedRulesInRuleGroup.yaml"

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

requestDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSet -> TestTree
requestDeleteSqlInjectionMatchSet =
  req
    "DeleteSqlInjectionMatchSet"
    "fixture/DeleteSqlInjectionMatchSet.yaml"

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

requestDeleteRegexPatternSet :: DeleteRegexPatternSet -> TestTree
requestDeleteRegexPatternSet =
  req
    "DeleteRegexPatternSet"
    "fixture/DeleteRegexPatternSet.yaml"

requestCreateSizeConstraintSet :: CreateSizeConstraintSet -> TestTree
requestCreateSizeConstraintSet =
  req
    "CreateSizeConstraintSet"
    "fixture/CreateSizeConstraintSet.yaml"

requestUpdateRegexPatternSet :: UpdateRegexPatternSet -> TestTree
requestUpdateRegexPatternSet =
  req
    "UpdateRegexPatternSet"
    "fixture/UpdateRegexPatternSet.yaml"

requestGetSampledRequests :: GetSampledRequests -> TestTree
requestGetSampledRequests =
  req
    "GetSampledRequests"
    "fixture/GetSampledRequests.yaml"

requestGetRateBasedRule :: GetRateBasedRule -> TestTree
requestGetRateBasedRule =
  req
    "GetRateBasedRule"
    "fixture/GetRateBasedRule.yaml"

requestDeleteXssMatchSet :: DeleteXssMatchSet -> TestTree
requestDeleteXssMatchSet =
  req
    "DeleteXssMatchSet"
    "fixture/DeleteXssMatchSet.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups =
  req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

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

requestCreateGeoMatchSet :: CreateGeoMatchSet -> TestTree
requestCreateGeoMatchSet =
  req
    "CreateGeoMatchSet"
    "fixture/CreateGeoMatchSet.yaml"

requestUpdateXssMatchSet :: UpdateXssMatchSet -> TestTree
requestUpdateXssMatchSet =
  req
    "UpdateXssMatchSet"
    "fixture/UpdateXssMatchSet.yaml"

requestGetIPSet :: GetIPSet -> TestTree
requestGetIPSet =
  req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

requestCreateXssMatchSet :: CreateXssMatchSet -> TestTree
requestCreateXssMatchSet =
  req
    "CreateXssMatchSet"
    "fixture/CreateXssMatchSet.yaml"

requestGetByteMatchSet :: GetByteMatchSet -> TestTree
requestGetByteMatchSet =
  req
    "GetByteMatchSet"
    "fixture/GetByteMatchSet.yaml"

requestUpdateGeoMatchSet :: UpdateGeoMatchSet -> TestTree
requestUpdateGeoMatchSet =
  req
    "UpdateGeoMatchSet"
    "fixture/UpdateGeoMatchSet.yaml"

requestListGeoMatchSets :: ListGeoMatchSets -> TestTree
requestListGeoMatchSets =
  req
    "ListGeoMatchSets"
    "fixture/ListGeoMatchSets.yaml"

requestDeleteGeoMatchSet :: DeleteGeoMatchSet -> TestTree
requestDeleteGeoMatchSet =
  req
    "DeleteGeoMatchSet"
    "fixture/DeleteGeoMatchSet.yaml"

requestGetPermissionPolicy :: GetPermissionPolicy -> TestTree
requestGetPermissionPolicy =
  req
    "GetPermissionPolicy"
    "fixture/GetPermissionPolicy.yaml"

requestDeleteByteMatchSet :: DeleteByteMatchSet -> TestTree
requestDeleteByteMatchSet =
  req
    "DeleteByteMatchSet"
    "fixture/DeleteByteMatchSet.yaml"

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

requestPutLoggingConfiguration :: PutLoggingConfiguration -> TestTree
requestPutLoggingConfiguration =
  req
    "PutLoggingConfiguration"
    "fixture/PutLoggingConfiguration.yaml"

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets =
  req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestListByteMatchSets :: ListByteMatchSets -> TestTree
requestListByteMatchSets =
  req
    "ListByteMatchSets"
    "fixture/ListByteMatchSets.yaml"

-- Responses

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

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRule)

responseGetRuleGroup :: GetRuleGroupResponse -> TestTree
responseGetRuleGroup =
  res
    "GetRuleGroupResponse"
    "fixture/GetRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetRuleGroup)

responseDeleteWebACL :: DeleteWebACLResponse -> TestTree
responseDeleteWebACL =
  res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteWebACL)

responseUpdateWebACL :: UpdateWebACLResponse -> TestTree
responseUpdateWebACL =
  res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateWebACL)

responseGetSizeConstraintSet :: GetSizeConstraintSetResponse -> TestTree
responseGetSizeConstraintSet =
  res
    "GetSizeConstraintSetResponse"
    "fixture/GetSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetSizeConstraintSet)

responseListRateBasedRules :: ListRateBasedRulesResponse -> TestTree
responseListRateBasedRules =
  res
    "ListRateBasedRulesResponse"
    "fixture/ListRateBasedRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRateBasedRules)

responseCreateRateBasedRule :: CreateRateBasedRuleResponse -> TestTree
responseCreateRateBasedRule =
  res
    "CreateRateBasedRuleResponse"
    "fixture/CreateRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRateBasedRule)

responseListSqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> TestTree
responseListSqlInjectionMatchSets =
  res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSqlInjectionMatchSets)

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

responseListSizeConstraintSets :: ListSizeConstraintSetsResponse -> TestTree
responseListSizeConstraintSets =
  res
    "ListSizeConstraintSetsResponse"
    "fixture/ListSizeConstraintSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSizeConstraintSets)

responseUpdateSizeConstraintSet :: UpdateSizeConstraintSetResponse -> TestTree
responseUpdateSizeConstraintSet =
  res
    "UpdateSizeConstraintSetResponse"
    "fixture/UpdateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSizeConstraintSet)

responseCreateRegexPatternSet :: CreateRegexPatternSetResponse -> TestTree
responseCreateRegexPatternSet =
  res
    "CreateRegexPatternSetResponse"
    "fixture/CreateRegexPatternSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRegexPatternSet)

responseDeleteSizeConstraintSet :: DeleteSizeConstraintSetResponse -> TestTree
responseDeleteSizeConstraintSet =
  res
    "DeleteSizeConstraintSetResponse"
    "fixture/DeleteSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSizeConstraintSet)

responseGetChangeToken :: GetChangeTokenResponse -> TestTree
responseGetChangeToken =
  res
    "GetChangeTokenResponse"
    "fixture/GetChangeTokenResponse.proto"
    defaultService
    (Proxy :: Proxy GetChangeToken)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

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

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup =
  res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRuleGroup)

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

responseDeleteRegexMatchSet :: DeleteRegexMatchSetResponse -> TestTree
responseDeleteRegexMatchSet =
  res
    "DeleteRegexMatchSetResponse"
    "fixture/DeleteRegexMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegexMatchSet)

responseListRegexMatchSets :: ListRegexMatchSetsResponse -> TestTree
responseListRegexMatchSets =
  res
    "ListRegexMatchSetsResponse"
    "fixture/ListRegexMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRegexMatchSets)

responseGetLoggingConfiguration :: GetLoggingConfigurationResponse -> TestTree
responseGetLoggingConfiguration =
  res
    "GetLoggingConfigurationResponse"
    "fixture/GetLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoggingConfiguration)

responseUpdateRegexMatchSet :: UpdateRegexMatchSetResponse -> TestTree
responseUpdateRegexMatchSet =
  res
    "UpdateRegexMatchSetResponse"
    "fixture/UpdateRegexMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRegexMatchSet)

responseDeleteLoggingConfiguration :: DeleteLoggingConfigurationResponse -> TestTree
responseDeleteLoggingConfiguration =
  res
    "DeleteLoggingConfigurationResponse"
    "fixture/DeleteLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoggingConfiguration)

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

responseCreateWebACL :: CreateWebACLResponse -> TestTree
responseCreateWebACL =
  res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateWebACL)

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

responseGetGeoMatchSet :: GetGeoMatchSetResponse -> TestTree
responseGetGeoMatchSet =
  res
    "GetGeoMatchSetResponse"
    "fixture/GetGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetGeoMatchSet)

responseGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeysResponse -> TestTree
responseGetRateBasedRuleManagedKeys =
  res
    "GetRateBasedRuleManagedKeysResponse"
    "fixture/GetRateBasedRuleManagedKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetRateBasedRuleManagedKeys)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet =
  res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIPSet)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRules)

responseListWebACLs :: ListWebACLsResponse -> TestTree
responseListWebACLs =
  res
    "ListWebACLsResponse"
    "fixture/ListWebACLsResponse.proto"
    defaultService
    (Proxy :: Proxy ListWebACLs)

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

responseUpdateRateBasedRule :: UpdateRateBasedRuleResponse -> TestTree
responseUpdateRateBasedRule =
  res
    "UpdateRateBasedRuleResponse"
    "fixture/UpdateRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRateBasedRule)

responseDeleteRateBasedRule :: DeleteRateBasedRuleResponse -> TestTree
responseDeleteRateBasedRule =
  res
    "DeleteRateBasedRuleResponse"
    "fixture/DeleteRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRateBasedRule)

responseListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroupResponse -> TestTree
responseListActivatedRulesInRuleGroup =
  res
    "ListActivatedRulesInRuleGroupResponse"
    "fixture/ListActivatedRulesInRuleGroupResponse.proto"
    defaultService
    (Proxy :: Proxy ListActivatedRulesInRuleGroup)

responseListSubscribedRuleGroups :: ListSubscribedRuleGroupsResponse -> TestTree
responseListSubscribedRuleGroups =
  res
    "ListSubscribedRuleGroupsResponse"
    "fixture/ListSubscribedRuleGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscribedRuleGroups)

responseCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSetResponse -> TestTree
responseCreateSqlInjectionMatchSet =
  res
    "CreateSqlInjectionMatchSetResponse"
    "fixture/CreateSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSqlInjectionMatchSet)

responseDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSetResponse -> TestTree
responseDeleteSqlInjectionMatchSet =
  res
    "DeleteSqlInjectionMatchSetResponse"
    "fixture/DeleteSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSqlInjectionMatchSet)

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

responseDeleteRegexPatternSet :: DeleteRegexPatternSetResponse -> TestTree
responseDeleteRegexPatternSet =
  res
    "DeleteRegexPatternSetResponse"
    "fixture/DeleteRegexPatternSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRegexPatternSet)

responseCreateSizeConstraintSet :: CreateSizeConstraintSetResponse -> TestTree
responseCreateSizeConstraintSet =
  res
    "CreateSizeConstraintSetResponse"
    "fixture/CreateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSizeConstraintSet)

responseUpdateRegexPatternSet :: UpdateRegexPatternSetResponse -> TestTree
responseUpdateRegexPatternSet =
  res
    "UpdateRegexPatternSetResponse"
    "fixture/UpdateRegexPatternSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRegexPatternSet)

responseGetSampledRequests :: GetSampledRequestsResponse -> TestTree
responseGetSampledRequests =
  res
    "GetSampledRequestsResponse"
    "fixture/GetSampledRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSampledRequests)

responseGetRateBasedRule :: GetRateBasedRuleResponse -> TestTree
responseGetRateBasedRule =
  res
    "GetRateBasedRuleResponse"
    "fixture/GetRateBasedRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetRateBasedRule)

responseDeleteXssMatchSet :: DeleteXssMatchSetResponse -> TestTree
responseDeleteXssMatchSet =
  res
    "DeleteXssMatchSetResponse"
    "fixture/DeleteXssMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteXssMatchSet)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups =
  res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRuleGroups)

responseGetWebACL :: GetWebACLResponse -> TestTree
responseGetWebACL =
  res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    defaultService
    (Proxy :: Proxy GetWebACL)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule =
  res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetRule)

responseCreateGeoMatchSet :: CreateGeoMatchSetResponse -> TestTree
responseCreateGeoMatchSet =
  res
    "CreateGeoMatchSetResponse"
    "fixture/CreateGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGeoMatchSet)

responseUpdateXssMatchSet :: UpdateXssMatchSetResponse -> TestTree
responseUpdateXssMatchSet =
  res
    "UpdateXssMatchSetResponse"
    "fixture/UpdateXssMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateXssMatchSet)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet =
  res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetIPSet)

responseCreateXssMatchSet :: CreateXssMatchSetResponse -> TestTree
responseCreateXssMatchSet =
  res
    "CreateXssMatchSetResponse"
    "fixture/CreateXssMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateXssMatchSet)

responseGetByteMatchSet :: GetByteMatchSetResponse -> TestTree
responseGetByteMatchSet =
  res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetByteMatchSet)

responseUpdateGeoMatchSet :: UpdateGeoMatchSetResponse -> TestTree
responseUpdateGeoMatchSet =
  res
    "UpdateGeoMatchSetResponse"
    "fixture/UpdateGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGeoMatchSet)

responseListGeoMatchSets :: ListGeoMatchSetsResponse -> TestTree
responseListGeoMatchSets =
  res
    "ListGeoMatchSetsResponse"
    "fixture/ListGeoMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGeoMatchSets)

responseDeleteGeoMatchSet :: DeleteGeoMatchSetResponse -> TestTree
responseDeleteGeoMatchSet =
  res
    "DeleteGeoMatchSetResponse"
    "fixture/DeleteGeoMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGeoMatchSet)

responseGetPermissionPolicy :: GetPermissionPolicyResponse -> TestTree
responseGetPermissionPolicy =
  res
    "GetPermissionPolicyResponse"
    "fixture/GetPermissionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetPermissionPolicy)

responseDeleteByteMatchSet :: DeleteByteMatchSetResponse -> TestTree
responseDeleteByteMatchSet =
  res
    "DeleteByteMatchSetResponse"
    "fixture/DeleteByteMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteByteMatchSet)

responseDeletePermissionPolicy :: DeletePermissionPolicyResponse -> TestTree
responseDeletePermissionPolicy =
  res
    "DeletePermissionPolicyResponse"
    "fixture/DeletePermissionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePermissionPolicy)

responseGetRegexMatchSet :: GetRegexMatchSetResponse -> TestTree
responseGetRegexMatchSet =
  res
    "GetRegexMatchSetResponse"
    "fixture/GetRegexMatchSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegexMatchSet)

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

responsePutLoggingConfiguration :: PutLoggingConfigurationResponse -> TestTree
responsePutLoggingConfiguration =
  res
    "PutLoggingConfigurationResponse"
    "fixture/PutLoggingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy PutLoggingConfiguration)

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets =
  res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIPSets)

responseListByteMatchSets :: ListByteMatchSetsResponse -> TestTree
responseListByteMatchSets =
  res
    "ListByteMatchSetsResponse"
    "fixture/ListByteMatchSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListByteMatchSets)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WAF
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WAF where

import Amazonka.WAF
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WAF.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateByteMatchSet $
--             newCreateByteMatchSet
--
--         , requestCreateGeoMatchSet $
--             newCreateGeoMatchSet
--
--         , requestCreateIPSet $
--             newCreateIPSet
--
--         , requestCreateRateBasedRule $
--             newCreateRateBasedRule
--
--         , requestCreateRegexMatchSet $
--             newCreateRegexMatchSet
--
--         , requestCreateRegexPatternSet $
--             newCreateRegexPatternSet
--
--         , requestCreateRule $
--             newCreateRule
--
--         , requestCreateRuleGroup $
--             newCreateRuleGroup
--
--         , requestCreateSizeConstraintSet $
--             newCreateSizeConstraintSet
--
--         , requestCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSet
--
--         , requestCreateWebACL $
--             newCreateWebACL
--
--         , requestCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStack
--
--         , requestCreateXssMatchSet $
--             newCreateXssMatchSet
--
--         , requestDeleteByteMatchSet $
--             newDeleteByteMatchSet
--
--         , requestDeleteGeoMatchSet $
--             newDeleteGeoMatchSet
--
--         , requestDeleteIPSet $
--             newDeleteIPSet
--
--         , requestDeleteLoggingConfiguration $
--             newDeleteLoggingConfiguration
--
--         , requestDeletePermissionPolicy $
--             newDeletePermissionPolicy
--
--         , requestDeleteRateBasedRule $
--             newDeleteRateBasedRule
--
--         , requestDeleteRegexMatchSet $
--             newDeleteRegexMatchSet
--
--         , requestDeleteRegexPatternSet $
--             newDeleteRegexPatternSet
--
--         , requestDeleteRule $
--             newDeleteRule
--
--         , requestDeleteRuleGroup $
--             newDeleteRuleGroup
--
--         , requestDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSet
--
--         , requestDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSet
--
--         , requestDeleteWebACL $
--             newDeleteWebACL
--
--         , requestDeleteXssMatchSet $
--             newDeleteXssMatchSet
--
--         , requestGetByteMatchSet $
--             newGetByteMatchSet
--
--         , requestGetChangeToken $
--             newGetChangeToken
--
--         , requestGetChangeTokenStatus $
--             newGetChangeTokenStatus
--
--         , requestGetGeoMatchSet $
--             newGetGeoMatchSet
--
--         , requestGetIPSet $
--             newGetIPSet
--
--         , requestGetLoggingConfiguration $
--             newGetLoggingConfiguration
--
--         , requestGetPermissionPolicy $
--             newGetPermissionPolicy
--
--         , requestGetRateBasedRule $
--             newGetRateBasedRule
--
--         , requestGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeys
--
--         , requestGetRegexMatchSet $
--             newGetRegexMatchSet
--
--         , requestGetRegexPatternSet $
--             newGetRegexPatternSet
--
--         , requestGetRule $
--             newGetRule
--
--         , requestGetRuleGroup $
--             newGetRuleGroup
--
--         , requestGetSampledRequests $
--             newGetSampledRequests
--
--         , requestGetSizeConstraintSet $
--             newGetSizeConstraintSet
--
--         , requestGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSet
--
--         , requestGetWebACL $
--             newGetWebACL
--
--         , requestGetXssMatchSet $
--             newGetXssMatchSet
--
--         , requestListActivatedRulesInRuleGroup $
--             newListActivatedRulesInRuleGroup
--
--         , requestListByteMatchSets $
--             newListByteMatchSets
--
--         , requestListGeoMatchSets $
--             newListGeoMatchSets
--
--         , requestListIPSets $
--             newListIPSets
--
--         , requestListLoggingConfigurations $
--             newListLoggingConfigurations
--
--         , requestListRateBasedRules $
--             newListRateBasedRules
--
--         , requestListRegexMatchSets $
--             newListRegexMatchSets
--
--         , requestListRegexPatternSets $
--             newListRegexPatternSets
--
--         , requestListRuleGroups $
--             newListRuleGroups
--
--         , requestListRules $
--             newListRules
--
--         , requestListSizeConstraintSets $
--             newListSizeConstraintSets
--
--         , requestListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSets
--
--         , requestListSubscribedRuleGroups $
--             newListSubscribedRuleGroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWebACLs $
--             newListWebACLs
--
--         , requestListXssMatchSets $
--             newListXssMatchSets
--
--         , requestPutLoggingConfiguration $
--             newPutLoggingConfiguration
--
--         , requestPutPermissionPolicy $
--             newPutPermissionPolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateByteMatchSet $
--             newUpdateByteMatchSet
--
--         , requestUpdateGeoMatchSet $
--             newUpdateGeoMatchSet
--
--         , requestUpdateIPSet $
--             newUpdateIPSet
--
--         , requestUpdateRateBasedRule $
--             newUpdateRateBasedRule
--
--         , requestUpdateRegexMatchSet $
--             newUpdateRegexMatchSet
--
--         , requestUpdateRegexPatternSet $
--             newUpdateRegexPatternSet
--
--         , requestUpdateRule $
--             newUpdateRule
--
--         , requestUpdateRuleGroup $
--             newUpdateRuleGroup
--
--         , requestUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSet
--
--         , requestUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSet
--
--         , requestUpdateWebACL $
--             newUpdateWebACL
--
--         , requestUpdateXssMatchSet $
--             newUpdateXssMatchSet
--
--           ]

--     , testGroup "response"
--         [ responseCreateByteMatchSet $
--             newCreateByteMatchSetResponse
--
--         , responseCreateGeoMatchSet $
--             newCreateGeoMatchSetResponse
--
--         , responseCreateIPSet $
--             newCreateIPSetResponse
--
--         , responseCreateRateBasedRule $
--             newCreateRateBasedRuleResponse
--
--         , responseCreateRegexMatchSet $
--             newCreateRegexMatchSetResponse
--
--         , responseCreateRegexPatternSet $
--             newCreateRegexPatternSetResponse
--
--         , responseCreateRule $
--             newCreateRuleResponse
--
--         , responseCreateRuleGroup $
--             newCreateRuleGroupResponse
--
--         , responseCreateSizeConstraintSet $
--             newCreateSizeConstraintSetResponse
--
--         , responseCreateSqlInjectionMatchSet $
--             newCreateSqlInjectionMatchSetResponse
--
--         , responseCreateWebACL $
--             newCreateWebACLResponse
--
--         , responseCreateWebACLMigrationStack $
--             newCreateWebACLMigrationStackResponse
--
--         , responseCreateXssMatchSet $
--             newCreateXssMatchSetResponse
--
--         , responseDeleteByteMatchSet $
--             newDeleteByteMatchSetResponse
--
--         , responseDeleteGeoMatchSet $
--             newDeleteGeoMatchSetResponse
--
--         , responseDeleteIPSet $
--             newDeleteIPSetResponse
--
--         , responseDeleteLoggingConfiguration $
--             newDeleteLoggingConfigurationResponse
--
--         , responseDeletePermissionPolicy $
--             newDeletePermissionPolicyResponse
--
--         , responseDeleteRateBasedRule $
--             newDeleteRateBasedRuleResponse
--
--         , responseDeleteRegexMatchSet $
--             newDeleteRegexMatchSetResponse
--
--         , responseDeleteRegexPatternSet $
--             newDeleteRegexPatternSetResponse
--
--         , responseDeleteRule $
--             newDeleteRuleResponse
--
--         , responseDeleteRuleGroup $
--             newDeleteRuleGroupResponse
--
--         , responseDeleteSizeConstraintSet $
--             newDeleteSizeConstraintSetResponse
--
--         , responseDeleteSqlInjectionMatchSet $
--             newDeleteSqlInjectionMatchSetResponse
--
--         , responseDeleteWebACL $
--             newDeleteWebACLResponse
--
--         , responseDeleteXssMatchSet $
--             newDeleteXssMatchSetResponse
--
--         , responseGetByteMatchSet $
--             newGetByteMatchSetResponse
--
--         , responseGetChangeToken $
--             newGetChangeTokenResponse
--
--         , responseGetChangeTokenStatus $
--             newGetChangeTokenStatusResponse
--
--         , responseGetGeoMatchSet $
--             newGetGeoMatchSetResponse
--
--         , responseGetIPSet $
--             newGetIPSetResponse
--
--         , responseGetLoggingConfiguration $
--             newGetLoggingConfigurationResponse
--
--         , responseGetPermissionPolicy $
--             newGetPermissionPolicyResponse
--
--         , responseGetRateBasedRule $
--             newGetRateBasedRuleResponse
--
--         , responseGetRateBasedRuleManagedKeys $
--             newGetRateBasedRuleManagedKeysResponse
--
--         , responseGetRegexMatchSet $
--             newGetRegexMatchSetResponse
--
--         , responseGetRegexPatternSet $
--             newGetRegexPatternSetResponse
--
--         , responseGetRule $
--             newGetRuleResponse
--
--         , responseGetRuleGroup $
--             newGetRuleGroupResponse
--
--         , responseGetSampledRequests $
--             newGetSampledRequestsResponse
--
--         , responseGetSizeConstraintSet $
--             newGetSizeConstraintSetResponse
--
--         , responseGetSqlInjectionMatchSet $
--             newGetSqlInjectionMatchSetResponse
--
--         , responseGetWebACL $
--             newGetWebACLResponse
--
--         , responseGetXssMatchSet $
--             newGetXssMatchSetResponse
--
--         , responseListActivatedRulesInRuleGroup $
--             newListActivatedRulesInRuleGroupResponse
--
--         , responseListByteMatchSets $
--             newListByteMatchSetsResponse
--
--         , responseListGeoMatchSets $
--             newListGeoMatchSetsResponse
--
--         , responseListIPSets $
--             newListIPSetsResponse
--
--         , responseListLoggingConfigurations $
--             newListLoggingConfigurationsResponse
--
--         , responseListRateBasedRules $
--             newListRateBasedRulesResponse
--
--         , responseListRegexMatchSets $
--             newListRegexMatchSetsResponse
--
--         , responseListRegexPatternSets $
--             newListRegexPatternSetsResponse
--
--         , responseListRuleGroups $
--             newListRuleGroupsResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListSizeConstraintSets $
--             newListSizeConstraintSetsResponse
--
--         , responseListSqlInjectionMatchSets $
--             newListSqlInjectionMatchSetsResponse
--
--         , responseListSubscribedRuleGroups $
--             newListSubscribedRuleGroupsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWebACLs $
--             newListWebACLsResponse
--
--         , responseListXssMatchSets $
--             newListXssMatchSetsResponse
--
--         , responsePutLoggingConfiguration $
--             newPutLoggingConfigurationResponse
--
--         , responsePutPermissionPolicy $
--             newPutPermissionPolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateByteMatchSet $
--             newUpdateByteMatchSetResponse
--
--         , responseUpdateGeoMatchSet $
--             newUpdateGeoMatchSetResponse
--
--         , responseUpdateIPSet $
--             newUpdateIPSetResponse
--
--         , responseUpdateRateBasedRule $
--             newUpdateRateBasedRuleResponse
--
--         , responseUpdateRegexMatchSet $
--             newUpdateRegexMatchSetResponse
--
--         , responseUpdateRegexPatternSet $
--             newUpdateRegexPatternSetResponse
--
--         , responseUpdateRule $
--             newUpdateRuleResponse
--
--         , responseUpdateRuleGroup $
--             newUpdateRuleGroupResponse
--
--         , responseUpdateSizeConstraintSet $
--             newUpdateSizeConstraintSetResponse
--
--         , responseUpdateSqlInjectionMatchSet $
--             newUpdateSqlInjectionMatchSetResponse
--
--         , responseUpdateWebACL $
--             newUpdateWebACLResponse
--
--         , responseUpdateXssMatchSet $
--             newUpdateXssMatchSetResponse
--
--           ]
--     ]

-- Requests

requestCreateByteMatchSet :: CreateByteMatchSet -> TestTree
requestCreateByteMatchSet =
  req
    "CreateByteMatchSet"
    "fixture/CreateByteMatchSet.yaml"

requestCreateGeoMatchSet :: CreateGeoMatchSet -> TestTree
requestCreateGeoMatchSet =
  req
    "CreateGeoMatchSet"
    "fixture/CreateGeoMatchSet.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet =
  req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestCreateRateBasedRule :: CreateRateBasedRule -> TestTree
requestCreateRateBasedRule =
  req
    "CreateRateBasedRule"
    "fixture/CreateRateBasedRule.yaml"

requestCreateRegexMatchSet :: CreateRegexMatchSet -> TestTree
requestCreateRegexMatchSet =
  req
    "CreateRegexMatchSet"
    "fixture/CreateRegexMatchSet.yaml"

requestCreateRegexPatternSet :: CreateRegexPatternSet -> TestTree
requestCreateRegexPatternSet =
  req
    "CreateRegexPatternSet"
    "fixture/CreateRegexPatternSet.yaml"

requestCreateRule :: CreateRule -> TestTree
requestCreateRule =
  req
    "CreateRule"
    "fixture/CreateRule.yaml"

requestCreateRuleGroup :: CreateRuleGroup -> TestTree
requestCreateRuleGroup =
  req
    "CreateRuleGroup"
    "fixture/CreateRuleGroup.yaml"

requestCreateSizeConstraintSet :: CreateSizeConstraintSet -> TestTree
requestCreateSizeConstraintSet =
  req
    "CreateSizeConstraintSet"
    "fixture/CreateSizeConstraintSet.yaml"

requestCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSet -> TestTree
requestCreateSqlInjectionMatchSet =
  req
    "CreateSqlInjectionMatchSet"
    "fixture/CreateSqlInjectionMatchSet.yaml"

requestCreateWebACL :: CreateWebACL -> TestTree
requestCreateWebACL =
  req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

requestCreateWebACLMigrationStack :: CreateWebACLMigrationStack -> TestTree
requestCreateWebACLMigrationStack =
  req
    "CreateWebACLMigrationStack"
    "fixture/CreateWebACLMigrationStack.yaml"

requestCreateXssMatchSet :: CreateXssMatchSet -> TestTree
requestCreateXssMatchSet =
  req
    "CreateXssMatchSet"
    "fixture/CreateXssMatchSet.yaml"

requestDeleteByteMatchSet :: DeleteByteMatchSet -> TestTree
requestDeleteByteMatchSet =
  req
    "DeleteByteMatchSet"
    "fixture/DeleteByteMatchSet.yaml"

requestDeleteGeoMatchSet :: DeleteGeoMatchSet -> TestTree
requestDeleteGeoMatchSet =
  req
    "DeleteGeoMatchSet"
    "fixture/DeleteGeoMatchSet.yaml"

requestDeleteIPSet :: DeleteIPSet -> TestTree
requestDeleteIPSet =
  req
    "DeleteIPSet"
    "fixture/DeleteIPSet.yaml"

requestDeleteLoggingConfiguration :: DeleteLoggingConfiguration -> TestTree
requestDeleteLoggingConfiguration =
  req
    "DeleteLoggingConfiguration"
    "fixture/DeleteLoggingConfiguration.yaml"

requestDeletePermissionPolicy :: DeletePermissionPolicy -> TestTree
requestDeletePermissionPolicy =
  req
    "DeletePermissionPolicy"
    "fixture/DeletePermissionPolicy.yaml"

requestDeleteRateBasedRule :: DeleteRateBasedRule -> TestTree
requestDeleteRateBasedRule =
  req
    "DeleteRateBasedRule"
    "fixture/DeleteRateBasedRule.yaml"

requestDeleteRegexMatchSet :: DeleteRegexMatchSet -> TestTree
requestDeleteRegexMatchSet =
  req
    "DeleteRegexMatchSet"
    "fixture/DeleteRegexMatchSet.yaml"

requestDeleteRegexPatternSet :: DeleteRegexPatternSet -> TestTree
requestDeleteRegexPatternSet =
  req
    "DeleteRegexPatternSet"
    "fixture/DeleteRegexPatternSet.yaml"

requestDeleteRule :: DeleteRule -> TestTree
requestDeleteRule =
  req
    "DeleteRule"
    "fixture/DeleteRule.yaml"

requestDeleteRuleGroup :: DeleteRuleGroup -> TestTree
requestDeleteRuleGroup =
  req
    "DeleteRuleGroup"
    "fixture/DeleteRuleGroup.yaml"

requestDeleteSizeConstraintSet :: DeleteSizeConstraintSet -> TestTree
requestDeleteSizeConstraintSet =
  req
    "DeleteSizeConstraintSet"
    "fixture/DeleteSizeConstraintSet.yaml"

requestDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSet -> TestTree
requestDeleteSqlInjectionMatchSet =
  req
    "DeleteSqlInjectionMatchSet"
    "fixture/DeleteSqlInjectionMatchSet.yaml"

requestDeleteWebACL :: DeleteWebACL -> TestTree
requestDeleteWebACL =
  req
    "DeleteWebACL"
    "fixture/DeleteWebACL.yaml"

requestDeleteXssMatchSet :: DeleteXssMatchSet -> TestTree
requestDeleteXssMatchSet =
  req
    "DeleteXssMatchSet"
    "fixture/DeleteXssMatchSet.yaml"

requestGetByteMatchSet :: GetByteMatchSet -> TestTree
requestGetByteMatchSet =
  req
    "GetByteMatchSet"
    "fixture/GetByteMatchSet.yaml"

requestGetChangeToken :: GetChangeToken -> TestTree
requestGetChangeToken =
  req
    "GetChangeToken"
    "fixture/GetChangeToken.yaml"

requestGetChangeTokenStatus :: GetChangeTokenStatus -> TestTree
requestGetChangeTokenStatus =
  req
    "GetChangeTokenStatus"
    "fixture/GetChangeTokenStatus.yaml"

requestGetGeoMatchSet :: GetGeoMatchSet -> TestTree
requestGetGeoMatchSet =
  req
    "GetGeoMatchSet"
    "fixture/GetGeoMatchSet.yaml"

requestGetIPSet :: GetIPSet -> TestTree
requestGetIPSet =
  req
    "GetIPSet"
    "fixture/GetIPSet.yaml"

requestGetLoggingConfiguration :: GetLoggingConfiguration -> TestTree
requestGetLoggingConfiguration =
  req
    "GetLoggingConfiguration"
    "fixture/GetLoggingConfiguration.yaml"

requestGetPermissionPolicy :: GetPermissionPolicy -> TestTree
requestGetPermissionPolicy =
  req
    "GetPermissionPolicy"
    "fixture/GetPermissionPolicy.yaml"

requestGetRateBasedRule :: GetRateBasedRule -> TestTree
requestGetRateBasedRule =
  req
    "GetRateBasedRule"
    "fixture/GetRateBasedRule.yaml"

requestGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeys -> TestTree
requestGetRateBasedRuleManagedKeys =
  req
    "GetRateBasedRuleManagedKeys"
    "fixture/GetRateBasedRuleManagedKeys.yaml"

requestGetRegexMatchSet :: GetRegexMatchSet -> TestTree
requestGetRegexMatchSet =
  req
    "GetRegexMatchSet"
    "fixture/GetRegexMatchSet.yaml"

requestGetRegexPatternSet :: GetRegexPatternSet -> TestTree
requestGetRegexPatternSet =
  req
    "GetRegexPatternSet"
    "fixture/GetRegexPatternSet.yaml"

requestGetRule :: GetRule -> TestTree
requestGetRule =
  req
    "GetRule"
    "fixture/GetRule.yaml"

requestGetRuleGroup :: GetRuleGroup -> TestTree
requestGetRuleGroup =
  req
    "GetRuleGroup"
    "fixture/GetRuleGroup.yaml"

requestGetSampledRequests :: GetSampledRequests -> TestTree
requestGetSampledRequests =
  req
    "GetSampledRequests"
    "fixture/GetSampledRequests.yaml"

requestGetSizeConstraintSet :: GetSizeConstraintSet -> TestTree
requestGetSizeConstraintSet =
  req
    "GetSizeConstraintSet"
    "fixture/GetSizeConstraintSet.yaml"

requestGetSqlInjectionMatchSet :: GetSqlInjectionMatchSet -> TestTree
requestGetSqlInjectionMatchSet =
  req
    "GetSqlInjectionMatchSet"
    "fixture/GetSqlInjectionMatchSet.yaml"

requestGetWebACL :: GetWebACL -> TestTree
requestGetWebACL =
  req
    "GetWebACL"
    "fixture/GetWebACL.yaml"

requestGetXssMatchSet :: GetXssMatchSet -> TestTree
requestGetXssMatchSet =
  req
    "GetXssMatchSet"
    "fixture/GetXssMatchSet.yaml"

requestListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroup -> TestTree
requestListActivatedRulesInRuleGroup =
  req
    "ListActivatedRulesInRuleGroup"
    "fixture/ListActivatedRulesInRuleGroup.yaml"

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

requestListIPSets :: ListIPSets -> TestTree
requestListIPSets =
  req
    "ListIPSets"
    "fixture/ListIPSets.yaml"

requestListLoggingConfigurations :: ListLoggingConfigurations -> TestTree
requestListLoggingConfigurations =
  req
    "ListLoggingConfigurations"
    "fixture/ListLoggingConfigurations.yaml"

requestListRateBasedRules :: ListRateBasedRules -> TestTree
requestListRateBasedRules =
  req
    "ListRateBasedRules"
    "fixture/ListRateBasedRules.yaml"

requestListRegexMatchSets :: ListRegexMatchSets -> TestTree
requestListRegexMatchSets =
  req
    "ListRegexMatchSets"
    "fixture/ListRegexMatchSets.yaml"

requestListRegexPatternSets :: ListRegexPatternSets -> TestTree
requestListRegexPatternSets =
  req
    "ListRegexPatternSets"
    "fixture/ListRegexPatternSets.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups =
  req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestListSizeConstraintSets :: ListSizeConstraintSets -> TestTree
requestListSizeConstraintSets =
  req
    "ListSizeConstraintSets"
    "fixture/ListSizeConstraintSets.yaml"

requestListSqlInjectionMatchSets :: ListSqlInjectionMatchSets -> TestTree
requestListSqlInjectionMatchSets =
  req
    "ListSqlInjectionMatchSets"
    "fixture/ListSqlInjectionMatchSets.yaml"

requestListSubscribedRuleGroups :: ListSubscribedRuleGroups -> TestTree
requestListSubscribedRuleGroups =
  req
    "ListSubscribedRuleGroups"
    "fixture/ListSubscribedRuleGroups.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWebACLs :: ListWebACLs -> TestTree
requestListWebACLs =
  req
    "ListWebACLs"
    "fixture/ListWebACLs.yaml"

requestListXssMatchSets :: ListXssMatchSets -> TestTree
requestListXssMatchSets =
  req
    "ListXssMatchSets"
    "fixture/ListXssMatchSets.yaml"

requestPutLoggingConfiguration :: PutLoggingConfiguration -> TestTree
requestPutLoggingConfiguration =
  req
    "PutLoggingConfiguration"
    "fixture/PutLoggingConfiguration.yaml"

requestPutPermissionPolicy :: PutPermissionPolicy -> TestTree
requestPutPermissionPolicy =
  req
    "PutPermissionPolicy"
    "fixture/PutPermissionPolicy.yaml"

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

requestUpdateByteMatchSet :: UpdateByteMatchSet -> TestTree
requestUpdateByteMatchSet =
  req
    "UpdateByteMatchSet"
    "fixture/UpdateByteMatchSet.yaml"

requestUpdateGeoMatchSet :: UpdateGeoMatchSet -> TestTree
requestUpdateGeoMatchSet =
  req
    "UpdateGeoMatchSet"
    "fixture/UpdateGeoMatchSet.yaml"

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet =
  req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestUpdateRateBasedRule :: UpdateRateBasedRule -> TestTree
requestUpdateRateBasedRule =
  req
    "UpdateRateBasedRule"
    "fixture/UpdateRateBasedRule.yaml"

requestUpdateRegexMatchSet :: UpdateRegexMatchSet -> TestTree
requestUpdateRegexMatchSet =
  req
    "UpdateRegexMatchSet"
    "fixture/UpdateRegexMatchSet.yaml"

requestUpdateRegexPatternSet :: UpdateRegexPatternSet -> TestTree
requestUpdateRegexPatternSet =
  req
    "UpdateRegexPatternSet"
    "fixture/UpdateRegexPatternSet.yaml"

requestUpdateRule :: UpdateRule -> TestTree
requestUpdateRule =
  req
    "UpdateRule"
    "fixture/UpdateRule.yaml"

requestUpdateRuleGroup :: UpdateRuleGroup -> TestTree
requestUpdateRuleGroup =
  req
    "UpdateRuleGroup"
    "fixture/UpdateRuleGroup.yaml"

requestUpdateSizeConstraintSet :: UpdateSizeConstraintSet -> TestTree
requestUpdateSizeConstraintSet =
  req
    "UpdateSizeConstraintSet"
    "fixture/UpdateSizeConstraintSet.yaml"

requestUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSet -> TestTree
requestUpdateSqlInjectionMatchSet =
  req
    "UpdateSqlInjectionMatchSet"
    "fixture/UpdateSqlInjectionMatchSet.yaml"

requestUpdateWebACL :: UpdateWebACL -> TestTree
requestUpdateWebACL =
  req
    "UpdateWebACL"
    "fixture/UpdateWebACL.yaml"

requestUpdateXssMatchSet :: UpdateXssMatchSet -> TestTree
requestUpdateXssMatchSet =
  req
    "UpdateXssMatchSet"
    "fixture/UpdateXssMatchSet.yaml"

-- Responses

responseCreateByteMatchSet :: CreateByteMatchSetResponse -> TestTree
responseCreateByteMatchSet =
  res
    "CreateByteMatchSetResponse"
    "fixture/CreateByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateByteMatchSet)

responseCreateGeoMatchSet :: CreateGeoMatchSetResponse -> TestTree
responseCreateGeoMatchSet =
  res
    "CreateGeoMatchSetResponse"
    "fixture/CreateGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGeoMatchSet)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet =
  res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIPSet)

responseCreateRateBasedRule :: CreateRateBasedRuleResponse -> TestTree
responseCreateRateBasedRule =
  res
    "CreateRateBasedRuleResponse"
    "fixture/CreateRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRateBasedRule)

responseCreateRegexMatchSet :: CreateRegexMatchSetResponse -> TestTree
responseCreateRegexMatchSet =
  res
    "CreateRegexMatchSetResponse"
    "fixture/CreateRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegexMatchSet)

responseCreateRegexPatternSet :: CreateRegexPatternSetResponse -> TestTree
responseCreateRegexPatternSet =
  res
    "CreateRegexPatternSetResponse"
    "fixture/CreateRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegexPatternSet)

responseCreateRule :: CreateRuleResponse -> TestTree
responseCreateRule =
  res
    "CreateRuleResponse"
    "fixture/CreateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRule)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup =
  res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleGroup)

responseCreateSizeConstraintSet :: CreateSizeConstraintSetResponse -> TestTree
responseCreateSizeConstraintSet =
  res
    "CreateSizeConstraintSetResponse"
    "fixture/CreateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSizeConstraintSet)

responseCreateSqlInjectionMatchSet :: CreateSqlInjectionMatchSetResponse -> TestTree
responseCreateSqlInjectionMatchSet =
  res
    "CreateSqlInjectionMatchSetResponse"
    "fixture/CreateSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSqlInjectionMatchSet)

responseCreateWebACL :: CreateWebACLResponse -> TestTree
responseCreateWebACL =
  res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebACL)

responseCreateWebACLMigrationStack :: CreateWebACLMigrationStackResponse -> TestTree
responseCreateWebACLMigrationStack =
  res
    "CreateWebACLMigrationStackResponse"
    "fixture/CreateWebACLMigrationStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebACLMigrationStack)

responseCreateXssMatchSet :: CreateXssMatchSetResponse -> TestTree
responseCreateXssMatchSet =
  res
    "CreateXssMatchSetResponse"
    "fixture/CreateXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateXssMatchSet)

responseDeleteByteMatchSet :: DeleteByteMatchSetResponse -> TestTree
responseDeleteByteMatchSet =
  res
    "DeleteByteMatchSetResponse"
    "fixture/DeleteByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteByteMatchSet)

responseDeleteGeoMatchSet :: DeleteGeoMatchSetResponse -> TestTree
responseDeleteGeoMatchSet =
  res
    "DeleteGeoMatchSetResponse"
    "fixture/DeleteGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGeoMatchSet)

responseDeleteIPSet :: DeleteIPSetResponse -> TestTree
responseDeleteIPSet =
  res
    "DeleteIPSetResponse"
    "fixture/DeleteIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIPSet)

responseDeleteLoggingConfiguration :: DeleteLoggingConfigurationResponse -> TestTree
responseDeleteLoggingConfiguration =
  res
    "DeleteLoggingConfigurationResponse"
    "fixture/DeleteLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoggingConfiguration)

responseDeletePermissionPolicy :: DeletePermissionPolicyResponse -> TestTree
responseDeletePermissionPolicy =
  res
    "DeletePermissionPolicyResponse"
    "fixture/DeletePermissionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermissionPolicy)

responseDeleteRateBasedRule :: DeleteRateBasedRuleResponse -> TestTree
responseDeleteRateBasedRule =
  res
    "DeleteRateBasedRuleResponse"
    "fixture/DeleteRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRateBasedRule)

responseDeleteRegexMatchSet :: DeleteRegexMatchSetResponse -> TestTree
responseDeleteRegexMatchSet =
  res
    "DeleteRegexMatchSetResponse"
    "fixture/DeleteRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegexMatchSet)

responseDeleteRegexPatternSet :: DeleteRegexPatternSetResponse -> TestTree
responseDeleteRegexPatternSet =
  res
    "DeleteRegexPatternSetResponse"
    "fixture/DeleteRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegexPatternSet)

responseDeleteRule :: DeleteRuleResponse -> TestTree
responseDeleteRule =
  res
    "DeleteRuleResponse"
    "fixture/DeleteRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRule)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup =
  res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleGroup)

responseDeleteSizeConstraintSet :: DeleteSizeConstraintSetResponse -> TestTree
responseDeleteSizeConstraintSet =
  res
    "DeleteSizeConstraintSetResponse"
    "fixture/DeleteSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSizeConstraintSet)

responseDeleteSqlInjectionMatchSet :: DeleteSqlInjectionMatchSetResponse -> TestTree
responseDeleteSqlInjectionMatchSet =
  res
    "DeleteSqlInjectionMatchSetResponse"
    "fixture/DeleteSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSqlInjectionMatchSet)

responseDeleteWebACL :: DeleteWebACLResponse -> TestTree
responseDeleteWebACL =
  res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebACL)

responseDeleteXssMatchSet :: DeleteXssMatchSetResponse -> TestTree
responseDeleteXssMatchSet =
  res
    "DeleteXssMatchSetResponse"
    "fixture/DeleteXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteXssMatchSet)

responseGetByteMatchSet :: GetByteMatchSetResponse -> TestTree
responseGetByteMatchSet =
  res
    "GetByteMatchSetResponse"
    "fixture/GetByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetByteMatchSet)

responseGetChangeToken :: GetChangeTokenResponse -> TestTree
responseGetChangeToken =
  res
    "GetChangeTokenResponse"
    "fixture/GetChangeTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChangeToken)

responseGetChangeTokenStatus :: GetChangeTokenStatusResponse -> TestTree
responseGetChangeTokenStatus =
  res
    "GetChangeTokenStatusResponse"
    "fixture/GetChangeTokenStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChangeTokenStatus)

responseGetGeoMatchSet :: GetGeoMatchSetResponse -> TestTree
responseGetGeoMatchSet =
  res
    "GetGeoMatchSetResponse"
    "fixture/GetGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeoMatchSet)

responseGetIPSet :: GetIPSetResponse -> TestTree
responseGetIPSet =
  res
    "GetIPSetResponse"
    "fixture/GetIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIPSet)

responseGetLoggingConfiguration :: GetLoggingConfigurationResponse -> TestTree
responseGetLoggingConfiguration =
  res
    "GetLoggingConfigurationResponse"
    "fixture/GetLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggingConfiguration)

responseGetPermissionPolicy :: GetPermissionPolicyResponse -> TestTree
responseGetPermissionPolicy =
  res
    "GetPermissionPolicyResponse"
    "fixture/GetPermissionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPermissionPolicy)

responseGetRateBasedRule :: GetRateBasedRuleResponse -> TestTree
responseGetRateBasedRule =
  res
    "GetRateBasedRuleResponse"
    "fixture/GetRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRateBasedRule)

responseGetRateBasedRuleManagedKeys :: GetRateBasedRuleManagedKeysResponse -> TestTree
responseGetRateBasedRuleManagedKeys =
  res
    "GetRateBasedRuleManagedKeysResponse"
    "fixture/GetRateBasedRuleManagedKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRateBasedRuleManagedKeys)

responseGetRegexMatchSet :: GetRegexMatchSetResponse -> TestTree
responseGetRegexMatchSet =
  res
    "GetRegexMatchSetResponse"
    "fixture/GetRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegexMatchSet)

responseGetRegexPatternSet :: GetRegexPatternSetResponse -> TestTree
responseGetRegexPatternSet =
  res
    "GetRegexPatternSetResponse"
    "fixture/GetRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegexPatternSet)

responseGetRule :: GetRuleResponse -> TestTree
responseGetRule =
  res
    "GetRuleResponse"
    "fixture/GetRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRule)

responseGetRuleGroup :: GetRuleGroupResponse -> TestTree
responseGetRuleGroup =
  res
    "GetRuleGroupResponse"
    "fixture/GetRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRuleGroup)

responseGetSampledRequests :: GetSampledRequestsResponse -> TestTree
responseGetSampledRequests =
  res
    "GetSampledRequestsResponse"
    "fixture/GetSampledRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSampledRequests)

responseGetSizeConstraintSet :: GetSizeConstraintSetResponse -> TestTree
responseGetSizeConstraintSet =
  res
    "GetSizeConstraintSetResponse"
    "fixture/GetSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSizeConstraintSet)

responseGetSqlInjectionMatchSet :: GetSqlInjectionMatchSetResponse -> TestTree
responseGetSqlInjectionMatchSet =
  res
    "GetSqlInjectionMatchSetResponse"
    "fixture/GetSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSqlInjectionMatchSet)

responseGetWebACL :: GetWebACLResponse -> TestTree
responseGetWebACL =
  res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWebACL)

responseGetXssMatchSet :: GetXssMatchSetResponse -> TestTree
responseGetXssMatchSet =
  res
    "GetXssMatchSetResponse"
    "fixture/GetXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetXssMatchSet)

responseListActivatedRulesInRuleGroup :: ListActivatedRulesInRuleGroupResponse -> TestTree
responseListActivatedRulesInRuleGroup =
  res
    "ListActivatedRulesInRuleGroupResponse"
    "fixture/ListActivatedRulesInRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListActivatedRulesInRuleGroup)

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

responseListIPSets :: ListIPSetsResponse -> TestTree
responseListIPSets =
  res
    "ListIPSetsResponse"
    "fixture/ListIPSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIPSets)

responseListLoggingConfigurations :: ListLoggingConfigurationsResponse -> TestTree
responseListLoggingConfigurations =
  res
    "ListLoggingConfigurationsResponse"
    "fixture/ListLoggingConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLoggingConfigurations)

responseListRateBasedRules :: ListRateBasedRulesResponse -> TestTree
responseListRateBasedRules =
  res
    "ListRateBasedRulesResponse"
    "fixture/ListRateBasedRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRateBasedRules)

responseListRegexMatchSets :: ListRegexMatchSetsResponse -> TestTree
responseListRegexMatchSets =
  res
    "ListRegexMatchSetsResponse"
    "fixture/ListRegexMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegexMatchSets)

responseListRegexPatternSets :: ListRegexPatternSetsResponse -> TestTree
responseListRegexPatternSets =
  res
    "ListRegexPatternSetsResponse"
    "fixture/ListRegexPatternSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegexPatternSets)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups =
  res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleGroups)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseListSizeConstraintSets :: ListSizeConstraintSetsResponse -> TestTree
responseListSizeConstraintSets =
  res
    "ListSizeConstraintSetsResponse"
    "fixture/ListSizeConstraintSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSizeConstraintSets)

responseListSqlInjectionMatchSets :: ListSqlInjectionMatchSetsResponse -> TestTree
responseListSqlInjectionMatchSets =
  res
    "ListSqlInjectionMatchSetsResponse"
    "fixture/ListSqlInjectionMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSqlInjectionMatchSets)

responseListSubscribedRuleGroups :: ListSubscribedRuleGroupsResponse -> TestTree
responseListSubscribedRuleGroups =
  res
    "ListSubscribedRuleGroupsResponse"
    "fixture/ListSubscribedRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscribedRuleGroups)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWebACLs :: ListWebACLsResponse -> TestTree
responseListWebACLs =
  res
    "ListWebACLsResponse"
    "fixture/ListWebACLsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWebACLs)

responseListXssMatchSets :: ListXssMatchSetsResponse -> TestTree
responseListXssMatchSets =
  res
    "ListXssMatchSetsResponse"
    "fixture/ListXssMatchSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListXssMatchSets)

responsePutLoggingConfiguration :: PutLoggingConfigurationResponse -> TestTree
responsePutLoggingConfiguration =
  res
    "PutLoggingConfigurationResponse"
    "fixture/PutLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingConfiguration)

responsePutPermissionPolicy :: PutPermissionPolicyResponse -> TestTree
responsePutPermissionPolicy =
  res
    "PutPermissionPolicyResponse"
    "fixture/PutPermissionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPermissionPolicy)

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

responseUpdateByteMatchSet :: UpdateByteMatchSetResponse -> TestTree
responseUpdateByteMatchSet =
  res
    "UpdateByteMatchSetResponse"
    "fixture/UpdateByteMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateByteMatchSet)

responseUpdateGeoMatchSet :: UpdateGeoMatchSetResponse -> TestTree
responseUpdateGeoMatchSet =
  res
    "UpdateGeoMatchSetResponse"
    "fixture/UpdateGeoMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGeoMatchSet)

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet =
  res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIPSet)

responseUpdateRateBasedRule :: UpdateRateBasedRuleResponse -> TestTree
responseUpdateRateBasedRule =
  res
    "UpdateRateBasedRuleResponse"
    "fixture/UpdateRateBasedRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRateBasedRule)

responseUpdateRegexMatchSet :: UpdateRegexMatchSetResponse -> TestTree
responseUpdateRegexMatchSet =
  res
    "UpdateRegexMatchSetResponse"
    "fixture/UpdateRegexMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegexMatchSet)

responseUpdateRegexPatternSet :: UpdateRegexPatternSetResponse -> TestTree
responseUpdateRegexPatternSet =
  res
    "UpdateRegexPatternSetResponse"
    "fixture/UpdateRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegexPatternSet)

responseUpdateRule :: UpdateRuleResponse -> TestTree
responseUpdateRule =
  res
    "UpdateRuleResponse"
    "fixture/UpdateRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRule)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup =
  res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleGroup)

responseUpdateSizeConstraintSet :: UpdateSizeConstraintSetResponse -> TestTree
responseUpdateSizeConstraintSet =
  res
    "UpdateSizeConstraintSetResponse"
    "fixture/UpdateSizeConstraintSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSizeConstraintSet)

responseUpdateSqlInjectionMatchSet :: UpdateSqlInjectionMatchSetResponse -> TestTree
responseUpdateSqlInjectionMatchSet =
  res
    "UpdateSqlInjectionMatchSetResponse"
    "fixture/UpdateSqlInjectionMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSqlInjectionMatchSet)

responseUpdateWebACL :: UpdateWebACLResponse -> TestTree
responseUpdateWebACL =
  res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWebACL)

responseUpdateXssMatchSet :: UpdateXssMatchSetResponse -> TestTree
responseUpdateXssMatchSet =
  res
    "UpdateXssMatchSetResponse"
    "fixture/UpdateXssMatchSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateXssMatchSet)

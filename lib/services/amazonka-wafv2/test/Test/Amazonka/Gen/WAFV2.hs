{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WAFV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WAFV2 where

import Amazonka.WAFV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WAFV2.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateWebACL $
--             newAssociateWebACL
--
--         , requestCheckCapacity $
--             newCheckCapacity
--
--         , requestCreateIPSet $
--             newCreateIPSet
--
--         , requestCreateRegexPatternSet $
--             newCreateRegexPatternSet
--
--         , requestCreateRuleGroup $
--             newCreateRuleGroup
--
--         , requestCreateWebACL $
--             newCreateWebACL
--
--         , requestDeleteFirewallManagerRuleGroups $
--             newDeleteFirewallManagerRuleGroups
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
--         , requestDeleteRegexPatternSet $
--             newDeleteRegexPatternSet
--
--         , requestDeleteRuleGroup $
--             newDeleteRuleGroup
--
--         , requestDeleteWebACL $
--             newDeleteWebACL
--
--         , requestDescribeManagedRuleGroup $
--             newDescribeManagedRuleGroup
--
--         , requestDisassociateWebACL $
--             newDisassociateWebACL
--
--         , requestGenerateMobileSdkReleaseUrl $
--             newGenerateMobileSdkReleaseUrl
--
--         , requestGetIPSet $
--             newGetIPSet
--
--         , requestGetLoggingConfiguration $
--             newGetLoggingConfiguration
--
--         , requestGetManagedRuleSet $
--             newGetManagedRuleSet
--
--         , requestGetMobileSdkRelease $
--             newGetMobileSdkRelease
--
--         , requestGetPermissionPolicy $
--             newGetPermissionPolicy
--
--         , requestGetRateBasedStatementManagedKeys $
--             newGetRateBasedStatementManagedKeys
--
--         , requestGetRegexPatternSet $
--             newGetRegexPatternSet
--
--         , requestGetRuleGroup $
--             newGetRuleGroup
--
--         , requestGetSampledRequests $
--             newGetSampledRequests
--
--         , requestGetWebACL $
--             newGetWebACL
--
--         , requestGetWebACLForResource $
--             newGetWebACLForResource
--
--         , requestListAvailableManagedRuleGroupVersions $
--             newListAvailableManagedRuleGroupVersions
--
--         , requestListAvailableManagedRuleGroups $
--             newListAvailableManagedRuleGroups
--
--         , requestListIPSets $
--             newListIPSets
--
--         , requestListLoggingConfigurations $
--             newListLoggingConfigurations
--
--         , requestListManagedRuleSets $
--             newListManagedRuleSets
--
--         , requestListMobileSdkReleases $
--             newListMobileSdkReleases
--
--         , requestListRegexPatternSets $
--             newListRegexPatternSets
--
--         , requestListResourcesForWebACL $
--             newListResourcesForWebACL
--
--         , requestListRuleGroups $
--             newListRuleGroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWebACLs $
--             newListWebACLs
--
--         , requestPutLoggingConfiguration $
--             newPutLoggingConfiguration
--
--         , requestPutManagedRuleSetVersions $
--             newPutManagedRuleSetVersions
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
--         , requestUpdateIPSet $
--             newUpdateIPSet
--
--         , requestUpdateManagedRuleSetVersionExpiryDate $
--             newUpdateManagedRuleSetVersionExpiryDate
--
--         , requestUpdateRegexPatternSet $
--             newUpdateRegexPatternSet
--
--         , requestUpdateRuleGroup $
--             newUpdateRuleGroup
--
--         , requestUpdateWebACL $
--             newUpdateWebACL
--
--           ]

--     , testGroup "response"
--         [ responseAssociateWebACL $
--             newAssociateWebACLResponse
--
--         , responseCheckCapacity $
--             newCheckCapacityResponse
--
--         , responseCreateIPSet $
--             newCreateIPSetResponse
--
--         , responseCreateRegexPatternSet $
--             newCreateRegexPatternSetResponse
--
--         , responseCreateRuleGroup $
--             newCreateRuleGroupResponse
--
--         , responseCreateWebACL $
--             newCreateWebACLResponse
--
--         , responseDeleteFirewallManagerRuleGroups $
--             newDeleteFirewallManagerRuleGroupsResponse
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
--         , responseDeleteRegexPatternSet $
--             newDeleteRegexPatternSetResponse
--
--         , responseDeleteRuleGroup $
--             newDeleteRuleGroupResponse
--
--         , responseDeleteWebACL $
--             newDeleteWebACLResponse
--
--         , responseDescribeManagedRuleGroup $
--             newDescribeManagedRuleGroupResponse
--
--         , responseDisassociateWebACL $
--             newDisassociateWebACLResponse
--
--         , responseGenerateMobileSdkReleaseUrl $
--             newGenerateMobileSdkReleaseUrlResponse
--
--         , responseGetIPSet $
--             newGetIPSetResponse
--
--         , responseGetLoggingConfiguration $
--             newGetLoggingConfigurationResponse
--
--         , responseGetManagedRuleSet $
--             newGetManagedRuleSetResponse
--
--         , responseGetMobileSdkRelease $
--             newGetMobileSdkReleaseResponse
--
--         , responseGetPermissionPolicy $
--             newGetPermissionPolicyResponse
--
--         , responseGetRateBasedStatementManagedKeys $
--             newGetRateBasedStatementManagedKeysResponse
--
--         , responseGetRegexPatternSet $
--             newGetRegexPatternSetResponse
--
--         , responseGetRuleGroup $
--             newGetRuleGroupResponse
--
--         , responseGetSampledRequests $
--             newGetSampledRequestsResponse
--
--         , responseGetWebACL $
--             newGetWebACLResponse
--
--         , responseGetWebACLForResource $
--             newGetWebACLForResourceResponse
--
--         , responseListAvailableManagedRuleGroupVersions $
--             newListAvailableManagedRuleGroupVersionsResponse
--
--         , responseListAvailableManagedRuleGroups $
--             newListAvailableManagedRuleGroupsResponse
--
--         , responseListIPSets $
--             newListIPSetsResponse
--
--         , responseListLoggingConfigurations $
--             newListLoggingConfigurationsResponse
--
--         , responseListManagedRuleSets $
--             newListManagedRuleSetsResponse
--
--         , responseListMobileSdkReleases $
--             newListMobileSdkReleasesResponse
--
--         , responseListRegexPatternSets $
--             newListRegexPatternSetsResponse
--
--         , responseListResourcesForWebACL $
--             newListResourcesForWebACLResponse
--
--         , responseListRuleGroups $
--             newListRuleGroupsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWebACLs $
--             newListWebACLsResponse
--
--         , responsePutLoggingConfiguration $
--             newPutLoggingConfigurationResponse
--
--         , responsePutManagedRuleSetVersions $
--             newPutManagedRuleSetVersionsResponse
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
--         , responseUpdateIPSet $
--             newUpdateIPSetResponse
--
--         , responseUpdateManagedRuleSetVersionExpiryDate $
--             newUpdateManagedRuleSetVersionExpiryDateResponse
--
--         , responseUpdateRegexPatternSet $
--             newUpdateRegexPatternSetResponse
--
--         , responseUpdateRuleGroup $
--             newUpdateRuleGroupResponse
--
--         , responseUpdateWebACL $
--             newUpdateWebACLResponse
--
--           ]
--     ]

-- Requests

requestAssociateWebACL :: AssociateWebACL -> TestTree
requestAssociateWebACL =
  req
    "AssociateWebACL"
    "fixture/AssociateWebACL.yaml"

requestCheckCapacity :: CheckCapacity -> TestTree
requestCheckCapacity =
  req
    "CheckCapacity"
    "fixture/CheckCapacity.yaml"

requestCreateIPSet :: CreateIPSet -> TestTree
requestCreateIPSet =
  req
    "CreateIPSet"
    "fixture/CreateIPSet.yaml"

requestCreateRegexPatternSet :: CreateRegexPatternSet -> TestTree
requestCreateRegexPatternSet =
  req
    "CreateRegexPatternSet"
    "fixture/CreateRegexPatternSet.yaml"

requestCreateRuleGroup :: CreateRuleGroup -> TestTree
requestCreateRuleGroup =
  req
    "CreateRuleGroup"
    "fixture/CreateRuleGroup.yaml"

requestCreateWebACL :: CreateWebACL -> TestTree
requestCreateWebACL =
  req
    "CreateWebACL"
    "fixture/CreateWebACL.yaml"

requestDeleteFirewallManagerRuleGroups :: DeleteFirewallManagerRuleGroups -> TestTree
requestDeleteFirewallManagerRuleGroups =
  req
    "DeleteFirewallManagerRuleGroups"
    "fixture/DeleteFirewallManagerRuleGroups.yaml"

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

requestDeleteRegexPatternSet :: DeleteRegexPatternSet -> TestTree
requestDeleteRegexPatternSet =
  req
    "DeleteRegexPatternSet"
    "fixture/DeleteRegexPatternSet.yaml"

requestDeleteRuleGroup :: DeleteRuleGroup -> TestTree
requestDeleteRuleGroup =
  req
    "DeleteRuleGroup"
    "fixture/DeleteRuleGroup.yaml"

requestDeleteWebACL :: DeleteWebACL -> TestTree
requestDeleteWebACL =
  req
    "DeleteWebACL"
    "fixture/DeleteWebACL.yaml"

requestDescribeManagedRuleGroup :: DescribeManagedRuleGroup -> TestTree
requestDescribeManagedRuleGroup =
  req
    "DescribeManagedRuleGroup"
    "fixture/DescribeManagedRuleGroup.yaml"

requestDisassociateWebACL :: DisassociateWebACL -> TestTree
requestDisassociateWebACL =
  req
    "DisassociateWebACL"
    "fixture/DisassociateWebACL.yaml"

requestGenerateMobileSdkReleaseUrl :: GenerateMobileSdkReleaseUrl -> TestTree
requestGenerateMobileSdkReleaseUrl =
  req
    "GenerateMobileSdkReleaseUrl"
    "fixture/GenerateMobileSdkReleaseUrl.yaml"

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

requestGetManagedRuleSet :: GetManagedRuleSet -> TestTree
requestGetManagedRuleSet =
  req
    "GetManagedRuleSet"
    "fixture/GetManagedRuleSet.yaml"

requestGetMobileSdkRelease :: GetMobileSdkRelease -> TestTree
requestGetMobileSdkRelease =
  req
    "GetMobileSdkRelease"
    "fixture/GetMobileSdkRelease.yaml"

requestGetPermissionPolicy :: GetPermissionPolicy -> TestTree
requestGetPermissionPolicy =
  req
    "GetPermissionPolicy"
    "fixture/GetPermissionPolicy.yaml"

requestGetRateBasedStatementManagedKeys :: GetRateBasedStatementManagedKeys -> TestTree
requestGetRateBasedStatementManagedKeys =
  req
    "GetRateBasedStatementManagedKeys"
    "fixture/GetRateBasedStatementManagedKeys.yaml"

requestGetRegexPatternSet :: GetRegexPatternSet -> TestTree
requestGetRegexPatternSet =
  req
    "GetRegexPatternSet"
    "fixture/GetRegexPatternSet.yaml"

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

requestGetWebACL :: GetWebACL -> TestTree
requestGetWebACL =
  req
    "GetWebACL"
    "fixture/GetWebACL.yaml"

requestGetWebACLForResource :: GetWebACLForResource -> TestTree
requestGetWebACLForResource =
  req
    "GetWebACLForResource"
    "fixture/GetWebACLForResource.yaml"

requestListAvailableManagedRuleGroupVersions :: ListAvailableManagedRuleGroupVersions -> TestTree
requestListAvailableManagedRuleGroupVersions =
  req
    "ListAvailableManagedRuleGroupVersions"
    "fixture/ListAvailableManagedRuleGroupVersions.yaml"

requestListAvailableManagedRuleGroups :: ListAvailableManagedRuleGroups -> TestTree
requestListAvailableManagedRuleGroups =
  req
    "ListAvailableManagedRuleGroups"
    "fixture/ListAvailableManagedRuleGroups.yaml"

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

requestListManagedRuleSets :: ListManagedRuleSets -> TestTree
requestListManagedRuleSets =
  req
    "ListManagedRuleSets"
    "fixture/ListManagedRuleSets.yaml"

requestListMobileSdkReleases :: ListMobileSdkReleases -> TestTree
requestListMobileSdkReleases =
  req
    "ListMobileSdkReleases"
    "fixture/ListMobileSdkReleases.yaml"

requestListRegexPatternSets :: ListRegexPatternSets -> TestTree
requestListRegexPatternSets =
  req
    "ListRegexPatternSets"
    "fixture/ListRegexPatternSets.yaml"

requestListResourcesForWebACL :: ListResourcesForWebACL -> TestTree
requestListResourcesForWebACL =
  req
    "ListResourcesForWebACL"
    "fixture/ListResourcesForWebACL.yaml"

requestListRuleGroups :: ListRuleGroups -> TestTree
requestListRuleGroups =
  req
    "ListRuleGroups"
    "fixture/ListRuleGroups.yaml"

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

requestPutLoggingConfiguration :: PutLoggingConfiguration -> TestTree
requestPutLoggingConfiguration =
  req
    "PutLoggingConfiguration"
    "fixture/PutLoggingConfiguration.yaml"

requestPutManagedRuleSetVersions :: PutManagedRuleSetVersions -> TestTree
requestPutManagedRuleSetVersions =
  req
    "PutManagedRuleSetVersions"
    "fixture/PutManagedRuleSetVersions.yaml"

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

requestUpdateIPSet :: UpdateIPSet -> TestTree
requestUpdateIPSet =
  req
    "UpdateIPSet"
    "fixture/UpdateIPSet.yaml"

requestUpdateManagedRuleSetVersionExpiryDate :: UpdateManagedRuleSetVersionExpiryDate -> TestTree
requestUpdateManagedRuleSetVersionExpiryDate =
  req
    "UpdateManagedRuleSetVersionExpiryDate"
    "fixture/UpdateManagedRuleSetVersionExpiryDate.yaml"

requestUpdateRegexPatternSet :: UpdateRegexPatternSet -> TestTree
requestUpdateRegexPatternSet =
  req
    "UpdateRegexPatternSet"
    "fixture/UpdateRegexPatternSet.yaml"

requestUpdateRuleGroup :: UpdateRuleGroup -> TestTree
requestUpdateRuleGroup =
  req
    "UpdateRuleGroup"
    "fixture/UpdateRuleGroup.yaml"

requestUpdateWebACL :: UpdateWebACL -> TestTree
requestUpdateWebACL =
  req
    "UpdateWebACL"
    "fixture/UpdateWebACL.yaml"

-- Responses

responseAssociateWebACL :: AssociateWebACLResponse -> TestTree
responseAssociateWebACL =
  res
    "AssociateWebACLResponse"
    "fixture/AssociateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateWebACL)

responseCheckCapacity :: CheckCapacityResponse -> TestTree
responseCheckCapacity =
  res
    "CheckCapacityResponse"
    "fixture/CheckCapacityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckCapacity)

responseCreateIPSet :: CreateIPSetResponse -> TestTree
responseCreateIPSet =
  res
    "CreateIPSetResponse"
    "fixture/CreateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIPSet)

responseCreateRegexPatternSet :: CreateRegexPatternSetResponse -> TestTree
responseCreateRegexPatternSet =
  res
    "CreateRegexPatternSetResponse"
    "fixture/CreateRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRegexPatternSet)

responseCreateRuleGroup :: CreateRuleGroupResponse -> TestTree
responseCreateRuleGroup =
  res
    "CreateRuleGroupResponse"
    "fixture/CreateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleGroup)

responseCreateWebACL :: CreateWebACLResponse -> TestTree
responseCreateWebACL =
  res
    "CreateWebACLResponse"
    "fixture/CreateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebACL)

responseDeleteFirewallManagerRuleGroups :: DeleteFirewallManagerRuleGroupsResponse -> TestTree
responseDeleteFirewallManagerRuleGroups =
  res
    "DeleteFirewallManagerRuleGroupsResponse"
    "fixture/DeleteFirewallManagerRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFirewallManagerRuleGroups)

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

responseDeleteRegexPatternSet :: DeleteRegexPatternSetResponse -> TestTree
responseDeleteRegexPatternSet =
  res
    "DeleteRegexPatternSetResponse"
    "fixture/DeleteRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRegexPatternSet)

responseDeleteRuleGroup :: DeleteRuleGroupResponse -> TestTree
responseDeleteRuleGroup =
  res
    "DeleteRuleGroupResponse"
    "fixture/DeleteRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleGroup)

responseDeleteWebACL :: DeleteWebACLResponse -> TestTree
responseDeleteWebACL =
  res
    "DeleteWebACLResponse"
    "fixture/DeleteWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWebACL)

responseDescribeManagedRuleGroup :: DescribeManagedRuleGroupResponse -> TestTree
responseDescribeManagedRuleGroup =
  res
    "DescribeManagedRuleGroupResponse"
    "fixture/DescribeManagedRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeManagedRuleGroup)

responseDisassociateWebACL :: DisassociateWebACLResponse -> TestTree
responseDisassociateWebACL =
  res
    "DisassociateWebACLResponse"
    "fixture/DisassociateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateWebACL)

responseGenerateMobileSdkReleaseUrl :: GenerateMobileSdkReleaseUrlResponse -> TestTree
responseGenerateMobileSdkReleaseUrl =
  res
    "GenerateMobileSdkReleaseUrlResponse"
    "fixture/GenerateMobileSdkReleaseUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GenerateMobileSdkReleaseUrl)

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

responseGetManagedRuleSet :: GetManagedRuleSetResponse -> TestTree
responseGetManagedRuleSet =
  res
    "GetManagedRuleSetResponse"
    "fixture/GetManagedRuleSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedRuleSet)

responseGetMobileSdkRelease :: GetMobileSdkReleaseResponse -> TestTree
responseGetMobileSdkRelease =
  res
    "GetMobileSdkReleaseResponse"
    "fixture/GetMobileSdkReleaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMobileSdkRelease)

responseGetPermissionPolicy :: GetPermissionPolicyResponse -> TestTree
responseGetPermissionPolicy =
  res
    "GetPermissionPolicyResponse"
    "fixture/GetPermissionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPermissionPolicy)

responseGetRateBasedStatementManagedKeys :: GetRateBasedStatementManagedKeysResponse -> TestTree
responseGetRateBasedStatementManagedKeys =
  res
    "GetRateBasedStatementManagedKeysResponse"
    "fixture/GetRateBasedStatementManagedKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRateBasedStatementManagedKeys)

responseGetRegexPatternSet :: GetRegexPatternSetResponse -> TestTree
responseGetRegexPatternSet =
  res
    "GetRegexPatternSetResponse"
    "fixture/GetRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegexPatternSet)

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

responseGetWebACL :: GetWebACLResponse -> TestTree
responseGetWebACL =
  res
    "GetWebACLResponse"
    "fixture/GetWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWebACL)

responseGetWebACLForResource :: GetWebACLForResourceResponse -> TestTree
responseGetWebACLForResource =
  res
    "GetWebACLForResourceResponse"
    "fixture/GetWebACLForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWebACLForResource)

responseListAvailableManagedRuleGroupVersions :: ListAvailableManagedRuleGroupVersionsResponse -> TestTree
responseListAvailableManagedRuleGroupVersions =
  res
    "ListAvailableManagedRuleGroupVersionsResponse"
    "fixture/ListAvailableManagedRuleGroupVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableManagedRuleGroupVersions)

responseListAvailableManagedRuleGroups :: ListAvailableManagedRuleGroupsResponse -> TestTree
responseListAvailableManagedRuleGroups =
  res
    "ListAvailableManagedRuleGroupsResponse"
    "fixture/ListAvailableManagedRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableManagedRuleGroups)

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

responseListManagedRuleSets :: ListManagedRuleSetsResponse -> TestTree
responseListManagedRuleSets =
  res
    "ListManagedRuleSetsResponse"
    "fixture/ListManagedRuleSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedRuleSets)

responseListMobileSdkReleases :: ListMobileSdkReleasesResponse -> TestTree
responseListMobileSdkReleases =
  res
    "ListMobileSdkReleasesResponse"
    "fixture/ListMobileSdkReleasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMobileSdkReleases)

responseListRegexPatternSets :: ListRegexPatternSetsResponse -> TestTree
responseListRegexPatternSets =
  res
    "ListRegexPatternSetsResponse"
    "fixture/ListRegexPatternSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRegexPatternSets)

responseListResourcesForWebACL :: ListResourcesForWebACLResponse -> TestTree
responseListResourcesForWebACL =
  res
    "ListResourcesForWebACLResponse"
    "fixture/ListResourcesForWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcesForWebACL)

responseListRuleGroups :: ListRuleGroupsResponse -> TestTree
responseListRuleGroups =
  res
    "ListRuleGroupsResponse"
    "fixture/ListRuleGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleGroups)

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

responsePutLoggingConfiguration :: PutLoggingConfigurationResponse -> TestTree
responsePutLoggingConfiguration =
  res
    "PutLoggingConfigurationResponse"
    "fixture/PutLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLoggingConfiguration)

responsePutManagedRuleSetVersions :: PutManagedRuleSetVersionsResponse -> TestTree
responsePutManagedRuleSetVersions =
  res
    "PutManagedRuleSetVersionsResponse"
    "fixture/PutManagedRuleSetVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutManagedRuleSetVersions)

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

responseUpdateIPSet :: UpdateIPSetResponse -> TestTree
responseUpdateIPSet =
  res
    "UpdateIPSetResponse"
    "fixture/UpdateIPSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIPSet)

responseUpdateManagedRuleSetVersionExpiryDate :: UpdateManagedRuleSetVersionExpiryDateResponse -> TestTree
responseUpdateManagedRuleSetVersionExpiryDate =
  res
    "UpdateManagedRuleSetVersionExpiryDateResponse"
    "fixture/UpdateManagedRuleSetVersionExpiryDateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateManagedRuleSetVersionExpiryDate)

responseUpdateRegexPatternSet :: UpdateRegexPatternSetResponse -> TestTree
responseUpdateRegexPatternSet =
  res
    "UpdateRegexPatternSetResponse"
    "fixture/UpdateRegexPatternSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRegexPatternSet)

responseUpdateRuleGroup :: UpdateRuleGroupResponse -> TestTree
responseUpdateRuleGroup =
  res
    "UpdateRuleGroupResponse"
    "fixture/UpdateRuleGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRuleGroup)

responseUpdateWebACL :: UpdateWebACLResponse -> TestTree
responseUpdateWebACL =
  res
    "UpdateWebACLResponse"
    "fixture/UpdateWebACLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWebACL)

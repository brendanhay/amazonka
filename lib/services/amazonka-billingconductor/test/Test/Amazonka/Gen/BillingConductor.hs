{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.BillingConductor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.BillingConductor where

import Amazonka.BillingConductor
import qualified Data.Proxy as Proxy
import Test.Amazonka.BillingConductor.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateAccounts $
--             newAssociateAccounts
--
--         , requestAssociatePricingRules $
--             newAssociatePricingRules
--
--         , requestBatchAssociateResourcesToCustomLineItem $
--             newBatchAssociateResourcesToCustomLineItem
--
--         , requestBatchDisassociateResourcesFromCustomLineItem $
--             newBatchDisassociateResourcesFromCustomLineItem
--
--         , requestCreateBillingGroup $
--             newCreateBillingGroup
--
--         , requestCreateCustomLineItem $
--             newCreateCustomLineItem
--
--         , requestCreatePricingPlan $
--             newCreatePricingPlan
--
--         , requestCreatePricingRule $
--             newCreatePricingRule
--
--         , requestDeleteBillingGroup $
--             newDeleteBillingGroup
--
--         , requestDeleteCustomLineItem $
--             newDeleteCustomLineItem
--
--         , requestDeletePricingPlan $
--             newDeletePricingPlan
--
--         , requestDeletePricingRule $
--             newDeletePricingRule
--
--         , requestDisassociateAccounts $
--             newDisassociateAccounts
--
--         , requestDisassociatePricingRules $
--             newDisassociatePricingRules
--
--         , requestListAccountAssociations $
--             newListAccountAssociations
--
--         , requestListBillingGroupCostReports $
--             newListBillingGroupCostReports
--
--         , requestListBillingGroups $
--             newListBillingGroups
--
--         , requestListCustomLineItemVersions $
--             newListCustomLineItemVersions
--
--         , requestListCustomLineItems $
--             newListCustomLineItems
--
--         , requestListPricingPlans $
--             newListPricingPlans
--
--         , requestListPricingPlansAssociatedWithPricingRule $
--             newListPricingPlansAssociatedWithPricingRule
--
--         , requestListPricingRules $
--             newListPricingRules
--
--         , requestListPricingRulesAssociatedToPricingPlan $
--             newListPricingRulesAssociatedToPricingPlan
--
--         , requestListResourcesAssociatedToCustomLineItem $
--             newListResourcesAssociatedToCustomLineItem
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
--         , requestUpdateBillingGroup $
--             newUpdateBillingGroup
--
--         , requestUpdateCustomLineItem $
--             newUpdateCustomLineItem
--
--         , requestUpdatePricingPlan $
--             newUpdatePricingPlan
--
--         , requestUpdatePricingRule $
--             newUpdatePricingRule
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAccounts $
--             newAssociateAccountsResponse
--
--         , responseAssociatePricingRules $
--             newAssociatePricingRulesResponse
--
--         , responseBatchAssociateResourcesToCustomLineItem $
--             newBatchAssociateResourcesToCustomLineItemResponse
--
--         , responseBatchDisassociateResourcesFromCustomLineItem $
--             newBatchDisassociateResourcesFromCustomLineItemResponse
--
--         , responseCreateBillingGroup $
--             newCreateBillingGroupResponse
--
--         , responseCreateCustomLineItem $
--             newCreateCustomLineItemResponse
--
--         , responseCreatePricingPlan $
--             newCreatePricingPlanResponse
--
--         , responseCreatePricingRule $
--             newCreatePricingRuleResponse
--
--         , responseDeleteBillingGroup $
--             newDeleteBillingGroupResponse
--
--         , responseDeleteCustomLineItem $
--             newDeleteCustomLineItemResponse
--
--         , responseDeletePricingPlan $
--             newDeletePricingPlanResponse
--
--         , responseDeletePricingRule $
--             newDeletePricingRuleResponse
--
--         , responseDisassociateAccounts $
--             newDisassociateAccountsResponse
--
--         , responseDisassociatePricingRules $
--             newDisassociatePricingRulesResponse
--
--         , responseListAccountAssociations $
--             newListAccountAssociationsResponse
--
--         , responseListBillingGroupCostReports $
--             newListBillingGroupCostReportsResponse
--
--         , responseListBillingGroups $
--             newListBillingGroupsResponse
--
--         , responseListCustomLineItemVersions $
--             newListCustomLineItemVersionsResponse
--
--         , responseListCustomLineItems $
--             newListCustomLineItemsResponse
--
--         , responseListPricingPlans $
--             newListPricingPlansResponse
--
--         , responseListPricingPlansAssociatedWithPricingRule $
--             newListPricingPlansAssociatedWithPricingRuleResponse
--
--         , responseListPricingRules $
--             newListPricingRulesResponse
--
--         , responseListPricingRulesAssociatedToPricingPlan $
--             newListPricingRulesAssociatedToPricingPlanResponse
--
--         , responseListResourcesAssociatedToCustomLineItem $
--             newListResourcesAssociatedToCustomLineItemResponse
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
--         , responseUpdateBillingGroup $
--             newUpdateBillingGroupResponse
--
--         , responseUpdateCustomLineItem $
--             newUpdateCustomLineItemResponse
--
--         , responseUpdatePricingPlan $
--             newUpdatePricingPlanResponse
--
--         , responseUpdatePricingRule $
--             newUpdatePricingRuleResponse
--
--           ]
--     ]

-- Requests

requestAssociateAccounts :: AssociateAccounts -> TestTree
requestAssociateAccounts =
  req
    "AssociateAccounts"
    "fixture/AssociateAccounts.yaml"

requestAssociatePricingRules :: AssociatePricingRules -> TestTree
requestAssociatePricingRules =
  req
    "AssociatePricingRules"
    "fixture/AssociatePricingRules.yaml"

requestBatchAssociateResourcesToCustomLineItem :: BatchAssociateResourcesToCustomLineItem -> TestTree
requestBatchAssociateResourcesToCustomLineItem =
  req
    "BatchAssociateResourcesToCustomLineItem"
    "fixture/BatchAssociateResourcesToCustomLineItem.yaml"

requestBatchDisassociateResourcesFromCustomLineItem :: BatchDisassociateResourcesFromCustomLineItem -> TestTree
requestBatchDisassociateResourcesFromCustomLineItem =
  req
    "BatchDisassociateResourcesFromCustomLineItem"
    "fixture/BatchDisassociateResourcesFromCustomLineItem.yaml"

requestCreateBillingGroup :: CreateBillingGroup -> TestTree
requestCreateBillingGroup =
  req
    "CreateBillingGroup"
    "fixture/CreateBillingGroup.yaml"

requestCreateCustomLineItem :: CreateCustomLineItem -> TestTree
requestCreateCustomLineItem =
  req
    "CreateCustomLineItem"
    "fixture/CreateCustomLineItem.yaml"

requestCreatePricingPlan :: CreatePricingPlan -> TestTree
requestCreatePricingPlan =
  req
    "CreatePricingPlan"
    "fixture/CreatePricingPlan.yaml"

requestCreatePricingRule :: CreatePricingRule -> TestTree
requestCreatePricingRule =
  req
    "CreatePricingRule"
    "fixture/CreatePricingRule.yaml"

requestDeleteBillingGroup :: DeleteBillingGroup -> TestTree
requestDeleteBillingGroup =
  req
    "DeleteBillingGroup"
    "fixture/DeleteBillingGroup.yaml"

requestDeleteCustomLineItem :: DeleteCustomLineItem -> TestTree
requestDeleteCustomLineItem =
  req
    "DeleteCustomLineItem"
    "fixture/DeleteCustomLineItem.yaml"

requestDeletePricingPlan :: DeletePricingPlan -> TestTree
requestDeletePricingPlan =
  req
    "DeletePricingPlan"
    "fixture/DeletePricingPlan.yaml"

requestDeletePricingRule :: DeletePricingRule -> TestTree
requestDeletePricingRule =
  req
    "DeletePricingRule"
    "fixture/DeletePricingRule.yaml"

requestDisassociateAccounts :: DisassociateAccounts -> TestTree
requestDisassociateAccounts =
  req
    "DisassociateAccounts"
    "fixture/DisassociateAccounts.yaml"

requestDisassociatePricingRules :: DisassociatePricingRules -> TestTree
requestDisassociatePricingRules =
  req
    "DisassociatePricingRules"
    "fixture/DisassociatePricingRules.yaml"

requestListAccountAssociations :: ListAccountAssociations -> TestTree
requestListAccountAssociations =
  req
    "ListAccountAssociations"
    "fixture/ListAccountAssociations.yaml"

requestListBillingGroupCostReports :: ListBillingGroupCostReports -> TestTree
requestListBillingGroupCostReports =
  req
    "ListBillingGroupCostReports"
    "fixture/ListBillingGroupCostReports.yaml"

requestListBillingGroups :: ListBillingGroups -> TestTree
requestListBillingGroups =
  req
    "ListBillingGroups"
    "fixture/ListBillingGroups.yaml"

requestListCustomLineItemVersions :: ListCustomLineItemVersions -> TestTree
requestListCustomLineItemVersions =
  req
    "ListCustomLineItemVersions"
    "fixture/ListCustomLineItemVersions.yaml"

requestListCustomLineItems :: ListCustomLineItems -> TestTree
requestListCustomLineItems =
  req
    "ListCustomLineItems"
    "fixture/ListCustomLineItems.yaml"

requestListPricingPlans :: ListPricingPlans -> TestTree
requestListPricingPlans =
  req
    "ListPricingPlans"
    "fixture/ListPricingPlans.yaml"

requestListPricingPlansAssociatedWithPricingRule :: ListPricingPlansAssociatedWithPricingRule -> TestTree
requestListPricingPlansAssociatedWithPricingRule =
  req
    "ListPricingPlansAssociatedWithPricingRule"
    "fixture/ListPricingPlansAssociatedWithPricingRule.yaml"

requestListPricingRules :: ListPricingRules -> TestTree
requestListPricingRules =
  req
    "ListPricingRules"
    "fixture/ListPricingRules.yaml"

requestListPricingRulesAssociatedToPricingPlan :: ListPricingRulesAssociatedToPricingPlan -> TestTree
requestListPricingRulesAssociatedToPricingPlan =
  req
    "ListPricingRulesAssociatedToPricingPlan"
    "fixture/ListPricingRulesAssociatedToPricingPlan.yaml"

requestListResourcesAssociatedToCustomLineItem :: ListResourcesAssociatedToCustomLineItem -> TestTree
requestListResourcesAssociatedToCustomLineItem =
  req
    "ListResourcesAssociatedToCustomLineItem"
    "fixture/ListResourcesAssociatedToCustomLineItem.yaml"

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

requestUpdateBillingGroup :: UpdateBillingGroup -> TestTree
requestUpdateBillingGroup =
  req
    "UpdateBillingGroup"
    "fixture/UpdateBillingGroup.yaml"

requestUpdateCustomLineItem :: UpdateCustomLineItem -> TestTree
requestUpdateCustomLineItem =
  req
    "UpdateCustomLineItem"
    "fixture/UpdateCustomLineItem.yaml"

requestUpdatePricingPlan :: UpdatePricingPlan -> TestTree
requestUpdatePricingPlan =
  req
    "UpdatePricingPlan"
    "fixture/UpdatePricingPlan.yaml"

requestUpdatePricingRule :: UpdatePricingRule -> TestTree
requestUpdatePricingRule =
  req
    "UpdatePricingRule"
    "fixture/UpdatePricingRule.yaml"

-- Responses

responseAssociateAccounts :: AssociateAccountsResponse -> TestTree
responseAssociateAccounts =
  res
    "AssociateAccountsResponse"
    "fixture/AssociateAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAccounts)

responseAssociatePricingRules :: AssociatePricingRulesResponse -> TestTree
responseAssociatePricingRules =
  res
    "AssociatePricingRulesResponse"
    "fixture/AssociatePricingRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePricingRules)

responseBatchAssociateResourcesToCustomLineItem :: BatchAssociateResourcesToCustomLineItemResponse -> TestTree
responseBatchAssociateResourcesToCustomLineItem =
  res
    "BatchAssociateResourcesToCustomLineItemResponse"
    "fixture/BatchAssociateResourcesToCustomLineItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateResourcesToCustomLineItem)

responseBatchDisassociateResourcesFromCustomLineItem :: BatchDisassociateResourcesFromCustomLineItemResponse -> TestTree
responseBatchDisassociateResourcesFromCustomLineItem =
  res
    "BatchDisassociateResourcesFromCustomLineItemResponse"
    "fixture/BatchDisassociateResourcesFromCustomLineItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateResourcesFromCustomLineItem)

responseCreateBillingGroup :: CreateBillingGroupResponse -> TestTree
responseCreateBillingGroup =
  res
    "CreateBillingGroupResponse"
    "fixture/CreateBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBillingGroup)

responseCreateCustomLineItem :: CreateCustomLineItemResponse -> TestTree
responseCreateCustomLineItem =
  res
    "CreateCustomLineItemResponse"
    "fixture/CreateCustomLineItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCustomLineItem)

responseCreatePricingPlan :: CreatePricingPlanResponse -> TestTree
responseCreatePricingPlan =
  res
    "CreatePricingPlanResponse"
    "fixture/CreatePricingPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePricingPlan)

responseCreatePricingRule :: CreatePricingRuleResponse -> TestTree
responseCreatePricingRule =
  res
    "CreatePricingRuleResponse"
    "fixture/CreatePricingRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePricingRule)

responseDeleteBillingGroup :: DeleteBillingGroupResponse -> TestTree
responseDeleteBillingGroup =
  res
    "DeleteBillingGroupResponse"
    "fixture/DeleteBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBillingGroup)

responseDeleteCustomLineItem :: DeleteCustomLineItemResponse -> TestTree
responseDeleteCustomLineItem =
  res
    "DeleteCustomLineItemResponse"
    "fixture/DeleteCustomLineItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomLineItem)

responseDeletePricingPlan :: DeletePricingPlanResponse -> TestTree
responseDeletePricingPlan =
  res
    "DeletePricingPlanResponse"
    "fixture/DeletePricingPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePricingPlan)

responseDeletePricingRule :: DeletePricingRuleResponse -> TestTree
responseDeletePricingRule =
  res
    "DeletePricingRuleResponse"
    "fixture/DeletePricingRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePricingRule)

responseDisassociateAccounts :: DisassociateAccountsResponse -> TestTree
responseDisassociateAccounts =
  res
    "DisassociateAccountsResponse"
    "fixture/DisassociateAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAccounts)

responseDisassociatePricingRules :: DisassociatePricingRulesResponse -> TestTree
responseDisassociatePricingRules =
  res
    "DisassociatePricingRulesResponse"
    "fixture/DisassociatePricingRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePricingRules)

responseListAccountAssociations :: ListAccountAssociationsResponse -> TestTree
responseListAccountAssociations =
  res
    "ListAccountAssociationsResponse"
    "fixture/ListAccountAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAssociations)

responseListBillingGroupCostReports :: ListBillingGroupCostReportsResponse -> TestTree
responseListBillingGroupCostReports =
  res
    "ListBillingGroupCostReportsResponse"
    "fixture/ListBillingGroupCostReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBillingGroupCostReports)

responseListBillingGroups :: ListBillingGroupsResponse -> TestTree
responseListBillingGroups =
  res
    "ListBillingGroupsResponse"
    "fixture/ListBillingGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBillingGroups)

responseListCustomLineItemVersions :: ListCustomLineItemVersionsResponse -> TestTree
responseListCustomLineItemVersions =
  res
    "ListCustomLineItemVersionsResponse"
    "fixture/ListCustomLineItemVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomLineItemVersions)

responseListCustomLineItems :: ListCustomLineItemsResponse -> TestTree
responseListCustomLineItems =
  res
    "ListCustomLineItemsResponse"
    "fixture/ListCustomLineItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomLineItems)

responseListPricingPlans :: ListPricingPlansResponse -> TestTree
responseListPricingPlans =
  res
    "ListPricingPlansResponse"
    "fixture/ListPricingPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPricingPlans)

responseListPricingPlansAssociatedWithPricingRule :: ListPricingPlansAssociatedWithPricingRuleResponse -> TestTree
responseListPricingPlansAssociatedWithPricingRule =
  res
    "ListPricingPlansAssociatedWithPricingRuleResponse"
    "fixture/ListPricingPlansAssociatedWithPricingRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPricingPlansAssociatedWithPricingRule)

responseListPricingRules :: ListPricingRulesResponse -> TestTree
responseListPricingRules =
  res
    "ListPricingRulesResponse"
    "fixture/ListPricingRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPricingRules)

responseListPricingRulesAssociatedToPricingPlan :: ListPricingRulesAssociatedToPricingPlanResponse -> TestTree
responseListPricingRulesAssociatedToPricingPlan =
  res
    "ListPricingRulesAssociatedToPricingPlanResponse"
    "fixture/ListPricingRulesAssociatedToPricingPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPricingRulesAssociatedToPricingPlan)

responseListResourcesAssociatedToCustomLineItem :: ListResourcesAssociatedToCustomLineItemResponse -> TestTree
responseListResourcesAssociatedToCustomLineItem =
  res
    "ListResourcesAssociatedToCustomLineItemResponse"
    "fixture/ListResourcesAssociatedToCustomLineItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourcesAssociatedToCustomLineItem)

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

responseUpdateBillingGroup :: UpdateBillingGroupResponse -> TestTree
responseUpdateBillingGroup =
  res
    "UpdateBillingGroupResponse"
    "fixture/UpdateBillingGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBillingGroup)

responseUpdateCustomLineItem :: UpdateCustomLineItemResponse -> TestTree
responseUpdateCustomLineItem =
  res
    "UpdateCustomLineItemResponse"
    "fixture/UpdateCustomLineItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCustomLineItem)

responseUpdatePricingPlan :: UpdatePricingPlanResponse -> TestTree
responseUpdatePricingPlan =
  res
    "UpdatePricingPlanResponse"
    "fixture/UpdatePricingPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePricingPlan)

responseUpdatePricingRule :: UpdatePricingRuleResponse -> TestTree
responseUpdatePricingRule =
  res
    "UpdatePricingRuleResponse"
    "fixture/UpdatePricingRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePricingRule)

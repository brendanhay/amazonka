{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSOAdmin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SSOAdmin where

import Amazonka.SSOAdmin
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SSOAdmin.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAttachCustomerManagedPolicyReferenceToPermissionSet $
--             newAttachCustomerManagedPolicyReferenceToPermissionSet
--
--         , requestAttachManagedPolicyToPermissionSet $
--             newAttachManagedPolicyToPermissionSet
--
--         , requestCreateAccountAssignment $
--             newCreateAccountAssignment
--
--         , requestCreateInstanceAccessControlAttributeConfiguration $
--             newCreateInstanceAccessControlAttributeConfiguration
--
--         , requestCreatePermissionSet $
--             newCreatePermissionSet
--
--         , requestDeleteAccountAssignment $
--             newDeleteAccountAssignment
--
--         , requestDeleteInlinePolicyFromPermissionSet $
--             newDeleteInlinePolicyFromPermissionSet
--
--         , requestDeleteInstanceAccessControlAttributeConfiguration $
--             newDeleteInstanceAccessControlAttributeConfiguration
--
--         , requestDeletePermissionSet $
--             newDeletePermissionSet
--
--         , requestDeletePermissionsBoundaryFromPermissionSet $
--             newDeletePermissionsBoundaryFromPermissionSet
--
--         , requestDescribeAccountAssignmentCreationStatus $
--             newDescribeAccountAssignmentCreationStatus
--
--         , requestDescribeAccountAssignmentDeletionStatus $
--             newDescribeAccountAssignmentDeletionStatus
--
--         , requestDescribeInstanceAccessControlAttributeConfiguration $
--             newDescribeInstanceAccessControlAttributeConfiguration
--
--         , requestDescribePermissionSet $
--             newDescribePermissionSet
--
--         , requestDescribePermissionSetProvisioningStatus $
--             newDescribePermissionSetProvisioningStatus
--
--         , requestDetachCustomerManagedPolicyReferenceFromPermissionSet $
--             newDetachCustomerManagedPolicyReferenceFromPermissionSet
--
--         , requestDetachManagedPolicyFromPermissionSet $
--             newDetachManagedPolicyFromPermissionSet
--
--         , requestGetInlinePolicyForPermissionSet $
--             newGetInlinePolicyForPermissionSet
--
--         , requestGetPermissionsBoundaryForPermissionSet $
--             newGetPermissionsBoundaryForPermissionSet
--
--         , requestListAccountAssignmentCreationStatus $
--             newListAccountAssignmentCreationStatus
--
--         , requestListAccountAssignmentDeletionStatus $
--             newListAccountAssignmentDeletionStatus
--
--         , requestListAccountAssignments $
--             newListAccountAssignments
--
--         , requestListAccountsForProvisionedPermissionSet $
--             newListAccountsForProvisionedPermissionSet
--
--         , requestListCustomerManagedPolicyReferencesInPermissionSet $
--             newListCustomerManagedPolicyReferencesInPermissionSet
--
--         , requestListInstances $
--             newListInstances
--
--         , requestListManagedPoliciesInPermissionSet $
--             newListManagedPoliciesInPermissionSet
--
--         , requestListPermissionSetProvisioningStatus $
--             newListPermissionSetProvisioningStatus
--
--         , requestListPermissionSets $
--             newListPermissionSets
--
--         , requestListPermissionSetsProvisionedToAccount $
--             newListPermissionSetsProvisionedToAccount
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestProvisionPermissionSet $
--             newProvisionPermissionSet
--
--         , requestPutInlinePolicyToPermissionSet $
--             newPutInlinePolicyToPermissionSet
--
--         , requestPutPermissionsBoundaryToPermissionSet $
--             newPutPermissionsBoundaryToPermissionSet
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateInstanceAccessControlAttributeConfiguration $
--             newUpdateInstanceAccessControlAttributeConfiguration
--
--         , requestUpdatePermissionSet $
--             newUpdatePermissionSet
--
--           ]

--     , testGroup "response"
--         [ responseAttachCustomerManagedPolicyReferenceToPermissionSet $
--             newAttachCustomerManagedPolicyReferenceToPermissionSetResponse
--
--         , responseAttachManagedPolicyToPermissionSet $
--             newAttachManagedPolicyToPermissionSetResponse
--
--         , responseCreateAccountAssignment $
--             newCreateAccountAssignmentResponse
--
--         , responseCreateInstanceAccessControlAttributeConfiguration $
--             newCreateInstanceAccessControlAttributeConfigurationResponse
--
--         , responseCreatePermissionSet $
--             newCreatePermissionSetResponse
--
--         , responseDeleteAccountAssignment $
--             newDeleteAccountAssignmentResponse
--
--         , responseDeleteInlinePolicyFromPermissionSet $
--             newDeleteInlinePolicyFromPermissionSetResponse
--
--         , responseDeleteInstanceAccessControlAttributeConfiguration $
--             newDeleteInstanceAccessControlAttributeConfigurationResponse
--
--         , responseDeletePermissionSet $
--             newDeletePermissionSetResponse
--
--         , responseDeletePermissionsBoundaryFromPermissionSet $
--             newDeletePermissionsBoundaryFromPermissionSetResponse
--
--         , responseDescribeAccountAssignmentCreationStatus $
--             newDescribeAccountAssignmentCreationStatusResponse
--
--         , responseDescribeAccountAssignmentDeletionStatus $
--             newDescribeAccountAssignmentDeletionStatusResponse
--
--         , responseDescribeInstanceAccessControlAttributeConfiguration $
--             newDescribeInstanceAccessControlAttributeConfigurationResponse
--
--         , responseDescribePermissionSet $
--             newDescribePermissionSetResponse
--
--         , responseDescribePermissionSetProvisioningStatus $
--             newDescribePermissionSetProvisioningStatusResponse
--
--         , responseDetachCustomerManagedPolicyReferenceFromPermissionSet $
--             newDetachCustomerManagedPolicyReferenceFromPermissionSetResponse
--
--         , responseDetachManagedPolicyFromPermissionSet $
--             newDetachManagedPolicyFromPermissionSetResponse
--
--         , responseGetInlinePolicyForPermissionSet $
--             newGetInlinePolicyForPermissionSetResponse
--
--         , responseGetPermissionsBoundaryForPermissionSet $
--             newGetPermissionsBoundaryForPermissionSetResponse
--
--         , responseListAccountAssignmentCreationStatus $
--             newListAccountAssignmentCreationStatusResponse
--
--         , responseListAccountAssignmentDeletionStatus $
--             newListAccountAssignmentDeletionStatusResponse
--
--         , responseListAccountAssignments $
--             newListAccountAssignmentsResponse
--
--         , responseListAccountsForProvisionedPermissionSet $
--             newListAccountsForProvisionedPermissionSetResponse
--
--         , responseListCustomerManagedPolicyReferencesInPermissionSet $
--             newListCustomerManagedPolicyReferencesInPermissionSetResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseListManagedPoliciesInPermissionSet $
--             newListManagedPoliciesInPermissionSetResponse
--
--         , responseListPermissionSetProvisioningStatus $
--             newListPermissionSetProvisioningStatusResponse
--
--         , responseListPermissionSets $
--             newListPermissionSetsResponse
--
--         , responseListPermissionSetsProvisionedToAccount $
--             newListPermissionSetsProvisionedToAccountResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseProvisionPermissionSet $
--             newProvisionPermissionSetResponse
--
--         , responsePutInlinePolicyToPermissionSet $
--             newPutInlinePolicyToPermissionSetResponse
--
--         , responsePutPermissionsBoundaryToPermissionSet $
--             newPutPermissionsBoundaryToPermissionSetResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateInstanceAccessControlAttributeConfiguration $
--             newUpdateInstanceAccessControlAttributeConfigurationResponse
--
--         , responseUpdatePermissionSet $
--             newUpdatePermissionSetResponse
--
--           ]
--     ]

-- Requests

requestAttachCustomerManagedPolicyReferenceToPermissionSet :: AttachCustomerManagedPolicyReferenceToPermissionSet -> TestTree
requestAttachCustomerManagedPolicyReferenceToPermissionSet =
  req
    "AttachCustomerManagedPolicyReferenceToPermissionSet"
    "fixture/AttachCustomerManagedPolicyReferenceToPermissionSet.yaml"

requestAttachManagedPolicyToPermissionSet :: AttachManagedPolicyToPermissionSet -> TestTree
requestAttachManagedPolicyToPermissionSet =
  req
    "AttachManagedPolicyToPermissionSet"
    "fixture/AttachManagedPolicyToPermissionSet.yaml"

requestCreateAccountAssignment :: CreateAccountAssignment -> TestTree
requestCreateAccountAssignment =
  req
    "CreateAccountAssignment"
    "fixture/CreateAccountAssignment.yaml"

requestCreateInstanceAccessControlAttributeConfiguration :: CreateInstanceAccessControlAttributeConfiguration -> TestTree
requestCreateInstanceAccessControlAttributeConfiguration =
  req
    "CreateInstanceAccessControlAttributeConfiguration"
    "fixture/CreateInstanceAccessControlAttributeConfiguration.yaml"

requestCreatePermissionSet :: CreatePermissionSet -> TestTree
requestCreatePermissionSet =
  req
    "CreatePermissionSet"
    "fixture/CreatePermissionSet.yaml"

requestDeleteAccountAssignment :: DeleteAccountAssignment -> TestTree
requestDeleteAccountAssignment =
  req
    "DeleteAccountAssignment"
    "fixture/DeleteAccountAssignment.yaml"

requestDeleteInlinePolicyFromPermissionSet :: DeleteInlinePolicyFromPermissionSet -> TestTree
requestDeleteInlinePolicyFromPermissionSet =
  req
    "DeleteInlinePolicyFromPermissionSet"
    "fixture/DeleteInlinePolicyFromPermissionSet.yaml"

requestDeleteInstanceAccessControlAttributeConfiguration :: DeleteInstanceAccessControlAttributeConfiguration -> TestTree
requestDeleteInstanceAccessControlAttributeConfiguration =
  req
    "DeleteInstanceAccessControlAttributeConfiguration"
    "fixture/DeleteInstanceAccessControlAttributeConfiguration.yaml"

requestDeletePermissionSet :: DeletePermissionSet -> TestTree
requestDeletePermissionSet =
  req
    "DeletePermissionSet"
    "fixture/DeletePermissionSet.yaml"

requestDeletePermissionsBoundaryFromPermissionSet :: DeletePermissionsBoundaryFromPermissionSet -> TestTree
requestDeletePermissionsBoundaryFromPermissionSet =
  req
    "DeletePermissionsBoundaryFromPermissionSet"
    "fixture/DeletePermissionsBoundaryFromPermissionSet.yaml"

requestDescribeAccountAssignmentCreationStatus :: DescribeAccountAssignmentCreationStatus -> TestTree
requestDescribeAccountAssignmentCreationStatus =
  req
    "DescribeAccountAssignmentCreationStatus"
    "fixture/DescribeAccountAssignmentCreationStatus.yaml"

requestDescribeAccountAssignmentDeletionStatus :: DescribeAccountAssignmentDeletionStatus -> TestTree
requestDescribeAccountAssignmentDeletionStatus =
  req
    "DescribeAccountAssignmentDeletionStatus"
    "fixture/DescribeAccountAssignmentDeletionStatus.yaml"

requestDescribeInstanceAccessControlAttributeConfiguration :: DescribeInstanceAccessControlAttributeConfiguration -> TestTree
requestDescribeInstanceAccessControlAttributeConfiguration =
  req
    "DescribeInstanceAccessControlAttributeConfiguration"
    "fixture/DescribeInstanceAccessControlAttributeConfiguration.yaml"

requestDescribePermissionSet :: DescribePermissionSet -> TestTree
requestDescribePermissionSet =
  req
    "DescribePermissionSet"
    "fixture/DescribePermissionSet.yaml"

requestDescribePermissionSetProvisioningStatus :: DescribePermissionSetProvisioningStatus -> TestTree
requestDescribePermissionSetProvisioningStatus =
  req
    "DescribePermissionSetProvisioningStatus"
    "fixture/DescribePermissionSetProvisioningStatus.yaml"

requestDetachCustomerManagedPolicyReferenceFromPermissionSet :: DetachCustomerManagedPolicyReferenceFromPermissionSet -> TestTree
requestDetachCustomerManagedPolicyReferenceFromPermissionSet =
  req
    "DetachCustomerManagedPolicyReferenceFromPermissionSet"
    "fixture/DetachCustomerManagedPolicyReferenceFromPermissionSet.yaml"

requestDetachManagedPolicyFromPermissionSet :: DetachManagedPolicyFromPermissionSet -> TestTree
requestDetachManagedPolicyFromPermissionSet =
  req
    "DetachManagedPolicyFromPermissionSet"
    "fixture/DetachManagedPolicyFromPermissionSet.yaml"

requestGetInlinePolicyForPermissionSet :: GetInlinePolicyForPermissionSet -> TestTree
requestGetInlinePolicyForPermissionSet =
  req
    "GetInlinePolicyForPermissionSet"
    "fixture/GetInlinePolicyForPermissionSet.yaml"

requestGetPermissionsBoundaryForPermissionSet :: GetPermissionsBoundaryForPermissionSet -> TestTree
requestGetPermissionsBoundaryForPermissionSet =
  req
    "GetPermissionsBoundaryForPermissionSet"
    "fixture/GetPermissionsBoundaryForPermissionSet.yaml"

requestListAccountAssignmentCreationStatus :: ListAccountAssignmentCreationStatus -> TestTree
requestListAccountAssignmentCreationStatus =
  req
    "ListAccountAssignmentCreationStatus"
    "fixture/ListAccountAssignmentCreationStatus.yaml"

requestListAccountAssignmentDeletionStatus :: ListAccountAssignmentDeletionStatus -> TestTree
requestListAccountAssignmentDeletionStatus =
  req
    "ListAccountAssignmentDeletionStatus"
    "fixture/ListAccountAssignmentDeletionStatus.yaml"

requestListAccountAssignments :: ListAccountAssignments -> TestTree
requestListAccountAssignments =
  req
    "ListAccountAssignments"
    "fixture/ListAccountAssignments.yaml"

requestListAccountsForProvisionedPermissionSet :: ListAccountsForProvisionedPermissionSet -> TestTree
requestListAccountsForProvisionedPermissionSet =
  req
    "ListAccountsForProvisionedPermissionSet"
    "fixture/ListAccountsForProvisionedPermissionSet.yaml"

requestListCustomerManagedPolicyReferencesInPermissionSet :: ListCustomerManagedPolicyReferencesInPermissionSet -> TestTree
requestListCustomerManagedPolicyReferencesInPermissionSet =
  req
    "ListCustomerManagedPolicyReferencesInPermissionSet"
    "fixture/ListCustomerManagedPolicyReferencesInPermissionSet.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestListManagedPoliciesInPermissionSet :: ListManagedPoliciesInPermissionSet -> TestTree
requestListManagedPoliciesInPermissionSet =
  req
    "ListManagedPoliciesInPermissionSet"
    "fixture/ListManagedPoliciesInPermissionSet.yaml"

requestListPermissionSetProvisioningStatus :: ListPermissionSetProvisioningStatus -> TestTree
requestListPermissionSetProvisioningStatus =
  req
    "ListPermissionSetProvisioningStatus"
    "fixture/ListPermissionSetProvisioningStatus.yaml"

requestListPermissionSets :: ListPermissionSets -> TestTree
requestListPermissionSets =
  req
    "ListPermissionSets"
    "fixture/ListPermissionSets.yaml"

requestListPermissionSetsProvisionedToAccount :: ListPermissionSetsProvisionedToAccount -> TestTree
requestListPermissionSetsProvisionedToAccount =
  req
    "ListPermissionSetsProvisionedToAccount"
    "fixture/ListPermissionSetsProvisionedToAccount.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestProvisionPermissionSet :: ProvisionPermissionSet -> TestTree
requestProvisionPermissionSet =
  req
    "ProvisionPermissionSet"
    "fixture/ProvisionPermissionSet.yaml"

requestPutInlinePolicyToPermissionSet :: PutInlinePolicyToPermissionSet -> TestTree
requestPutInlinePolicyToPermissionSet =
  req
    "PutInlinePolicyToPermissionSet"
    "fixture/PutInlinePolicyToPermissionSet.yaml"

requestPutPermissionsBoundaryToPermissionSet :: PutPermissionsBoundaryToPermissionSet -> TestTree
requestPutPermissionsBoundaryToPermissionSet =
  req
    "PutPermissionsBoundaryToPermissionSet"
    "fixture/PutPermissionsBoundaryToPermissionSet.yaml"

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

requestUpdateInstanceAccessControlAttributeConfiguration :: UpdateInstanceAccessControlAttributeConfiguration -> TestTree
requestUpdateInstanceAccessControlAttributeConfiguration =
  req
    "UpdateInstanceAccessControlAttributeConfiguration"
    "fixture/UpdateInstanceAccessControlAttributeConfiguration.yaml"

requestUpdatePermissionSet :: UpdatePermissionSet -> TestTree
requestUpdatePermissionSet =
  req
    "UpdatePermissionSet"
    "fixture/UpdatePermissionSet.yaml"

-- Responses

responseAttachCustomerManagedPolicyReferenceToPermissionSet :: AttachCustomerManagedPolicyReferenceToPermissionSetResponse -> TestTree
responseAttachCustomerManagedPolicyReferenceToPermissionSet =
  res
    "AttachCustomerManagedPolicyReferenceToPermissionSetResponse"
    "fixture/AttachCustomerManagedPolicyReferenceToPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachCustomerManagedPolicyReferenceToPermissionSet)

responseAttachManagedPolicyToPermissionSet :: AttachManagedPolicyToPermissionSetResponse -> TestTree
responseAttachManagedPolicyToPermissionSet =
  res
    "AttachManagedPolicyToPermissionSetResponse"
    "fixture/AttachManagedPolicyToPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachManagedPolicyToPermissionSet)

responseCreateAccountAssignment :: CreateAccountAssignmentResponse -> TestTree
responseCreateAccountAssignment =
  res
    "CreateAccountAssignmentResponse"
    "fixture/CreateAccountAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccountAssignment)

responseCreateInstanceAccessControlAttributeConfiguration :: CreateInstanceAccessControlAttributeConfigurationResponse -> TestTree
responseCreateInstanceAccessControlAttributeConfiguration =
  res
    "CreateInstanceAccessControlAttributeConfigurationResponse"
    "fixture/CreateInstanceAccessControlAttributeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceAccessControlAttributeConfiguration)

responseCreatePermissionSet :: CreatePermissionSetResponse -> TestTree
responseCreatePermissionSet =
  res
    "CreatePermissionSetResponse"
    "fixture/CreatePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePermissionSet)

responseDeleteAccountAssignment :: DeleteAccountAssignmentResponse -> TestTree
responseDeleteAccountAssignment =
  res
    "DeleteAccountAssignmentResponse"
    "fixture/DeleteAccountAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountAssignment)

responseDeleteInlinePolicyFromPermissionSet :: DeleteInlinePolicyFromPermissionSetResponse -> TestTree
responseDeleteInlinePolicyFromPermissionSet =
  res
    "DeleteInlinePolicyFromPermissionSetResponse"
    "fixture/DeleteInlinePolicyFromPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInlinePolicyFromPermissionSet)

responseDeleteInstanceAccessControlAttributeConfiguration :: DeleteInstanceAccessControlAttributeConfigurationResponse -> TestTree
responseDeleteInstanceAccessControlAttributeConfiguration =
  res
    "DeleteInstanceAccessControlAttributeConfigurationResponse"
    "fixture/DeleteInstanceAccessControlAttributeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceAccessControlAttributeConfiguration)

responseDeletePermissionSet :: DeletePermissionSetResponse -> TestTree
responseDeletePermissionSet =
  res
    "DeletePermissionSetResponse"
    "fixture/DeletePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermissionSet)

responseDeletePermissionsBoundaryFromPermissionSet :: DeletePermissionsBoundaryFromPermissionSetResponse -> TestTree
responseDeletePermissionsBoundaryFromPermissionSet =
  res
    "DeletePermissionsBoundaryFromPermissionSetResponse"
    "fixture/DeletePermissionsBoundaryFromPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermissionsBoundaryFromPermissionSet)

responseDescribeAccountAssignmentCreationStatus :: DescribeAccountAssignmentCreationStatusResponse -> TestTree
responseDescribeAccountAssignmentCreationStatus =
  res
    "DescribeAccountAssignmentCreationStatusResponse"
    "fixture/DescribeAccountAssignmentCreationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAssignmentCreationStatus)

responseDescribeAccountAssignmentDeletionStatus :: DescribeAccountAssignmentDeletionStatusResponse -> TestTree
responseDescribeAccountAssignmentDeletionStatus =
  res
    "DescribeAccountAssignmentDeletionStatusResponse"
    "fixture/DescribeAccountAssignmentDeletionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAssignmentDeletionStatus)

responseDescribeInstanceAccessControlAttributeConfiguration :: DescribeInstanceAccessControlAttributeConfigurationResponse -> TestTree
responseDescribeInstanceAccessControlAttributeConfiguration =
  res
    "DescribeInstanceAccessControlAttributeConfigurationResponse"
    "fixture/DescribeInstanceAccessControlAttributeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAccessControlAttributeConfiguration)

responseDescribePermissionSet :: DescribePermissionSetResponse -> TestTree
responseDescribePermissionSet =
  res
    "DescribePermissionSetResponse"
    "fixture/DescribePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePermissionSet)

responseDescribePermissionSetProvisioningStatus :: DescribePermissionSetProvisioningStatusResponse -> TestTree
responseDescribePermissionSetProvisioningStatus =
  res
    "DescribePermissionSetProvisioningStatusResponse"
    "fixture/DescribePermissionSetProvisioningStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePermissionSetProvisioningStatus)

responseDetachCustomerManagedPolicyReferenceFromPermissionSet :: DetachCustomerManagedPolicyReferenceFromPermissionSetResponse -> TestTree
responseDetachCustomerManagedPolicyReferenceFromPermissionSet =
  res
    "DetachCustomerManagedPolicyReferenceFromPermissionSetResponse"
    "fixture/DetachCustomerManagedPolicyReferenceFromPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachCustomerManagedPolicyReferenceFromPermissionSet)

responseDetachManagedPolicyFromPermissionSet :: DetachManagedPolicyFromPermissionSetResponse -> TestTree
responseDetachManagedPolicyFromPermissionSet =
  res
    "DetachManagedPolicyFromPermissionSetResponse"
    "fixture/DetachManagedPolicyFromPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachManagedPolicyFromPermissionSet)

responseGetInlinePolicyForPermissionSet :: GetInlinePolicyForPermissionSetResponse -> TestTree
responseGetInlinePolicyForPermissionSet =
  res
    "GetInlinePolicyForPermissionSetResponse"
    "fixture/GetInlinePolicyForPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInlinePolicyForPermissionSet)

responseGetPermissionsBoundaryForPermissionSet :: GetPermissionsBoundaryForPermissionSetResponse -> TestTree
responseGetPermissionsBoundaryForPermissionSet =
  res
    "GetPermissionsBoundaryForPermissionSetResponse"
    "fixture/GetPermissionsBoundaryForPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPermissionsBoundaryForPermissionSet)

responseListAccountAssignmentCreationStatus :: ListAccountAssignmentCreationStatusResponse -> TestTree
responseListAccountAssignmentCreationStatus =
  res
    "ListAccountAssignmentCreationStatusResponse"
    "fixture/ListAccountAssignmentCreationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAssignmentCreationStatus)

responseListAccountAssignmentDeletionStatus :: ListAccountAssignmentDeletionStatusResponse -> TestTree
responseListAccountAssignmentDeletionStatus =
  res
    "ListAccountAssignmentDeletionStatusResponse"
    "fixture/ListAccountAssignmentDeletionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAssignmentDeletionStatus)

responseListAccountAssignments :: ListAccountAssignmentsResponse -> TestTree
responseListAccountAssignments =
  res
    "ListAccountAssignmentsResponse"
    "fixture/ListAccountAssignmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAssignments)

responseListAccountsForProvisionedPermissionSet :: ListAccountsForProvisionedPermissionSetResponse -> TestTree
responseListAccountsForProvisionedPermissionSet =
  res
    "ListAccountsForProvisionedPermissionSetResponse"
    "fixture/ListAccountsForProvisionedPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountsForProvisionedPermissionSet)

responseListCustomerManagedPolicyReferencesInPermissionSet :: ListCustomerManagedPolicyReferencesInPermissionSetResponse -> TestTree
responseListCustomerManagedPolicyReferencesInPermissionSet =
  res
    "ListCustomerManagedPolicyReferencesInPermissionSetResponse"
    "fixture/ListCustomerManagedPolicyReferencesInPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomerManagedPolicyReferencesInPermissionSet)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseListManagedPoliciesInPermissionSet :: ListManagedPoliciesInPermissionSetResponse -> TestTree
responseListManagedPoliciesInPermissionSet =
  res
    "ListManagedPoliciesInPermissionSetResponse"
    "fixture/ListManagedPoliciesInPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedPoliciesInPermissionSet)

responseListPermissionSetProvisioningStatus :: ListPermissionSetProvisioningStatusResponse -> TestTree
responseListPermissionSetProvisioningStatus =
  res
    "ListPermissionSetProvisioningStatusResponse"
    "fixture/ListPermissionSetProvisioningStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionSetProvisioningStatus)

responseListPermissionSets :: ListPermissionSetsResponse -> TestTree
responseListPermissionSets =
  res
    "ListPermissionSetsResponse"
    "fixture/ListPermissionSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionSets)

responseListPermissionSetsProvisionedToAccount :: ListPermissionSetsProvisionedToAccountResponse -> TestTree
responseListPermissionSetsProvisionedToAccount =
  res
    "ListPermissionSetsProvisionedToAccountResponse"
    "fixture/ListPermissionSetsProvisionedToAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionSetsProvisionedToAccount)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseProvisionPermissionSet :: ProvisionPermissionSetResponse -> TestTree
responseProvisionPermissionSet =
  res
    "ProvisionPermissionSetResponse"
    "fixture/ProvisionPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionPermissionSet)

responsePutInlinePolicyToPermissionSet :: PutInlinePolicyToPermissionSetResponse -> TestTree
responsePutInlinePolicyToPermissionSet =
  res
    "PutInlinePolicyToPermissionSetResponse"
    "fixture/PutInlinePolicyToPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInlinePolicyToPermissionSet)

responsePutPermissionsBoundaryToPermissionSet :: PutPermissionsBoundaryToPermissionSetResponse -> TestTree
responsePutPermissionsBoundaryToPermissionSet =
  res
    "PutPermissionsBoundaryToPermissionSetResponse"
    "fixture/PutPermissionsBoundaryToPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPermissionsBoundaryToPermissionSet)

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

responseUpdateInstanceAccessControlAttributeConfiguration :: UpdateInstanceAccessControlAttributeConfigurationResponse -> TestTree
responseUpdateInstanceAccessControlAttributeConfiguration =
  res
    "UpdateInstanceAccessControlAttributeConfigurationResponse"
    "fixture/UpdateInstanceAccessControlAttributeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceAccessControlAttributeConfiguration)

responseUpdatePermissionSet :: UpdatePermissionSetResponse -> TestTree
responseUpdatePermissionSet =
  res
    "UpdatePermissionSetResponse"
    "fixture/UpdatePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePermissionSet)

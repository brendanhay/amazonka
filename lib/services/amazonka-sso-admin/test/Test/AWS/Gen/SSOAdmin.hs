{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSOAdmin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SSOAdmin where

import qualified Data.Proxy as Proxy
import Network.AWS.SSOAdmin
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SSOAdmin.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribePermissionSet $
--             newDescribePermissionSet
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateInstanceAccessControlAttributeConfiguration $
--             newCreateInstanceAccessControlAttributeConfiguration
--
--         , requestGetInlinePolicyForPermissionSet $
--             newGetInlinePolicyForPermissionSet
--
--         , requestListPermissionSets $
--             newListPermissionSets
--
--         , requestDeletePermissionSet $
--             newDeletePermissionSet
--
--         , requestUpdatePermissionSet $
--             newUpdatePermissionSet
--
--         , requestProvisionPermissionSet $
--             newProvisionPermissionSet
--
--         , requestListAccountAssignments $
--             newListAccountAssignments
--
--         , requestDescribePermissionSetProvisioningStatus $
--             newDescribePermissionSetProvisioningStatus
--
--         , requestAttachManagedPolicyToPermissionSet $
--             newAttachManagedPolicyToPermissionSet
--
--         , requestListManagedPoliciesInPermissionSet $
--             newListManagedPoliciesInPermissionSet
--
--         , requestDeleteInlinePolicyFromPermissionSet $
--             newDeleteInlinePolicyFromPermissionSet
--
--         , requestDeleteInstanceAccessControlAttributeConfiguration $
--             newDeleteInstanceAccessControlAttributeConfiguration
--
--         , requestUpdateInstanceAccessControlAttributeConfiguration $
--             newUpdateInstanceAccessControlAttributeConfiguration
--
--         , requestDescribeAccountAssignmentDeletionStatus $
--             newDescribeAccountAssignmentDeletionStatus
--
--         , requestDescribeAccountAssignmentCreationStatus $
--             newDescribeAccountAssignmentCreationStatus
--
--         , requestPutInlinePolicyToPermissionSet $
--             newPutInlinePolicyToPermissionSet
--
--         , requestListAccountsForProvisionedPermissionSet $
--             newListAccountsForProvisionedPermissionSet
--
--         , requestListPermissionSetsProvisionedToAccount $
--             newListPermissionSetsProvisionedToAccount
--
--         , requestDetachManagedPolicyFromPermissionSet $
--             newDetachManagedPolicyFromPermissionSet
--
--         , requestListAccountAssignmentCreationStatus $
--             newListAccountAssignmentCreationStatus
--
--         , requestCreatePermissionSet $
--             newCreatePermissionSet
--
--         , requestListAccountAssignmentDeletionStatus $
--             newListAccountAssignmentDeletionStatus
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListInstances $
--             newListInstances
--
--         , requestDescribeInstanceAccessControlAttributeConfiguration $
--             newDescribeInstanceAccessControlAttributeConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteAccountAssignment $
--             newDeleteAccountAssignment
--
--         , requestListPermissionSetProvisioningStatus $
--             newListPermissionSetProvisioningStatus
--
--         , requestCreateAccountAssignment $
--             newCreateAccountAssignment
--
--           ]

--     , testGroup "response"
--         [ responseDescribePermissionSet $
--             newDescribePermissionSetResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateInstanceAccessControlAttributeConfiguration $
--             newCreateInstanceAccessControlAttributeConfigurationResponse
--
--         , responseGetInlinePolicyForPermissionSet $
--             newGetInlinePolicyForPermissionSetResponse
--
--         , responseListPermissionSets $
--             newListPermissionSetsResponse
--
--         , responseDeletePermissionSet $
--             newDeletePermissionSetResponse
--
--         , responseUpdatePermissionSet $
--             newUpdatePermissionSetResponse
--
--         , responseProvisionPermissionSet $
--             newProvisionPermissionSetResponse
--
--         , responseListAccountAssignments $
--             newListAccountAssignmentsResponse
--
--         , responseDescribePermissionSetProvisioningStatus $
--             newDescribePermissionSetProvisioningStatusResponse
--
--         , responseAttachManagedPolicyToPermissionSet $
--             newAttachManagedPolicyToPermissionSetResponse
--
--         , responseListManagedPoliciesInPermissionSet $
--             newListManagedPoliciesInPermissionSetResponse
--
--         , responseDeleteInlinePolicyFromPermissionSet $
--             newDeleteInlinePolicyFromPermissionSetResponse
--
--         , responseDeleteInstanceAccessControlAttributeConfiguration $
--             newDeleteInstanceAccessControlAttributeConfigurationResponse
--
--         , responseUpdateInstanceAccessControlAttributeConfiguration $
--             newUpdateInstanceAccessControlAttributeConfigurationResponse
--
--         , responseDescribeAccountAssignmentDeletionStatus $
--             newDescribeAccountAssignmentDeletionStatusResponse
--
--         , responseDescribeAccountAssignmentCreationStatus $
--             newDescribeAccountAssignmentCreationStatusResponse
--
--         , responsePutInlinePolicyToPermissionSet $
--             newPutInlinePolicyToPermissionSetResponse
--
--         , responseListAccountsForProvisionedPermissionSet $
--             newListAccountsForProvisionedPermissionSetResponse
--
--         , responseListPermissionSetsProvisionedToAccount $
--             newListPermissionSetsProvisionedToAccountResponse
--
--         , responseDetachManagedPolicyFromPermissionSet $
--             newDetachManagedPolicyFromPermissionSetResponse
--
--         , responseListAccountAssignmentCreationStatus $
--             newListAccountAssignmentCreationStatusResponse
--
--         , responseCreatePermissionSet $
--             newCreatePermissionSetResponse
--
--         , responseListAccountAssignmentDeletionStatus $
--             newListAccountAssignmentDeletionStatusResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListInstances $
--             newListInstancesResponse
--
--         , responseDescribeInstanceAccessControlAttributeConfiguration $
--             newDescribeInstanceAccessControlAttributeConfigurationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteAccountAssignment $
--             newDeleteAccountAssignmentResponse
--
--         , responseListPermissionSetProvisioningStatus $
--             newListPermissionSetProvisioningStatusResponse
--
--         , responseCreateAccountAssignment $
--             newCreateAccountAssignmentResponse
--
--           ]
--     ]

-- Requests

requestDescribePermissionSet :: DescribePermissionSet -> TestTree
requestDescribePermissionSet =
  req
    "DescribePermissionSet"
    "fixture/DescribePermissionSet.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateInstanceAccessControlAttributeConfiguration :: CreateInstanceAccessControlAttributeConfiguration -> TestTree
requestCreateInstanceAccessControlAttributeConfiguration =
  req
    "CreateInstanceAccessControlAttributeConfiguration"
    "fixture/CreateInstanceAccessControlAttributeConfiguration.yaml"

requestGetInlinePolicyForPermissionSet :: GetInlinePolicyForPermissionSet -> TestTree
requestGetInlinePolicyForPermissionSet =
  req
    "GetInlinePolicyForPermissionSet"
    "fixture/GetInlinePolicyForPermissionSet.yaml"

requestListPermissionSets :: ListPermissionSets -> TestTree
requestListPermissionSets =
  req
    "ListPermissionSets"
    "fixture/ListPermissionSets.yaml"

requestDeletePermissionSet :: DeletePermissionSet -> TestTree
requestDeletePermissionSet =
  req
    "DeletePermissionSet"
    "fixture/DeletePermissionSet.yaml"

requestUpdatePermissionSet :: UpdatePermissionSet -> TestTree
requestUpdatePermissionSet =
  req
    "UpdatePermissionSet"
    "fixture/UpdatePermissionSet.yaml"

requestProvisionPermissionSet :: ProvisionPermissionSet -> TestTree
requestProvisionPermissionSet =
  req
    "ProvisionPermissionSet"
    "fixture/ProvisionPermissionSet.yaml"

requestListAccountAssignments :: ListAccountAssignments -> TestTree
requestListAccountAssignments =
  req
    "ListAccountAssignments"
    "fixture/ListAccountAssignments.yaml"

requestDescribePermissionSetProvisioningStatus :: DescribePermissionSetProvisioningStatus -> TestTree
requestDescribePermissionSetProvisioningStatus =
  req
    "DescribePermissionSetProvisioningStatus"
    "fixture/DescribePermissionSetProvisioningStatus.yaml"

requestAttachManagedPolicyToPermissionSet :: AttachManagedPolicyToPermissionSet -> TestTree
requestAttachManagedPolicyToPermissionSet =
  req
    "AttachManagedPolicyToPermissionSet"
    "fixture/AttachManagedPolicyToPermissionSet.yaml"

requestListManagedPoliciesInPermissionSet :: ListManagedPoliciesInPermissionSet -> TestTree
requestListManagedPoliciesInPermissionSet =
  req
    "ListManagedPoliciesInPermissionSet"
    "fixture/ListManagedPoliciesInPermissionSet.yaml"

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

requestUpdateInstanceAccessControlAttributeConfiguration :: UpdateInstanceAccessControlAttributeConfiguration -> TestTree
requestUpdateInstanceAccessControlAttributeConfiguration =
  req
    "UpdateInstanceAccessControlAttributeConfiguration"
    "fixture/UpdateInstanceAccessControlAttributeConfiguration.yaml"

requestDescribeAccountAssignmentDeletionStatus :: DescribeAccountAssignmentDeletionStatus -> TestTree
requestDescribeAccountAssignmentDeletionStatus =
  req
    "DescribeAccountAssignmentDeletionStatus"
    "fixture/DescribeAccountAssignmentDeletionStatus.yaml"

requestDescribeAccountAssignmentCreationStatus :: DescribeAccountAssignmentCreationStatus -> TestTree
requestDescribeAccountAssignmentCreationStatus =
  req
    "DescribeAccountAssignmentCreationStatus"
    "fixture/DescribeAccountAssignmentCreationStatus.yaml"

requestPutInlinePolicyToPermissionSet :: PutInlinePolicyToPermissionSet -> TestTree
requestPutInlinePolicyToPermissionSet =
  req
    "PutInlinePolicyToPermissionSet"
    "fixture/PutInlinePolicyToPermissionSet.yaml"

requestListAccountsForProvisionedPermissionSet :: ListAccountsForProvisionedPermissionSet -> TestTree
requestListAccountsForProvisionedPermissionSet =
  req
    "ListAccountsForProvisionedPermissionSet"
    "fixture/ListAccountsForProvisionedPermissionSet.yaml"

requestListPermissionSetsProvisionedToAccount :: ListPermissionSetsProvisionedToAccount -> TestTree
requestListPermissionSetsProvisionedToAccount =
  req
    "ListPermissionSetsProvisionedToAccount"
    "fixture/ListPermissionSetsProvisionedToAccount.yaml"

requestDetachManagedPolicyFromPermissionSet :: DetachManagedPolicyFromPermissionSet -> TestTree
requestDetachManagedPolicyFromPermissionSet =
  req
    "DetachManagedPolicyFromPermissionSet"
    "fixture/DetachManagedPolicyFromPermissionSet.yaml"

requestListAccountAssignmentCreationStatus :: ListAccountAssignmentCreationStatus -> TestTree
requestListAccountAssignmentCreationStatus =
  req
    "ListAccountAssignmentCreationStatus"
    "fixture/ListAccountAssignmentCreationStatus.yaml"

requestCreatePermissionSet :: CreatePermissionSet -> TestTree
requestCreatePermissionSet =
  req
    "CreatePermissionSet"
    "fixture/CreatePermissionSet.yaml"

requestListAccountAssignmentDeletionStatus :: ListAccountAssignmentDeletionStatus -> TestTree
requestListAccountAssignmentDeletionStatus =
  req
    "ListAccountAssignmentDeletionStatus"
    "fixture/ListAccountAssignmentDeletionStatus.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListInstances :: ListInstances -> TestTree
requestListInstances =
  req
    "ListInstances"
    "fixture/ListInstances.yaml"

requestDescribeInstanceAccessControlAttributeConfiguration :: DescribeInstanceAccessControlAttributeConfiguration -> TestTree
requestDescribeInstanceAccessControlAttributeConfiguration =
  req
    "DescribeInstanceAccessControlAttributeConfiguration"
    "fixture/DescribeInstanceAccessControlAttributeConfiguration.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteAccountAssignment :: DeleteAccountAssignment -> TestTree
requestDeleteAccountAssignment =
  req
    "DeleteAccountAssignment"
    "fixture/DeleteAccountAssignment.yaml"

requestListPermissionSetProvisioningStatus :: ListPermissionSetProvisioningStatus -> TestTree
requestListPermissionSetProvisioningStatus =
  req
    "ListPermissionSetProvisioningStatus"
    "fixture/ListPermissionSetProvisioningStatus.yaml"

requestCreateAccountAssignment :: CreateAccountAssignment -> TestTree
requestCreateAccountAssignment =
  req
    "CreateAccountAssignment"
    "fixture/CreateAccountAssignment.yaml"

-- Responses

responseDescribePermissionSet :: DescribePermissionSetResponse -> TestTree
responseDescribePermissionSet =
  res
    "DescribePermissionSetResponse"
    "fixture/DescribePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePermissionSet)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateInstanceAccessControlAttributeConfiguration :: CreateInstanceAccessControlAttributeConfigurationResponse -> TestTree
responseCreateInstanceAccessControlAttributeConfiguration =
  res
    "CreateInstanceAccessControlAttributeConfigurationResponse"
    "fixture/CreateInstanceAccessControlAttributeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceAccessControlAttributeConfiguration)

responseGetInlinePolicyForPermissionSet :: GetInlinePolicyForPermissionSetResponse -> TestTree
responseGetInlinePolicyForPermissionSet =
  res
    "GetInlinePolicyForPermissionSetResponse"
    "fixture/GetInlinePolicyForPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInlinePolicyForPermissionSet)

responseListPermissionSets :: ListPermissionSetsResponse -> TestTree
responseListPermissionSets =
  res
    "ListPermissionSetsResponse"
    "fixture/ListPermissionSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionSets)

responseDeletePermissionSet :: DeletePermissionSetResponse -> TestTree
responseDeletePermissionSet =
  res
    "DeletePermissionSetResponse"
    "fixture/DeletePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePermissionSet)

responseUpdatePermissionSet :: UpdatePermissionSetResponse -> TestTree
responseUpdatePermissionSet =
  res
    "UpdatePermissionSetResponse"
    "fixture/UpdatePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePermissionSet)

responseProvisionPermissionSet :: ProvisionPermissionSetResponse -> TestTree
responseProvisionPermissionSet =
  res
    "ProvisionPermissionSetResponse"
    "fixture/ProvisionPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionPermissionSet)

responseListAccountAssignments :: ListAccountAssignmentsResponse -> TestTree
responseListAccountAssignments =
  res
    "ListAccountAssignmentsResponse"
    "fixture/ListAccountAssignmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAssignments)

responseDescribePermissionSetProvisioningStatus :: DescribePermissionSetProvisioningStatusResponse -> TestTree
responseDescribePermissionSetProvisioningStatus =
  res
    "DescribePermissionSetProvisioningStatusResponse"
    "fixture/DescribePermissionSetProvisioningStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePermissionSetProvisioningStatus)

responseAttachManagedPolicyToPermissionSet :: AttachManagedPolicyToPermissionSetResponse -> TestTree
responseAttachManagedPolicyToPermissionSet =
  res
    "AttachManagedPolicyToPermissionSetResponse"
    "fixture/AttachManagedPolicyToPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachManagedPolicyToPermissionSet)

responseListManagedPoliciesInPermissionSet :: ListManagedPoliciesInPermissionSetResponse -> TestTree
responseListManagedPoliciesInPermissionSet =
  res
    "ListManagedPoliciesInPermissionSetResponse"
    "fixture/ListManagedPoliciesInPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedPoliciesInPermissionSet)

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

responseUpdateInstanceAccessControlAttributeConfiguration :: UpdateInstanceAccessControlAttributeConfigurationResponse -> TestTree
responseUpdateInstanceAccessControlAttributeConfiguration =
  res
    "UpdateInstanceAccessControlAttributeConfigurationResponse"
    "fixture/UpdateInstanceAccessControlAttributeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceAccessControlAttributeConfiguration)

responseDescribeAccountAssignmentDeletionStatus :: DescribeAccountAssignmentDeletionStatusResponse -> TestTree
responseDescribeAccountAssignmentDeletionStatus =
  res
    "DescribeAccountAssignmentDeletionStatusResponse"
    "fixture/DescribeAccountAssignmentDeletionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAssignmentDeletionStatus)

responseDescribeAccountAssignmentCreationStatus :: DescribeAccountAssignmentCreationStatusResponse -> TestTree
responseDescribeAccountAssignmentCreationStatus =
  res
    "DescribeAccountAssignmentCreationStatusResponse"
    "fixture/DescribeAccountAssignmentCreationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAssignmentCreationStatus)

responsePutInlinePolicyToPermissionSet :: PutInlinePolicyToPermissionSetResponse -> TestTree
responsePutInlinePolicyToPermissionSet =
  res
    "PutInlinePolicyToPermissionSetResponse"
    "fixture/PutInlinePolicyToPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInlinePolicyToPermissionSet)

responseListAccountsForProvisionedPermissionSet :: ListAccountsForProvisionedPermissionSetResponse -> TestTree
responseListAccountsForProvisionedPermissionSet =
  res
    "ListAccountsForProvisionedPermissionSetResponse"
    "fixture/ListAccountsForProvisionedPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountsForProvisionedPermissionSet)

responseListPermissionSetsProvisionedToAccount :: ListPermissionSetsProvisionedToAccountResponse -> TestTree
responseListPermissionSetsProvisionedToAccount =
  res
    "ListPermissionSetsProvisionedToAccountResponse"
    "fixture/ListPermissionSetsProvisionedToAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionSetsProvisionedToAccount)

responseDetachManagedPolicyFromPermissionSet :: DetachManagedPolicyFromPermissionSetResponse -> TestTree
responseDetachManagedPolicyFromPermissionSet =
  res
    "DetachManagedPolicyFromPermissionSetResponse"
    "fixture/DetachManagedPolicyFromPermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachManagedPolicyFromPermissionSet)

responseListAccountAssignmentCreationStatus :: ListAccountAssignmentCreationStatusResponse -> TestTree
responseListAccountAssignmentCreationStatus =
  res
    "ListAccountAssignmentCreationStatusResponse"
    "fixture/ListAccountAssignmentCreationStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAssignmentCreationStatus)

responseCreatePermissionSet :: CreatePermissionSetResponse -> TestTree
responseCreatePermissionSet =
  res
    "CreatePermissionSetResponse"
    "fixture/CreatePermissionSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePermissionSet)

responseListAccountAssignmentDeletionStatus :: ListAccountAssignmentDeletionStatusResponse -> TestTree
responseListAccountAssignmentDeletionStatus =
  res
    "ListAccountAssignmentDeletionStatusResponse"
    "fixture/ListAccountAssignmentDeletionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountAssignmentDeletionStatus)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListInstances :: ListInstancesResponse -> TestTree
responseListInstances =
  res
    "ListInstancesResponse"
    "fixture/ListInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInstances)

responseDescribeInstanceAccessControlAttributeConfiguration :: DescribeInstanceAccessControlAttributeConfigurationResponse -> TestTree
responseDescribeInstanceAccessControlAttributeConfiguration =
  res
    "DescribeInstanceAccessControlAttributeConfigurationResponse"
    "fixture/DescribeInstanceAccessControlAttributeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstanceAccessControlAttributeConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteAccountAssignment :: DeleteAccountAssignmentResponse -> TestTree
responseDeleteAccountAssignment =
  res
    "DeleteAccountAssignmentResponse"
    "fixture/DeleteAccountAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccountAssignment)

responseListPermissionSetProvisioningStatus :: ListPermissionSetProvisioningStatusResponse -> TestTree
responseListPermissionSetProvisioningStatus =
  res
    "ListPermissionSetProvisioningStatusResponse"
    "fixture/ListPermissionSetProvisioningStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionSetProvisioningStatus)

responseCreateAccountAssignment :: CreateAccountAssignmentResponse -> TestTree
responseCreateAccountAssignment =
  res
    "CreateAccountAssignmentResponse"
    "fixture/CreateAccountAssignmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccountAssignment)

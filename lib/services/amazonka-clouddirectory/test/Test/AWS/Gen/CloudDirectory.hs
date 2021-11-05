{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CloudDirectory where

import Amazonka.CloudDirectory
import qualified Data.Proxy as Proxy
import Test.AWS.CloudDirectory.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributes
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestListIndex $
--             newListIndex
--
--         , requestUpgradeAppliedSchema $
--             newUpgradeAppliedSchema
--
--         , requestGetDirectory $
--             newGetDirectory
--
--         , requestGetObjectInformation $
--             newGetObjectInformation
--
--         , requestListAttachedIndices $
--             newListAttachedIndices
--
--         , requestDetachFromIndex $
--             newDetachFromIndex
--
--         , requestLookupPolicy $
--             newLookupPolicy
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListPublishedSchemaArns $
--             newListPublishedSchemaArns
--
--         , requestListDirectories $
--             newListDirectories
--
--         , requestCreateTypedLinkFacet $
--             newCreateTypedLinkFacet
--
--         , requestListObjectParentPaths $
--             newListObjectParentPaths
--
--         , requestDisableDirectory $
--             newDisableDirectory
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestListFacetAttributes $
--             newListFacetAttributes
--
--         , requestListManagedSchemaArns $
--             newListManagedSchemaArns
--
--         , requestUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacet
--
--         , requestDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacet
--
--         , requestGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersion
--
--         , requestRemoveFacetFromObject $
--             newRemoveFacetFromObject
--
--         , requestEnableDirectory $
--             newEnableDirectory
--
--         , requestListObjectAttributes $
--             newListObjectAttributes
--
--         , requestListAppliedSchemaArns $
--             newListAppliedSchemaArns
--
--         , requestListIncomingTypedLinks $
--             newListIncomingTypedLinks
--
--         , requestGetFacet $
--             newGetFacet
--
--         , requestGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformation
--
--         , requestListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArns
--
--         , requestAttachObject $
--             newAttachObject
--
--         , requestBatchWrite $
--             newBatchWrite
--
--         , requestCreateObject $
--             newCreateObject
--
--         , requestUpgradePublishedSchema $
--             newUpgradePublishedSchema
--
--         , requestCreateFacet $
--             newCreateFacet
--
--         , requestGetLinkAttributes $
--             newGetLinkAttributes
--
--         , requestGetObjectAttributes $
--             newGetObjectAttributes
--
--         , requestDeleteFacet $
--             newDeleteFacet
--
--         , requestUpdateFacet $
--             newUpdateFacet
--
--         , requestListObjectChildren $
--             newListObjectChildren
--
--         , requestListTypedLinkFacetNames $
--             newListTypedLinkFacetNames
--
--         , requestAttachTypedLink $
--             newAttachTypedLink
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestCreateIndex $
--             newCreateIndex
--
--         , requestDetachObject $
--             newDetachObject
--
--         , requestAddFacetToObject $
--             newAddFacetToObject
--
--         , requestApplySchema $
--             newApplySchema
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestGetSchemaAsJson $
--             newGetSchemaAsJson
--
--         , requestPublishSchema $
--             newPublishSchema
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestListObjectParents $
--             newListObjectParents
--
--         , requestListPolicyAttachments $
--             newListPolicyAttachments
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestDetachTypedLink $
--             newDetachTypedLink
--
--         , requestListFacetNames $
--             newListFacetNames
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListOutgoingTypedLinks $
--             newListOutgoingTypedLinks
--
--         , requestUpdateObjectAttributes $
--             newUpdateObjectAttributes
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestBatchRead $
--             newBatchRead
--
--         , requestPutSchemaFromJson $
--             newPutSchemaFromJson
--
--         , requestUpdateLinkAttributes $
--             newUpdateLinkAttributes
--
--         , requestAttachToIndex $
--             newAttachToIndex
--
--         , requestListObjectPolicies $
--             newListObjectPolicies
--
--           ]

--     , testGroup "response"
--         [ responseListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributesResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseListIndex $
--             newListIndexResponse
--
--         , responseUpgradeAppliedSchema $
--             newUpgradeAppliedSchemaResponse
--
--         , responseGetDirectory $
--             newGetDirectoryResponse
--
--         , responseGetObjectInformation $
--             newGetObjectInformationResponse
--
--         , responseListAttachedIndices $
--             newListAttachedIndicesResponse
--
--         , responseDetachFromIndex $
--             newDetachFromIndexResponse
--
--         , responseLookupPolicy $
--             newLookupPolicyResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListPublishedSchemaArns $
--             newListPublishedSchemaArnsResponse
--
--         , responseListDirectories $
--             newListDirectoriesResponse
--
--         , responseCreateTypedLinkFacet $
--             newCreateTypedLinkFacetResponse
--
--         , responseListObjectParentPaths $
--             newListObjectParentPathsResponse
--
--         , responseDisableDirectory $
--             newDisableDirectoryResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseListFacetAttributes $
--             newListFacetAttributesResponse
--
--         , responseListManagedSchemaArns $
--             newListManagedSchemaArnsResponse
--
--         , responseUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacetResponse
--
--         , responseDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacetResponse
--
--         , responseGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersionResponse
--
--         , responseRemoveFacetFromObject $
--             newRemoveFacetFromObjectResponse
--
--         , responseEnableDirectory $
--             newEnableDirectoryResponse
--
--         , responseListObjectAttributes $
--             newListObjectAttributesResponse
--
--         , responseListAppliedSchemaArns $
--             newListAppliedSchemaArnsResponse
--
--         , responseListIncomingTypedLinks $
--             newListIncomingTypedLinksResponse
--
--         , responseGetFacet $
--             newGetFacetResponse
--
--         , responseGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformationResponse
--
--         , responseListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArnsResponse
--
--         , responseAttachObject $
--             newAttachObjectResponse
--
--         , responseBatchWrite $
--             newBatchWriteResponse
--
--         , responseCreateObject $
--             newCreateObjectResponse
--
--         , responseUpgradePublishedSchema $
--             newUpgradePublishedSchemaResponse
--
--         , responseCreateFacet $
--             newCreateFacetResponse
--
--         , responseGetLinkAttributes $
--             newGetLinkAttributesResponse
--
--         , responseGetObjectAttributes $
--             newGetObjectAttributesResponse
--
--         , responseDeleteFacet $
--             newDeleteFacetResponse
--
--         , responseUpdateFacet $
--             newUpdateFacetResponse
--
--         , responseListObjectChildren $
--             newListObjectChildrenResponse
--
--         , responseListTypedLinkFacetNames $
--             newListTypedLinkFacetNamesResponse
--
--         , responseAttachTypedLink $
--             newAttachTypedLinkResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseCreateIndex $
--             newCreateIndexResponse
--
--         , responseDetachObject $
--             newDetachObjectResponse
--
--         , responseAddFacetToObject $
--             newAddFacetToObjectResponse
--
--         , responseApplySchema $
--             newApplySchemaResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseGetSchemaAsJson $
--             newGetSchemaAsJsonResponse
--
--         , responsePublishSchema $
--             newPublishSchemaResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseListObjectParents $
--             newListObjectParentsResponse
--
--         , responseListPolicyAttachments $
--             newListPolicyAttachmentsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseDetachTypedLink $
--             newDetachTypedLinkResponse
--
--         , responseListFacetNames $
--             newListFacetNamesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListOutgoingTypedLinks $
--             newListOutgoingTypedLinksResponse
--
--         , responseUpdateObjectAttributes $
--             newUpdateObjectAttributesResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseBatchRead $
--             newBatchReadResponse
--
--         , responsePutSchemaFromJson $
--             newPutSchemaFromJsonResponse
--
--         , responseUpdateLinkAttributes $
--             newUpdateLinkAttributesResponse
--
--         , responseAttachToIndex $
--             newAttachToIndexResponse
--
--         , responseListObjectPolicies $
--             newListObjectPoliciesResponse
--
--           ]
--     ]

-- Requests

requestListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributes -> TestTree
requestListTypedLinkFacetAttributes =
  req
    "ListTypedLinkFacetAttributes"
    "fixture/ListTypedLinkFacetAttributes.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestListIndex :: ListIndex -> TestTree
requestListIndex =
  req
    "ListIndex"
    "fixture/ListIndex.yaml"

requestUpgradeAppliedSchema :: UpgradeAppliedSchema -> TestTree
requestUpgradeAppliedSchema =
  req
    "UpgradeAppliedSchema"
    "fixture/UpgradeAppliedSchema.yaml"

requestGetDirectory :: GetDirectory -> TestTree
requestGetDirectory =
  req
    "GetDirectory"
    "fixture/GetDirectory.yaml"

requestGetObjectInformation :: GetObjectInformation -> TestTree
requestGetObjectInformation =
  req
    "GetObjectInformation"
    "fixture/GetObjectInformation.yaml"

requestListAttachedIndices :: ListAttachedIndices -> TestTree
requestListAttachedIndices =
  req
    "ListAttachedIndices"
    "fixture/ListAttachedIndices.yaml"

requestDetachFromIndex :: DetachFromIndex -> TestTree
requestDetachFromIndex =
  req
    "DetachFromIndex"
    "fixture/DetachFromIndex.yaml"

requestLookupPolicy :: LookupPolicy -> TestTree
requestLookupPolicy =
  req
    "LookupPolicy"
    "fixture/LookupPolicy.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListPublishedSchemaArns :: ListPublishedSchemaArns -> TestTree
requestListPublishedSchemaArns =
  req
    "ListPublishedSchemaArns"
    "fixture/ListPublishedSchemaArns.yaml"

requestListDirectories :: ListDirectories -> TestTree
requestListDirectories =
  req
    "ListDirectories"
    "fixture/ListDirectories.yaml"

requestCreateTypedLinkFacet :: CreateTypedLinkFacet -> TestTree
requestCreateTypedLinkFacet =
  req
    "CreateTypedLinkFacet"
    "fixture/CreateTypedLinkFacet.yaml"

requestListObjectParentPaths :: ListObjectParentPaths -> TestTree
requestListObjectParentPaths =
  req
    "ListObjectParentPaths"
    "fixture/ListObjectParentPaths.yaml"

requestDisableDirectory :: DisableDirectory -> TestTree
requestDisableDirectory =
  req
    "DisableDirectory"
    "fixture/DisableDirectory.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

requestListFacetAttributes :: ListFacetAttributes -> TestTree
requestListFacetAttributes =
  req
    "ListFacetAttributes"
    "fixture/ListFacetAttributes.yaml"

requestListManagedSchemaArns :: ListManagedSchemaArns -> TestTree
requestListManagedSchemaArns =
  req
    "ListManagedSchemaArns"
    "fixture/ListManagedSchemaArns.yaml"

requestUpdateTypedLinkFacet :: UpdateTypedLinkFacet -> TestTree
requestUpdateTypedLinkFacet =
  req
    "UpdateTypedLinkFacet"
    "fixture/UpdateTypedLinkFacet.yaml"

requestDeleteTypedLinkFacet :: DeleteTypedLinkFacet -> TestTree
requestDeleteTypedLinkFacet =
  req
    "DeleteTypedLinkFacet"
    "fixture/DeleteTypedLinkFacet.yaml"

requestGetAppliedSchemaVersion :: GetAppliedSchemaVersion -> TestTree
requestGetAppliedSchemaVersion =
  req
    "GetAppliedSchemaVersion"
    "fixture/GetAppliedSchemaVersion.yaml"

requestRemoveFacetFromObject :: RemoveFacetFromObject -> TestTree
requestRemoveFacetFromObject =
  req
    "RemoveFacetFromObject"
    "fixture/RemoveFacetFromObject.yaml"

requestEnableDirectory :: EnableDirectory -> TestTree
requestEnableDirectory =
  req
    "EnableDirectory"
    "fixture/EnableDirectory.yaml"

requestListObjectAttributes :: ListObjectAttributes -> TestTree
requestListObjectAttributes =
  req
    "ListObjectAttributes"
    "fixture/ListObjectAttributes.yaml"

requestListAppliedSchemaArns :: ListAppliedSchemaArns -> TestTree
requestListAppliedSchemaArns =
  req
    "ListAppliedSchemaArns"
    "fixture/ListAppliedSchemaArns.yaml"

requestListIncomingTypedLinks :: ListIncomingTypedLinks -> TestTree
requestListIncomingTypedLinks =
  req
    "ListIncomingTypedLinks"
    "fixture/ListIncomingTypedLinks.yaml"

requestGetFacet :: GetFacet -> TestTree
requestGetFacet =
  req
    "GetFacet"
    "fixture/GetFacet.yaml"

requestGetTypedLinkFacetInformation :: GetTypedLinkFacetInformation -> TestTree
requestGetTypedLinkFacetInformation =
  req
    "GetTypedLinkFacetInformation"
    "fixture/GetTypedLinkFacetInformation.yaml"

requestListDevelopmentSchemaArns :: ListDevelopmentSchemaArns -> TestTree
requestListDevelopmentSchemaArns =
  req
    "ListDevelopmentSchemaArns"
    "fixture/ListDevelopmentSchemaArns.yaml"

requestAttachObject :: AttachObject -> TestTree
requestAttachObject =
  req
    "AttachObject"
    "fixture/AttachObject.yaml"

requestBatchWrite :: BatchWrite -> TestTree
requestBatchWrite =
  req
    "BatchWrite"
    "fixture/BatchWrite.yaml"

requestCreateObject :: CreateObject -> TestTree
requestCreateObject =
  req
    "CreateObject"
    "fixture/CreateObject.yaml"

requestUpgradePublishedSchema :: UpgradePublishedSchema -> TestTree
requestUpgradePublishedSchema =
  req
    "UpgradePublishedSchema"
    "fixture/UpgradePublishedSchema.yaml"

requestCreateFacet :: CreateFacet -> TestTree
requestCreateFacet =
  req
    "CreateFacet"
    "fixture/CreateFacet.yaml"

requestGetLinkAttributes :: GetLinkAttributes -> TestTree
requestGetLinkAttributes =
  req
    "GetLinkAttributes"
    "fixture/GetLinkAttributes.yaml"

requestGetObjectAttributes :: GetObjectAttributes -> TestTree
requestGetObjectAttributes =
  req
    "GetObjectAttributes"
    "fixture/GetObjectAttributes.yaml"

requestDeleteFacet :: DeleteFacet -> TestTree
requestDeleteFacet =
  req
    "DeleteFacet"
    "fixture/DeleteFacet.yaml"

requestUpdateFacet :: UpdateFacet -> TestTree
requestUpdateFacet =
  req
    "UpdateFacet"
    "fixture/UpdateFacet.yaml"

requestListObjectChildren :: ListObjectChildren -> TestTree
requestListObjectChildren =
  req
    "ListObjectChildren"
    "fixture/ListObjectChildren.yaml"

requestListTypedLinkFacetNames :: ListTypedLinkFacetNames -> TestTree
requestListTypedLinkFacetNames =
  req
    "ListTypedLinkFacetNames"
    "fixture/ListTypedLinkFacetNames.yaml"

requestAttachTypedLink :: AttachTypedLink -> TestTree
requestAttachTypedLink =
  req
    "AttachTypedLink"
    "fixture/AttachTypedLink.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestCreateIndex :: CreateIndex -> TestTree
requestCreateIndex =
  req
    "CreateIndex"
    "fixture/CreateIndex.yaml"

requestDetachObject :: DetachObject -> TestTree
requestDetachObject =
  req
    "DetachObject"
    "fixture/DetachObject.yaml"

requestAddFacetToObject :: AddFacetToObject -> TestTree
requestAddFacetToObject =
  req
    "AddFacetToObject"
    "fixture/AddFacetToObject.yaml"

requestApplySchema :: ApplySchema -> TestTree
requestApplySchema =
  req
    "ApplySchema"
    "fixture/ApplySchema.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestGetSchemaAsJson :: GetSchemaAsJson -> TestTree
requestGetSchemaAsJson =
  req
    "GetSchemaAsJson"
    "fixture/GetSchemaAsJson.yaml"

requestPublishSchema :: PublishSchema -> TestTree
requestPublishSchema =
  req
    "PublishSchema"
    "fixture/PublishSchema.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory =
  req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

requestListObjectParents :: ListObjectParents -> TestTree
requestListObjectParents =
  req
    "ListObjectParents"
    "fixture/ListObjectParents.yaml"

requestListPolicyAttachments :: ListPolicyAttachments -> TestTree
requestListPolicyAttachments =
  req
    "ListPolicyAttachments"
    "fixture/ListPolicyAttachments.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestDetachTypedLink :: DetachTypedLink -> TestTree
requestDetachTypedLink =
  req
    "DetachTypedLink"
    "fixture/DetachTypedLink.yaml"

requestListFacetNames :: ListFacetNames -> TestTree
requestListFacetNames =
  req
    "ListFacetNames"
    "fixture/ListFacetNames.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListOutgoingTypedLinks :: ListOutgoingTypedLinks -> TestTree
requestListOutgoingTypedLinks =
  req
    "ListOutgoingTypedLinks"
    "fixture/ListOutgoingTypedLinks.yaml"

requestUpdateObjectAttributes :: UpdateObjectAttributes -> TestTree
requestUpdateObjectAttributes =
  req
    "UpdateObjectAttributes"
    "fixture/UpdateObjectAttributes.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestBatchRead :: BatchRead -> TestTree
requestBatchRead =
  req
    "BatchRead"
    "fixture/BatchRead.yaml"

requestPutSchemaFromJson :: PutSchemaFromJson -> TestTree
requestPutSchemaFromJson =
  req
    "PutSchemaFromJson"
    "fixture/PutSchemaFromJson.yaml"

requestUpdateLinkAttributes :: UpdateLinkAttributes -> TestTree
requestUpdateLinkAttributes =
  req
    "UpdateLinkAttributes"
    "fixture/UpdateLinkAttributes.yaml"

requestAttachToIndex :: AttachToIndex -> TestTree
requestAttachToIndex =
  req
    "AttachToIndex"
    "fixture/AttachToIndex.yaml"

requestListObjectPolicies :: ListObjectPolicies -> TestTree
requestListObjectPolicies =
  req
    "ListObjectPolicies"
    "fixture/ListObjectPolicies.yaml"

-- Responses

responseListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributesResponse -> TestTree
responseListTypedLinkFacetAttributes =
  res
    "ListTypedLinkFacetAttributesResponse"
    "fixture/ListTypedLinkFacetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypedLinkFacetAttributes)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObject)

responseListIndex :: ListIndexResponse -> TestTree
responseListIndex =
  res
    "ListIndexResponse"
    "fixture/ListIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIndex)

responseUpgradeAppliedSchema :: UpgradeAppliedSchemaResponse -> TestTree
responseUpgradeAppliedSchema =
  res
    "UpgradeAppliedSchemaResponse"
    "fixture/UpgradeAppliedSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeAppliedSchema)

responseGetDirectory :: GetDirectoryResponse -> TestTree
responseGetDirectory =
  res
    "GetDirectoryResponse"
    "fixture/GetDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDirectory)

responseGetObjectInformation :: GetObjectInformationResponse -> TestTree
responseGetObjectInformation =
  res
    "GetObjectInformationResponse"
    "fixture/GetObjectInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectInformation)

responseListAttachedIndices :: ListAttachedIndicesResponse -> TestTree
responseListAttachedIndices =
  res
    "ListAttachedIndicesResponse"
    "fixture/ListAttachedIndicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedIndices)

responseDetachFromIndex :: DetachFromIndexResponse -> TestTree
responseDetachFromIndex =
  res
    "DetachFromIndexResponse"
    "fixture/DetachFromIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachFromIndex)

responseLookupPolicy :: LookupPolicyResponse -> TestTree
responseLookupPolicy =
  res
    "LookupPolicyResponse"
    "fixture/LookupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LookupPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListPublishedSchemaArns :: ListPublishedSchemaArnsResponse -> TestTree
responseListPublishedSchemaArns =
  res
    "ListPublishedSchemaArnsResponse"
    "fixture/ListPublishedSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPublishedSchemaArns)

responseListDirectories :: ListDirectoriesResponse -> TestTree
responseListDirectories =
  res
    "ListDirectoriesResponse"
    "fixture/ListDirectoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDirectories)

responseCreateTypedLinkFacet :: CreateTypedLinkFacetResponse -> TestTree
responseCreateTypedLinkFacet =
  res
    "CreateTypedLinkFacetResponse"
    "fixture/CreateTypedLinkFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTypedLinkFacet)

responseListObjectParentPaths :: ListObjectParentPathsResponse -> TestTree
responseListObjectParentPaths =
  res
    "ListObjectParentPathsResponse"
    "fixture/ListObjectParentPathsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectParentPaths)

responseDisableDirectory :: DisableDirectoryResponse -> TestTree
responseDisableDirectory =
  res
    "DisableDirectoryResponse"
    "fixture/DisableDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableDirectory)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectory)

responseListFacetAttributes :: ListFacetAttributesResponse -> TestTree
responseListFacetAttributes =
  res
    "ListFacetAttributesResponse"
    "fixture/ListFacetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFacetAttributes)

responseListManagedSchemaArns :: ListManagedSchemaArnsResponse -> TestTree
responseListManagedSchemaArns =
  res
    "ListManagedSchemaArnsResponse"
    "fixture/ListManagedSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedSchemaArns)

responseUpdateTypedLinkFacet :: UpdateTypedLinkFacetResponse -> TestTree
responseUpdateTypedLinkFacet =
  res
    "UpdateTypedLinkFacetResponse"
    "fixture/UpdateTypedLinkFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTypedLinkFacet)

responseDeleteTypedLinkFacet :: DeleteTypedLinkFacetResponse -> TestTree
responseDeleteTypedLinkFacet =
  res
    "DeleteTypedLinkFacetResponse"
    "fixture/DeleteTypedLinkFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTypedLinkFacet)

responseGetAppliedSchemaVersion :: GetAppliedSchemaVersionResponse -> TestTree
responseGetAppliedSchemaVersion =
  res
    "GetAppliedSchemaVersionResponse"
    "fixture/GetAppliedSchemaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppliedSchemaVersion)

responseRemoveFacetFromObject :: RemoveFacetFromObjectResponse -> TestTree
responseRemoveFacetFromObject =
  res
    "RemoveFacetFromObjectResponse"
    "fixture/RemoveFacetFromObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFacetFromObject)

responseEnableDirectory :: EnableDirectoryResponse -> TestTree
responseEnableDirectory =
  res
    "EnableDirectoryResponse"
    "fixture/EnableDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableDirectory)

responseListObjectAttributes :: ListObjectAttributesResponse -> TestTree
responseListObjectAttributes =
  res
    "ListObjectAttributesResponse"
    "fixture/ListObjectAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectAttributes)

responseListAppliedSchemaArns :: ListAppliedSchemaArnsResponse -> TestTree
responseListAppliedSchemaArns =
  res
    "ListAppliedSchemaArnsResponse"
    "fixture/ListAppliedSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppliedSchemaArns)

responseListIncomingTypedLinks :: ListIncomingTypedLinksResponse -> TestTree
responseListIncomingTypedLinks =
  res
    "ListIncomingTypedLinksResponse"
    "fixture/ListIncomingTypedLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIncomingTypedLinks)

responseGetFacet :: GetFacetResponse -> TestTree
responseGetFacet =
  res
    "GetFacetResponse"
    "fixture/GetFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFacet)

responseGetTypedLinkFacetInformation :: GetTypedLinkFacetInformationResponse -> TestTree
responseGetTypedLinkFacetInformation =
  res
    "GetTypedLinkFacetInformationResponse"
    "fixture/GetTypedLinkFacetInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTypedLinkFacetInformation)

responseListDevelopmentSchemaArns :: ListDevelopmentSchemaArnsResponse -> TestTree
responseListDevelopmentSchemaArns =
  res
    "ListDevelopmentSchemaArnsResponse"
    "fixture/ListDevelopmentSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevelopmentSchemaArns)

responseAttachObject :: AttachObjectResponse -> TestTree
responseAttachObject =
  res
    "AttachObjectResponse"
    "fixture/AttachObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachObject)

responseBatchWrite :: BatchWriteResponse -> TestTree
responseBatchWrite =
  res
    "BatchWriteResponse"
    "fixture/BatchWriteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchWrite)

responseCreateObject :: CreateObjectResponse -> TestTree
responseCreateObject =
  res
    "CreateObjectResponse"
    "fixture/CreateObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateObject)

responseUpgradePublishedSchema :: UpgradePublishedSchemaResponse -> TestTree
responseUpgradePublishedSchema =
  res
    "UpgradePublishedSchemaResponse"
    "fixture/UpgradePublishedSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradePublishedSchema)

responseCreateFacet :: CreateFacetResponse -> TestTree
responseCreateFacet =
  res
    "CreateFacetResponse"
    "fixture/CreateFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFacet)

responseGetLinkAttributes :: GetLinkAttributesResponse -> TestTree
responseGetLinkAttributes =
  res
    "GetLinkAttributesResponse"
    "fixture/GetLinkAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLinkAttributes)

responseGetObjectAttributes :: GetObjectAttributesResponse -> TestTree
responseGetObjectAttributes =
  res
    "GetObjectAttributesResponse"
    "fixture/GetObjectAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectAttributes)

responseDeleteFacet :: DeleteFacetResponse -> TestTree
responseDeleteFacet =
  res
    "DeleteFacetResponse"
    "fixture/DeleteFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFacet)

responseUpdateFacet :: UpdateFacetResponse -> TestTree
responseUpdateFacet =
  res
    "UpdateFacetResponse"
    "fixture/UpdateFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFacet)

responseListObjectChildren :: ListObjectChildrenResponse -> TestTree
responseListObjectChildren =
  res
    "ListObjectChildrenResponse"
    "fixture/ListObjectChildrenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectChildren)

responseListTypedLinkFacetNames :: ListTypedLinkFacetNamesResponse -> TestTree
responseListTypedLinkFacetNames =
  res
    "ListTypedLinkFacetNamesResponse"
    "fixture/ListTypedLinkFacetNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypedLinkFacetNames)

responseAttachTypedLink :: AttachTypedLinkResponse -> TestTree
responseAttachTypedLink =
  res
    "AttachTypedLinkResponse"
    "fixture/AttachTypedLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachTypedLink)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachPolicy)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex =
  res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIndex)

responseDetachObject :: DetachObjectResponse -> TestTree
responseDetachObject =
  res
    "DetachObjectResponse"
    "fixture/DetachObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachObject)

responseAddFacetToObject :: AddFacetToObjectResponse -> TestTree
responseAddFacetToObject =
  res
    "AddFacetToObjectResponse"
    "fixture/AddFacetToObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddFacetToObject)

responseApplySchema :: ApplySchemaResponse -> TestTree
responseApplySchema =
  res
    "ApplySchemaResponse"
    "fixture/ApplySchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplySchema)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchema)

responseGetSchemaAsJson :: GetSchemaAsJsonResponse -> TestTree
responseGetSchemaAsJson =
  res
    "GetSchemaAsJsonResponse"
    "fixture/GetSchemaAsJsonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaAsJson)

responsePublishSchema :: PublishSchemaResponse -> TestTree
responsePublishSchema =
  res
    "PublishSchemaResponse"
    "fixture/PublishSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishSchema)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectory)

responseListObjectParents :: ListObjectParentsResponse -> TestTree
responseListObjectParents =
  res
    "ListObjectParentsResponse"
    "fixture/ListObjectParentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectParents)

responseListPolicyAttachments :: ListPolicyAttachmentsResponse -> TestTree
responseListPolicyAttachments =
  res
    "ListPolicyAttachmentsResponse"
    "fixture/ListPolicyAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyAttachments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchema)

responseDetachTypedLink :: DetachTypedLinkResponse -> TestTree
responseDetachTypedLink =
  res
    "DetachTypedLinkResponse"
    "fixture/DetachTypedLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachTypedLink)

responseListFacetNames :: ListFacetNamesResponse -> TestTree
responseListFacetNames =
  res
    "ListFacetNamesResponse"
    "fixture/ListFacetNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFacetNames)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListOutgoingTypedLinks :: ListOutgoingTypedLinksResponse -> TestTree
responseListOutgoingTypedLinks =
  res
    "ListOutgoingTypedLinksResponse"
    "fixture/ListOutgoingTypedLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOutgoingTypedLinks)

responseUpdateObjectAttributes :: UpdateObjectAttributesResponse -> TestTree
responseUpdateObjectAttributes =
  res
    "UpdateObjectAttributesResponse"
    "fixture/UpdateObjectAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateObjectAttributes)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachPolicy)

responseBatchRead :: BatchReadResponse -> TestTree
responseBatchRead =
  res
    "BatchReadResponse"
    "fixture/BatchReadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchRead)

responsePutSchemaFromJson :: PutSchemaFromJsonResponse -> TestTree
responsePutSchemaFromJson =
  res
    "PutSchemaFromJsonResponse"
    "fixture/PutSchemaFromJsonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSchemaFromJson)

responseUpdateLinkAttributes :: UpdateLinkAttributesResponse -> TestTree
responseUpdateLinkAttributes =
  res
    "UpdateLinkAttributesResponse"
    "fixture/UpdateLinkAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLinkAttributes)

responseAttachToIndex :: AttachToIndexResponse -> TestTree
responseAttachToIndex =
  res
    "AttachToIndexResponse"
    "fixture/AttachToIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachToIndex)

responseListObjectPolicies :: ListObjectPoliciesResponse -> TestTree
responseListObjectPolicies =
  res
    "ListObjectPoliciesResponse"
    "fixture/ListObjectPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectPolicies)

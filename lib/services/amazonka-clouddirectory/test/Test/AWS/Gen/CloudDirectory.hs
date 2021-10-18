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

import Data.Proxy
import Network.AWS.CloudDirectory
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
--         [ requestListTypedLinkFacetNames $
--             newListTypedLinkFacetNames
--
--         , requestGetDirectory $
--             newGetDirectory
--
--         , requestAttachTypedLink $
--             newAttachTypedLink
--
--         , requestGetObjectInformation $
--             newGetObjectInformation
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestUpdateFacet $
--             newUpdateFacet
--
--         , requestGetObjectAttributes $
--             newGetObjectAttributes
--
--         , requestUpgradeAppliedSchema $
--             newUpgradeAppliedSchema
--
--         , requestDeleteFacet $
--             newDeleteFacet
--
--         , requestListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributes
--
--         , requestUpgradePublishedSchema $
--             newUpgradePublishedSchema
--
--         , requestCreateObject $
--             newCreateObject
--
--         , requestBatchWrite $
--             newBatchWrite
--
--         , requestUpdateLinkAttributes $
--             newUpdateLinkAttributes
--
--         , requestListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArns
--
--         , requestGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformation
--
--         , requestDetachTypedLink $
--             newDetachTypedLink
--
--         , requestGetFacet $
--             newGetFacet
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListIncomingTypedLinks $
--             newListIncomingTypedLinks
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
--         , requestListObjectAttributes $
--             newListObjectAttributes
--
--         , requestDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacet
--
--         , requestPublishSchema $
--             newPublishSchema
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacet
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestListPublishedSchemaArns $
--             newListPublishedSchemaArns
--
--         , requestListDirectories $
--             newListDirectories
--
--         , requestListObjectParentPaths $
--             newListObjectParentPaths
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestLookupPolicy $
--             newLookupPolicy
--
--         , requestCreateIndex $
--             newCreateIndex
--
--         , requestDetachFromIndex $
--             newDetachFromIndex
--
--         , requestListAttachedIndices $
--             newListAttachedIndices
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestListObjectChildren $
--             newListObjectChildren
--
--         , requestListIndex $
--             newListIndex
--
--         , requestListObjectPolicies $
--             newListObjectPolicies
--
--         , requestGetLinkAttributes $
--             newGetLinkAttributes
--
--         , requestCreateFacet $
--             newCreateFacet
--
--         , requestPutSchemaFromJson $
--             newPutSchemaFromJson
--
--         , requestBatchRead $
--             newBatchRead
--
--         , requestAttachObject $
--             newAttachObject
--
--         , requestAttachToIndex $
--             newAttachToIndex
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestListFacetNames $
--             newListFacetNames
--
--         , requestListOutgoingTypedLinks $
--             newListOutgoingTypedLinks
--
--         , requestUpdateObjectAttributes $
--             newUpdateObjectAttributes
--
--         , requestListAppliedSchemaArns $
--             newListAppliedSchemaArns
--
--         , requestListManagedSchemaArns $
--             newListManagedSchemaArns
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestRemoveFacetFromObject $
--             newRemoveFacetFromObject
--
--         , requestListFacetAttributes $
--             newListFacetAttributes
--
--         , requestGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersion
--
--         , requestEnableDirectory $
--             newEnableDirectory
--
--         , requestApplySchema $
--             newApplySchema
--
--         , requestDisableDirectory $
--             newDisableDirectory
--
--         , requestCreateTypedLinkFacet $
--             newCreateTypedLinkFacet
--
--         , requestGetSchemaAsJson $
--             newGetSchemaAsJson
--
--         , requestDetachObject $
--             newDetachObject
--
--         , requestAddFacetToObject $
--             newAddFacetToObject
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseListTypedLinkFacetNames $
--             newListTypedLinkFacetNamesResponse
--
--         , responseGetDirectory $
--             newGetDirectoryResponse
--
--         , responseAttachTypedLink $
--             newAttachTypedLinkResponse
--
--         , responseGetObjectInformation $
--             newGetObjectInformationResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseUpdateFacet $
--             newUpdateFacetResponse
--
--         , responseGetObjectAttributes $
--             newGetObjectAttributesResponse
--
--         , responseUpgradeAppliedSchema $
--             newUpgradeAppliedSchemaResponse
--
--         , responseDeleteFacet $
--             newDeleteFacetResponse
--
--         , responseListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributesResponse
--
--         , responseUpgradePublishedSchema $
--             newUpgradePublishedSchemaResponse
--
--         , responseCreateObject $
--             newCreateObjectResponse
--
--         , responseBatchWrite $
--             newBatchWriteResponse
--
--         , responseUpdateLinkAttributes $
--             newUpdateLinkAttributesResponse
--
--         , responseListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArnsResponse
--
--         , responseGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformationResponse
--
--         , responseDetachTypedLink $
--             newDetachTypedLinkResponse
--
--         , responseGetFacet $
--             newGetFacetResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListIncomingTypedLinks $
--             newListIncomingTypedLinksResponse
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
--         , responseListObjectAttributes $
--             newListObjectAttributesResponse
--
--         , responseDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacetResponse
--
--         , responsePublishSchema $
--             newPublishSchemaResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacetResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseListPublishedSchemaArns $
--             newListPublishedSchemaArnsResponse
--
--         , responseListDirectories $
--             newListDirectoriesResponse
--
--         , responseListObjectParentPaths $
--             newListObjectParentPathsResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseLookupPolicy $
--             newLookupPolicyResponse
--
--         , responseCreateIndex $
--             newCreateIndexResponse
--
--         , responseDetachFromIndex $
--             newDetachFromIndexResponse
--
--         , responseListAttachedIndices $
--             newListAttachedIndicesResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseListObjectChildren $
--             newListObjectChildrenResponse
--
--         , responseListIndex $
--             newListIndexResponse
--
--         , responseListObjectPolicies $
--             newListObjectPoliciesResponse
--
--         , responseGetLinkAttributes $
--             newGetLinkAttributesResponse
--
--         , responseCreateFacet $
--             newCreateFacetResponse
--
--         , responsePutSchemaFromJson $
--             newPutSchemaFromJsonResponse
--
--         , responseBatchRead $
--             newBatchReadResponse
--
--         , responseAttachObject $
--             newAttachObjectResponse
--
--         , responseAttachToIndex $
--             newAttachToIndexResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseListFacetNames $
--             newListFacetNamesResponse
--
--         , responseListOutgoingTypedLinks $
--             newListOutgoingTypedLinksResponse
--
--         , responseUpdateObjectAttributes $
--             newUpdateObjectAttributesResponse
--
--         , responseListAppliedSchemaArns $
--             newListAppliedSchemaArnsResponse
--
--         , responseListManagedSchemaArns $
--             newListManagedSchemaArnsResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseRemoveFacetFromObject $
--             newRemoveFacetFromObjectResponse
--
--         , responseListFacetAttributes $
--             newListFacetAttributesResponse
--
--         , responseGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersionResponse
--
--         , responseEnableDirectory $
--             newEnableDirectoryResponse
--
--         , responseApplySchema $
--             newApplySchemaResponse
--
--         , responseDisableDirectory $
--             newDisableDirectoryResponse
--
--         , responseCreateTypedLinkFacet $
--             newCreateTypedLinkFacetResponse
--
--         , responseGetSchemaAsJson $
--             newGetSchemaAsJsonResponse
--
--         , responseDetachObject $
--             newDetachObjectResponse
--
--         , responseAddFacetToObject $
--             newAddFacetToObjectResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestListTypedLinkFacetNames :: ListTypedLinkFacetNames -> TestTree
requestListTypedLinkFacetNames =
  req
    "ListTypedLinkFacetNames"
    "fixture/ListTypedLinkFacetNames.yaml"

requestGetDirectory :: GetDirectory -> TestTree
requestGetDirectory =
  req
    "GetDirectory"
    "fixture/GetDirectory.yaml"

requestAttachTypedLink :: AttachTypedLink -> TestTree
requestAttachTypedLink =
  req
    "AttachTypedLink"
    "fixture/AttachTypedLink.yaml"

requestGetObjectInformation :: GetObjectInformation -> TestTree
requestGetObjectInformation =
  req
    "GetObjectInformation"
    "fixture/GetObjectInformation.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestUpdateFacet :: UpdateFacet -> TestTree
requestUpdateFacet =
  req
    "UpdateFacet"
    "fixture/UpdateFacet.yaml"

requestGetObjectAttributes :: GetObjectAttributes -> TestTree
requestGetObjectAttributes =
  req
    "GetObjectAttributes"
    "fixture/GetObjectAttributes.yaml"

requestUpgradeAppliedSchema :: UpgradeAppliedSchema -> TestTree
requestUpgradeAppliedSchema =
  req
    "UpgradeAppliedSchema"
    "fixture/UpgradeAppliedSchema.yaml"

requestDeleteFacet :: DeleteFacet -> TestTree
requestDeleteFacet =
  req
    "DeleteFacet"
    "fixture/DeleteFacet.yaml"

requestListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributes -> TestTree
requestListTypedLinkFacetAttributes =
  req
    "ListTypedLinkFacetAttributes"
    "fixture/ListTypedLinkFacetAttributes.yaml"

requestUpgradePublishedSchema :: UpgradePublishedSchema -> TestTree
requestUpgradePublishedSchema =
  req
    "UpgradePublishedSchema"
    "fixture/UpgradePublishedSchema.yaml"

requestCreateObject :: CreateObject -> TestTree
requestCreateObject =
  req
    "CreateObject"
    "fixture/CreateObject.yaml"

requestBatchWrite :: BatchWrite -> TestTree
requestBatchWrite =
  req
    "BatchWrite"
    "fixture/BatchWrite.yaml"

requestUpdateLinkAttributes :: UpdateLinkAttributes -> TestTree
requestUpdateLinkAttributes =
  req
    "UpdateLinkAttributes"
    "fixture/UpdateLinkAttributes.yaml"

requestListDevelopmentSchemaArns :: ListDevelopmentSchemaArns -> TestTree
requestListDevelopmentSchemaArns =
  req
    "ListDevelopmentSchemaArns"
    "fixture/ListDevelopmentSchemaArns.yaml"

requestGetTypedLinkFacetInformation :: GetTypedLinkFacetInformation -> TestTree
requestGetTypedLinkFacetInformation =
  req
    "GetTypedLinkFacetInformation"
    "fixture/GetTypedLinkFacetInformation.yaml"

requestDetachTypedLink :: DetachTypedLink -> TestTree
requestDetachTypedLink =
  req
    "DetachTypedLink"
    "fixture/DetachTypedLink.yaml"

requestGetFacet :: GetFacet -> TestTree
requestGetFacet =
  req
    "GetFacet"
    "fixture/GetFacet.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListIncomingTypedLinks :: ListIncomingTypedLinks -> TestTree
requestListIncomingTypedLinks =
  req
    "ListIncomingTypedLinks"
    "fixture/ListIncomingTypedLinks.yaml"

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

requestListObjectAttributes :: ListObjectAttributes -> TestTree
requestListObjectAttributes =
  req
    "ListObjectAttributes"
    "fixture/ListObjectAttributes.yaml"

requestDeleteTypedLinkFacet :: DeleteTypedLinkFacet -> TestTree
requestDeleteTypedLinkFacet =
  req
    "DeleteTypedLinkFacet"
    "fixture/DeleteTypedLinkFacet.yaml"

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

requestUpdateTypedLinkFacet :: UpdateTypedLinkFacet -> TestTree
requestUpdateTypedLinkFacet =
  req
    "UpdateTypedLinkFacet"
    "fixture/UpdateTypedLinkFacet.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

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

requestListObjectParentPaths :: ListObjectParentPaths -> TestTree
requestListObjectParentPaths =
  req
    "ListObjectParentPaths"
    "fixture/ListObjectParentPaths.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestLookupPolicy :: LookupPolicy -> TestTree
requestLookupPolicy =
  req
    "LookupPolicy"
    "fixture/LookupPolicy.yaml"

requestCreateIndex :: CreateIndex -> TestTree
requestCreateIndex =
  req
    "CreateIndex"
    "fixture/CreateIndex.yaml"

requestDetachFromIndex :: DetachFromIndex -> TestTree
requestDetachFromIndex =
  req
    "DetachFromIndex"
    "fixture/DetachFromIndex.yaml"

requestListAttachedIndices :: ListAttachedIndices -> TestTree
requestListAttachedIndices =
  req
    "ListAttachedIndices"
    "fixture/ListAttachedIndices.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestListObjectChildren :: ListObjectChildren -> TestTree
requestListObjectChildren =
  req
    "ListObjectChildren"
    "fixture/ListObjectChildren.yaml"

requestListIndex :: ListIndex -> TestTree
requestListIndex =
  req
    "ListIndex"
    "fixture/ListIndex.yaml"

requestListObjectPolicies :: ListObjectPolicies -> TestTree
requestListObjectPolicies =
  req
    "ListObjectPolicies"
    "fixture/ListObjectPolicies.yaml"

requestGetLinkAttributes :: GetLinkAttributes -> TestTree
requestGetLinkAttributes =
  req
    "GetLinkAttributes"
    "fixture/GetLinkAttributes.yaml"

requestCreateFacet :: CreateFacet -> TestTree
requestCreateFacet =
  req
    "CreateFacet"
    "fixture/CreateFacet.yaml"

requestPutSchemaFromJson :: PutSchemaFromJson -> TestTree
requestPutSchemaFromJson =
  req
    "PutSchemaFromJson"
    "fixture/PutSchemaFromJson.yaml"

requestBatchRead :: BatchRead -> TestTree
requestBatchRead =
  req
    "BatchRead"
    "fixture/BatchRead.yaml"

requestAttachObject :: AttachObject -> TestTree
requestAttachObject =
  req
    "AttachObject"
    "fixture/AttachObject.yaml"

requestAttachToIndex :: AttachToIndex -> TestTree
requestAttachToIndex =
  req
    "AttachToIndex"
    "fixture/AttachToIndex.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestListFacetNames :: ListFacetNames -> TestTree
requestListFacetNames =
  req
    "ListFacetNames"
    "fixture/ListFacetNames.yaml"

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

requestListAppliedSchemaArns :: ListAppliedSchemaArns -> TestTree
requestListAppliedSchemaArns =
  req
    "ListAppliedSchemaArns"
    "fixture/ListAppliedSchemaArns.yaml"

requestListManagedSchemaArns :: ListManagedSchemaArns -> TestTree
requestListManagedSchemaArns =
  req
    "ListManagedSchemaArns"
    "fixture/ListManagedSchemaArns.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestRemoveFacetFromObject :: RemoveFacetFromObject -> TestTree
requestRemoveFacetFromObject =
  req
    "RemoveFacetFromObject"
    "fixture/RemoveFacetFromObject.yaml"

requestListFacetAttributes :: ListFacetAttributes -> TestTree
requestListFacetAttributes =
  req
    "ListFacetAttributes"
    "fixture/ListFacetAttributes.yaml"

requestGetAppliedSchemaVersion :: GetAppliedSchemaVersion -> TestTree
requestGetAppliedSchemaVersion =
  req
    "GetAppliedSchemaVersion"
    "fixture/GetAppliedSchemaVersion.yaml"

requestEnableDirectory :: EnableDirectory -> TestTree
requestEnableDirectory =
  req
    "EnableDirectory"
    "fixture/EnableDirectory.yaml"

requestApplySchema :: ApplySchema -> TestTree
requestApplySchema =
  req
    "ApplySchema"
    "fixture/ApplySchema.yaml"

requestDisableDirectory :: DisableDirectory -> TestTree
requestDisableDirectory =
  req
    "DisableDirectory"
    "fixture/DisableDirectory.yaml"

requestCreateTypedLinkFacet :: CreateTypedLinkFacet -> TestTree
requestCreateTypedLinkFacet =
  req
    "CreateTypedLinkFacet"
    "fixture/CreateTypedLinkFacet.yaml"

requestGetSchemaAsJson :: GetSchemaAsJson -> TestTree
requestGetSchemaAsJson =
  req
    "GetSchemaAsJson"
    "fixture/GetSchemaAsJson.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseListTypedLinkFacetNames :: ListTypedLinkFacetNamesResponse -> TestTree
responseListTypedLinkFacetNames =
  res
    "ListTypedLinkFacetNamesResponse"
    "fixture/ListTypedLinkFacetNamesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypedLinkFacetNames)

responseGetDirectory :: GetDirectoryResponse -> TestTree
responseGetDirectory =
  res
    "GetDirectoryResponse"
    "fixture/GetDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetDirectory)

responseAttachTypedLink :: AttachTypedLinkResponse -> TestTree
responseAttachTypedLink =
  res
    "AttachTypedLinkResponse"
    "fixture/AttachTypedLinkResponse.proto"
    defaultService
    (Proxy :: Proxy AttachTypedLink)

responseGetObjectInformation :: GetObjectInformationResponse -> TestTree
responseGetObjectInformation =
  res
    "GetObjectInformationResponse"
    "fixture/GetObjectInformationResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectInformation)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteObject)

responseUpdateFacet :: UpdateFacetResponse -> TestTree
responseUpdateFacet =
  res
    "UpdateFacetResponse"
    "fixture/UpdateFacetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFacet)

responseGetObjectAttributes :: GetObjectAttributesResponse -> TestTree
responseGetObjectAttributes =
  res
    "GetObjectAttributesResponse"
    "fixture/GetObjectAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectAttributes)

responseUpgradeAppliedSchema :: UpgradeAppliedSchemaResponse -> TestTree
responseUpgradeAppliedSchema =
  res
    "UpgradeAppliedSchemaResponse"
    "fixture/UpgradeAppliedSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpgradeAppliedSchema)

responseDeleteFacet :: DeleteFacetResponse -> TestTree
responseDeleteFacet =
  res
    "DeleteFacetResponse"
    "fixture/DeleteFacetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFacet)

responseListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributesResponse -> TestTree
responseListTypedLinkFacetAttributes =
  res
    "ListTypedLinkFacetAttributesResponse"
    "fixture/ListTypedLinkFacetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypedLinkFacetAttributes)

responseUpgradePublishedSchema :: UpgradePublishedSchemaResponse -> TestTree
responseUpgradePublishedSchema =
  res
    "UpgradePublishedSchemaResponse"
    "fixture/UpgradePublishedSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpgradePublishedSchema)

responseCreateObject :: CreateObjectResponse -> TestTree
responseCreateObject =
  res
    "CreateObjectResponse"
    "fixture/CreateObjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateObject)

responseBatchWrite :: BatchWriteResponse -> TestTree
responseBatchWrite =
  res
    "BatchWriteResponse"
    "fixture/BatchWriteResponse.proto"
    defaultService
    (Proxy :: Proxy BatchWrite)

responseUpdateLinkAttributes :: UpdateLinkAttributesResponse -> TestTree
responseUpdateLinkAttributes =
  res
    "UpdateLinkAttributesResponse"
    "fixture/UpdateLinkAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLinkAttributes)

responseListDevelopmentSchemaArns :: ListDevelopmentSchemaArnsResponse -> TestTree
responseListDevelopmentSchemaArns =
  res
    "ListDevelopmentSchemaArnsResponse"
    "fixture/ListDevelopmentSchemaArnsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevelopmentSchemaArns)

responseGetTypedLinkFacetInformation :: GetTypedLinkFacetInformationResponse -> TestTree
responseGetTypedLinkFacetInformation =
  res
    "GetTypedLinkFacetInformationResponse"
    "fixture/GetTypedLinkFacetInformationResponse.proto"
    defaultService
    (Proxy :: Proxy GetTypedLinkFacetInformation)

responseDetachTypedLink :: DetachTypedLinkResponse -> TestTree
responseDetachTypedLink =
  res
    "DetachTypedLinkResponse"
    "fixture/DetachTypedLinkResponse.proto"
    defaultService
    (Proxy :: Proxy DetachTypedLink)

responseGetFacet :: GetFacetResponse -> TestTree
responseGetFacet =
  res
    "GetFacetResponse"
    "fixture/GetFacetResponse.proto"
    defaultService
    (Proxy :: Proxy GetFacet)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListIncomingTypedLinks :: ListIncomingTypedLinksResponse -> TestTree
responseListIncomingTypedLinks =
  res
    "ListIncomingTypedLinksResponse"
    "fixture/ListIncomingTypedLinksResponse.proto"
    defaultService
    (Proxy :: Proxy ListIncomingTypedLinks)

responseListObjectParents :: ListObjectParentsResponse -> TestTree
responseListObjectParents =
  res
    "ListObjectParentsResponse"
    "fixture/ListObjectParentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectParents)

responseListPolicyAttachments :: ListPolicyAttachmentsResponse -> TestTree
responseListPolicyAttachments =
  res
    "ListPolicyAttachmentsResponse"
    "fixture/ListPolicyAttachmentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyAttachments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListObjectAttributes :: ListObjectAttributesResponse -> TestTree
responseListObjectAttributes =
  res
    "ListObjectAttributesResponse"
    "fixture/ListObjectAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectAttributes)

responseDeleteTypedLinkFacet :: DeleteTypedLinkFacetResponse -> TestTree
responseDeleteTypedLinkFacet =
  res
    "DeleteTypedLinkFacetResponse"
    "fixture/DeleteTypedLinkFacetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTypedLinkFacet)

responsePublishSchema :: PublishSchemaResponse -> TestTree
responsePublishSchema =
  res
    "PublishSchemaResponse"
    "fixture/PublishSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy PublishSchema)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectory)

responseUpdateTypedLinkFacet :: UpdateTypedLinkFacetResponse -> TestTree
responseUpdateTypedLinkFacet =
  res
    "UpdateTypedLinkFacetResponse"
    "fixture/UpdateTypedLinkFacetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTypedLinkFacet)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectory)

responseListPublishedSchemaArns :: ListPublishedSchemaArnsResponse -> TestTree
responseListPublishedSchemaArns =
  res
    "ListPublishedSchemaArnsResponse"
    "fixture/ListPublishedSchemaArnsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPublishedSchemaArns)

responseListDirectories :: ListDirectoriesResponse -> TestTree
responseListDirectories =
  res
    "ListDirectoriesResponse"
    "fixture/ListDirectoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDirectories)

responseListObjectParentPaths :: ListObjectParentPathsResponse -> TestTree
responseListObjectParentPaths =
  res
    "ListObjectParentPathsResponse"
    "fixture/ListObjectParentPathsResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectParentPaths)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSchema)

responseLookupPolicy :: LookupPolicyResponse -> TestTree
responseLookupPolicy =
  res
    "LookupPolicyResponse"
    "fixture/LookupPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy LookupPolicy)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex =
  res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIndex)

responseDetachFromIndex :: DetachFromIndexResponse -> TestTree
responseDetachFromIndex =
  res
    "DetachFromIndexResponse"
    "fixture/DetachFromIndexResponse.proto"
    defaultService
    (Proxy :: Proxy DetachFromIndex)

responseListAttachedIndices :: ListAttachedIndicesResponse -> TestTree
responseListAttachedIndices =
  res
    "ListAttachedIndicesResponse"
    "fixture/ListAttachedIndicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAttachedIndices)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachPolicy)

responseListObjectChildren :: ListObjectChildrenResponse -> TestTree
responseListObjectChildren =
  res
    "ListObjectChildrenResponse"
    "fixture/ListObjectChildrenResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectChildren)

responseListIndex :: ListIndexResponse -> TestTree
responseListIndex =
  res
    "ListIndexResponse"
    "fixture/ListIndexResponse.proto"
    defaultService
    (Proxy :: Proxy ListIndex)

responseListObjectPolicies :: ListObjectPoliciesResponse -> TestTree
responseListObjectPolicies =
  res
    "ListObjectPoliciesResponse"
    "fixture/ListObjectPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectPolicies)

responseGetLinkAttributes :: GetLinkAttributesResponse -> TestTree
responseGetLinkAttributes =
  res
    "GetLinkAttributesResponse"
    "fixture/GetLinkAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetLinkAttributes)

responseCreateFacet :: CreateFacetResponse -> TestTree
responseCreateFacet =
  res
    "CreateFacetResponse"
    "fixture/CreateFacetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFacet)

responsePutSchemaFromJson :: PutSchemaFromJsonResponse -> TestTree
responsePutSchemaFromJson =
  res
    "PutSchemaFromJsonResponse"
    "fixture/PutSchemaFromJsonResponse.proto"
    defaultService
    (Proxy :: Proxy PutSchemaFromJson)

responseBatchRead :: BatchReadResponse -> TestTree
responseBatchRead =
  res
    "BatchReadResponse"
    "fixture/BatchReadResponse.proto"
    defaultService
    (Proxy :: Proxy BatchRead)

responseAttachObject :: AttachObjectResponse -> TestTree
responseAttachObject =
  res
    "AttachObjectResponse"
    "fixture/AttachObjectResponse.proto"
    defaultService
    (Proxy :: Proxy AttachObject)

responseAttachToIndex :: AttachToIndexResponse -> TestTree
responseAttachToIndex =
  res
    "AttachToIndexResponse"
    "fixture/AttachToIndexResponse.proto"
    defaultService
    (Proxy :: Proxy AttachToIndex)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachPolicy)

responseListFacetNames :: ListFacetNamesResponse -> TestTree
responseListFacetNames =
  res
    "ListFacetNamesResponse"
    "fixture/ListFacetNamesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFacetNames)

responseListOutgoingTypedLinks :: ListOutgoingTypedLinksResponse -> TestTree
responseListOutgoingTypedLinks =
  res
    "ListOutgoingTypedLinksResponse"
    "fixture/ListOutgoingTypedLinksResponse.proto"
    defaultService
    (Proxy :: Proxy ListOutgoingTypedLinks)

responseUpdateObjectAttributes :: UpdateObjectAttributesResponse -> TestTree
responseUpdateObjectAttributes =
  res
    "UpdateObjectAttributesResponse"
    "fixture/UpdateObjectAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateObjectAttributes)

responseListAppliedSchemaArns :: ListAppliedSchemaArnsResponse -> TestTree
responseListAppliedSchemaArns =
  res
    "ListAppliedSchemaArnsResponse"
    "fixture/ListAppliedSchemaArnsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAppliedSchemaArns)

responseListManagedSchemaArns :: ListManagedSchemaArnsResponse -> TestTree
responseListManagedSchemaArns =
  res
    "ListManagedSchemaArnsResponse"
    "fixture/ListManagedSchemaArnsResponse.proto"
    defaultService
    (Proxy :: Proxy ListManagedSchemaArns)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchema)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSchema)

responseRemoveFacetFromObject :: RemoveFacetFromObjectResponse -> TestTree
responseRemoveFacetFromObject =
  res
    "RemoveFacetFromObjectResponse"
    "fixture/RemoveFacetFromObjectResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveFacetFromObject)

responseListFacetAttributes :: ListFacetAttributesResponse -> TestTree
responseListFacetAttributes =
  res
    "ListFacetAttributesResponse"
    "fixture/ListFacetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFacetAttributes)

responseGetAppliedSchemaVersion :: GetAppliedSchemaVersionResponse -> TestTree
responseGetAppliedSchemaVersion =
  res
    "GetAppliedSchemaVersionResponse"
    "fixture/GetAppliedSchemaVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppliedSchemaVersion)

responseEnableDirectory :: EnableDirectoryResponse -> TestTree
responseEnableDirectory =
  res
    "EnableDirectoryResponse"
    "fixture/EnableDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy EnableDirectory)

responseApplySchema :: ApplySchemaResponse -> TestTree
responseApplySchema =
  res
    "ApplySchemaResponse"
    "fixture/ApplySchemaResponse.proto"
    defaultService
    (Proxy :: Proxy ApplySchema)

responseDisableDirectory :: DisableDirectoryResponse -> TestTree
responseDisableDirectory =
  res
    "DisableDirectoryResponse"
    "fixture/DisableDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy DisableDirectory)

responseCreateTypedLinkFacet :: CreateTypedLinkFacetResponse -> TestTree
responseCreateTypedLinkFacet =
  res
    "CreateTypedLinkFacetResponse"
    "fixture/CreateTypedLinkFacetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTypedLinkFacet)

responseGetSchemaAsJson :: GetSchemaAsJsonResponse -> TestTree
responseGetSchemaAsJson =
  res
    "GetSchemaAsJsonResponse"
    "fixture/GetSchemaAsJsonResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaAsJson)

responseDetachObject :: DetachObjectResponse -> TestTree
responseDetachObject =
  res
    "DetachObjectResponse"
    "fixture/DetachObjectResponse.proto"
    defaultService
    (Proxy :: Proxy DetachObject)

responseAddFacetToObject :: AddFacetToObjectResponse -> TestTree
responseAddFacetToObject =
  res
    "AddFacetToObjectResponse"
    "fixture/AddFacetToObjectResponse.proto"
    defaultService
    (Proxy :: Proxy AddFacetToObject)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

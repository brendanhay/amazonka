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
--         [ requestGetDirectory $
--             newGetDirectory
--
--         , requestListTypedLinkFacetNames $
--             newListTypedLinkFacetNames
--
--         , requestGetObjectInformation $
--             newGetObjectInformation
--
--         , requestAttachTypedLink $
--             newAttachTypedLink
--
--         , requestDeleteFacet $
--             newDeleteFacet
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
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributes
--
--         , requestCreateObject $
--             newCreateObject
--
--         , requestBatchWrite $
--             newBatchWrite
--
--         , requestUpgradePublishedSchema $
--             newUpgradePublishedSchema
--
--         , requestListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArns
--
--         , requestUpdateLinkAttributes $
--             newUpdateLinkAttributes
--
--         , requestDetachTypedLink $
--             newDetachTypedLink
--
--         , requestGetFacet $
--             newGetFacet
--
--         , requestGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformation
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListIncomingTypedLinks $
--             newListIncomingTypedLinks
--
--         , requestPublishSchema $
--             newPublishSchema
--
--         , requestUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacet
--
--         , requestListObjectParents $
--             newListObjectParents
--
--         , requestListPolicyAttachments $
--             newListPolicyAttachments
--
--         , requestListObjectAttributes $
--             newListObjectAttributes
--
--         , requestDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacet
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestListObjectParentPaths $
--             newListObjectParentPaths
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestListPublishedSchemaArns $
--             newListPublishedSchemaArns
--
--         , requestListDirectories $
--             newListDirectories
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestLookupPolicy $
--             newLookupPolicy
--
--         , requestCreateIndex $
--             newCreateIndex
--
--         , requestListAttachedIndices $
--             newListAttachedIndices
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestDetachFromIndex $
--             newDetachFromIndex
--
--         , requestListIndex $
--             newListIndex
--
--         , requestListObjectChildren $
--             newListObjectChildren
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
--         , requestBatchRead $
--             newBatchRead
--
--         , requestAttachObject $
--             newAttachObject
--
--         , requestAttachToIndex $
--             newAttachToIndex
--
--         , requestPutSchemaFromJson $
--             newPutSchemaFromJson
--
--         , requestUpdateObjectAttributes $
--             newUpdateObjectAttributes
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
--         , requestEnableDirectory $
--             newEnableDirectory
--
--         , requestListAppliedSchemaArns $
--             newListAppliedSchemaArns
--
--         , requestListManagedSchemaArns $
--             newListManagedSchemaArns
--
--         , requestGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersion
--
--         , requestRemoveFacetFromObject $
--             newRemoveFacetFromObject
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestListFacetAttributes $
--             newListFacetAttributes
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestGetSchemaAsJson $
--             newGetSchemaAsJson
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
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestAddFacetToObject $
--             newAddFacetToObject
--
--         , requestDetachObject $
--             newDetachObject
--
--           ]

--     , testGroup "response"
--         [ responseGetDirectory $
--             newGetDirectoryResponse
--
--         , responseListTypedLinkFacetNames $
--             newListTypedLinkFacetNamesResponse
--
--         , responseGetObjectInformation $
--             newGetObjectInformationResponse
--
--         , responseAttachTypedLink $
--             newAttachTypedLinkResponse
--
--         , responseDeleteFacet $
--             newDeleteFacetResponse
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
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributesResponse
--
--         , responseCreateObject $
--             newCreateObjectResponse
--
--         , responseBatchWrite $
--             newBatchWriteResponse
--
--         , responseUpgradePublishedSchema $
--             newUpgradePublishedSchemaResponse
--
--         , responseListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArnsResponse
--
--         , responseUpdateLinkAttributes $
--             newUpdateLinkAttributesResponse
--
--         , responseDetachTypedLink $
--             newDetachTypedLinkResponse
--
--         , responseGetFacet $
--             newGetFacetResponse
--
--         , responseGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListIncomingTypedLinks $
--             newListIncomingTypedLinksResponse
--
--         , responsePublishSchema $
--             newPublishSchemaResponse
--
--         , responseUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacetResponse
--
--         , responseListObjectParents $
--             newListObjectParentsResponse
--
--         , responseListPolicyAttachments $
--             newListPolicyAttachmentsResponse
--
--         , responseListObjectAttributes $
--             newListObjectAttributesResponse
--
--         , responseDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacetResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseListObjectParentPaths $
--             newListObjectParentPathsResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseListPublishedSchemaArns $
--             newListPublishedSchemaArnsResponse
--
--         , responseListDirectories $
--             newListDirectoriesResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseLookupPolicy $
--             newLookupPolicyResponse
--
--         , responseCreateIndex $
--             newCreateIndexResponse
--
--         , responseListAttachedIndices $
--             newListAttachedIndicesResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseDetachFromIndex $
--             newDetachFromIndexResponse
--
--         , responseListIndex $
--             newListIndexResponse
--
--         , responseListObjectChildren $
--             newListObjectChildrenResponse
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
--         , responseBatchRead $
--             newBatchReadResponse
--
--         , responseAttachObject $
--             newAttachObjectResponse
--
--         , responseAttachToIndex $
--             newAttachToIndexResponse
--
--         , responsePutSchemaFromJson $
--             newPutSchemaFromJsonResponse
--
--         , responseUpdateObjectAttributes $
--             newUpdateObjectAttributesResponse
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
--         , responseEnableDirectory $
--             newEnableDirectoryResponse
--
--         , responseListAppliedSchemaArns $
--             newListAppliedSchemaArnsResponse
--
--         , responseListManagedSchemaArns $
--             newListManagedSchemaArnsResponse
--
--         , responseGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersionResponse
--
--         , responseRemoveFacetFromObject $
--             newRemoveFacetFromObjectResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseListFacetAttributes $
--             newListFacetAttributesResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseGetSchemaAsJson $
--             newGetSchemaAsJsonResponse
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
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseAddFacetToObject $
--             newAddFacetToObjectResponse
--
--         , responseDetachObject $
--             newDetachObjectResponse
--
--           ]
--     ]

-- Requests

requestGetDirectory :: GetDirectory -> TestTree
requestGetDirectory =
  req
    "GetDirectory"
    "fixture/GetDirectory.yaml"

requestListTypedLinkFacetNames :: ListTypedLinkFacetNames -> TestTree
requestListTypedLinkFacetNames =
  req
    "ListTypedLinkFacetNames"
    "fixture/ListTypedLinkFacetNames.yaml"

requestGetObjectInformation :: GetObjectInformation -> TestTree
requestGetObjectInformation =
  req
    "GetObjectInformation"
    "fixture/GetObjectInformation.yaml"

requestAttachTypedLink :: AttachTypedLink -> TestTree
requestAttachTypedLink =
  req
    "AttachTypedLink"
    "fixture/AttachTypedLink.yaml"

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

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributes -> TestTree
requestListTypedLinkFacetAttributes =
  req
    "ListTypedLinkFacetAttributes"
    "fixture/ListTypedLinkFacetAttributes.yaml"

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

requestUpgradePublishedSchema :: UpgradePublishedSchema -> TestTree
requestUpgradePublishedSchema =
  req
    "UpgradePublishedSchema"
    "fixture/UpgradePublishedSchema.yaml"

requestListDevelopmentSchemaArns :: ListDevelopmentSchemaArns -> TestTree
requestListDevelopmentSchemaArns =
  req
    "ListDevelopmentSchemaArns"
    "fixture/ListDevelopmentSchemaArns.yaml"

requestUpdateLinkAttributes :: UpdateLinkAttributes -> TestTree
requestUpdateLinkAttributes =
  req
    "UpdateLinkAttributes"
    "fixture/UpdateLinkAttributes.yaml"

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

requestGetTypedLinkFacetInformation :: GetTypedLinkFacetInformation -> TestTree
requestGetTypedLinkFacetInformation =
  req
    "GetTypedLinkFacetInformation"
    "fixture/GetTypedLinkFacetInformation.yaml"

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

requestPublishSchema :: PublishSchema -> TestTree
requestPublishSchema =
  req
    "PublishSchema"
    "fixture/PublishSchema.yaml"

requestUpdateTypedLinkFacet :: UpdateTypedLinkFacet -> TestTree
requestUpdateTypedLinkFacet =
  req
    "UpdateTypedLinkFacet"
    "fixture/UpdateTypedLinkFacet.yaml"

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

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory =
  req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

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

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

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

requestDetachFromIndex :: DetachFromIndex -> TestTree
requestDetachFromIndex =
  req
    "DetachFromIndex"
    "fixture/DetachFromIndex.yaml"

requestListIndex :: ListIndex -> TestTree
requestListIndex =
  req
    "ListIndex"
    "fixture/ListIndex.yaml"

requestListObjectChildren :: ListObjectChildren -> TestTree
requestListObjectChildren =
  req
    "ListObjectChildren"
    "fixture/ListObjectChildren.yaml"

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

requestPutSchemaFromJson :: PutSchemaFromJson -> TestTree
requestPutSchemaFromJson =
  req
    "PutSchemaFromJson"
    "fixture/PutSchemaFromJson.yaml"

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

requestEnableDirectory :: EnableDirectory -> TestTree
requestEnableDirectory =
  req
    "EnableDirectory"
    "fixture/EnableDirectory.yaml"

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

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestListFacetAttributes :: ListFacetAttributes -> TestTree
requestListFacetAttributes =
  req
    "ListFacetAttributes"
    "fixture/ListFacetAttributes.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestGetSchemaAsJson :: GetSchemaAsJson -> TestTree
requestGetSchemaAsJson =
  req
    "GetSchemaAsJson"
    "fixture/GetSchemaAsJson.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestAddFacetToObject :: AddFacetToObject -> TestTree
requestAddFacetToObject =
  req
    "AddFacetToObject"
    "fixture/AddFacetToObject.yaml"

requestDetachObject :: DetachObject -> TestTree
requestDetachObject =
  req
    "DetachObject"
    "fixture/DetachObject.yaml"

-- Responses

responseGetDirectory :: GetDirectoryResponse -> TestTree
responseGetDirectory =
  res
    "GetDirectoryResponse"
    "fixture/GetDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetDirectory)

responseListTypedLinkFacetNames :: ListTypedLinkFacetNamesResponse -> TestTree
responseListTypedLinkFacetNames =
  res
    "ListTypedLinkFacetNamesResponse"
    "fixture/ListTypedLinkFacetNamesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypedLinkFacetNames)

responseGetObjectInformation :: GetObjectInformationResponse -> TestTree
responseGetObjectInformation =
  res
    "GetObjectInformationResponse"
    "fixture/GetObjectInformationResponse.proto"
    defaultService
    (Proxy :: Proxy GetObjectInformation)

responseAttachTypedLink :: AttachTypedLinkResponse -> TestTree
responseAttachTypedLink =
  res
    "AttachTypedLinkResponse"
    "fixture/AttachTypedLinkResponse.proto"
    defaultService
    (Proxy :: Proxy AttachTypedLink)

responseDeleteFacet :: DeleteFacetResponse -> TestTree
responseDeleteFacet =
  res
    "DeleteFacetResponse"
    "fixture/DeleteFacetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFacet)

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

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteObject)

responseListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributesResponse -> TestTree
responseListTypedLinkFacetAttributes =
  res
    "ListTypedLinkFacetAttributesResponse"
    "fixture/ListTypedLinkFacetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTypedLinkFacetAttributes)

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

responseUpgradePublishedSchema :: UpgradePublishedSchemaResponse -> TestTree
responseUpgradePublishedSchema =
  res
    "UpgradePublishedSchemaResponse"
    "fixture/UpgradePublishedSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpgradePublishedSchema)

responseListDevelopmentSchemaArns :: ListDevelopmentSchemaArnsResponse -> TestTree
responseListDevelopmentSchemaArns =
  res
    "ListDevelopmentSchemaArnsResponse"
    "fixture/ListDevelopmentSchemaArnsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevelopmentSchemaArns)

responseUpdateLinkAttributes :: UpdateLinkAttributesResponse -> TestTree
responseUpdateLinkAttributes =
  res
    "UpdateLinkAttributesResponse"
    "fixture/UpdateLinkAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLinkAttributes)

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

responseGetTypedLinkFacetInformation :: GetTypedLinkFacetInformationResponse -> TestTree
responseGetTypedLinkFacetInformation =
  res
    "GetTypedLinkFacetInformationResponse"
    "fixture/GetTypedLinkFacetInformationResponse.proto"
    defaultService
    (Proxy :: Proxy GetTypedLinkFacetInformation)

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

responsePublishSchema :: PublishSchemaResponse -> TestTree
responsePublishSchema =
  res
    "PublishSchemaResponse"
    "fixture/PublishSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy PublishSchema)

responseUpdateTypedLinkFacet :: UpdateTypedLinkFacetResponse -> TestTree
responseUpdateTypedLinkFacet =
  res
    "UpdateTypedLinkFacetResponse"
    "fixture/UpdateTypedLinkFacetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTypedLinkFacet)

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

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectory)

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

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectory)

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

responseDetachFromIndex :: DetachFromIndexResponse -> TestTree
responseDetachFromIndex =
  res
    "DetachFromIndexResponse"
    "fixture/DetachFromIndexResponse.proto"
    defaultService
    (Proxy :: Proxy DetachFromIndex)

responseListIndex :: ListIndexResponse -> TestTree
responseListIndex =
  res
    "ListIndexResponse"
    "fixture/ListIndexResponse.proto"
    defaultService
    (Proxy :: Proxy ListIndex)

responseListObjectChildren :: ListObjectChildrenResponse -> TestTree
responseListObjectChildren =
  res
    "ListObjectChildrenResponse"
    "fixture/ListObjectChildrenResponse.proto"
    defaultService
    (Proxy :: Proxy ListObjectChildren)

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

responsePutSchemaFromJson :: PutSchemaFromJsonResponse -> TestTree
responsePutSchemaFromJson =
  res
    "PutSchemaFromJsonResponse"
    "fixture/PutSchemaFromJsonResponse.proto"
    defaultService
    (Proxy :: Proxy PutSchemaFromJson)

responseUpdateObjectAttributes :: UpdateObjectAttributesResponse -> TestTree
responseUpdateObjectAttributes =
  res
    "UpdateObjectAttributesResponse"
    "fixture/UpdateObjectAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateObjectAttributes)

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

responseEnableDirectory :: EnableDirectoryResponse -> TestTree
responseEnableDirectory =
  res
    "EnableDirectoryResponse"
    "fixture/EnableDirectoryResponse.proto"
    defaultService
    (Proxy :: Proxy EnableDirectory)

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

responseGetAppliedSchemaVersion :: GetAppliedSchemaVersionResponse -> TestTree
responseGetAppliedSchemaVersion =
  res
    "GetAppliedSchemaVersionResponse"
    "fixture/GetAppliedSchemaVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetAppliedSchemaVersion)

responseRemoveFacetFromObject :: RemoveFacetFromObjectResponse -> TestTree
responseRemoveFacetFromObject =
  res
    "RemoveFacetFromObjectResponse"
    "fixture/RemoveFacetFromObjectResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveFacetFromObject)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSchema)

responseListFacetAttributes :: ListFacetAttributesResponse -> TestTree
responseListFacetAttributes =
  res
    "ListFacetAttributesResponse"
    "fixture/ListFacetAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy ListFacetAttributes)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSchema)

responseGetSchemaAsJson :: GetSchemaAsJsonResponse -> TestTree
responseGetSchemaAsJson =
  res
    "GetSchemaAsJsonResponse"
    "fixture/GetSchemaAsJsonResponse.proto"
    defaultService
    (Proxy :: Proxy GetSchemaAsJson)

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseAddFacetToObject :: AddFacetToObjectResponse -> TestTree
responseAddFacetToObject =
  res
    "AddFacetToObjectResponse"
    "fixture/AddFacetToObjectResponse.proto"
    defaultService
    (Proxy :: Proxy AddFacetToObject)

responseDetachObject :: DetachObjectResponse -> TestTree
responseDetachObject =
  res
    "DetachObjectResponse"
    "fixture/DetachObjectResponse.proto"
    defaultService
    (Proxy :: Proxy DetachObject)

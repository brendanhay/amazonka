{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CloudDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CloudDirectory where

import Amazonka.CloudDirectory
import qualified Data.Proxy as Proxy
import Test.Amazonka.CloudDirectory.Internal
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
--         [ requestAddFacetToObject $
--             newAddFacetToObject
--
--         , requestApplySchema $
--             newApplySchema
--
--         , requestAttachObject $
--             newAttachObject
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestAttachToIndex $
--             newAttachToIndex
--
--         , requestAttachTypedLink $
--             newAttachTypedLink
--
--         , requestBatchRead $
--             newBatchRead
--
--         , requestBatchWrite $
--             newBatchWrite
--
--         , requestCreateDirectory $
--             newCreateDirectory
--
--         , requestCreateFacet $
--             newCreateFacet
--
--         , requestCreateIndex $
--             newCreateIndex
--
--         , requestCreateObject $
--             newCreateObject
--
--         , requestCreateSchema $
--             newCreateSchema
--
--         , requestCreateTypedLinkFacet $
--             newCreateTypedLinkFacet
--
--         , requestDeleteDirectory $
--             newDeleteDirectory
--
--         , requestDeleteFacet $
--             newDeleteFacet
--
--         , requestDeleteObject $
--             newDeleteObject
--
--         , requestDeleteSchema $
--             newDeleteSchema
--
--         , requestDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacet
--
--         , requestDetachFromIndex $
--             newDetachFromIndex
--
--         , requestDetachObject $
--             newDetachObject
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestDetachTypedLink $
--             newDetachTypedLink
--
--         , requestDisableDirectory $
--             newDisableDirectory
--
--         , requestEnableDirectory $
--             newEnableDirectory
--
--         , requestGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersion
--
--         , requestGetDirectory $
--             newGetDirectory
--
--         , requestGetFacet $
--             newGetFacet
--
--         , requestGetLinkAttributes $
--             newGetLinkAttributes
--
--         , requestGetObjectAttributes $
--             newGetObjectAttributes
--
--         , requestGetObjectInformation $
--             newGetObjectInformation
--
--         , requestGetSchemaAsJson $
--             newGetSchemaAsJson
--
--         , requestGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformation
--
--         , requestListAppliedSchemaArns $
--             newListAppliedSchemaArns
--
--         , requestListAttachedIndices $
--             newListAttachedIndices
--
--         , requestListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArns
--
--         , requestListDirectories $
--             newListDirectories
--
--         , requestListFacetAttributes $
--             newListFacetAttributes
--
--         , requestListFacetNames $
--             newListFacetNames
--
--         , requestListIncomingTypedLinks $
--             newListIncomingTypedLinks
--
--         , requestListIndex $
--             newListIndex
--
--         , requestListManagedSchemaArns $
--             newListManagedSchemaArns
--
--         , requestListObjectAttributes $
--             newListObjectAttributes
--
--         , requestListObjectChildren $
--             newListObjectChildren
--
--         , requestListObjectParentPaths $
--             newListObjectParentPaths
--
--         , requestListObjectParents $
--             newListObjectParents
--
--         , requestListObjectPolicies $
--             newListObjectPolicies
--
--         , requestListOutgoingTypedLinks $
--             newListOutgoingTypedLinks
--
--         , requestListPolicyAttachments $
--             newListPolicyAttachments
--
--         , requestListPublishedSchemaArns $
--             newListPublishedSchemaArns
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributes
--
--         , requestListTypedLinkFacetNames $
--             newListTypedLinkFacetNames
--
--         , requestLookupPolicy $
--             newLookupPolicy
--
--         , requestPublishSchema $
--             newPublishSchema
--
--         , requestPutSchemaFromJson $
--             newPutSchemaFromJson
--
--         , requestRemoveFacetFromObject $
--             newRemoveFacetFromObject
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFacet $
--             newUpdateFacet
--
--         , requestUpdateLinkAttributes $
--             newUpdateLinkAttributes
--
--         , requestUpdateObjectAttributes $
--             newUpdateObjectAttributes
--
--         , requestUpdateSchema $
--             newUpdateSchema
--
--         , requestUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacet
--
--         , requestUpgradeAppliedSchema $
--             newUpgradeAppliedSchema
--
--         , requestUpgradePublishedSchema $
--             newUpgradePublishedSchema
--
--           ]

--     , testGroup "response"
--         [ responseAddFacetToObject $
--             newAddFacetToObjectResponse
--
--         , responseApplySchema $
--             newApplySchemaResponse
--
--         , responseAttachObject $
--             newAttachObjectResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseAttachToIndex $
--             newAttachToIndexResponse
--
--         , responseAttachTypedLink $
--             newAttachTypedLinkResponse
--
--         , responseBatchRead $
--             newBatchReadResponse
--
--         , responseBatchWrite $
--             newBatchWriteResponse
--
--         , responseCreateDirectory $
--             newCreateDirectoryResponse
--
--         , responseCreateFacet $
--             newCreateFacetResponse
--
--         , responseCreateIndex $
--             newCreateIndexResponse
--
--         , responseCreateObject $
--             newCreateObjectResponse
--
--         , responseCreateSchema $
--             newCreateSchemaResponse
--
--         , responseCreateTypedLinkFacet $
--             newCreateTypedLinkFacetResponse
--
--         , responseDeleteDirectory $
--             newDeleteDirectoryResponse
--
--         , responseDeleteFacet $
--             newDeleteFacetResponse
--
--         , responseDeleteObject $
--             newDeleteObjectResponse
--
--         , responseDeleteSchema $
--             newDeleteSchemaResponse
--
--         , responseDeleteTypedLinkFacet $
--             newDeleteTypedLinkFacetResponse
--
--         , responseDetachFromIndex $
--             newDetachFromIndexResponse
--
--         , responseDetachObject $
--             newDetachObjectResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseDetachTypedLink $
--             newDetachTypedLinkResponse
--
--         , responseDisableDirectory $
--             newDisableDirectoryResponse
--
--         , responseEnableDirectory $
--             newEnableDirectoryResponse
--
--         , responseGetAppliedSchemaVersion $
--             newGetAppliedSchemaVersionResponse
--
--         , responseGetDirectory $
--             newGetDirectoryResponse
--
--         , responseGetFacet $
--             newGetFacetResponse
--
--         , responseGetLinkAttributes $
--             newGetLinkAttributesResponse
--
--         , responseGetObjectAttributes $
--             newGetObjectAttributesResponse
--
--         , responseGetObjectInformation $
--             newGetObjectInformationResponse
--
--         , responseGetSchemaAsJson $
--             newGetSchemaAsJsonResponse
--
--         , responseGetTypedLinkFacetInformation $
--             newGetTypedLinkFacetInformationResponse
--
--         , responseListAppliedSchemaArns $
--             newListAppliedSchemaArnsResponse
--
--         , responseListAttachedIndices $
--             newListAttachedIndicesResponse
--
--         , responseListDevelopmentSchemaArns $
--             newListDevelopmentSchemaArnsResponse
--
--         , responseListDirectories $
--             newListDirectoriesResponse
--
--         , responseListFacetAttributes $
--             newListFacetAttributesResponse
--
--         , responseListFacetNames $
--             newListFacetNamesResponse
--
--         , responseListIncomingTypedLinks $
--             newListIncomingTypedLinksResponse
--
--         , responseListIndex $
--             newListIndexResponse
--
--         , responseListManagedSchemaArns $
--             newListManagedSchemaArnsResponse
--
--         , responseListObjectAttributes $
--             newListObjectAttributesResponse
--
--         , responseListObjectChildren $
--             newListObjectChildrenResponse
--
--         , responseListObjectParentPaths $
--             newListObjectParentPathsResponse
--
--         , responseListObjectParents $
--             newListObjectParentsResponse
--
--         , responseListObjectPolicies $
--             newListObjectPoliciesResponse
--
--         , responseListOutgoingTypedLinks $
--             newListOutgoingTypedLinksResponse
--
--         , responseListPolicyAttachments $
--             newListPolicyAttachmentsResponse
--
--         , responseListPublishedSchemaArns $
--             newListPublishedSchemaArnsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTypedLinkFacetAttributes $
--             newListTypedLinkFacetAttributesResponse
--
--         , responseListTypedLinkFacetNames $
--             newListTypedLinkFacetNamesResponse
--
--         , responseLookupPolicy $
--             newLookupPolicyResponse
--
--         , responsePublishSchema $
--             newPublishSchemaResponse
--
--         , responsePutSchemaFromJson $
--             newPutSchemaFromJsonResponse
--
--         , responseRemoveFacetFromObject $
--             newRemoveFacetFromObjectResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFacet $
--             newUpdateFacetResponse
--
--         , responseUpdateLinkAttributes $
--             newUpdateLinkAttributesResponse
--
--         , responseUpdateObjectAttributes $
--             newUpdateObjectAttributesResponse
--
--         , responseUpdateSchema $
--             newUpdateSchemaResponse
--
--         , responseUpdateTypedLinkFacet $
--             newUpdateTypedLinkFacetResponse
--
--         , responseUpgradeAppliedSchema $
--             newUpgradeAppliedSchemaResponse
--
--         , responseUpgradePublishedSchema $
--             newUpgradePublishedSchemaResponse
--
--           ]
--     ]

-- Requests

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

requestAttachObject :: AttachObject -> TestTree
requestAttachObject =
  req
    "AttachObject"
    "fixture/AttachObject.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestAttachToIndex :: AttachToIndex -> TestTree
requestAttachToIndex =
  req
    "AttachToIndex"
    "fixture/AttachToIndex.yaml"

requestAttachTypedLink :: AttachTypedLink -> TestTree
requestAttachTypedLink =
  req
    "AttachTypedLink"
    "fixture/AttachTypedLink.yaml"

requestBatchRead :: BatchRead -> TestTree
requestBatchRead =
  req
    "BatchRead"
    "fixture/BatchRead.yaml"

requestBatchWrite :: BatchWrite -> TestTree
requestBatchWrite =
  req
    "BatchWrite"
    "fixture/BatchWrite.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory =
  req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

requestCreateFacet :: CreateFacet -> TestTree
requestCreateFacet =
  req
    "CreateFacet"
    "fixture/CreateFacet.yaml"

requestCreateIndex :: CreateIndex -> TestTree
requestCreateIndex =
  req
    "CreateIndex"
    "fixture/CreateIndex.yaml"

requestCreateObject :: CreateObject -> TestTree
requestCreateObject =
  req
    "CreateObject"
    "fixture/CreateObject.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema =
  req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestCreateTypedLinkFacet :: CreateTypedLinkFacet -> TestTree
requestCreateTypedLinkFacet =
  req
    "CreateTypedLinkFacet"
    "fixture/CreateTypedLinkFacet.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory =
  req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

requestDeleteFacet :: DeleteFacet -> TestTree
requestDeleteFacet =
  req
    "DeleteFacet"
    "fixture/DeleteFacet.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject =
  req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema =
  req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestDeleteTypedLinkFacet :: DeleteTypedLinkFacet -> TestTree
requestDeleteTypedLinkFacet =
  req
    "DeleteTypedLinkFacet"
    "fixture/DeleteTypedLinkFacet.yaml"

requestDetachFromIndex :: DetachFromIndex -> TestTree
requestDetachFromIndex =
  req
    "DetachFromIndex"
    "fixture/DetachFromIndex.yaml"

requestDetachObject :: DetachObject -> TestTree
requestDetachObject =
  req
    "DetachObject"
    "fixture/DetachObject.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestDetachTypedLink :: DetachTypedLink -> TestTree
requestDetachTypedLink =
  req
    "DetachTypedLink"
    "fixture/DetachTypedLink.yaml"

requestDisableDirectory :: DisableDirectory -> TestTree
requestDisableDirectory =
  req
    "DisableDirectory"
    "fixture/DisableDirectory.yaml"

requestEnableDirectory :: EnableDirectory -> TestTree
requestEnableDirectory =
  req
    "EnableDirectory"
    "fixture/EnableDirectory.yaml"

requestGetAppliedSchemaVersion :: GetAppliedSchemaVersion -> TestTree
requestGetAppliedSchemaVersion =
  req
    "GetAppliedSchemaVersion"
    "fixture/GetAppliedSchemaVersion.yaml"

requestGetDirectory :: GetDirectory -> TestTree
requestGetDirectory =
  req
    "GetDirectory"
    "fixture/GetDirectory.yaml"

requestGetFacet :: GetFacet -> TestTree
requestGetFacet =
  req
    "GetFacet"
    "fixture/GetFacet.yaml"

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

requestGetObjectInformation :: GetObjectInformation -> TestTree
requestGetObjectInformation =
  req
    "GetObjectInformation"
    "fixture/GetObjectInformation.yaml"

requestGetSchemaAsJson :: GetSchemaAsJson -> TestTree
requestGetSchemaAsJson =
  req
    "GetSchemaAsJson"
    "fixture/GetSchemaAsJson.yaml"

requestGetTypedLinkFacetInformation :: GetTypedLinkFacetInformation -> TestTree
requestGetTypedLinkFacetInformation =
  req
    "GetTypedLinkFacetInformation"
    "fixture/GetTypedLinkFacetInformation.yaml"

requestListAppliedSchemaArns :: ListAppliedSchemaArns -> TestTree
requestListAppliedSchemaArns =
  req
    "ListAppliedSchemaArns"
    "fixture/ListAppliedSchemaArns.yaml"

requestListAttachedIndices :: ListAttachedIndices -> TestTree
requestListAttachedIndices =
  req
    "ListAttachedIndices"
    "fixture/ListAttachedIndices.yaml"

requestListDevelopmentSchemaArns :: ListDevelopmentSchemaArns -> TestTree
requestListDevelopmentSchemaArns =
  req
    "ListDevelopmentSchemaArns"
    "fixture/ListDevelopmentSchemaArns.yaml"

requestListDirectories :: ListDirectories -> TestTree
requestListDirectories =
  req
    "ListDirectories"
    "fixture/ListDirectories.yaml"

requestListFacetAttributes :: ListFacetAttributes -> TestTree
requestListFacetAttributes =
  req
    "ListFacetAttributes"
    "fixture/ListFacetAttributes.yaml"

requestListFacetNames :: ListFacetNames -> TestTree
requestListFacetNames =
  req
    "ListFacetNames"
    "fixture/ListFacetNames.yaml"

requestListIncomingTypedLinks :: ListIncomingTypedLinks -> TestTree
requestListIncomingTypedLinks =
  req
    "ListIncomingTypedLinks"
    "fixture/ListIncomingTypedLinks.yaml"

requestListIndex :: ListIndex -> TestTree
requestListIndex =
  req
    "ListIndex"
    "fixture/ListIndex.yaml"

requestListManagedSchemaArns :: ListManagedSchemaArns -> TestTree
requestListManagedSchemaArns =
  req
    "ListManagedSchemaArns"
    "fixture/ListManagedSchemaArns.yaml"

requestListObjectAttributes :: ListObjectAttributes -> TestTree
requestListObjectAttributes =
  req
    "ListObjectAttributes"
    "fixture/ListObjectAttributes.yaml"

requestListObjectChildren :: ListObjectChildren -> TestTree
requestListObjectChildren =
  req
    "ListObjectChildren"
    "fixture/ListObjectChildren.yaml"

requestListObjectParentPaths :: ListObjectParentPaths -> TestTree
requestListObjectParentPaths =
  req
    "ListObjectParentPaths"
    "fixture/ListObjectParentPaths.yaml"

requestListObjectParents :: ListObjectParents -> TestTree
requestListObjectParents =
  req
    "ListObjectParents"
    "fixture/ListObjectParents.yaml"

requestListObjectPolicies :: ListObjectPolicies -> TestTree
requestListObjectPolicies =
  req
    "ListObjectPolicies"
    "fixture/ListObjectPolicies.yaml"

requestListOutgoingTypedLinks :: ListOutgoingTypedLinks -> TestTree
requestListOutgoingTypedLinks =
  req
    "ListOutgoingTypedLinks"
    "fixture/ListOutgoingTypedLinks.yaml"

requestListPolicyAttachments :: ListPolicyAttachments -> TestTree
requestListPolicyAttachments =
  req
    "ListPolicyAttachments"
    "fixture/ListPolicyAttachments.yaml"

requestListPublishedSchemaArns :: ListPublishedSchemaArns -> TestTree
requestListPublishedSchemaArns =
  req
    "ListPublishedSchemaArns"
    "fixture/ListPublishedSchemaArns.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributes -> TestTree
requestListTypedLinkFacetAttributes =
  req
    "ListTypedLinkFacetAttributes"
    "fixture/ListTypedLinkFacetAttributes.yaml"

requestListTypedLinkFacetNames :: ListTypedLinkFacetNames -> TestTree
requestListTypedLinkFacetNames =
  req
    "ListTypedLinkFacetNames"
    "fixture/ListTypedLinkFacetNames.yaml"

requestLookupPolicy :: LookupPolicy -> TestTree
requestLookupPolicy =
  req
    "LookupPolicy"
    "fixture/LookupPolicy.yaml"

requestPublishSchema :: PublishSchema -> TestTree
requestPublishSchema =
  req
    "PublishSchema"
    "fixture/PublishSchema.yaml"

requestPutSchemaFromJson :: PutSchemaFromJson -> TestTree
requestPutSchemaFromJson =
  req
    "PutSchemaFromJson"
    "fixture/PutSchemaFromJson.yaml"

requestRemoveFacetFromObject :: RemoveFacetFromObject -> TestTree
requestRemoveFacetFromObject =
  req
    "RemoveFacetFromObject"
    "fixture/RemoveFacetFromObject.yaml"

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

requestUpdateFacet :: UpdateFacet -> TestTree
requestUpdateFacet =
  req
    "UpdateFacet"
    "fixture/UpdateFacet.yaml"

requestUpdateLinkAttributes :: UpdateLinkAttributes -> TestTree
requestUpdateLinkAttributes =
  req
    "UpdateLinkAttributes"
    "fixture/UpdateLinkAttributes.yaml"

requestUpdateObjectAttributes :: UpdateObjectAttributes -> TestTree
requestUpdateObjectAttributes =
  req
    "UpdateObjectAttributes"
    "fixture/UpdateObjectAttributes.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema =
  req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestUpdateTypedLinkFacet :: UpdateTypedLinkFacet -> TestTree
requestUpdateTypedLinkFacet =
  req
    "UpdateTypedLinkFacet"
    "fixture/UpdateTypedLinkFacet.yaml"

requestUpgradeAppliedSchema :: UpgradeAppliedSchema -> TestTree
requestUpgradeAppliedSchema =
  req
    "UpgradeAppliedSchema"
    "fixture/UpgradeAppliedSchema.yaml"

requestUpgradePublishedSchema :: UpgradePublishedSchema -> TestTree
requestUpgradePublishedSchema =
  req
    "UpgradePublishedSchema"
    "fixture/UpgradePublishedSchema.yaml"

-- Responses

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

responseAttachObject :: AttachObjectResponse -> TestTree
responseAttachObject =
  res
    "AttachObjectResponse"
    "fixture/AttachObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachObject)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachPolicy)

responseAttachToIndex :: AttachToIndexResponse -> TestTree
responseAttachToIndex =
  res
    "AttachToIndexResponse"
    "fixture/AttachToIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachToIndex)

responseAttachTypedLink :: AttachTypedLinkResponse -> TestTree
responseAttachTypedLink =
  res
    "AttachTypedLinkResponse"
    "fixture/AttachTypedLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachTypedLink)

responseBatchRead :: BatchReadResponse -> TestTree
responseBatchRead =
  res
    "BatchReadResponse"
    "fixture/BatchReadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchRead)

responseBatchWrite :: BatchWriteResponse -> TestTree
responseBatchWrite =
  res
    "BatchWriteResponse"
    "fixture/BatchWriteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchWrite)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectory)

responseCreateFacet :: CreateFacetResponse -> TestTree
responseCreateFacet =
  res
    "CreateFacetResponse"
    "fixture/CreateFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFacet)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex =
  res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIndex)

responseCreateObject :: CreateObjectResponse -> TestTree
responseCreateObject =
  res
    "CreateObjectResponse"
    "fixture/CreateObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateObject)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSchema)

responseCreateTypedLinkFacet :: CreateTypedLinkFacetResponse -> TestTree
responseCreateTypedLinkFacet =
  res
    "CreateTypedLinkFacetResponse"
    "fixture/CreateTypedLinkFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTypedLinkFacet)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectory)

responseDeleteFacet :: DeleteFacetResponse -> TestTree
responseDeleteFacet =
  res
    "DeleteFacetResponse"
    "fixture/DeleteFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFacet)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteObject)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSchema)

responseDeleteTypedLinkFacet :: DeleteTypedLinkFacetResponse -> TestTree
responseDeleteTypedLinkFacet =
  res
    "DeleteTypedLinkFacetResponse"
    "fixture/DeleteTypedLinkFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTypedLinkFacet)

responseDetachFromIndex :: DetachFromIndexResponse -> TestTree
responseDetachFromIndex =
  res
    "DetachFromIndexResponse"
    "fixture/DetachFromIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachFromIndex)

responseDetachObject :: DetachObjectResponse -> TestTree
responseDetachObject =
  res
    "DetachObjectResponse"
    "fixture/DetachObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachObject)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachPolicy)

responseDetachTypedLink :: DetachTypedLinkResponse -> TestTree
responseDetachTypedLink =
  res
    "DetachTypedLinkResponse"
    "fixture/DetachTypedLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachTypedLink)

responseDisableDirectory :: DisableDirectoryResponse -> TestTree
responseDisableDirectory =
  res
    "DisableDirectoryResponse"
    "fixture/DisableDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableDirectory)

responseEnableDirectory :: EnableDirectoryResponse -> TestTree
responseEnableDirectory =
  res
    "EnableDirectoryResponse"
    "fixture/EnableDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableDirectory)

responseGetAppliedSchemaVersion :: GetAppliedSchemaVersionResponse -> TestTree
responseGetAppliedSchemaVersion =
  res
    "GetAppliedSchemaVersionResponse"
    "fixture/GetAppliedSchemaVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAppliedSchemaVersion)

responseGetDirectory :: GetDirectoryResponse -> TestTree
responseGetDirectory =
  res
    "GetDirectoryResponse"
    "fixture/GetDirectoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDirectory)

responseGetFacet :: GetFacetResponse -> TestTree
responseGetFacet =
  res
    "GetFacetResponse"
    "fixture/GetFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFacet)

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

responseGetObjectInformation :: GetObjectInformationResponse -> TestTree
responseGetObjectInformation =
  res
    "GetObjectInformationResponse"
    "fixture/GetObjectInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetObjectInformation)

responseGetSchemaAsJson :: GetSchemaAsJsonResponse -> TestTree
responseGetSchemaAsJson =
  res
    "GetSchemaAsJsonResponse"
    "fixture/GetSchemaAsJsonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaAsJson)

responseGetTypedLinkFacetInformation :: GetTypedLinkFacetInformationResponse -> TestTree
responseGetTypedLinkFacetInformation =
  res
    "GetTypedLinkFacetInformationResponse"
    "fixture/GetTypedLinkFacetInformationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTypedLinkFacetInformation)

responseListAppliedSchemaArns :: ListAppliedSchemaArnsResponse -> TestTree
responseListAppliedSchemaArns =
  res
    "ListAppliedSchemaArnsResponse"
    "fixture/ListAppliedSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppliedSchemaArns)

responseListAttachedIndices :: ListAttachedIndicesResponse -> TestTree
responseListAttachedIndices =
  res
    "ListAttachedIndicesResponse"
    "fixture/ListAttachedIndicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedIndices)

responseListDevelopmentSchemaArns :: ListDevelopmentSchemaArnsResponse -> TestTree
responseListDevelopmentSchemaArns =
  res
    "ListDevelopmentSchemaArnsResponse"
    "fixture/ListDevelopmentSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevelopmentSchemaArns)

responseListDirectories :: ListDirectoriesResponse -> TestTree
responseListDirectories =
  res
    "ListDirectoriesResponse"
    "fixture/ListDirectoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDirectories)

responseListFacetAttributes :: ListFacetAttributesResponse -> TestTree
responseListFacetAttributes =
  res
    "ListFacetAttributesResponse"
    "fixture/ListFacetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFacetAttributes)

responseListFacetNames :: ListFacetNamesResponse -> TestTree
responseListFacetNames =
  res
    "ListFacetNamesResponse"
    "fixture/ListFacetNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFacetNames)

responseListIncomingTypedLinks :: ListIncomingTypedLinksResponse -> TestTree
responseListIncomingTypedLinks =
  res
    "ListIncomingTypedLinksResponse"
    "fixture/ListIncomingTypedLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIncomingTypedLinks)

responseListIndex :: ListIndexResponse -> TestTree
responseListIndex =
  res
    "ListIndexResponse"
    "fixture/ListIndexResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIndex)

responseListManagedSchemaArns :: ListManagedSchemaArnsResponse -> TestTree
responseListManagedSchemaArns =
  res
    "ListManagedSchemaArnsResponse"
    "fixture/ListManagedSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedSchemaArns)

responseListObjectAttributes :: ListObjectAttributesResponse -> TestTree
responseListObjectAttributes =
  res
    "ListObjectAttributesResponse"
    "fixture/ListObjectAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectAttributes)

responseListObjectChildren :: ListObjectChildrenResponse -> TestTree
responseListObjectChildren =
  res
    "ListObjectChildrenResponse"
    "fixture/ListObjectChildrenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectChildren)

responseListObjectParentPaths :: ListObjectParentPathsResponse -> TestTree
responseListObjectParentPaths =
  res
    "ListObjectParentPathsResponse"
    "fixture/ListObjectParentPathsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectParentPaths)

responseListObjectParents :: ListObjectParentsResponse -> TestTree
responseListObjectParents =
  res
    "ListObjectParentsResponse"
    "fixture/ListObjectParentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectParents)

responseListObjectPolicies :: ListObjectPoliciesResponse -> TestTree
responseListObjectPolicies =
  res
    "ListObjectPoliciesResponse"
    "fixture/ListObjectPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListObjectPolicies)

responseListOutgoingTypedLinks :: ListOutgoingTypedLinksResponse -> TestTree
responseListOutgoingTypedLinks =
  res
    "ListOutgoingTypedLinksResponse"
    "fixture/ListOutgoingTypedLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOutgoingTypedLinks)

responseListPolicyAttachments :: ListPolicyAttachmentsResponse -> TestTree
responseListPolicyAttachments =
  res
    "ListPolicyAttachmentsResponse"
    "fixture/ListPolicyAttachmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyAttachments)

responseListPublishedSchemaArns :: ListPublishedSchemaArnsResponse -> TestTree
responseListPublishedSchemaArns =
  res
    "ListPublishedSchemaArnsResponse"
    "fixture/ListPublishedSchemaArnsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPublishedSchemaArns)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributesResponse -> TestTree
responseListTypedLinkFacetAttributes =
  res
    "ListTypedLinkFacetAttributesResponse"
    "fixture/ListTypedLinkFacetAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypedLinkFacetAttributes)

responseListTypedLinkFacetNames :: ListTypedLinkFacetNamesResponse -> TestTree
responseListTypedLinkFacetNames =
  res
    "ListTypedLinkFacetNamesResponse"
    "fixture/ListTypedLinkFacetNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTypedLinkFacetNames)

responseLookupPolicy :: LookupPolicyResponse -> TestTree
responseLookupPolicy =
  res
    "LookupPolicyResponse"
    "fixture/LookupPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LookupPolicy)

responsePublishSchema :: PublishSchemaResponse -> TestTree
responsePublishSchema =
  res
    "PublishSchemaResponse"
    "fixture/PublishSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishSchema)

responsePutSchemaFromJson :: PutSchemaFromJsonResponse -> TestTree
responsePutSchemaFromJson =
  res
    "PutSchemaFromJsonResponse"
    "fixture/PutSchemaFromJsonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSchemaFromJson)

responseRemoveFacetFromObject :: RemoveFacetFromObjectResponse -> TestTree
responseRemoveFacetFromObject =
  res
    "RemoveFacetFromObjectResponse"
    "fixture/RemoveFacetFromObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveFacetFromObject)

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

responseUpdateFacet :: UpdateFacetResponse -> TestTree
responseUpdateFacet =
  res
    "UpdateFacetResponse"
    "fixture/UpdateFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFacet)

responseUpdateLinkAttributes :: UpdateLinkAttributesResponse -> TestTree
responseUpdateLinkAttributes =
  res
    "UpdateLinkAttributesResponse"
    "fixture/UpdateLinkAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLinkAttributes)

responseUpdateObjectAttributes :: UpdateObjectAttributesResponse -> TestTree
responseUpdateObjectAttributes =
  res
    "UpdateObjectAttributesResponse"
    "fixture/UpdateObjectAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateObjectAttributes)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSchema)

responseUpdateTypedLinkFacet :: UpdateTypedLinkFacetResponse -> TestTree
responseUpdateTypedLinkFacet =
  res
    "UpdateTypedLinkFacetResponse"
    "fixture/UpdateTypedLinkFacetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTypedLinkFacet)

responseUpgradeAppliedSchema :: UpgradeAppliedSchemaResponse -> TestTree
responseUpgradeAppliedSchema =
  res
    "UpgradeAppliedSchemaResponse"
    "fixture/UpgradeAppliedSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradeAppliedSchema)

responseUpgradePublishedSchema :: UpgradePublishedSchemaResponse -> TestTree
responseUpgradePublishedSchema =
  res
    "UpgradePublishedSchemaResponse"
    "fixture/UpgradePublishedSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpgradePublishedSchema)

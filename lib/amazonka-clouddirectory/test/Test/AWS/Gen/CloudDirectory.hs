{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestListTypedLinkFacetAttributes $
--             mkListTypedLinkFacetAttributes
--
--         , requestDeleteObject $
--             mkDeleteObject
--
--         , requestListIndex $
--             mkListIndex
--
--         , requestUpgradeAppliedSchema $
--             mkUpgradeAppliedSchema
--
--         , requestGetDirectory $
--             mkGetDirectory
--
--         , requestGetObjectInformation $
--             mkGetObjectInformation
--
--         , requestListAttachedIndices $
--             mkListAttachedIndices
--
--         , requestDetachFromIndex $
--             mkDetachFromIndex
--
--         , requestLookupPolicy $
--             mkLookupPolicy
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestListPublishedSchemaARNs $
--             mkListPublishedSchemaARNs
--
--         , requestListDirectories $
--             mkListDirectories
--
--         , requestCreateTypedLinkFacet $
--             mkCreateTypedLinkFacet
--
--         , requestListObjectParentPaths $
--             mkListObjectParentPaths
--
--         , requestDisableDirectory $
--             mkDisableDirectory
--
--         , requestCreateDirectory $
--             mkCreateDirectory
--
--         , requestListFacetAttributes $
--             mkListFacetAttributes
--
--         , requestListManagedSchemaARNs $
--             mkListManagedSchemaARNs
--
--         , requestUpdateTypedLinkFacet $
--             mkUpdateTypedLinkFacet
--
--         , requestDeleteTypedLinkFacet $
--             mkDeleteTypedLinkFacet
--
--         , requestGetAppliedSchemaVersion $
--             mkGetAppliedSchemaVersion
--
--         , requestRemoveFacetFromObject $
--             mkRemoveFacetFromObject
--
--         , requestEnableDirectory $
--             mkEnableDirectory
--
--         , requestListObjectAttributes $
--             mkListObjectAttributes
--
--         , requestListAppliedSchemaARNs $
--             mkListAppliedSchemaARNs
--
--         , requestListIncomingTypedLinks $
--             mkListIncomingTypedLinks
--
--         , requestGetFacet $
--             mkGetFacet
--
--         , requestGetTypedLinkFacetInformation $
--             mkGetTypedLinkFacetInformation
--
--         , requestListDevelopmentSchemaARNs $
--             mkListDevelopmentSchemaARNs
--
--         , requestAttachObject $
--             mkAttachObject
--
--         , requestBatchWrite $
--             mkBatchWrite
--
--         , requestCreateObject $
--             mkCreateObject
--
--         , requestUpgradePublishedSchema $
--             mkUpgradePublishedSchema
--
--         , requestCreateFacet $
--             mkCreateFacet
--
--         , requestGetLinkAttributes $
--             mkGetLinkAttributes
--
--         , requestGetObjectAttributes $
--             mkGetObjectAttributes
--
--         , requestDeleteFacet $
--             mkDeleteFacet
--
--         , requestUpdateFacet $
--             mkUpdateFacet
--
--         , requestListObjectChildren $
--             mkListObjectChildren
--
--         , requestListTypedLinkFacetNames $
--             mkListTypedLinkFacetNames
--
--         , requestAttachTypedLink $
--             mkAttachTypedLink
--
--         , requestDetachPolicy $
--             mkDetachPolicy
--
--         , requestCreateIndex $
--             mkCreateIndex
--
--         , requestDetachObject $
--             mkDetachObject
--
--         , requestAddFacetToObject $
--             mkAddFacetToObject
--
--         , requestApplySchema $
--             mkApplySchema
--
--         , requestCreateSchema $
--             mkCreateSchema
--
--         , requestGetSchemaAsJSON $
--             mkGetSchemaAsJSON
--
--         , requestPublishSchema $
--             mkPublishSchema
--
--         , requestDeleteDirectory $
--             mkDeleteDirectory
--
--         , requestListObjectParents $
--             mkListObjectParents
--
--         , requestListPolicyAttachments $
--             mkListPolicyAttachments
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUpdateSchema $
--             mkUpdateSchema
--
--         , requestDeleteSchema $
--             mkDeleteSchema
--
--         , requestDetachTypedLink $
--             mkDetachTypedLink
--
--         , requestListFacetNames $
--             mkListFacetNames
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestListOutgoingTypedLinks $
--             mkListOutgoingTypedLinks
--
--         , requestUpdateObjectAttributes $
--             mkUpdateObjectAttributes
--
--         , requestAttachPolicy $
--             mkAttachPolicy
--
--         , requestBatchRead $
--             mkBatchRead
--
--         , requestPutSchemaFromJSON $
--             mkPutSchemaFromJSON
--
--         , requestUpdateLinkAttributes $
--             mkUpdateLinkAttributes
--
--         , requestAttachToIndex $
--             mkAttachToIndex
--
--         , requestListObjectPolicies $
--             mkListObjectPolicies
--
--           ]

--     , testGroup "response"
--         [ responseListTypedLinkFacetAttributes $
--             mkListTypedLinkFacetAttributesResponse
--
--         , responseDeleteObject $
--             mkDeleteObjectResponse
--
--         , responseListIndex $
--             mkListIndexResponse
--
--         , responseUpgradeAppliedSchema $
--             mkUpgradeAppliedSchemaResponse
--
--         , responseGetDirectory $
--             mkGetDirectoryResponse
--
--         , responseGetObjectInformation $
--             mkGetObjectInformationResponse
--
--         , responseListAttachedIndices $
--             mkListAttachedIndicesResponse
--
--         , responseDetachFromIndex $
--             mkDetachFromIndexResponse
--
--         , responseLookupPolicy $
--             mkLookupPolicyResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseListPublishedSchemaARNs $
--             mkListPublishedSchemaARNsResponse
--
--         , responseListDirectories $
--             mkListDirectoriesResponse
--
--         , responseCreateTypedLinkFacet $
--             mkCreateTypedLinkFacetResponse
--
--         , responseListObjectParentPaths $
--             mkListObjectParentPathsResponse
--
--         , responseDisableDirectory $
--             mkDisableDirectoryResponse
--
--         , responseCreateDirectory $
--             mkCreateDirectoryResponse
--
--         , responseListFacetAttributes $
--             mkListFacetAttributesResponse
--
--         , responseListManagedSchemaARNs $
--             mkListManagedSchemaARNsResponse
--
--         , responseUpdateTypedLinkFacet $
--             mkUpdateTypedLinkFacetResponse
--
--         , responseDeleteTypedLinkFacet $
--             mkDeleteTypedLinkFacetResponse
--
--         , responseGetAppliedSchemaVersion $
--             mkGetAppliedSchemaVersionResponse
--
--         , responseRemoveFacetFromObject $
--             mkRemoveFacetFromObjectResponse
--
--         , responseEnableDirectory $
--             mkEnableDirectoryResponse
--
--         , responseListObjectAttributes $
--             mkListObjectAttributesResponse
--
--         , responseListAppliedSchemaARNs $
--             mkListAppliedSchemaARNsResponse
--
--         , responseListIncomingTypedLinks $
--             mkListIncomingTypedLinksResponse
--
--         , responseGetFacet $
--             mkGetFacetResponse
--
--         , responseGetTypedLinkFacetInformation $
--             mkGetTypedLinkFacetInformationResponse
--
--         , responseListDevelopmentSchemaARNs $
--             mkListDevelopmentSchemaARNsResponse
--
--         , responseAttachObject $
--             mkAttachObjectResponse
--
--         , responseBatchWrite $
--             mkBatchWriteResponse
--
--         , responseCreateObject $
--             mkCreateObjectResponse
--
--         , responseUpgradePublishedSchema $
--             mkUpgradePublishedSchemaResponse
--
--         , responseCreateFacet $
--             mkCreateFacetResponse
--
--         , responseGetLinkAttributes $
--             mkGetLinkAttributesResponse
--
--         , responseGetObjectAttributes $
--             mkGetObjectAttributesResponse
--
--         , responseDeleteFacet $
--             mkDeleteFacetResponse
--
--         , responseUpdateFacet $
--             mkUpdateFacetResponse
--
--         , responseListObjectChildren $
--             mkListObjectChildrenResponse
--
--         , responseListTypedLinkFacetNames $
--             mkListTypedLinkFacetNamesResponse
--
--         , responseAttachTypedLink $
--             mkAttachTypedLinkResponse
--
--         , responseDetachPolicy $
--             mkDetachPolicyResponse
--
--         , responseCreateIndex $
--             mkCreateIndexResponse
--
--         , responseDetachObject $
--             mkDetachObjectResponse
--
--         , responseAddFacetToObject $
--             mkAddFacetToObjectResponse
--
--         , responseApplySchema $
--             mkApplySchemaResponse
--
--         , responseCreateSchema $
--             mkCreateSchemaResponse
--
--         , responseGetSchemaAsJSON $
--             mkGetSchemaAsJSONResponse
--
--         , responsePublishSchema $
--             mkPublishSchemaResponse
--
--         , responseDeleteDirectory $
--             mkDeleteDirectoryResponse
--
--         , responseListObjectParents $
--             mkListObjectParentsResponse
--
--         , responseListPolicyAttachments $
--             mkListPolicyAttachmentsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUpdateSchema $
--             mkUpdateSchemaResponse
--
--         , responseDeleteSchema $
--             mkDeleteSchemaResponse
--
--         , responseDetachTypedLink $
--             mkDetachTypedLinkResponse
--
--         , responseListFacetNames $
--             mkListFacetNamesResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseListOutgoingTypedLinks $
--             mkListOutgoingTypedLinksResponse
--
--         , responseUpdateObjectAttributes $
--             mkUpdateObjectAttributesResponse
--
--         , responseAttachPolicy $
--             mkAttachPolicyResponse
--
--         , responseBatchRead $
--             mkBatchReadResponse
--
--         , responsePutSchemaFromJSON $
--             mkPutSchemaFromJSONResponse
--
--         , responseUpdateLinkAttributes $
--             mkUpdateLinkAttributesResponse
--
--         , responseAttachToIndex $
--             mkAttachToIndexResponse
--
--         , responseListObjectPolicies $
--             mkListObjectPoliciesResponse
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

requestListPublishedSchemaARNs :: ListPublishedSchemaARNs -> TestTree
requestListPublishedSchemaARNs =
  req
    "ListPublishedSchemaARNs"
    "fixture/ListPublishedSchemaARNs.yaml"

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

requestListManagedSchemaARNs :: ListManagedSchemaARNs -> TestTree
requestListManagedSchemaARNs =
  req
    "ListManagedSchemaARNs"
    "fixture/ListManagedSchemaARNs.yaml"

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

requestListAppliedSchemaARNs :: ListAppliedSchemaARNs -> TestTree
requestListAppliedSchemaARNs =
  req
    "ListAppliedSchemaARNs"
    "fixture/ListAppliedSchemaARNs.yaml"

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

requestListDevelopmentSchemaARNs :: ListDevelopmentSchemaARNs -> TestTree
requestListDevelopmentSchemaARNs =
  req
    "ListDevelopmentSchemaARNs"
    "fixture/ListDevelopmentSchemaARNs.yaml"

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

requestGetSchemaAsJSON :: GetSchemaAsJSON -> TestTree
requestGetSchemaAsJSON =
  req
    "GetSchemaAsJSON"
    "fixture/GetSchemaAsJSON.yaml"

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

requestPutSchemaFromJSON :: PutSchemaFromJSON -> TestTree
requestPutSchemaFromJSON =
  req
    "PutSchemaFromJSON"
    "fixture/PutSchemaFromJSON.yaml"

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
    cloudDirectoryService
    (Proxy :: Proxy ListTypedLinkFacetAttributes)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject =
  res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DeleteObject)

responseListIndex :: ListIndexResponse -> TestTree
responseListIndex =
  res
    "ListIndexResponse"
    "fixture/ListIndexResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListIndex)

responseUpgradeAppliedSchema :: UpgradeAppliedSchemaResponse -> TestTree
responseUpgradeAppliedSchema =
  res
    "UpgradeAppliedSchemaResponse"
    "fixture/UpgradeAppliedSchemaResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UpgradeAppliedSchema)

responseGetDirectory :: GetDirectoryResponse -> TestTree
responseGetDirectory =
  res
    "GetDirectoryResponse"
    "fixture/GetDirectoryResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetDirectory)

responseGetObjectInformation :: GetObjectInformationResponse -> TestTree
responseGetObjectInformation =
  res
    "GetObjectInformationResponse"
    "fixture/GetObjectInformationResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetObjectInformation)

responseListAttachedIndices :: ListAttachedIndicesResponse -> TestTree
responseListAttachedIndices =
  res
    "ListAttachedIndicesResponse"
    "fixture/ListAttachedIndicesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListAttachedIndices)

responseDetachFromIndex :: DetachFromIndexResponse -> TestTree
responseDetachFromIndex =
  res
    "DetachFromIndexResponse"
    "fixture/DetachFromIndexResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DetachFromIndex)

responseLookupPolicy :: LookupPolicyResponse -> TestTree
responseLookupPolicy =
  res
    "LookupPolicyResponse"
    "fixture/LookupPolicyResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy LookupPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListTagsForResource)

responseListPublishedSchemaARNs :: ListPublishedSchemaARNsResponse -> TestTree
responseListPublishedSchemaARNs =
  res
    "ListPublishedSchemaARNsResponse"
    "fixture/ListPublishedSchemaARNsResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListPublishedSchemaARNs)

responseListDirectories :: ListDirectoriesResponse -> TestTree
responseListDirectories =
  res
    "ListDirectoriesResponse"
    "fixture/ListDirectoriesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListDirectories)

responseCreateTypedLinkFacet :: CreateTypedLinkFacetResponse -> TestTree
responseCreateTypedLinkFacet =
  res
    "CreateTypedLinkFacetResponse"
    "fixture/CreateTypedLinkFacetResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy CreateTypedLinkFacet)

responseListObjectParentPaths :: ListObjectParentPathsResponse -> TestTree
responseListObjectParentPaths =
  res
    "ListObjectParentPathsResponse"
    "fixture/ListObjectParentPathsResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListObjectParentPaths)

responseDisableDirectory :: DisableDirectoryResponse -> TestTree
responseDisableDirectory =
  res
    "DisableDirectoryResponse"
    "fixture/DisableDirectoryResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DisableDirectory)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory =
  res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy CreateDirectory)

responseListFacetAttributes :: ListFacetAttributesResponse -> TestTree
responseListFacetAttributes =
  res
    "ListFacetAttributesResponse"
    "fixture/ListFacetAttributesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListFacetAttributes)

responseListManagedSchemaARNs :: ListManagedSchemaARNsResponse -> TestTree
responseListManagedSchemaARNs =
  res
    "ListManagedSchemaARNsResponse"
    "fixture/ListManagedSchemaARNsResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListManagedSchemaARNs)

responseUpdateTypedLinkFacet :: UpdateTypedLinkFacetResponse -> TestTree
responseUpdateTypedLinkFacet =
  res
    "UpdateTypedLinkFacetResponse"
    "fixture/UpdateTypedLinkFacetResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UpdateTypedLinkFacet)

responseDeleteTypedLinkFacet :: DeleteTypedLinkFacetResponse -> TestTree
responseDeleteTypedLinkFacet =
  res
    "DeleteTypedLinkFacetResponse"
    "fixture/DeleteTypedLinkFacetResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DeleteTypedLinkFacet)

responseGetAppliedSchemaVersion :: GetAppliedSchemaVersionResponse -> TestTree
responseGetAppliedSchemaVersion =
  res
    "GetAppliedSchemaVersionResponse"
    "fixture/GetAppliedSchemaVersionResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetAppliedSchemaVersion)

responseRemoveFacetFromObject :: RemoveFacetFromObjectResponse -> TestTree
responseRemoveFacetFromObject =
  res
    "RemoveFacetFromObjectResponse"
    "fixture/RemoveFacetFromObjectResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy RemoveFacetFromObject)

responseEnableDirectory :: EnableDirectoryResponse -> TestTree
responseEnableDirectory =
  res
    "EnableDirectoryResponse"
    "fixture/EnableDirectoryResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy EnableDirectory)

responseListObjectAttributes :: ListObjectAttributesResponse -> TestTree
responseListObjectAttributes =
  res
    "ListObjectAttributesResponse"
    "fixture/ListObjectAttributesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListObjectAttributes)

responseListAppliedSchemaARNs :: ListAppliedSchemaARNsResponse -> TestTree
responseListAppliedSchemaARNs =
  res
    "ListAppliedSchemaARNsResponse"
    "fixture/ListAppliedSchemaARNsResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListAppliedSchemaARNs)

responseListIncomingTypedLinks :: ListIncomingTypedLinksResponse -> TestTree
responseListIncomingTypedLinks =
  res
    "ListIncomingTypedLinksResponse"
    "fixture/ListIncomingTypedLinksResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListIncomingTypedLinks)

responseGetFacet :: GetFacetResponse -> TestTree
responseGetFacet =
  res
    "GetFacetResponse"
    "fixture/GetFacetResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetFacet)

responseGetTypedLinkFacetInformation :: GetTypedLinkFacetInformationResponse -> TestTree
responseGetTypedLinkFacetInformation =
  res
    "GetTypedLinkFacetInformationResponse"
    "fixture/GetTypedLinkFacetInformationResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetTypedLinkFacetInformation)

responseListDevelopmentSchemaARNs :: ListDevelopmentSchemaARNsResponse -> TestTree
responseListDevelopmentSchemaARNs =
  res
    "ListDevelopmentSchemaARNsResponse"
    "fixture/ListDevelopmentSchemaARNsResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListDevelopmentSchemaARNs)

responseAttachObject :: AttachObjectResponse -> TestTree
responseAttachObject =
  res
    "AttachObjectResponse"
    "fixture/AttachObjectResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy AttachObject)

responseBatchWrite :: BatchWriteResponse -> TestTree
responseBatchWrite =
  res
    "BatchWriteResponse"
    "fixture/BatchWriteResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy BatchWrite)

responseCreateObject :: CreateObjectResponse -> TestTree
responseCreateObject =
  res
    "CreateObjectResponse"
    "fixture/CreateObjectResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy CreateObject)

responseUpgradePublishedSchema :: UpgradePublishedSchemaResponse -> TestTree
responseUpgradePublishedSchema =
  res
    "UpgradePublishedSchemaResponse"
    "fixture/UpgradePublishedSchemaResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UpgradePublishedSchema)

responseCreateFacet :: CreateFacetResponse -> TestTree
responseCreateFacet =
  res
    "CreateFacetResponse"
    "fixture/CreateFacetResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy CreateFacet)

responseGetLinkAttributes :: GetLinkAttributesResponse -> TestTree
responseGetLinkAttributes =
  res
    "GetLinkAttributesResponse"
    "fixture/GetLinkAttributesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetLinkAttributes)

responseGetObjectAttributes :: GetObjectAttributesResponse -> TestTree
responseGetObjectAttributes =
  res
    "GetObjectAttributesResponse"
    "fixture/GetObjectAttributesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetObjectAttributes)

responseDeleteFacet :: DeleteFacetResponse -> TestTree
responseDeleteFacet =
  res
    "DeleteFacetResponse"
    "fixture/DeleteFacetResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DeleteFacet)

responseUpdateFacet :: UpdateFacetResponse -> TestTree
responseUpdateFacet =
  res
    "UpdateFacetResponse"
    "fixture/UpdateFacetResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UpdateFacet)

responseListObjectChildren :: ListObjectChildrenResponse -> TestTree
responseListObjectChildren =
  res
    "ListObjectChildrenResponse"
    "fixture/ListObjectChildrenResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListObjectChildren)

responseListTypedLinkFacetNames :: ListTypedLinkFacetNamesResponse -> TestTree
responseListTypedLinkFacetNames =
  res
    "ListTypedLinkFacetNamesResponse"
    "fixture/ListTypedLinkFacetNamesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListTypedLinkFacetNames)

responseAttachTypedLink :: AttachTypedLinkResponse -> TestTree
responseAttachTypedLink =
  res
    "AttachTypedLinkResponse"
    "fixture/AttachTypedLinkResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy AttachTypedLink)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DetachPolicy)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex =
  res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy CreateIndex)

responseDetachObject :: DetachObjectResponse -> TestTree
responseDetachObject =
  res
    "DetachObjectResponse"
    "fixture/DetachObjectResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DetachObject)

responseAddFacetToObject :: AddFacetToObjectResponse -> TestTree
responseAddFacetToObject =
  res
    "AddFacetToObjectResponse"
    "fixture/AddFacetToObjectResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy AddFacetToObject)

responseApplySchema :: ApplySchemaResponse -> TestTree
responseApplySchema =
  res
    "ApplySchemaResponse"
    "fixture/ApplySchemaResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ApplySchema)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema =
  res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy CreateSchema)

responseGetSchemaAsJSON :: GetSchemaAsJSONResponse -> TestTree
responseGetSchemaAsJSON =
  res
    "GetSchemaAsJSONResponse"
    "fixture/GetSchemaAsJSONResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy GetSchemaAsJSON)

responsePublishSchema :: PublishSchemaResponse -> TestTree
responsePublishSchema =
  res
    "PublishSchemaResponse"
    "fixture/PublishSchemaResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy PublishSchema)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory =
  res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DeleteDirectory)

responseListObjectParents :: ListObjectParentsResponse -> TestTree
responseListObjectParents =
  res
    "ListObjectParentsResponse"
    "fixture/ListObjectParentsResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListObjectParents)

responseListPolicyAttachments :: ListPolicyAttachmentsResponse -> TestTree
responseListPolicyAttachments =
  res
    "ListPolicyAttachmentsResponse"
    "fixture/ListPolicyAttachmentsResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListPolicyAttachments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy TagResource)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema =
  res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema =
  res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DeleteSchema)

responseDetachTypedLink :: DetachTypedLinkResponse -> TestTree
responseDetachTypedLink =
  res
    "DetachTypedLinkResponse"
    "fixture/DetachTypedLinkResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy DetachTypedLink)

responseListFacetNames :: ListFacetNamesResponse -> TestTree
responseListFacetNames =
  res
    "ListFacetNamesResponse"
    "fixture/ListFacetNamesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListFacetNames)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UntagResource)

responseListOutgoingTypedLinks :: ListOutgoingTypedLinksResponse -> TestTree
responseListOutgoingTypedLinks =
  res
    "ListOutgoingTypedLinksResponse"
    "fixture/ListOutgoingTypedLinksResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListOutgoingTypedLinks)

responseUpdateObjectAttributes :: UpdateObjectAttributesResponse -> TestTree
responseUpdateObjectAttributes =
  res
    "UpdateObjectAttributesResponse"
    "fixture/UpdateObjectAttributesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UpdateObjectAttributes)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy AttachPolicy)

responseBatchRead :: BatchReadResponse -> TestTree
responseBatchRead =
  res
    "BatchReadResponse"
    "fixture/BatchReadResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy BatchRead)

responsePutSchemaFromJSON :: PutSchemaFromJSONResponse -> TestTree
responsePutSchemaFromJSON =
  res
    "PutSchemaFromJSONResponse"
    "fixture/PutSchemaFromJSONResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy PutSchemaFromJSON)

responseUpdateLinkAttributes :: UpdateLinkAttributesResponse -> TestTree
responseUpdateLinkAttributes =
  res
    "UpdateLinkAttributesResponse"
    "fixture/UpdateLinkAttributesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy UpdateLinkAttributes)

responseAttachToIndex :: AttachToIndexResponse -> TestTree
responseAttachToIndex =
  res
    "AttachToIndexResponse"
    "fixture/AttachToIndexResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy AttachToIndex)

responseListObjectPolicies :: ListObjectPoliciesResponse -> TestTree
responseListObjectPolicies =
  res
    "ListObjectPoliciesResponse"
    "fixture/ListObjectPoliciesResponse.proto"
    cloudDirectoryService
    (Proxy :: Proxy ListObjectPolicies)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CloudDirectory where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CloudDirectory
import Test.AWS.CloudDirectory.Internal

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
--         , requestListPublishedSchemaArns $
--             mkListPublishedSchemaArns
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
--         , requestListManagedSchemaArns $
--             mkListManagedSchemaArns
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
--         , requestListAppliedSchemaArns $
--             mkListAppliedSchemaArns
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
--         , requestListDevelopmentSchemaArns $
--             mkListDevelopmentSchemaArns
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
--         , requestGetSchemaAsJson $
--             mkGetSchemaAsJson
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
--         , requestPutSchemaFromJson $
--             mkPutSchemaFromJson
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
--         , responseListPublishedSchemaArns $
--             mkListPublishedSchemaArnsResponse
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
--         , responseListManagedSchemaArns $
--             mkListManagedSchemaArnsResponse
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
--         , responseListAppliedSchemaArns $
--             mkListAppliedSchemaArnsResponse
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
--         , responseListDevelopmentSchemaArns $
--             mkListDevelopmentSchemaArnsResponse
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
--         , responseGetSchemaAsJson $
--             mkGetSchemaAsJsonResponse
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
--         , responsePutSchemaFromJson $
--             mkPutSchemaFromJsonResponse
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
requestListTypedLinkFacetAttributes = req
    "ListTypedLinkFacetAttributes"
    "fixture/ListTypedLinkFacetAttributes.yaml"

requestDeleteObject :: DeleteObject -> TestTree
requestDeleteObject = req
    "DeleteObject"
    "fixture/DeleteObject.yaml"

requestListIndex :: ListIndex -> TestTree
requestListIndex = req
    "ListIndex"
    "fixture/ListIndex.yaml"

requestUpgradeAppliedSchema :: UpgradeAppliedSchema -> TestTree
requestUpgradeAppliedSchema = req
    "UpgradeAppliedSchema"
    "fixture/UpgradeAppliedSchema.yaml"

requestGetDirectory :: GetDirectory -> TestTree
requestGetDirectory = req
    "GetDirectory"
    "fixture/GetDirectory.yaml"

requestGetObjectInformation :: GetObjectInformation -> TestTree
requestGetObjectInformation = req
    "GetObjectInformation"
    "fixture/GetObjectInformation.yaml"

requestListAttachedIndices :: ListAttachedIndices -> TestTree
requestListAttachedIndices = req
    "ListAttachedIndices"
    "fixture/ListAttachedIndices.yaml"

requestDetachFromIndex :: DetachFromIndex -> TestTree
requestDetachFromIndex = req
    "DetachFromIndex"
    "fixture/DetachFromIndex.yaml"

requestLookupPolicy :: LookupPolicy -> TestTree
requestLookupPolicy = req
    "LookupPolicy"
    "fixture/LookupPolicy.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListPublishedSchemaArns :: ListPublishedSchemaArns -> TestTree
requestListPublishedSchemaArns = req
    "ListPublishedSchemaArns"
    "fixture/ListPublishedSchemaArns.yaml"

requestListDirectories :: ListDirectories -> TestTree
requestListDirectories = req
    "ListDirectories"
    "fixture/ListDirectories.yaml"

requestCreateTypedLinkFacet :: CreateTypedLinkFacet -> TestTree
requestCreateTypedLinkFacet = req
    "CreateTypedLinkFacet"
    "fixture/CreateTypedLinkFacet.yaml"

requestListObjectParentPaths :: ListObjectParentPaths -> TestTree
requestListObjectParentPaths = req
    "ListObjectParentPaths"
    "fixture/ListObjectParentPaths.yaml"

requestDisableDirectory :: DisableDirectory -> TestTree
requestDisableDirectory = req
    "DisableDirectory"
    "fixture/DisableDirectory.yaml"

requestCreateDirectory :: CreateDirectory -> TestTree
requestCreateDirectory = req
    "CreateDirectory"
    "fixture/CreateDirectory.yaml"

requestListFacetAttributes :: ListFacetAttributes -> TestTree
requestListFacetAttributes = req
    "ListFacetAttributes"
    "fixture/ListFacetAttributes.yaml"

requestListManagedSchemaArns :: ListManagedSchemaArns -> TestTree
requestListManagedSchemaArns = req
    "ListManagedSchemaArns"
    "fixture/ListManagedSchemaArns.yaml"

requestUpdateTypedLinkFacet :: UpdateTypedLinkFacet -> TestTree
requestUpdateTypedLinkFacet = req
    "UpdateTypedLinkFacet"
    "fixture/UpdateTypedLinkFacet.yaml"

requestDeleteTypedLinkFacet :: DeleteTypedLinkFacet -> TestTree
requestDeleteTypedLinkFacet = req
    "DeleteTypedLinkFacet"
    "fixture/DeleteTypedLinkFacet.yaml"

requestGetAppliedSchemaVersion :: GetAppliedSchemaVersion -> TestTree
requestGetAppliedSchemaVersion = req
    "GetAppliedSchemaVersion"
    "fixture/GetAppliedSchemaVersion.yaml"

requestRemoveFacetFromObject :: RemoveFacetFromObject -> TestTree
requestRemoveFacetFromObject = req
    "RemoveFacetFromObject"
    "fixture/RemoveFacetFromObject.yaml"

requestEnableDirectory :: EnableDirectory -> TestTree
requestEnableDirectory = req
    "EnableDirectory"
    "fixture/EnableDirectory.yaml"

requestListObjectAttributes :: ListObjectAttributes -> TestTree
requestListObjectAttributes = req
    "ListObjectAttributes"
    "fixture/ListObjectAttributes.yaml"

requestListAppliedSchemaArns :: ListAppliedSchemaArns -> TestTree
requestListAppliedSchemaArns = req
    "ListAppliedSchemaArns"
    "fixture/ListAppliedSchemaArns.yaml"

requestListIncomingTypedLinks :: ListIncomingTypedLinks -> TestTree
requestListIncomingTypedLinks = req
    "ListIncomingTypedLinks"
    "fixture/ListIncomingTypedLinks.yaml"

requestGetFacet :: GetFacet -> TestTree
requestGetFacet = req
    "GetFacet"
    "fixture/GetFacet.yaml"

requestGetTypedLinkFacetInformation :: GetTypedLinkFacetInformation -> TestTree
requestGetTypedLinkFacetInformation = req
    "GetTypedLinkFacetInformation"
    "fixture/GetTypedLinkFacetInformation.yaml"

requestListDevelopmentSchemaArns :: ListDevelopmentSchemaArns -> TestTree
requestListDevelopmentSchemaArns = req
    "ListDevelopmentSchemaArns"
    "fixture/ListDevelopmentSchemaArns.yaml"

requestAttachObject :: AttachObject -> TestTree
requestAttachObject = req
    "AttachObject"
    "fixture/AttachObject.yaml"

requestBatchWrite :: BatchWrite -> TestTree
requestBatchWrite = req
    "BatchWrite"
    "fixture/BatchWrite.yaml"

requestCreateObject :: CreateObject -> TestTree
requestCreateObject = req
    "CreateObject"
    "fixture/CreateObject.yaml"

requestUpgradePublishedSchema :: UpgradePublishedSchema -> TestTree
requestUpgradePublishedSchema = req
    "UpgradePublishedSchema"
    "fixture/UpgradePublishedSchema.yaml"

requestCreateFacet :: CreateFacet -> TestTree
requestCreateFacet = req
    "CreateFacet"
    "fixture/CreateFacet.yaml"

requestGetLinkAttributes :: GetLinkAttributes -> TestTree
requestGetLinkAttributes = req
    "GetLinkAttributes"
    "fixture/GetLinkAttributes.yaml"

requestGetObjectAttributes :: GetObjectAttributes -> TestTree
requestGetObjectAttributes = req
    "GetObjectAttributes"
    "fixture/GetObjectAttributes.yaml"

requestDeleteFacet :: DeleteFacet -> TestTree
requestDeleteFacet = req
    "DeleteFacet"
    "fixture/DeleteFacet.yaml"

requestUpdateFacet :: UpdateFacet -> TestTree
requestUpdateFacet = req
    "UpdateFacet"
    "fixture/UpdateFacet.yaml"

requestListObjectChildren :: ListObjectChildren -> TestTree
requestListObjectChildren = req
    "ListObjectChildren"
    "fixture/ListObjectChildren.yaml"

requestListTypedLinkFacetNames :: ListTypedLinkFacetNames -> TestTree
requestListTypedLinkFacetNames = req
    "ListTypedLinkFacetNames"
    "fixture/ListTypedLinkFacetNames.yaml"

requestAttachTypedLink :: AttachTypedLink -> TestTree
requestAttachTypedLink = req
    "AttachTypedLink"
    "fixture/AttachTypedLink.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy = req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestCreateIndex :: CreateIndex -> TestTree
requestCreateIndex = req
    "CreateIndex"
    "fixture/CreateIndex.yaml"

requestDetachObject :: DetachObject -> TestTree
requestDetachObject = req
    "DetachObject"
    "fixture/DetachObject.yaml"

requestAddFacetToObject :: AddFacetToObject -> TestTree
requestAddFacetToObject = req
    "AddFacetToObject"
    "fixture/AddFacetToObject.yaml"

requestApplySchema :: ApplySchema -> TestTree
requestApplySchema = req
    "ApplySchema"
    "fixture/ApplySchema.yaml"

requestCreateSchema :: CreateSchema -> TestTree
requestCreateSchema = req
    "CreateSchema"
    "fixture/CreateSchema.yaml"

requestGetSchemaAsJson :: GetSchemaAsJson -> TestTree
requestGetSchemaAsJson = req
    "GetSchemaAsJson"
    "fixture/GetSchemaAsJson.yaml"

requestPublishSchema :: PublishSchema -> TestTree
requestPublishSchema = req
    "PublishSchema"
    "fixture/PublishSchema.yaml"

requestDeleteDirectory :: DeleteDirectory -> TestTree
requestDeleteDirectory = req
    "DeleteDirectory"
    "fixture/DeleteDirectory.yaml"

requestListObjectParents :: ListObjectParents -> TestTree
requestListObjectParents = req
    "ListObjectParents"
    "fixture/ListObjectParents.yaml"

requestListPolicyAttachments :: ListPolicyAttachments -> TestTree
requestListPolicyAttachments = req
    "ListPolicyAttachments"
    "fixture/ListPolicyAttachments.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestUpdateSchema :: UpdateSchema -> TestTree
requestUpdateSchema = req
    "UpdateSchema"
    "fixture/UpdateSchema.yaml"

requestDeleteSchema :: DeleteSchema -> TestTree
requestDeleteSchema = req
    "DeleteSchema"
    "fixture/DeleteSchema.yaml"

requestDetachTypedLink :: DetachTypedLink -> TestTree
requestDetachTypedLink = req
    "DetachTypedLink"
    "fixture/DetachTypedLink.yaml"

requestListFacetNames :: ListFacetNames -> TestTree
requestListFacetNames = req
    "ListFacetNames"
    "fixture/ListFacetNames.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListOutgoingTypedLinks :: ListOutgoingTypedLinks -> TestTree
requestListOutgoingTypedLinks = req
    "ListOutgoingTypedLinks"
    "fixture/ListOutgoingTypedLinks.yaml"

requestUpdateObjectAttributes :: UpdateObjectAttributes -> TestTree
requestUpdateObjectAttributes = req
    "UpdateObjectAttributes"
    "fixture/UpdateObjectAttributes.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy = req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestBatchRead :: BatchRead -> TestTree
requestBatchRead = req
    "BatchRead"
    "fixture/BatchRead.yaml"

requestPutSchemaFromJson :: PutSchemaFromJson -> TestTree
requestPutSchemaFromJson = req
    "PutSchemaFromJson"
    "fixture/PutSchemaFromJson.yaml"

requestUpdateLinkAttributes :: UpdateLinkAttributes -> TestTree
requestUpdateLinkAttributes = req
    "UpdateLinkAttributes"
    "fixture/UpdateLinkAttributes.yaml"

requestAttachToIndex :: AttachToIndex -> TestTree
requestAttachToIndex = req
    "AttachToIndex"
    "fixture/AttachToIndex.yaml"

requestListObjectPolicies :: ListObjectPolicies -> TestTree
requestListObjectPolicies = req
    "ListObjectPolicies"
    "fixture/ListObjectPolicies.yaml"

-- Responses

responseListTypedLinkFacetAttributes :: ListTypedLinkFacetAttributesResponse -> TestTree
responseListTypedLinkFacetAttributes = res
    "ListTypedLinkFacetAttributesResponse"
    "fixture/ListTypedLinkFacetAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTypedLinkFacetAttributes)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteObject)

responseListIndex :: ListIndexResponse -> TestTree
responseListIndex = res
    "ListIndexResponse"
    "fixture/ListIndexResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIndex)

responseUpgradeAppliedSchema :: UpgradeAppliedSchemaResponse -> TestTree
responseUpgradeAppliedSchema = res
    "UpgradeAppliedSchemaResponse"
    "fixture/UpgradeAppliedSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpgradeAppliedSchema)

responseGetDirectory :: GetDirectoryResponse -> TestTree
responseGetDirectory = res
    "GetDirectoryResponse"
    "fixture/GetDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDirectory)

responseGetObjectInformation :: GetObjectInformationResponse -> TestTree
responseGetObjectInformation = res
    "GetObjectInformationResponse"
    "fixture/GetObjectInformationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetObjectInformation)

responseListAttachedIndices :: ListAttachedIndicesResponse -> TestTree
responseListAttachedIndices = res
    "ListAttachedIndicesResponse"
    "fixture/ListAttachedIndicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAttachedIndices)

responseDetachFromIndex :: DetachFromIndexResponse -> TestTree
responseDetachFromIndex = res
    "DetachFromIndexResponse"
    "fixture/DetachFromIndexResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachFromIndex)

responseLookupPolicy :: LookupPolicyResponse -> TestTree
responseLookupPolicy = res
    "LookupPolicyResponse"
    "fixture/LookupPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy LookupPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseListPublishedSchemaArns :: ListPublishedSchemaArnsResponse -> TestTree
responseListPublishedSchemaArns = res
    "ListPublishedSchemaArnsResponse"
    "fixture/ListPublishedSchemaArnsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPublishedSchemaArns)

responseListDirectories :: ListDirectoriesResponse -> TestTree
responseListDirectories = res
    "ListDirectoriesResponse"
    "fixture/ListDirectoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDirectories)

responseCreateTypedLinkFacet :: CreateTypedLinkFacetResponse -> TestTree
responseCreateTypedLinkFacet = res
    "CreateTypedLinkFacetResponse"
    "fixture/CreateTypedLinkFacetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTypedLinkFacet)

responseListObjectParentPaths :: ListObjectParentPathsResponse -> TestTree
responseListObjectParentPaths = res
    "ListObjectParentPathsResponse"
    "fixture/ListObjectParentPathsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjectParentPaths)

responseDisableDirectory :: DisableDirectoryResponse -> TestTree
responseDisableDirectory = res
    "DisableDirectoryResponse"
    "fixture/DisableDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableDirectory)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDirectory)

responseListFacetAttributes :: ListFacetAttributesResponse -> TestTree
responseListFacetAttributes = res
    "ListFacetAttributesResponse"
    "fixture/ListFacetAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFacetAttributes)

responseListManagedSchemaArns :: ListManagedSchemaArnsResponse -> TestTree
responseListManagedSchemaArns = res
    "ListManagedSchemaArnsResponse"
    "fixture/ListManagedSchemaArnsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListManagedSchemaArns)

responseUpdateTypedLinkFacet :: UpdateTypedLinkFacetResponse -> TestTree
responseUpdateTypedLinkFacet = res
    "UpdateTypedLinkFacetResponse"
    "fixture/UpdateTypedLinkFacetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTypedLinkFacet)

responseDeleteTypedLinkFacet :: DeleteTypedLinkFacetResponse -> TestTree
responseDeleteTypedLinkFacet = res
    "DeleteTypedLinkFacetResponse"
    "fixture/DeleteTypedLinkFacetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTypedLinkFacet)

responseGetAppliedSchemaVersion :: GetAppliedSchemaVersionResponse -> TestTree
responseGetAppliedSchemaVersion = res
    "GetAppliedSchemaVersionResponse"
    "fixture/GetAppliedSchemaVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAppliedSchemaVersion)

responseRemoveFacetFromObject :: RemoveFacetFromObjectResponse -> TestTree
responseRemoveFacetFromObject = res
    "RemoveFacetFromObjectResponse"
    "fixture/RemoveFacetFromObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveFacetFromObject)

responseEnableDirectory :: EnableDirectoryResponse -> TestTree
responseEnableDirectory = res
    "EnableDirectoryResponse"
    "fixture/EnableDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableDirectory)

responseListObjectAttributes :: ListObjectAttributesResponse -> TestTree
responseListObjectAttributes = res
    "ListObjectAttributesResponse"
    "fixture/ListObjectAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjectAttributes)

responseListAppliedSchemaArns :: ListAppliedSchemaArnsResponse -> TestTree
responseListAppliedSchemaArns = res
    "ListAppliedSchemaArnsResponse"
    "fixture/ListAppliedSchemaArnsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAppliedSchemaArns)

responseListIncomingTypedLinks :: ListIncomingTypedLinksResponse -> TestTree
responseListIncomingTypedLinks = res
    "ListIncomingTypedLinksResponse"
    "fixture/ListIncomingTypedLinksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIncomingTypedLinks)

responseGetFacet :: GetFacetResponse -> TestTree
responseGetFacet = res
    "GetFacetResponse"
    "fixture/GetFacetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFacet)

responseGetTypedLinkFacetInformation :: GetTypedLinkFacetInformationResponse -> TestTree
responseGetTypedLinkFacetInformation = res
    "GetTypedLinkFacetInformationResponse"
    "fixture/GetTypedLinkFacetInformationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTypedLinkFacetInformation)

responseListDevelopmentSchemaArns :: ListDevelopmentSchemaArnsResponse -> TestTree
responseListDevelopmentSchemaArns = res
    "ListDevelopmentSchemaArnsResponse"
    "fixture/ListDevelopmentSchemaArnsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDevelopmentSchemaArns)

responseAttachObject :: AttachObjectResponse -> TestTree
responseAttachObject = res
    "AttachObjectResponse"
    "fixture/AttachObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachObject)

responseBatchWrite :: BatchWriteResponse -> TestTree
responseBatchWrite = res
    "BatchWriteResponse"
    "fixture/BatchWriteResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchWrite)

responseCreateObject :: CreateObjectResponse -> TestTree
responseCreateObject = res
    "CreateObjectResponse"
    "fixture/CreateObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateObject)

responseUpgradePublishedSchema :: UpgradePublishedSchemaResponse -> TestTree
responseUpgradePublishedSchema = res
    "UpgradePublishedSchemaResponse"
    "fixture/UpgradePublishedSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpgradePublishedSchema)

responseCreateFacet :: CreateFacetResponse -> TestTree
responseCreateFacet = res
    "CreateFacetResponse"
    "fixture/CreateFacetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateFacet)

responseGetLinkAttributes :: GetLinkAttributesResponse -> TestTree
responseGetLinkAttributes = res
    "GetLinkAttributesResponse"
    "fixture/GetLinkAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLinkAttributes)

responseGetObjectAttributes :: GetObjectAttributesResponse -> TestTree
responseGetObjectAttributes = res
    "GetObjectAttributesResponse"
    "fixture/GetObjectAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetObjectAttributes)

responseDeleteFacet :: DeleteFacetResponse -> TestTree
responseDeleteFacet = res
    "DeleteFacetResponse"
    "fixture/DeleteFacetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFacet)

responseUpdateFacet :: UpdateFacetResponse -> TestTree
responseUpdateFacet = res
    "UpdateFacetResponse"
    "fixture/UpdateFacetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateFacet)

responseListObjectChildren :: ListObjectChildrenResponse -> TestTree
responseListObjectChildren = res
    "ListObjectChildrenResponse"
    "fixture/ListObjectChildrenResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjectChildren)

responseListTypedLinkFacetNames :: ListTypedLinkFacetNamesResponse -> TestTree
responseListTypedLinkFacetNames = res
    "ListTypedLinkFacetNamesResponse"
    "fixture/ListTypedLinkFacetNamesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTypedLinkFacetNames)

responseAttachTypedLink :: AttachTypedLinkResponse -> TestTree
responseAttachTypedLink = res
    "AttachTypedLinkResponse"
    "fixture/AttachTypedLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachTypedLink)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy = res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachPolicy)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex = res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateIndex)

responseDetachObject :: DetachObjectResponse -> TestTree
responseDetachObject = res
    "DetachObjectResponse"
    "fixture/DetachObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachObject)

responseAddFacetToObject :: AddFacetToObjectResponse -> TestTree
responseAddFacetToObject = res
    "AddFacetToObjectResponse"
    "fixture/AddFacetToObjectResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddFacetToObject)

responseApplySchema :: ApplySchemaResponse -> TestTree
responseApplySchema = res
    "ApplySchemaResponse"
    "fixture/ApplySchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ApplySchema)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema = res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSchema)

responseGetSchemaAsJson :: GetSchemaAsJsonResponse -> TestTree
responseGetSchemaAsJson = res
    "GetSchemaAsJsonResponse"
    "fixture/GetSchemaAsJsonResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSchemaAsJson)

responsePublishSchema :: PublishSchemaResponse -> TestTree
responsePublishSchema = res
    "PublishSchemaResponse"
    "fixture/PublishSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PublishSchema)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory = res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDirectory)

responseListObjectParents :: ListObjectParentsResponse -> TestTree
responseListObjectParents = res
    "ListObjectParentsResponse"
    "fixture/ListObjectParentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjectParents)

responseListPolicyAttachments :: ListPolicyAttachmentsResponse -> TestTree
responseListPolicyAttachments = res
    "ListPolicyAttachmentsResponse"
    "fixture/ListPolicyAttachmentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPolicyAttachments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema = res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema = res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSchema)

responseDetachTypedLink :: DetachTypedLinkResponse -> TestTree
responseDetachTypedLink = res
    "DetachTypedLinkResponse"
    "fixture/DetachTypedLinkResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachTypedLink)

responseListFacetNames :: ListFacetNamesResponse -> TestTree
responseListFacetNames = res
    "ListFacetNamesResponse"
    "fixture/ListFacetNamesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListFacetNames)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseListOutgoingTypedLinks :: ListOutgoingTypedLinksResponse -> TestTree
responseListOutgoingTypedLinks = res
    "ListOutgoingTypedLinksResponse"
    "fixture/ListOutgoingTypedLinksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListOutgoingTypedLinks)

responseUpdateObjectAttributes :: UpdateObjectAttributesResponse -> TestTree
responseUpdateObjectAttributes = res
    "UpdateObjectAttributesResponse"
    "fixture/UpdateObjectAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateObjectAttributes)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy = res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachPolicy)

responseBatchRead :: BatchReadResponse -> TestTree
responseBatchRead = res
    "BatchReadResponse"
    "fixture/BatchReadResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchRead)

responsePutSchemaFromJson :: PutSchemaFromJsonResponse -> TestTree
responsePutSchemaFromJson = res
    "PutSchemaFromJsonResponse"
    "fixture/PutSchemaFromJsonResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutSchemaFromJson)

responseUpdateLinkAttributes :: UpdateLinkAttributesResponse -> TestTree
responseUpdateLinkAttributes = res
    "UpdateLinkAttributesResponse"
    "fixture/UpdateLinkAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateLinkAttributes)

responseAttachToIndex :: AttachToIndexResponse -> TestTree
responseAttachToIndex = res
    "AttachToIndexResponse"
    "fixture/AttachToIndexResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachToIndex)

responseListObjectPolicies :: ListObjectPoliciesResponse -> TestTree
responseListObjectPolicies = res
    "ListObjectPoliciesResponse"
    "fixture/ListObjectPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListObjectPolicies)

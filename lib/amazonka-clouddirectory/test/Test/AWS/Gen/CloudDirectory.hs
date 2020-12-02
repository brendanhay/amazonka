{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CloudDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             listTypedLinkFacetAttributes
--
--         , requestDeleteObject $
--             deleteObject
--
--         , requestListIndex $
--             listIndex
--
--         , requestUpgradeAppliedSchema $
--             upgradeAppliedSchema
--
--         , requestGetDirectory $
--             getDirectory
--
--         , requestGetObjectInformation $
--             getObjectInformation
--
--         , requestListAttachedIndices $
--             listAttachedIndices
--
--         , requestDetachFromIndex $
--             detachFromIndex
--
--         , requestLookupPolicy $
--             lookupPolicy
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestListPublishedSchemaARNs $
--             listPublishedSchemaARNs
--
--         , requestListDirectories $
--             listDirectories
--
--         , requestCreateTypedLinkFacet $
--             createTypedLinkFacet
--
--         , requestListObjectParentPaths $
--             listObjectParentPaths
--
--         , requestDisableDirectory $
--             disableDirectory
--
--         , requestCreateDirectory $
--             createDirectory
--
--         , requestListFacetAttributes $
--             listFacetAttributes
--
--         , requestUpdateTypedLinkFacet $
--             updateTypedLinkFacet
--
--         , requestDeleteTypedLinkFacet $
--             deleteTypedLinkFacet
--
--         , requestGetAppliedSchemaVersion $
--             getAppliedSchemaVersion
--
--         , requestRemoveFacetFromObject $
--             removeFacetFromObject
--
--         , requestEnableDirectory $
--             enableDirectory
--
--         , requestListObjectAttributes $
--             listObjectAttributes
--
--         , requestListAppliedSchemaARNs $
--             listAppliedSchemaARNs
--
--         , requestListIncomingTypedLinks $
--             listIncomingTypedLinks
--
--         , requestGetFacet $
--             getFacet
--
--         , requestGetTypedLinkFacetInformation $
--             getTypedLinkFacetInformation
--
--         , requestListDevelopmentSchemaARNs $
--             listDevelopmentSchemaARNs
--
--         , requestAttachObject $
--             attachObject
--
--         , requestBatchWrite $
--             batchWrite
--
--         , requestCreateObject $
--             createObject
--
--         , requestUpgradePublishedSchema $
--             upgradePublishedSchema
--
--         , requestCreateFacet $
--             createFacet
--
--         , requestGetObjectAttributes $
--             getObjectAttributes
--
--         , requestDeleteFacet $
--             deleteFacet
--
--         , requestUpdateFacet $
--             updateFacet
--
--         , requestListObjectChildren $
--             listObjectChildren
--
--         , requestListTypedLinkFacetNames $
--             listTypedLinkFacetNames
--
--         , requestAttachTypedLink $
--             attachTypedLink
--
--         , requestDetachPolicy $
--             detachPolicy
--
--         , requestCreateIndex $
--             createIndex
--
--         , requestDetachObject $
--             detachObject
--
--         , requestAddFacetToObject $
--             addFacetToObject
--
--         , requestApplySchema $
--             applySchema
--
--         , requestCreateSchema $
--             createSchema
--
--         , requestGetSchemaAsJSON $
--             getSchemaAsJSON
--
--         , requestPublishSchema $
--             publishSchema
--
--         , requestDeleteDirectory $
--             deleteDirectory
--
--         , requestListObjectParents $
--             listObjectParents
--
--         , requestListPolicyAttachments $
--             listPolicyAttachments
--
--         , requestTagResource $
--             tagResource
--
--         , requestUpdateSchema $
--             updateSchema
--
--         , requestDeleteSchema $
--             deleteSchema
--
--         , requestDetachTypedLink $
--             detachTypedLink
--
--         , requestListFacetNames $
--             listFacetNames
--
--         , requestUntagResource $
--             untagResource
--
--         , requestListOutgoingTypedLinks $
--             listOutgoingTypedLinks
--
--         , requestUpdateObjectAttributes $
--             updateObjectAttributes
--
--         , requestAttachPolicy $
--             attachPolicy
--
--         , requestBatchRead $
--             batchRead
--
--         , requestPutSchemaFromJSON $
--             putSchemaFromJSON
--
--         , requestAttachToIndex $
--             attachToIndex
--
--         , requestListObjectPolicies $
--             listObjectPolicies
--
--           ]

--     , testGroup "response"
--         [ responseListTypedLinkFacetAttributes $
--             listTypedLinkFacetAttributesResponse
--
--         , responseDeleteObject $
--             deleteObjectResponse
--
--         , responseListIndex $
--             listIndexResponse
--
--         , responseUpgradeAppliedSchema $
--             upgradeAppliedSchemaResponse
--
--         , responseGetDirectory $
--             getDirectoryResponse
--
--         , responseGetObjectInformation $
--             getObjectInformationResponse
--
--         , responseListAttachedIndices $
--             listAttachedIndicesResponse
--
--         , responseDetachFromIndex $
--             detachFromIndexResponse
--
--         , responseLookupPolicy $
--             lookupPolicyResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseListPublishedSchemaARNs $
--             listPublishedSchemaARNsResponse
--
--         , responseListDirectories $
--             listDirectoriesResponse
--
--         , responseCreateTypedLinkFacet $
--             createTypedLinkFacetResponse
--
--         , responseListObjectParentPaths $
--             listObjectParentPathsResponse
--
--         , responseDisableDirectory $
--             disableDirectoryResponse
--
--         , responseCreateDirectory $
--             createDirectoryResponse
--
--         , responseListFacetAttributes $
--             listFacetAttributesResponse
--
--         , responseUpdateTypedLinkFacet $
--             updateTypedLinkFacetResponse
--
--         , responseDeleteTypedLinkFacet $
--             deleteTypedLinkFacetResponse
--
--         , responseGetAppliedSchemaVersion $
--             getAppliedSchemaVersionResponse
--
--         , responseRemoveFacetFromObject $
--             removeFacetFromObjectResponse
--
--         , responseEnableDirectory $
--             enableDirectoryResponse
--
--         , responseListObjectAttributes $
--             listObjectAttributesResponse
--
--         , responseListAppliedSchemaARNs $
--             listAppliedSchemaARNsResponse
--
--         , responseListIncomingTypedLinks $
--             listIncomingTypedLinksResponse
--
--         , responseGetFacet $
--             getFacetResponse
--
--         , responseGetTypedLinkFacetInformation $
--             getTypedLinkFacetInformationResponse
--
--         , responseListDevelopmentSchemaARNs $
--             listDevelopmentSchemaARNsResponse
--
--         , responseAttachObject $
--             attachObjectResponse
--
--         , responseBatchWrite $
--             batchWriteResponse
--
--         , responseCreateObject $
--             createObjectResponse
--
--         , responseUpgradePublishedSchema $
--             upgradePublishedSchemaResponse
--
--         , responseCreateFacet $
--             createFacetResponse
--
--         , responseGetObjectAttributes $
--             getObjectAttributesResponse
--
--         , responseDeleteFacet $
--             deleteFacetResponse
--
--         , responseUpdateFacet $
--             updateFacetResponse
--
--         , responseListObjectChildren $
--             listObjectChildrenResponse
--
--         , responseListTypedLinkFacetNames $
--             listTypedLinkFacetNamesResponse
--
--         , responseAttachTypedLink $
--             attachTypedLinkResponse
--
--         , responseDetachPolicy $
--             detachPolicyResponse
--
--         , responseCreateIndex $
--             createIndexResponse
--
--         , responseDetachObject $
--             detachObjectResponse
--
--         , responseAddFacetToObject $
--             addFacetToObjectResponse
--
--         , responseApplySchema $
--             applySchemaResponse
--
--         , responseCreateSchema $
--             createSchemaResponse
--
--         , responseGetSchemaAsJSON $
--             getSchemaAsJSONResponse
--
--         , responsePublishSchema $
--             publishSchemaResponse
--
--         , responseDeleteDirectory $
--             deleteDirectoryResponse
--
--         , responseListObjectParents $
--             listObjectParentsResponse
--
--         , responseListPolicyAttachments $
--             listPolicyAttachmentsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseUpdateSchema $
--             updateSchemaResponse
--
--         , responseDeleteSchema $
--             deleteSchemaResponse
--
--         , responseDetachTypedLink $
--             detachTypedLinkResponse
--
--         , responseListFacetNames $
--             listFacetNamesResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseListOutgoingTypedLinks $
--             listOutgoingTypedLinksResponse
--
--         , responseUpdateObjectAttributes $
--             updateObjectAttributesResponse
--
--         , responseAttachPolicy $
--             attachPolicyResponse
--
--         , responseBatchRead $
--             batchReadResponse
--
--         , responsePutSchemaFromJSON $
--             putSchemaFromJSONResponse
--
--         , responseAttachToIndex $
--             attachToIndexResponse
--
--         , responseListObjectPolicies $
--             listObjectPoliciesResponse
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

requestListPublishedSchemaARNs :: ListPublishedSchemaARNs -> TestTree
requestListPublishedSchemaARNs = req
    "ListPublishedSchemaARNs"
    "fixture/ListPublishedSchemaARNs.yaml"

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

requestListAppliedSchemaARNs :: ListAppliedSchemaARNs -> TestTree
requestListAppliedSchemaARNs = req
    "ListAppliedSchemaARNs"
    "fixture/ListAppliedSchemaARNs.yaml"

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

requestListDevelopmentSchemaARNs :: ListDevelopmentSchemaARNs -> TestTree
requestListDevelopmentSchemaARNs = req
    "ListDevelopmentSchemaARNs"
    "fixture/ListDevelopmentSchemaARNs.yaml"

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

requestGetSchemaAsJSON :: GetSchemaAsJSON -> TestTree
requestGetSchemaAsJSON = req
    "GetSchemaAsJSON"
    "fixture/GetSchemaAsJSON.yaml"

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

requestPutSchemaFromJSON :: PutSchemaFromJSON -> TestTree
requestPutSchemaFromJSON = req
    "PutSchemaFromJSON"
    "fixture/PutSchemaFromJSON.yaml"

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
    cloudDirectory
    (Proxy :: Proxy ListTypedLinkFacetAttributes)

responseDeleteObject :: DeleteObjectResponse -> TestTree
responseDeleteObject = res
    "DeleteObjectResponse"
    "fixture/DeleteObjectResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DeleteObject)

responseListIndex :: ListIndexResponse -> TestTree
responseListIndex = res
    "ListIndexResponse"
    "fixture/ListIndexResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListIndex)

responseUpgradeAppliedSchema :: UpgradeAppliedSchemaResponse -> TestTree
responseUpgradeAppliedSchema = res
    "UpgradeAppliedSchemaResponse"
    "fixture/UpgradeAppliedSchemaResponse.proto"
    cloudDirectory
    (Proxy :: Proxy UpgradeAppliedSchema)

responseGetDirectory :: GetDirectoryResponse -> TestTree
responseGetDirectory = res
    "GetDirectoryResponse"
    "fixture/GetDirectoryResponse.proto"
    cloudDirectory
    (Proxy :: Proxy GetDirectory)

responseGetObjectInformation :: GetObjectInformationResponse -> TestTree
responseGetObjectInformation = res
    "GetObjectInformationResponse"
    "fixture/GetObjectInformationResponse.proto"
    cloudDirectory
    (Proxy :: Proxy GetObjectInformation)

responseListAttachedIndices :: ListAttachedIndicesResponse -> TestTree
responseListAttachedIndices = res
    "ListAttachedIndicesResponse"
    "fixture/ListAttachedIndicesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListAttachedIndices)

responseDetachFromIndex :: DetachFromIndexResponse -> TestTree
responseDetachFromIndex = res
    "DetachFromIndexResponse"
    "fixture/DetachFromIndexResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DetachFromIndex)

responseLookupPolicy :: LookupPolicyResponse -> TestTree
responseLookupPolicy = res
    "LookupPolicyResponse"
    "fixture/LookupPolicyResponse.proto"
    cloudDirectory
    (Proxy :: Proxy LookupPolicy)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListTagsForResource)

responseListPublishedSchemaARNs :: ListPublishedSchemaARNsResponse -> TestTree
responseListPublishedSchemaARNs = res
    "ListPublishedSchemaARNsResponse"
    "fixture/ListPublishedSchemaARNsResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListPublishedSchemaARNs)

responseListDirectories :: ListDirectoriesResponse -> TestTree
responseListDirectories = res
    "ListDirectoriesResponse"
    "fixture/ListDirectoriesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListDirectories)

responseCreateTypedLinkFacet :: CreateTypedLinkFacetResponse -> TestTree
responseCreateTypedLinkFacet = res
    "CreateTypedLinkFacetResponse"
    "fixture/CreateTypedLinkFacetResponse.proto"
    cloudDirectory
    (Proxy :: Proxy CreateTypedLinkFacet)

responseListObjectParentPaths :: ListObjectParentPathsResponse -> TestTree
responseListObjectParentPaths = res
    "ListObjectParentPathsResponse"
    "fixture/ListObjectParentPathsResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListObjectParentPaths)

responseDisableDirectory :: DisableDirectoryResponse -> TestTree
responseDisableDirectory = res
    "DisableDirectoryResponse"
    "fixture/DisableDirectoryResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DisableDirectory)

responseCreateDirectory :: CreateDirectoryResponse -> TestTree
responseCreateDirectory = res
    "CreateDirectoryResponse"
    "fixture/CreateDirectoryResponse.proto"
    cloudDirectory
    (Proxy :: Proxy CreateDirectory)

responseListFacetAttributes :: ListFacetAttributesResponse -> TestTree
responseListFacetAttributes = res
    "ListFacetAttributesResponse"
    "fixture/ListFacetAttributesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListFacetAttributes)

responseUpdateTypedLinkFacet :: UpdateTypedLinkFacetResponse -> TestTree
responseUpdateTypedLinkFacet = res
    "UpdateTypedLinkFacetResponse"
    "fixture/UpdateTypedLinkFacetResponse.proto"
    cloudDirectory
    (Proxy :: Proxy UpdateTypedLinkFacet)

responseDeleteTypedLinkFacet :: DeleteTypedLinkFacetResponse -> TestTree
responseDeleteTypedLinkFacet = res
    "DeleteTypedLinkFacetResponse"
    "fixture/DeleteTypedLinkFacetResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DeleteTypedLinkFacet)

responseGetAppliedSchemaVersion :: GetAppliedSchemaVersionResponse -> TestTree
responseGetAppliedSchemaVersion = res
    "GetAppliedSchemaVersionResponse"
    "fixture/GetAppliedSchemaVersionResponse.proto"
    cloudDirectory
    (Proxy :: Proxy GetAppliedSchemaVersion)

responseRemoveFacetFromObject :: RemoveFacetFromObjectResponse -> TestTree
responseRemoveFacetFromObject = res
    "RemoveFacetFromObjectResponse"
    "fixture/RemoveFacetFromObjectResponse.proto"
    cloudDirectory
    (Proxy :: Proxy RemoveFacetFromObject)

responseEnableDirectory :: EnableDirectoryResponse -> TestTree
responseEnableDirectory = res
    "EnableDirectoryResponse"
    "fixture/EnableDirectoryResponse.proto"
    cloudDirectory
    (Proxy :: Proxy EnableDirectory)

responseListObjectAttributes :: ListObjectAttributesResponse -> TestTree
responseListObjectAttributes = res
    "ListObjectAttributesResponse"
    "fixture/ListObjectAttributesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListObjectAttributes)

responseListAppliedSchemaARNs :: ListAppliedSchemaARNsResponse -> TestTree
responseListAppliedSchemaARNs = res
    "ListAppliedSchemaARNsResponse"
    "fixture/ListAppliedSchemaARNsResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListAppliedSchemaARNs)

responseListIncomingTypedLinks :: ListIncomingTypedLinksResponse -> TestTree
responseListIncomingTypedLinks = res
    "ListIncomingTypedLinksResponse"
    "fixture/ListIncomingTypedLinksResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListIncomingTypedLinks)

responseGetFacet :: GetFacetResponse -> TestTree
responseGetFacet = res
    "GetFacetResponse"
    "fixture/GetFacetResponse.proto"
    cloudDirectory
    (Proxy :: Proxy GetFacet)

responseGetTypedLinkFacetInformation :: GetTypedLinkFacetInformationResponse -> TestTree
responseGetTypedLinkFacetInformation = res
    "GetTypedLinkFacetInformationResponse"
    "fixture/GetTypedLinkFacetInformationResponse.proto"
    cloudDirectory
    (Proxy :: Proxy GetTypedLinkFacetInformation)

responseListDevelopmentSchemaARNs :: ListDevelopmentSchemaARNsResponse -> TestTree
responseListDevelopmentSchemaARNs = res
    "ListDevelopmentSchemaARNsResponse"
    "fixture/ListDevelopmentSchemaARNsResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListDevelopmentSchemaARNs)

responseAttachObject :: AttachObjectResponse -> TestTree
responseAttachObject = res
    "AttachObjectResponse"
    "fixture/AttachObjectResponse.proto"
    cloudDirectory
    (Proxy :: Proxy AttachObject)

responseBatchWrite :: BatchWriteResponse -> TestTree
responseBatchWrite = res
    "BatchWriteResponse"
    "fixture/BatchWriteResponse.proto"
    cloudDirectory
    (Proxy :: Proxy BatchWrite)

responseCreateObject :: CreateObjectResponse -> TestTree
responseCreateObject = res
    "CreateObjectResponse"
    "fixture/CreateObjectResponse.proto"
    cloudDirectory
    (Proxy :: Proxy CreateObject)

responseUpgradePublishedSchema :: UpgradePublishedSchemaResponse -> TestTree
responseUpgradePublishedSchema = res
    "UpgradePublishedSchemaResponse"
    "fixture/UpgradePublishedSchemaResponse.proto"
    cloudDirectory
    (Proxy :: Proxy UpgradePublishedSchema)

responseCreateFacet :: CreateFacetResponse -> TestTree
responseCreateFacet = res
    "CreateFacetResponse"
    "fixture/CreateFacetResponse.proto"
    cloudDirectory
    (Proxy :: Proxy CreateFacet)

responseGetObjectAttributes :: GetObjectAttributesResponse -> TestTree
responseGetObjectAttributes = res
    "GetObjectAttributesResponse"
    "fixture/GetObjectAttributesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy GetObjectAttributes)

responseDeleteFacet :: DeleteFacetResponse -> TestTree
responseDeleteFacet = res
    "DeleteFacetResponse"
    "fixture/DeleteFacetResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DeleteFacet)

responseUpdateFacet :: UpdateFacetResponse -> TestTree
responseUpdateFacet = res
    "UpdateFacetResponse"
    "fixture/UpdateFacetResponse.proto"
    cloudDirectory
    (Proxy :: Proxy UpdateFacet)

responseListObjectChildren :: ListObjectChildrenResponse -> TestTree
responseListObjectChildren = res
    "ListObjectChildrenResponse"
    "fixture/ListObjectChildrenResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListObjectChildren)

responseListTypedLinkFacetNames :: ListTypedLinkFacetNamesResponse -> TestTree
responseListTypedLinkFacetNames = res
    "ListTypedLinkFacetNamesResponse"
    "fixture/ListTypedLinkFacetNamesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListTypedLinkFacetNames)

responseAttachTypedLink :: AttachTypedLinkResponse -> TestTree
responseAttachTypedLink = res
    "AttachTypedLinkResponse"
    "fixture/AttachTypedLinkResponse.proto"
    cloudDirectory
    (Proxy :: Proxy AttachTypedLink)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy = res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DetachPolicy)

responseCreateIndex :: CreateIndexResponse -> TestTree
responseCreateIndex = res
    "CreateIndexResponse"
    "fixture/CreateIndexResponse.proto"
    cloudDirectory
    (Proxy :: Proxy CreateIndex)

responseDetachObject :: DetachObjectResponse -> TestTree
responseDetachObject = res
    "DetachObjectResponse"
    "fixture/DetachObjectResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DetachObject)

responseAddFacetToObject :: AddFacetToObjectResponse -> TestTree
responseAddFacetToObject = res
    "AddFacetToObjectResponse"
    "fixture/AddFacetToObjectResponse.proto"
    cloudDirectory
    (Proxy :: Proxy AddFacetToObject)

responseApplySchema :: ApplySchemaResponse -> TestTree
responseApplySchema = res
    "ApplySchemaResponse"
    "fixture/ApplySchemaResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ApplySchema)

responseCreateSchema :: CreateSchemaResponse -> TestTree
responseCreateSchema = res
    "CreateSchemaResponse"
    "fixture/CreateSchemaResponse.proto"
    cloudDirectory
    (Proxy :: Proxy CreateSchema)

responseGetSchemaAsJSON :: GetSchemaAsJSONResponse -> TestTree
responseGetSchemaAsJSON = res
    "GetSchemaAsJSONResponse"
    "fixture/GetSchemaAsJSONResponse.proto"
    cloudDirectory
    (Proxy :: Proxy GetSchemaAsJSON)

responsePublishSchema :: PublishSchemaResponse -> TestTree
responsePublishSchema = res
    "PublishSchemaResponse"
    "fixture/PublishSchemaResponse.proto"
    cloudDirectory
    (Proxy :: Proxy PublishSchema)

responseDeleteDirectory :: DeleteDirectoryResponse -> TestTree
responseDeleteDirectory = res
    "DeleteDirectoryResponse"
    "fixture/DeleteDirectoryResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DeleteDirectory)

responseListObjectParents :: ListObjectParentsResponse -> TestTree
responseListObjectParents = res
    "ListObjectParentsResponse"
    "fixture/ListObjectParentsResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListObjectParents)

responseListPolicyAttachments :: ListPolicyAttachmentsResponse -> TestTree
responseListPolicyAttachments = res
    "ListPolicyAttachmentsResponse"
    "fixture/ListPolicyAttachmentsResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListPolicyAttachments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloudDirectory
    (Proxy :: Proxy TagResource)

responseUpdateSchema :: UpdateSchemaResponse -> TestTree
responseUpdateSchema = res
    "UpdateSchemaResponse"
    "fixture/UpdateSchemaResponse.proto"
    cloudDirectory
    (Proxy :: Proxy UpdateSchema)

responseDeleteSchema :: DeleteSchemaResponse -> TestTree
responseDeleteSchema = res
    "DeleteSchemaResponse"
    "fixture/DeleteSchemaResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DeleteSchema)

responseDetachTypedLink :: DetachTypedLinkResponse -> TestTree
responseDetachTypedLink = res
    "DetachTypedLinkResponse"
    "fixture/DetachTypedLinkResponse.proto"
    cloudDirectory
    (Proxy :: Proxy DetachTypedLink)

responseListFacetNames :: ListFacetNamesResponse -> TestTree
responseListFacetNames = res
    "ListFacetNamesResponse"
    "fixture/ListFacetNamesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListFacetNames)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloudDirectory
    (Proxy :: Proxy UntagResource)

responseListOutgoingTypedLinks :: ListOutgoingTypedLinksResponse -> TestTree
responseListOutgoingTypedLinks = res
    "ListOutgoingTypedLinksResponse"
    "fixture/ListOutgoingTypedLinksResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListOutgoingTypedLinks)

responseUpdateObjectAttributes :: UpdateObjectAttributesResponse -> TestTree
responseUpdateObjectAttributes = res
    "UpdateObjectAttributesResponse"
    "fixture/UpdateObjectAttributesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy UpdateObjectAttributes)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy = res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    cloudDirectory
    (Proxy :: Proxy AttachPolicy)

responseBatchRead :: BatchReadResponse -> TestTree
responseBatchRead = res
    "BatchReadResponse"
    "fixture/BatchReadResponse.proto"
    cloudDirectory
    (Proxy :: Proxy BatchRead)

responsePutSchemaFromJSON :: PutSchemaFromJSONResponse -> TestTree
responsePutSchemaFromJSON = res
    "PutSchemaFromJSONResponse"
    "fixture/PutSchemaFromJSONResponse.proto"
    cloudDirectory
    (Proxy :: Proxy PutSchemaFromJSON)

responseAttachToIndex :: AttachToIndexResponse -> TestTree
responseAttachToIndex = res
    "AttachToIndexResponse"
    "fixture/AttachToIndexResponse.proto"
    cloudDirectory
    (Proxy :: Proxy AttachToIndex)

responseListObjectPolicies :: ListObjectPoliciesResponse -> TestTree
responseListObjectPolicies = res
    "ListObjectPoliciesResponse"
    "fixture/ListObjectPoliciesResponse.proto"
    cloudDirectory
    (Proxy :: Proxy ListObjectPolicies)

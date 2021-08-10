{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Lens
  ( -- * Operations

    -- ** GetDirectory
    getDirectory_directoryArn,
    getDirectoryResponse_httpStatus,
    getDirectoryResponse_directory,

    -- ** ListTypedLinkFacetNames
    listTypedLinkFacetNames_nextToken,
    listTypedLinkFacetNames_maxResults,
    listTypedLinkFacetNames_schemaArn,
    listTypedLinkFacetNamesResponse_nextToken,
    listTypedLinkFacetNamesResponse_facetNames,
    listTypedLinkFacetNamesResponse_httpStatus,

    -- ** GetObjectInformation
    getObjectInformation_consistencyLevel,
    getObjectInformation_directoryArn,
    getObjectInformation_objectReference,
    getObjectInformationResponse_schemaFacets,
    getObjectInformationResponse_objectIdentifier,
    getObjectInformationResponse_httpStatus,

    -- ** AttachTypedLink
    attachTypedLink_directoryArn,
    attachTypedLink_sourceObjectReference,
    attachTypedLink_targetObjectReference,
    attachTypedLink_typedLinkFacet,
    attachTypedLink_attributes,
    attachTypedLinkResponse_typedLinkSpecifier,
    attachTypedLinkResponse_httpStatus,

    -- ** DeleteFacet
    deleteFacet_schemaArn,
    deleteFacet_name,
    deleteFacetResponse_httpStatus,

    -- ** UpdateFacet
    updateFacet_attributeUpdates,
    updateFacet_objectType,
    updateFacet_schemaArn,
    updateFacet_name,
    updateFacetResponse_httpStatus,

    -- ** GetObjectAttributes
    getObjectAttributes_consistencyLevel,
    getObjectAttributes_directoryArn,
    getObjectAttributes_objectReference,
    getObjectAttributes_schemaFacet,
    getObjectAttributes_attributeNames,
    getObjectAttributesResponse_attributes,
    getObjectAttributesResponse_httpStatus,

    -- ** UpgradeAppliedSchema
    upgradeAppliedSchema_dryRun,
    upgradeAppliedSchema_publishedSchemaArn,
    upgradeAppliedSchema_directoryArn,
    upgradeAppliedSchemaResponse_directoryArn,
    upgradeAppliedSchemaResponse_upgradedSchemaArn,
    upgradeAppliedSchemaResponse_httpStatus,

    -- ** DeleteObject
    deleteObject_directoryArn,
    deleteObject_objectReference,
    deleteObjectResponse_httpStatus,

    -- ** ListTypedLinkFacetAttributes
    listTypedLinkFacetAttributes_nextToken,
    listTypedLinkFacetAttributes_maxResults,
    listTypedLinkFacetAttributes_schemaArn,
    listTypedLinkFacetAttributes_name,
    listTypedLinkFacetAttributesResponse_nextToken,
    listTypedLinkFacetAttributesResponse_attributes,
    listTypedLinkFacetAttributesResponse_httpStatus,

    -- ** CreateObject
    createObject_parentReference,
    createObject_linkName,
    createObject_objectAttributeList,
    createObject_directoryArn,
    createObject_schemaFacets,
    createObjectResponse_objectIdentifier,
    createObjectResponse_httpStatus,

    -- ** BatchWrite
    batchWrite_directoryArn,
    batchWrite_operations,
    batchWriteResponse_responses,
    batchWriteResponse_httpStatus,

    -- ** UpgradePublishedSchema
    upgradePublishedSchema_dryRun,
    upgradePublishedSchema_developmentSchemaArn,
    upgradePublishedSchema_publishedSchemaArn,
    upgradePublishedSchema_minorVersion,
    upgradePublishedSchemaResponse_upgradedSchemaArn,
    upgradePublishedSchemaResponse_httpStatus,

    -- ** ListDevelopmentSchemaArns
    listDevelopmentSchemaArns_nextToken,
    listDevelopmentSchemaArns_maxResults,
    listDevelopmentSchemaArnsResponse_schemaArns,
    listDevelopmentSchemaArnsResponse_nextToken,
    listDevelopmentSchemaArnsResponse_httpStatus,

    -- ** UpdateLinkAttributes
    updateLinkAttributes_directoryArn,
    updateLinkAttributes_typedLinkSpecifier,
    updateLinkAttributes_attributeUpdates,
    updateLinkAttributesResponse_httpStatus,

    -- ** DetachTypedLink
    detachTypedLink_directoryArn,
    detachTypedLink_typedLinkSpecifier,

    -- ** GetFacet
    getFacet_schemaArn,
    getFacet_name,
    getFacetResponse_facet,
    getFacetResponse_httpStatus,

    -- ** GetTypedLinkFacetInformation
    getTypedLinkFacetInformation_schemaArn,
    getTypedLinkFacetInformation_name,
    getTypedLinkFacetInformationResponse_identityAttributeOrder,
    getTypedLinkFacetInformationResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListIncomingTypedLinks
    listIncomingTypedLinks_nextToken,
    listIncomingTypedLinks_filterTypedLink,
    listIncomingTypedLinks_maxResults,
    listIncomingTypedLinks_consistencyLevel,
    listIncomingTypedLinks_filterAttributeRanges,
    listIncomingTypedLinks_directoryArn,
    listIncomingTypedLinks_objectReference,
    listIncomingTypedLinksResponse_linkSpecifiers,
    listIncomingTypedLinksResponse_nextToken,
    listIncomingTypedLinksResponse_httpStatus,

    -- ** PublishSchema
    publishSchema_name,
    publishSchema_minorVersion,
    publishSchema_developmentSchemaArn,
    publishSchema_version,
    publishSchemaResponse_publishedSchemaArn,
    publishSchemaResponse_httpStatus,

    -- ** UpdateTypedLinkFacet
    updateTypedLinkFacet_schemaArn,
    updateTypedLinkFacet_name,
    updateTypedLinkFacet_attributeUpdates,
    updateTypedLinkFacet_identityAttributeOrder,
    updateTypedLinkFacetResponse_httpStatus,

    -- ** ListObjectParents
    listObjectParents_nextToken,
    listObjectParents_maxResults,
    listObjectParents_includeAllLinksToEachParent,
    listObjectParents_consistencyLevel,
    listObjectParents_directoryArn,
    listObjectParents_objectReference,
    listObjectParentsResponse_parents,
    listObjectParentsResponse_parentLinks,
    listObjectParentsResponse_nextToken,
    listObjectParentsResponse_httpStatus,

    -- ** ListPolicyAttachments
    listPolicyAttachments_nextToken,
    listPolicyAttachments_maxResults,
    listPolicyAttachments_consistencyLevel,
    listPolicyAttachments_directoryArn,
    listPolicyAttachments_policyReference,
    listPolicyAttachmentsResponse_nextToken,
    listPolicyAttachmentsResponse_objectIdentifiers,
    listPolicyAttachmentsResponse_httpStatus,

    -- ** ListObjectAttributes
    listObjectAttributes_nextToken,
    listObjectAttributes_maxResults,
    listObjectAttributes_consistencyLevel,
    listObjectAttributes_facetFilter,
    listObjectAttributes_directoryArn,
    listObjectAttributes_objectReference,
    listObjectAttributesResponse_nextToken,
    listObjectAttributesResponse_attributes,
    listObjectAttributesResponse_httpStatus,

    -- ** DeleteTypedLinkFacet
    deleteTypedLinkFacet_schemaArn,
    deleteTypedLinkFacet_name,
    deleteTypedLinkFacetResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DeleteDirectory
    deleteDirectory_directoryArn,
    deleteDirectoryResponse_httpStatus,
    deleteDirectoryResponse_directoryArn,

    -- ** ListObjectParentPaths
    listObjectParentPaths_nextToken,
    listObjectParentPaths_maxResults,
    listObjectParentPaths_directoryArn,
    listObjectParentPaths_objectReference,
    listObjectParentPathsResponse_nextToken,
    listObjectParentPathsResponse_pathToObjectIdentifiersList,
    listObjectParentPathsResponse_httpStatus,

    -- ** CreateSchema
    createSchema_name,
    createSchemaResponse_schemaArn,
    createSchemaResponse_httpStatus,

    -- ** ListPublishedSchemaArns
    listPublishedSchemaArns_schemaArn,
    listPublishedSchemaArns_nextToken,
    listPublishedSchemaArns_maxResults,
    listPublishedSchemaArnsResponse_schemaArns,
    listPublishedSchemaArnsResponse_nextToken,
    listPublishedSchemaArnsResponse_httpStatus,

    -- ** ListDirectories
    listDirectories_nextToken,
    listDirectories_maxResults,
    listDirectories_state,
    listDirectoriesResponse_nextToken,
    listDirectoriesResponse_httpStatus,
    listDirectoriesResponse_directories,

    -- ** CreateDirectory
    createDirectory_name,
    createDirectory_schemaArn,
    createDirectoryResponse_httpStatus,
    createDirectoryResponse_directoryArn,
    createDirectoryResponse_name,
    createDirectoryResponse_objectIdentifier,
    createDirectoryResponse_appliedSchemaArn,

    -- ** LookupPolicy
    lookupPolicy_nextToken,
    lookupPolicy_maxResults,
    lookupPolicy_directoryArn,
    lookupPolicy_objectReference,
    lookupPolicyResponse_nextToken,
    lookupPolicyResponse_policyToPathList,
    lookupPolicyResponse_httpStatus,

    -- ** CreateIndex
    createIndex_parentReference,
    createIndex_linkName,
    createIndex_directoryArn,
    createIndex_orderedIndexedAttributeList,
    createIndex_isUnique,
    createIndexResponse_objectIdentifier,
    createIndexResponse_httpStatus,

    -- ** ListAttachedIndices
    listAttachedIndices_nextToken,
    listAttachedIndices_maxResults,
    listAttachedIndices_consistencyLevel,
    listAttachedIndices_directoryArn,
    listAttachedIndices_targetReference,
    listAttachedIndicesResponse_nextToken,
    listAttachedIndicesResponse_indexAttachments,
    listAttachedIndicesResponse_httpStatus,

    -- ** DetachPolicy
    detachPolicy_directoryArn,
    detachPolicy_policyReference,
    detachPolicy_objectReference,
    detachPolicyResponse_httpStatus,

    -- ** DetachFromIndex
    detachFromIndex_directoryArn,
    detachFromIndex_indexReference,
    detachFromIndex_targetReference,
    detachFromIndexResponse_detachedObjectIdentifier,
    detachFromIndexResponse_httpStatus,

    -- ** ListIndex
    listIndex_nextToken,
    listIndex_maxResults,
    listIndex_consistencyLevel,
    listIndex_rangesOnIndexedValues,
    listIndex_directoryArn,
    listIndex_indexReference,
    listIndexResponse_nextToken,
    listIndexResponse_indexAttachments,
    listIndexResponse_httpStatus,

    -- ** ListObjectChildren
    listObjectChildren_nextToken,
    listObjectChildren_maxResults,
    listObjectChildren_consistencyLevel,
    listObjectChildren_directoryArn,
    listObjectChildren_objectReference,
    listObjectChildrenResponse_nextToken,
    listObjectChildrenResponse_children,
    listObjectChildrenResponse_httpStatus,

    -- ** ListObjectPolicies
    listObjectPolicies_nextToken,
    listObjectPolicies_maxResults,
    listObjectPolicies_consistencyLevel,
    listObjectPolicies_directoryArn,
    listObjectPolicies_objectReference,
    listObjectPoliciesResponse_nextToken,
    listObjectPoliciesResponse_attachedPolicyIds,
    listObjectPoliciesResponse_httpStatus,

    -- ** GetLinkAttributes
    getLinkAttributes_consistencyLevel,
    getLinkAttributes_directoryArn,
    getLinkAttributes_typedLinkSpecifier,
    getLinkAttributes_attributeNames,
    getLinkAttributesResponse_attributes,
    getLinkAttributesResponse_httpStatus,

    -- ** CreateFacet
    createFacet_facetStyle,
    createFacet_attributes,
    createFacet_objectType,
    createFacet_schemaArn,
    createFacet_name,
    createFacetResponse_httpStatus,

    -- ** BatchRead
    batchRead_consistencyLevel,
    batchRead_directoryArn,
    batchRead_operations,
    batchReadResponse_responses,
    batchReadResponse_httpStatus,

    -- ** AttachObject
    attachObject_directoryArn,
    attachObject_parentReference,
    attachObject_childReference,
    attachObject_linkName,
    attachObjectResponse_attachedObjectIdentifier,
    attachObjectResponse_httpStatus,

    -- ** AttachToIndex
    attachToIndex_directoryArn,
    attachToIndex_indexReference,
    attachToIndex_targetReference,
    attachToIndexResponse_attachedObjectIdentifier,
    attachToIndexResponse_httpStatus,

    -- ** PutSchemaFromJson
    putSchemaFromJson_schemaArn,
    putSchemaFromJson_document,
    putSchemaFromJsonResponse_arn,
    putSchemaFromJsonResponse_httpStatus,

    -- ** UpdateObjectAttributes
    updateObjectAttributes_directoryArn,
    updateObjectAttributes_objectReference,
    updateObjectAttributes_attributeUpdates,
    updateObjectAttributesResponse_objectIdentifier,
    updateObjectAttributesResponse_httpStatus,

    -- ** AttachPolicy
    attachPolicy_directoryArn,
    attachPolicy_policyReference,
    attachPolicy_objectReference,
    attachPolicyResponse_httpStatus,

    -- ** ListFacetNames
    listFacetNames_nextToken,
    listFacetNames_maxResults,
    listFacetNames_schemaArn,
    listFacetNamesResponse_nextToken,
    listFacetNamesResponse_facetNames,
    listFacetNamesResponse_httpStatus,

    -- ** ListOutgoingTypedLinks
    listOutgoingTypedLinks_nextToken,
    listOutgoingTypedLinks_filterTypedLink,
    listOutgoingTypedLinks_maxResults,
    listOutgoingTypedLinks_consistencyLevel,
    listOutgoingTypedLinks_filterAttributeRanges,
    listOutgoingTypedLinks_directoryArn,
    listOutgoingTypedLinks_objectReference,
    listOutgoingTypedLinksResponse_nextToken,
    listOutgoingTypedLinksResponse_typedLinkSpecifiers,
    listOutgoingTypedLinksResponse_httpStatus,

    -- ** EnableDirectory
    enableDirectory_directoryArn,
    enableDirectoryResponse_httpStatus,
    enableDirectoryResponse_directoryArn,

    -- ** ListAppliedSchemaArns
    listAppliedSchemaArns_schemaArn,
    listAppliedSchemaArns_nextToken,
    listAppliedSchemaArns_maxResults,
    listAppliedSchemaArns_directoryArn,
    listAppliedSchemaArnsResponse_schemaArns,
    listAppliedSchemaArnsResponse_nextToken,
    listAppliedSchemaArnsResponse_httpStatus,

    -- ** ListManagedSchemaArns
    listManagedSchemaArns_schemaArn,
    listManagedSchemaArns_nextToken,
    listManagedSchemaArns_maxResults,
    listManagedSchemaArnsResponse_schemaArns,
    listManagedSchemaArnsResponse_nextToken,
    listManagedSchemaArnsResponse_httpStatus,

    -- ** GetAppliedSchemaVersion
    getAppliedSchemaVersion_schemaArn,
    getAppliedSchemaVersionResponse_appliedSchemaArn,
    getAppliedSchemaVersionResponse_httpStatus,

    -- ** RemoveFacetFromObject
    removeFacetFromObject_directoryArn,
    removeFacetFromObject_schemaFacet,
    removeFacetFromObject_objectReference,
    removeFacetFromObjectResponse_httpStatus,

    -- ** DeleteSchema
    deleteSchema_schemaArn,
    deleteSchemaResponse_schemaArn,
    deleteSchemaResponse_httpStatus,

    -- ** ListFacetAttributes
    listFacetAttributes_nextToken,
    listFacetAttributes_maxResults,
    listFacetAttributes_schemaArn,
    listFacetAttributes_name,
    listFacetAttributesResponse_nextToken,
    listFacetAttributesResponse_attributes,
    listFacetAttributesResponse_httpStatus,

    -- ** UpdateSchema
    updateSchema_schemaArn,
    updateSchema_name,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_httpStatus,

    -- ** GetSchemaAsJson
    getSchemaAsJson_schemaArn,
    getSchemaAsJsonResponse_name,
    getSchemaAsJsonResponse_document,
    getSchemaAsJsonResponse_httpStatus,

    -- ** ApplySchema
    applySchema_publishedSchemaArn,
    applySchema_directoryArn,
    applySchemaResponse_directoryArn,
    applySchemaResponse_appliedSchemaArn,
    applySchemaResponse_httpStatus,

    -- ** DisableDirectory
    disableDirectory_directoryArn,
    disableDirectoryResponse_httpStatus,
    disableDirectoryResponse_directoryArn,

    -- ** CreateTypedLinkFacet
    createTypedLinkFacet_schemaArn,
    createTypedLinkFacet_facet,
    createTypedLinkFacetResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_maxResults,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** AddFacetToObject
    addFacetToObject_objectAttributeList,
    addFacetToObject_directoryArn,
    addFacetToObject_schemaFacet,
    addFacetToObject_objectReference,
    addFacetToObjectResponse_httpStatus,

    -- ** DetachObject
    detachObject_directoryArn,
    detachObject_parentReference,
    detachObject_linkName,
    detachObjectResponse_detachedObjectIdentifier,
    detachObjectResponse_httpStatus,

    -- * Types

    -- ** AttributeKey
    attributeKey_schemaArn,
    attributeKey_facetName,
    attributeKey_name,

    -- ** AttributeKeyAndValue
    attributeKeyAndValue_key,
    attributeKeyAndValue_value,

    -- ** AttributeNameAndValue
    attributeNameAndValue_attributeName,
    attributeNameAndValue_value,

    -- ** BatchAddFacetToObject
    batchAddFacetToObject_schemaFacet,
    batchAddFacetToObject_objectAttributeList,
    batchAddFacetToObject_objectReference,

    -- ** BatchAddFacetToObjectResponse

    -- ** BatchAttachObject
    batchAttachObject_parentReference,
    batchAttachObject_childReference,
    batchAttachObject_linkName,

    -- ** BatchAttachObjectResponse
    batchAttachObjectResponse_attachedObjectIdentifier,

    -- ** BatchAttachPolicy
    batchAttachPolicy_policyReference,
    batchAttachPolicy_objectReference,

    -- ** BatchAttachPolicyResponse

    -- ** BatchAttachToIndex
    batchAttachToIndex_indexReference,
    batchAttachToIndex_targetReference,

    -- ** BatchAttachToIndexResponse
    batchAttachToIndexResponse_attachedObjectIdentifier,

    -- ** BatchAttachTypedLink
    batchAttachTypedLink_sourceObjectReference,
    batchAttachTypedLink_targetObjectReference,
    batchAttachTypedLink_typedLinkFacet,
    batchAttachTypedLink_attributes,

    -- ** BatchAttachTypedLinkResponse
    batchAttachTypedLinkResponse_typedLinkSpecifier,

    -- ** BatchCreateIndex
    batchCreateIndex_parentReference,
    batchCreateIndex_linkName,
    batchCreateIndex_batchReferenceName,
    batchCreateIndex_orderedIndexedAttributeList,
    batchCreateIndex_isUnique,

    -- ** BatchCreateIndexResponse
    batchCreateIndexResponse_objectIdentifier,

    -- ** BatchCreateObject
    batchCreateObject_parentReference,
    batchCreateObject_linkName,
    batchCreateObject_batchReferenceName,
    batchCreateObject_schemaFacet,
    batchCreateObject_objectAttributeList,

    -- ** BatchCreateObjectResponse
    batchCreateObjectResponse_objectIdentifier,

    -- ** BatchDeleteObject
    batchDeleteObject_objectReference,

    -- ** BatchDeleteObjectResponse

    -- ** BatchDetachFromIndex
    batchDetachFromIndex_indexReference,
    batchDetachFromIndex_targetReference,

    -- ** BatchDetachFromIndexResponse
    batchDetachFromIndexResponse_detachedObjectIdentifier,

    -- ** BatchDetachObject
    batchDetachObject_batchReferenceName,
    batchDetachObject_parentReference,
    batchDetachObject_linkName,

    -- ** BatchDetachObjectResponse
    batchDetachObjectResponse_detachedObjectIdentifier,

    -- ** BatchDetachPolicy
    batchDetachPolicy_policyReference,
    batchDetachPolicy_objectReference,

    -- ** BatchDetachPolicyResponse

    -- ** BatchDetachTypedLink
    batchDetachTypedLink_typedLinkSpecifier,

    -- ** BatchDetachTypedLinkResponse

    -- ** BatchGetLinkAttributes
    batchGetLinkAttributes_typedLinkSpecifier,
    batchGetLinkAttributes_attributeNames,

    -- ** BatchGetLinkAttributesResponse
    batchGetLinkAttributesResponse_attributes,

    -- ** BatchGetObjectAttributes
    batchGetObjectAttributes_objectReference,
    batchGetObjectAttributes_schemaFacet,
    batchGetObjectAttributes_attributeNames,

    -- ** BatchGetObjectAttributesResponse
    batchGetObjectAttributesResponse_attributes,

    -- ** BatchGetObjectInformation
    batchGetObjectInformation_objectReference,

    -- ** BatchGetObjectInformationResponse
    batchGetObjectInformationResponse_schemaFacets,
    batchGetObjectInformationResponse_objectIdentifier,

    -- ** BatchListAttachedIndices
    batchListAttachedIndices_nextToken,
    batchListAttachedIndices_maxResults,
    batchListAttachedIndices_targetReference,

    -- ** BatchListAttachedIndicesResponse
    batchListAttachedIndicesResponse_nextToken,
    batchListAttachedIndicesResponse_indexAttachments,

    -- ** BatchListIncomingTypedLinks
    batchListIncomingTypedLinks_nextToken,
    batchListIncomingTypedLinks_filterTypedLink,
    batchListIncomingTypedLinks_maxResults,
    batchListIncomingTypedLinks_filterAttributeRanges,
    batchListIncomingTypedLinks_objectReference,

    -- ** BatchListIncomingTypedLinksResponse
    batchListIncomingTypedLinksResponse_linkSpecifiers,
    batchListIncomingTypedLinksResponse_nextToken,

    -- ** BatchListIndex
    batchListIndex_nextToken,
    batchListIndex_maxResults,
    batchListIndex_rangesOnIndexedValues,
    batchListIndex_indexReference,

    -- ** BatchListIndexResponse
    batchListIndexResponse_nextToken,
    batchListIndexResponse_indexAttachments,

    -- ** BatchListObjectAttributes
    batchListObjectAttributes_nextToken,
    batchListObjectAttributes_maxResults,
    batchListObjectAttributes_facetFilter,
    batchListObjectAttributes_objectReference,

    -- ** BatchListObjectAttributesResponse
    batchListObjectAttributesResponse_nextToken,
    batchListObjectAttributesResponse_attributes,

    -- ** BatchListObjectChildren
    batchListObjectChildren_nextToken,
    batchListObjectChildren_maxResults,
    batchListObjectChildren_objectReference,

    -- ** BatchListObjectChildrenResponse
    batchListObjectChildrenResponse_nextToken,
    batchListObjectChildrenResponse_children,

    -- ** BatchListObjectParentPaths
    batchListObjectParentPaths_nextToken,
    batchListObjectParentPaths_maxResults,
    batchListObjectParentPaths_objectReference,

    -- ** BatchListObjectParentPathsResponse
    batchListObjectParentPathsResponse_nextToken,
    batchListObjectParentPathsResponse_pathToObjectIdentifiersList,

    -- ** BatchListObjectParents
    batchListObjectParents_nextToken,
    batchListObjectParents_maxResults,
    batchListObjectParents_objectReference,

    -- ** BatchListObjectParentsResponse
    batchListObjectParentsResponse_parentLinks,
    batchListObjectParentsResponse_nextToken,

    -- ** BatchListObjectPolicies
    batchListObjectPolicies_nextToken,
    batchListObjectPolicies_maxResults,
    batchListObjectPolicies_objectReference,

    -- ** BatchListObjectPoliciesResponse
    batchListObjectPoliciesResponse_nextToken,
    batchListObjectPoliciesResponse_attachedPolicyIds,

    -- ** BatchListOutgoingTypedLinks
    batchListOutgoingTypedLinks_nextToken,
    batchListOutgoingTypedLinks_filterTypedLink,
    batchListOutgoingTypedLinks_maxResults,
    batchListOutgoingTypedLinks_filterAttributeRanges,
    batchListOutgoingTypedLinks_objectReference,

    -- ** BatchListOutgoingTypedLinksResponse
    batchListOutgoingTypedLinksResponse_nextToken,
    batchListOutgoingTypedLinksResponse_typedLinkSpecifiers,

    -- ** BatchListPolicyAttachments
    batchListPolicyAttachments_nextToken,
    batchListPolicyAttachments_maxResults,
    batchListPolicyAttachments_policyReference,

    -- ** BatchListPolicyAttachmentsResponse
    batchListPolicyAttachmentsResponse_nextToken,
    batchListPolicyAttachmentsResponse_objectIdentifiers,

    -- ** BatchLookupPolicy
    batchLookupPolicy_nextToken,
    batchLookupPolicy_maxResults,
    batchLookupPolicy_objectReference,

    -- ** BatchLookupPolicyResponse
    batchLookupPolicyResponse_nextToken,
    batchLookupPolicyResponse_policyToPathList,

    -- ** BatchReadException
    batchReadException_message,
    batchReadException_type,

    -- ** BatchReadOperation
    batchReadOperation_getObjectInformation,
    batchReadOperation_getObjectAttributes,
    batchReadOperation_listIncomingTypedLinks,
    batchReadOperation_listObjectParents,
    batchReadOperation_listPolicyAttachments,
    batchReadOperation_listObjectAttributes,
    batchReadOperation_listObjectParentPaths,
    batchReadOperation_lookupPolicy,
    batchReadOperation_listAttachedIndices,
    batchReadOperation_listIndex,
    batchReadOperation_listObjectChildren,
    batchReadOperation_listObjectPolicies,
    batchReadOperation_getLinkAttributes,
    batchReadOperation_listOutgoingTypedLinks,

    -- ** BatchReadOperationResponse
    batchReadOperationResponse_successfulResponse,
    batchReadOperationResponse_exceptionResponse,

    -- ** BatchReadSuccessfulResponse
    batchReadSuccessfulResponse_getObjectInformation,
    batchReadSuccessfulResponse_getObjectAttributes,
    batchReadSuccessfulResponse_listIncomingTypedLinks,
    batchReadSuccessfulResponse_listObjectParents,
    batchReadSuccessfulResponse_listPolicyAttachments,
    batchReadSuccessfulResponse_listObjectAttributes,
    batchReadSuccessfulResponse_listObjectParentPaths,
    batchReadSuccessfulResponse_lookupPolicy,
    batchReadSuccessfulResponse_listAttachedIndices,
    batchReadSuccessfulResponse_listIndex,
    batchReadSuccessfulResponse_listObjectChildren,
    batchReadSuccessfulResponse_listObjectPolicies,
    batchReadSuccessfulResponse_getLinkAttributes,
    batchReadSuccessfulResponse_listOutgoingTypedLinks,

    -- ** BatchRemoveFacetFromObject
    batchRemoveFacetFromObject_schemaFacet,
    batchRemoveFacetFromObject_objectReference,

    -- ** BatchRemoveFacetFromObjectResponse

    -- ** BatchUpdateLinkAttributes
    batchUpdateLinkAttributes_typedLinkSpecifier,
    batchUpdateLinkAttributes_attributeUpdates,

    -- ** BatchUpdateLinkAttributesResponse

    -- ** BatchUpdateObjectAttributes
    batchUpdateObjectAttributes_objectReference,
    batchUpdateObjectAttributes_attributeUpdates,

    -- ** BatchUpdateObjectAttributesResponse
    batchUpdateObjectAttributesResponse_objectIdentifier,

    -- ** BatchWriteOperation
    batchWriteOperation_attachTypedLink,
    batchWriteOperation_deleteObject,
    batchWriteOperation_createObject,
    batchWriteOperation_updateLinkAttributes,
    batchWriteOperation_detachTypedLink,
    batchWriteOperation_createIndex,
    batchWriteOperation_detachPolicy,
    batchWriteOperation_detachFromIndex,
    batchWriteOperation_attachObject,
    batchWriteOperation_attachToIndex,
    batchWriteOperation_updateObjectAttributes,
    batchWriteOperation_attachPolicy,
    batchWriteOperation_removeFacetFromObject,
    batchWriteOperation_addFacetToObject,
    batchWriteOperation_detachObject,

    -- ** BatchWriteOperationResponse
    batchWriteOperationResponse_attachTypedLink,
    batchWriteOperationResponse_deleteObject,
    batchWriteOperationResponse_createObject,
    batchWriteOperationResponse_updateLinkAttributes,
    batchWriteOperationResponse_detachTypedLink,
    batchWriteOperationResponse_createIndex,
    batchWriteOperationResponse_detachPolicy,
    batchWriteOperationResponse_detachFromIndex,
    batchWriteOperationResponse_attachObject,
    batchWriteOperationResponse_attachToIndex,
    batchWriteOperationResponse_updateObjectAttributes,
    batchWriteOperationResponse_attachPolicy,
    batchWriteOperationResponse_removeFacetFromObject,
    batchWriteOperationResponse_addFacetToObject,
    batchWriteOperationResponse_detachObject,

    -- ** Directory
    directory_directoryArn,
    directory_state,
    directory_name,
    directory_creationDateTime,

    -- ** Facet
    facet_facetStyle,
    facet_name,
    facet_objectType,

    -- ** FacetAttribute
    facetAttribute_attributeReference,
    facetAttribute_requiredBehavior,
    facetAttribute_attributeDefinition,
    facetAttribute_name,

    -- ** FacetAttributeDefinition
    facetAttributeDefinition_isImmutable,
    facetAttributeDefinition_rules,
    facetAttributeDefinition_defaultValue,
    facetAttributeDefinition_type,

    -- ** FacetAttributeReference
    facetAttributeReference_targetFacetName,
    facetAttributeReference_targetAttributeName,

    -- ** FacetAttributeUpdate
    facetAttributeUpdate_attribute,
    facetAttributeUpdate_action,

    -- ** IndexAttachment
    indexAttachment_objectIdentifier,
    indexAttachment_indexedAttributes,

    -- ** LinkAttributeAction
    linkAttributeAction_attributeUpdateValue,
    linkAttributeAction_attributeActionType,

    -- ** LinkAttributeUpdate
    linkAttributeUpdate_attributeAction,
    linkAttributeUpdate_attributeKey,

    -- ** ObjectAttributeAction
    objectAttributeAction_objectAttributeActionType,
    objectAttributeAction_objectAttributeUpdateValue,

    -- ** ObjectAttributeRange
    objectAttributeRange_range,
    objectAttributeRange_attributeKey,

    -- ** ObjectAttributeUpdate
    objectAttributeUpdate_objectAttributeAction,
    objectAttributeUpdate_objectAttributeKey,

    -- ** ObjectIdentifierAndLinkNameTuple
    objectIdentifierAndLinkNameTuple_linkName,
    objectIdentifierAndLinkNameTuple_objectIdentifier,

    -- ** ObjectReference
    objectReference_selector,

    -- ** PathToObjectIdentifiers
    pathToObjectIdentifiers_objectIdentifiers,
    pathToObjectIdentifiers_path,

    -- ** PolicyAttachment
    policyAttachment_policyType,
    policyAttachment_objectIdentifier,
    policyAttachment_policyId,

    -- ** PolicyToPath
    policyToPath_policies,
    policyToPath_path,

    -- ** Rule
    rule_type,
    rule_parameters,

    -- ** SchemaFacet
    schemaFacet_schemaArn,
    schemaFacet_facetName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TypedAttributeValue
    typedAttributeValue_stringValue,
    typedAttributeValue_booleanValue,
    typedAttributeValue_binaryValue,
    typedAttributeValue_numberValue,
    typedAttributeValue_datetimeValue,

    -- ** TypedAttributeValueRange
    typedAttributeValueRange_endValue,
    typedAttributeValueRange_startValue,
    typedAttributeValueRange_startMode,
    typedAttributeValueRange_endMode,

    -- ** TypedLinkAttributeDefinition
    typedLinkAttributeDefinition_isImmutable,
    typedLinkAttributeDefinition_rules,
    typedLinkAttributeDefinition_defaultValue,
    typedLinkAttributeDefinition_name,
    typedLinkAttributeDefinition_type,
    typedLinkAttributeDefinition_requiredBehavior,

    -- ** TypedLinkAttributeRange
    typedLinkAttributeRange_attributeName,
    typedLinkAttributeRange_range,

    -- ** TypedLinkFacet
    typedLinkFacet_name,
    typedLinkFacet_attributes,
    typedLinkFacet_identityAttributeOrder,

    -- ** TypedLinkFacetAttributeUpdate
    typedLinkFacetAttributeUpdate_attribute,
    typedLinkFacetAttributeUpdate_action,

    -- ** TypedLinkSchemaAndFacetName
    typedLinkSchemaAndFacetName_schemaArn,
    typedLinkSchemaAndFacetName_typedLinkName,

    -- ** TypedLinkSpecifier
    typedLinkSpecifier_typedLinkFacet,
    typedLinkSpecifier_sourceObjectReference,
    typedLinkSpecifier_targetObjectReference,
    typedLinkSpecifier_identityAttributeValues,
  )
where

import Network.AWS.CloudDirectory.AddFacetToObject
import Network.AWS.CloudDirectory.ApplySchema
import Network.AWS.CloudDirectory.AttachObject
import Network.AWS.CloudDirectory.AttachPolicy
import Network.AWS.CloudDirectory.AttachToIndex
import Network.AWS.CloudDirectory.AttachTypedLink
import Network.AWS.CloudDirectory.BatchRead
import Network.AWS.CloudDirectory.BatchWrite
import Network.AWS.CloudDirectory.CreateDirectory
import Network.AWS.CloudDirectory.CreateFacet
import Network.AWS.CloudDirectory.CreateIndex
import Network.AWS.CloudDirectory.CreateObject
import Network.AWS.CloudDirectory.CreateSchema
import Network.AWS.CloudDirectory.CreateTypedLinkFacet
import Network.AWS.CloudDirectory.DeleteDirectory
import Network.AWS.CloudDirectory.DeleteFacet
import Network.AWS.CloudDirectory.DeleteObject
import Network.AWS.CloudDirectory.DeleteSchema
import Network.AWS.CloudDirectory.DeleteTypedLinkFacet
import Network.AWS.CloudDirectory.DetachFromIndex
import Network.AWS.CloudDirectory.DetachObject
import Network.AWS.CloudDirectory.DetachPolicy
import Network.AWS.CloudDirectory.DetachTypedLink
import Network.AWS.CloudDirectory.DisableDirectory
import Network.AWS.CloudDirectory.EnableDirectory
import Network.AWS.CloudDirectory.GetAppliedSchemaVersion
import Network.AWS.CloudDirectory.GetDirectory
import Network.AWS.CloudDirectory.GetFacet
import Network.AWS.CloudDirectory.GetLinkAttributes
import Network.AWS.CloudDirectory.GetObjectAttributes
import Network.AWS.CloudDirectory.GetObjectInformation
import Network.AWS.CloudDirectory.GetSchemaAsJson
import Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
import Network.AWS.CloudDirectory.ListAppliedSchemaArns
import Network.AWS.CloudDirectory.ListAttachedIndices
import Network.AWS.CloudDirectory.ListDevelopmentSchemaArns
import Network.AWS.CloudDirectory.ListDirectories
import Network.AWS.CloudDirectory.ListFacetAttributes
import Network.AWS.CloudDirectory.ListFacetNames
import Network.AWS.CloudDirectory.ListIncomingTypedLinks
import Network.AWS.CloudDirectory.ListIndex
import Network.AWS.CloudDirectory.ListManagedSchemaArns
import Network.AWS.CloudDirectory.ListObjectAttributes
import Network.AWS.CloudDirectory.ListObjectChildren
import Network.AWS.CloudDirectory.ListObjectParentPaths
import Network.AWS.CloudDirectory.ListObjectParents
import Network.AWS.CloudDirectory.ListObjectPolicies
import Network.AWS.CloudDirectory.ListOutgoingTypedLinks
import Network.AWS.CloudDirectory.ListPolicyAttachments
import Network.AWS.CloudDirectory.ListPublishedSchemaArns
import Network.AWS.CloudDirectory.ListTagsForResource
import Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
import Network.AWS.CloudDirectory.ListTypedLinkFacetNames
import Network.AWS.CloudDirectory.LookupPolicy
import Network.AWS.CloudDirectory.PublishSchema
import Network.AWS.CloudDirectory.PutSchemaFromJson
import Network.AWS.CloudDirectory.RemoveFacetFromObject
import Network.AWS.CloudDirectory.TagResource
import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
import Network.AWS.CloudDirectory.Types.BatchAddFacetToObject
import Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse
import Network.AWS.CloudDirectory.Types.BatchAttachObject
import Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
import Network.AWS.CloudDirectory.Types.BatchAttachPolicy
import Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse
import Network.AWS.CloudDirectory.Types.BatchAttachToIndex
import Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
import Network.AWS.CloudDirectory.Types.BatchAttachTypedLink
import Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
import Network.AWS.CloudDirectory.Types.BatchCreateIndex
import Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
import Network.AWS.CloudDirectory.Types.BatchCreateObject
import Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
import Network.AWS.CloudDirectory.Types.BatchDeleteObject
import Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse
import Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
import Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
import Network.AWS.CloudDirectory.Types.BatchDetachObject
import Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
import Network.AWS.CloudDirectory.Types.BatchDetachPolicy
import Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse
import Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
import Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse
import Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes
import Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes
import Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchGetObjectInformation
import Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
import Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
import Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
import Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks
import Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
import Network.AWS.CloudDirectory.Types.BatchListIndex
import Network.AWS.CloudDirectory.Types.BatchListIndexResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectAttributes
import Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectChildren
import Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths
import Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectParents
import Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
import Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
import Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
import Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
import Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
import Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
import Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
import Network.AWS.CloudDirectory.Types.BatchLookupPolicy
import Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
import Network.AWS.CloudDirectory.Types.BatchReadException
import Network.AWS.CloudDirectory.Types.BatchReadOperation
import Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
import Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
import Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject
import Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
import Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes
import Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes
import Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
import Network.AWS.CloudDirectory.Types.BatchWriteOperation
import Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
import Network.AWS.CloudDirectory.Types.Directory
import Network.AWS.CloudDirectory.Types.Facet
import Network.AWS.CloudDirectory.Types.FacetAttribute
import Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
import Network.AWS.CloudDirectory.Types.FacetAttributeReference
import Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
import Network.AWS.CloudDirectory.Types.IndexAttachment
import Network.AWS.CloudDirectory.Types.LinkAttributeAction
import Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
import Network.AWS.CloudDirectory.Types.ObjectAttributeAction
import Network.AWS.CloudDirectory.Types.ObjectAttributeRange
import Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
import Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
import Network.AWS.CloudDirectory.Types.PolicyAttachment
import Network.AWS.CloudDirectory.Types.PolicyToPath
import Network.AWS.CloudDirectory.Types.Rule
import Network.AWS.CloudDirectory.Types.SchemaFacet
import Network.AWS.CloudDirectory.Types.Tag
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
import Network.AWS.CloudDirectory.Types.TypedLinkFacet
import Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import Network.AWS.CloudDirectory.UntagResource
import Network.AWS.CloudDirectory.UpdateFacet
import Network.AWS.CloudDirectory.UpdateLinkAttributes
import Network.AWS.CloudDirectory.UpdateObjectAttributes
import Network.AWS.CloudDirectory.UpdateSchema
import Network.AWS.CloudDirectory.UpdateTypedLinkFacet
import Network.AWS.CloudDirectory.UpgradeAppliedSchema
import Network.AWS.CloudDirectory.UpgradePublishedSchema

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _IncompatibleSchemaException,
    _InvalidAttachmentException,
    _FacetValidationException,
    _BatchWriteException,
    _FacetInUseException,
    _InvalidArnException,
    _DirectoryDeletedException,
    _InvalidRuleException,
    _InternalServiceException,
    _NotPolicyException,
    _InvalidNextTokenException,
    _InvalidTaggingRequestException,
    _LinkNameAlreadyInUseException,
    _CannotListParentOfRootException,
    _InvalidSchemaDocException,
    _AccessDeniedException,
    _ValidationException,
    _FacetAlreadyExistsException,
    _NotIndexException,
    _UnsupportedIndexTypeException,
    _LimitExceededException,
    _ObjectNotDetachedException,
    _StillContainsLinksException,
    _FacetNotFoundException,
    _DirectoryNotEnabledException,
    _ResourceNotFoundException,
    _DirectoryNotDisabledException,
    _RetryableConflictException,
    _DirectoryAlreadyExistsException,
    _IndexedAttributeMissingException,
    _SchemaAlreadyExistsException,
    _SchemaAlreadyPublishedException,
    _NotNodeException,
    _ObjectAlreadyDetachedException,
    _InvalidFacetUpdateException,

    -- * BatchReadExceptionType
    BatchReadExceptionType (..),

    -- * ConsistencyLevel
    ConsistencyLevel (..),

    -- * DirectoryState
    DirectoryState (..),

    -- * FacetAttributeType
    FacetAttributeType (..),

    -- * FacetStyle
    FacetStyle (..),

    -- * ObjectType
    ObjectType (..),

    -- * RangeMode
    RangeMode (..),

    -- * RequiredAttributeBehavior
    RequiredAttributeBehavior (..),

    -- * RuleType
    RuleType (..),

    -- * UpdateActionType
    UpdateActionType (..),

    -- * AttributeKey
    AttributeKey (..),
    newAttributeKey,
    attributeKey_schemaArn,
    attributeKey_facetName,
    attributeKey_name,

    -- * AttributeKeyAndValue
    AttributeKeyAndValue (..),
    newAttributeKeyAndValue,
    attributeKeyAndValue_key,
    attributeKeyAndValue_value,

    -- * AttributeNameAndValue
    AttributeNameAndValue (..),
    newAttributeNameAndValue,
    attributeNameAndValue_attributeName,
    attributeNameAndValue_value,

    -- * BatchAddFacetToObject
    BatchAddFacetToObject (..),
    newBatchAddFacetToObject,
    batchAddFacetToObject_schemaFacet,
    batchAddFacetToObject_objectAttributeList,
    batchAddFacetToObject_objectReference,

    -- * BatchAddFacetToObjectResponse
    BatchAddFacetToObjectResponse (..),
    newBatchAddFacetToObjectResponse,

    -- * BatchAttachObject
    BatchAttachObject (..),
    newBatchAttachObject,
    batchAttachObject_parentReference,
    batchAttachObject_childReference,
    batchAttachObject_linkName,

    -- * BatchAttachObjectResponse
    BatchAttachObjectResponse (..),
    newBatchAttachObjectResponse,
    batchAttachObjectResponse_attachedObjectIdentifier,

    -- * BatchAttachPolicy
    BatchAttachPolicy (..),
    newBatchAttachPolicy,
    batchAttachPolicy_policyReference,
    batchAttachPolicy_objectReference,

    -- * BatchAttachPolicyResponse
    BatchAttachPolicyResponse (..),
    newBatchAttachPolicyResponse,

    -- * BatchAttachToIndex
    BatchAttachToIndex (..),
    newBatchAttachToIndex,
    batchAttachToIndex_indexReference,
    batchAttachToIndex_targetReference,

    -- * BatchAttachToIndexResponse
    BatchAttachToIndexResponse (..),
    newBatchAttachToIndexResponse,
    batchAttachToIndexResponse_attachedObjectIdentifier,

    -- * BatchAttachTypedLink
    BatchAttachTypedLink (..),
    newBatchAttachTypedLink,
    batchAttachTypedLink_sourceObjectReference,
    batchAttachTypedLink_targetObjectReference,
    batchAttachTypedLink_typedLinkFacet,
    batchAttachTypedLink_attributes,

    -- * BatchAttachTypedLinkResponse
    BatchAttachTypedLinkResponse (..),
    newBatchAttachTypedLinkResponse,
    batchAttachTypedLinkResponse_typedLinkSpecifier,

    -- * BatchCreateIndex
    BatchCreateIndex (..),
    newBatchCreateIndex,
    batchCreateIndex_parentReference,
    batchCreateIndex_linkName,
    batchCreateIndex_batchReferenceName,
    batchCreateIndex_orderedIndexedAttributeList,
    batchCreateIndex_isUnique,

    -- * BatchCreateIndexResponse
    BatchCreateIndexResponse (..),
    newBatchCreateIndexResponse,
    batchCreateIndexResponse_objectIdentifier,

    -- * BatchCreateObject
    BatchCreateObject (..),
    newBatchCreateObject,
    batchCreateObject_parentReference,
    batchCreateObject_linkName,
    batchCreateObject_batchReferenceName,
    batchCreateObject_schemaFacet,
    batchCreateObject_objectAttributeList,

    -- * BatchCreateObjectResponse
    BatchCreateObjectResponse (..),
    newBatchCreateObjectResponse,
    batchCreateObjectResponse_objectIdentifier,

    -- * BatchDeleteObject
    BatchDeleteObject (..),
    newBatchDeleteObject,
    batchDeleteObject_objectReference,

    -- * BatchDeleteObjectResponse
    BatchDeleteObjectResponse (..),
    newBatchDeleteObjectResponse,

    -- * BatchDetachFromIndex
    BatchDetachFromIndex (..),
    newBatchDetachFromIndex,
    batchDetachFromIndex_indexReference,
    batchDetachFromIndex_targetReference,

    -- * BatchDetachFromIndexResponse
    BatchDetachFromIndexResponse (..),
    newBatchDetachFromIndexResponse,
    batchDetachFromIndexResponse_detachedObjectIdentifier,

    -- * BatchDetachObject
    BatchDetachObject (..),
    newBatchDetachObject,
    batchDetachObject_batchReferenceName,
    batchDetachObject_parentReference,
    batchDetachObject_linkName,

    -- * BatchDetachObjectResponse
    BatchDetachObjectResponse (..),
    newBatchDetachObjectResponse,
    batchDetachObjectResponse_detachedObjectIdentifier,

    -- * BatchDetachPolicy
    BatchDetachPolicy (..),
    newBatchDetachPolicy,
    batchDetachPolicy_policyReference,
    batchDetachPolicy_objectReference,

    -- * BatchDetachPolicyResponse
    BatchDetachPolicyResponse (..),
    newBatchDetachPolicyResponse,

    -- * BatchDetachTypedLink
    BatchDetachTypedLink (..),
    newBatchDetachTypedLink,
    batchDetachTypedLink_typedLinkSpecifier,

    -- * BatchDetachTypedLinkResponse
    BatchDetachTypedLinkResponse (..),
    newBatchDetachTypedLinkResponse,

    -- * BatchGetLinkAttributes
    BatchGetLinkAttributes (..),
    newBatchGetLinkAttributes,
    batchGetLinkAttributes_typedLinkSpecifier,
    batchGetLinkAttributes_attributeNames,

    -- * BatchGetLinkAttributesResponse
    BatchGetLinkAttributesResponse (..),
    newBatchGetLinkAttributesResponse,
    batchGetLinkAttributesResponse_attributes,

    -- * BatchGetObjectAttributes
    BatchGetObjectAttributes (..),
    newBatchGetObjectAttributes,
    batchGetObjectAttributes_objectReference,
    batchGetObjectAttributes_schemaFacet,
    batchGetObjectAttributes_attributeNames,

    -- * BatchGetObjectAttributesResponse
    BatchGetObjectAttributesResponse (..),
    newBatchGetObjectAttributesResponse,
    batchGetObjectAttributesResponse_attributes,

    -- * BatchGetObjectInformation
    BatchGetObjectInformation (..),
    newBatchGetObjectInformation,
    batchGetObjectInformation_objectReference,

    -- * BatchGetObjectInformationResponse
    BatchGetObjectInformationResponse (..),
    newBatchGetObjectInformationResponse,
    batchGetObjectInformationResponse_schemaFacets,
    batchGetObjectInformationResponse_objectIdentifier,

    -- * BatchListAttachedIndices
    BatchListAttachedIndices (..),
    newBatchListAttachedIndices,
    batchListAttachedIndices_nextToken,
    batchListAttachedIndices_maxResults,
    batchListAttachedIndices_targetReference,

    -- * BatchListAttachedIndicesResponse
    BatchListAttachedIndicesResponse (..),
    newBatchListAttachedIndicesResponse,
    batchListAttachedIndicesResponse_nextToken,
    batchListAttachedIndicesResponse_indexAttachments,

    -- * BatchListIncomingTypedLinks
    BatchListIncomingTypedLinks (..),
    newBatchListIncomingTypedLinks,
    batchListIncomingTypedLinks_nextToken,
    batchListIncomingTypedLinks_filterTypedLink,
    batchListIncomingTypedLinks_maxResults,
    batchListIncomingTypedLinks_filterAttributeRanges,
    batchListIncomingTypedLinks_objectReference,

    -- * BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse (..),
    newBatchListIncomingTypedLinksResponse,
    batchListIncomingTypedLinksResponse_linkSpecifiers,
    batchListIncomingTypedLinksResponse_nextToken,

    -- * BatchListIndex
    BatchListIndex (..),
    newBatchListIndex,
    batchListIndex_nextToken,
    batchListIndex_maxResults,
    batchListIndex_rangesOnIndexedValues,
    batchListIndex_indexReference,

    -- * BatchListIndexResponse
    BatchListIndexResponse (..),
    newBatchListIndexResponse,
    batchListIndexResponse_nextToken,
    batchListIndexResponse_indexAttachments,

    -- * BatchListObjectAttributes
    BatchListObjectAttributes (..),
    newBatchListObjectAttributes,
    batchListObjectAttributes_nextToken,
    batchListObjectAttributes_maxResults,
    batchListObjectAttributes_facetFilter,
    batchListObjectAttributes_objectReference,

    -- * BatchListObjectAttributesResponse
    BatchListObjectAttributesResponse (..),
    newBatchListObjectAttributesResponse,
    batchListObjectAttributesResponse_nextToken,
    batchListObjectAttributesResponse_attributes,

    -- * BatchListObjectChildren
    BatchListObjectChildren (..),
    newBatchListObjectChildren,
    batchListObjectChildren_nextToken,
    batchListObjectChildren_maxResults,
    batchListObjectChildren_objectReference,

    -- * BatchListObjectChildrenResponse
    BatchListObjectChildrenResponse (..),
    newBatchListObjectChildrenResponse,
    batchListObjectChildrenResponse_nextToken,
    batchListObjectChildrenResponse_children,

    -- * BatchListObjectParentPaths
    BatchListObjectParentPaths (..),
    newBatchListObjectParentPaths,
    batchListObjectParentPaths_nextToken,
    batchListObjectParentPaths_maxResults,
    batchListObjectParentPaths_objectReference,

    -- * BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse (..),
    newBatchListObjectParentPathsResponse,
    batchListObjectParentPathsResponse_nextToken,
    batchListObjectParentPathsResponse_pathToObjectIdentifiersList,

    -- * BatchListObjectParents
    BatchListObjectParents (..),
    newBatchListObjectParents,
    batchListObjectParents_nextToken,
    batchListObjectParents_maxResults,
    batchListObjectParents_objectReference,

    -- * BatchListObjectParentsResponse
    BatchListObjectParentsResponse (..),
    newBatchListObjectParentsResponse,
    batchListObjectParentsResponse_parentLinks,
    batchListObjectParentsResponse_nextToken,

    -- * BatchListObjectPolicies
    BatchListObjectPolicies (..),
    newBatchListObjectPolicies,
    batchListObjectPolicies_nextToken,
    batchListObjectPolicies_maxResults,
    batchListObjectPolicies_objectReference,

    -- * BatchListObjectPoliciesResponse
    BatchListObjectPoliciesResponse (..),
    newBatchListObjectPoliciesResponse,
    batchListObjectPoliciesResponse_nextToken,
    batchListObjectPoliciesResponse_attachedPolicyIds,

    -- * BatchListOutgoingTypedLinks
    BatchListOutgoingTypedLinks (..),
    newBatchListOutgoingTypedLinks,
    batchListOutgoingTypedLinks_nextToken,
    batchListOutgoingTypedLinks_filterTypedLink,
    batchListOutgoingTypedLinks_maxResults,
    batchListOutgoingTypedLinks_filterAttributeRanges,
    batchListOutgoingTypedLinks_objectReference,

    -- * BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse (..),
    newBatchListOutgoingTypedLinksResponse,
    batchListOutgoingTypedLinksResponse_nextToken,
    batchListOutgoingTypedLinksResponse_typedLinkSpecifiers,

    -- * BatchListPolicyAttachments
    BatchListPolicyAttachments (..),
    newBatchListPolicyAttachments,
    batchListPolicyAttachments_nextToken,
    batchListPolicyAttachments_maxResults,
    batchListPolicyAttachments_policyReference,

    -- * BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse (..),
    newBatchListPolicyAttachmentsResponse,
    batchListPolicyAttachmentsResponse_nextToken,
    batchListPolicyAttachmentsResponse_objectIdentifiers,

    -- * BatchLookupPolicy
    BatchLookupPolicy (..),
    newBatchLookupPolicy,
    batchLookupPolicy_nextToken,
    batchLookupPolicy_maxResults,
    batchLookupPolicy_objectReference,

    -- * BatchLookupPolicyResponse
    BatchLookupPolicyResponse (..),
    newBatchLookupPolicyResponse,
    batchLookupPolicyResponse_nextToken,
    batchLookupPolicyResponse_policyToPathList,

    -- * BatchReadException
    BatchReadException (..),
    newBatchReadException,
    batchReadException_message,
    batchReadException_type,

    -- * BatchReadOperation
    BatchReadOperation (..),
    newBatchReadOperation,
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

    -- * BatchReadOperationResponse
    BatchReadOperationResponse (..),
    newBatchReadOperationResponse,
    batchReadOperationResponse_successfulResponse,
    batchReadOperationResponse_exceptionResponse,

    -- * BatchReadSuccessfulResponse
    BatchReadSuccessfulResponse (..),
    newBatchReadSuccessfulResponse,
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

    -- * BatchRemoveFacetFromObject
    BatchRemoveFacetFromObject (..),
    newBatchRemoveFacetFromObject,
    batchRemoveFacetFromObject_schemaFacet,
    batchRemoveFacetFromObject_objectReference,

    -- * BatchRemoveFacetFromObjectResponse
    BatchRemoveFacetFromObjectResponse (..),
    newBatchRemoveFacetFromObjectResponse,

    -- * BatchUpdateLinkAttributes
    BatchUpdateLinkAttributes (..),
    newBatchUpdateLinkAttributes,
    batchUpdateLinkAttributes_typedLinkSpecifier,
    batchUpdateLinkAttributes_attributeUpdates,

    -- * BatchUpdateLinkAttributesResponse
    BatchUpdateLinkAttributesResponse (..),
    newBatchUpdateLinkAttributesResponse,

    -- * BatchUpdateObjectAttributes
    BatchUpdateObjectAttributes (..),
    newBatchUpdateObjectAttributes,
    batchUpdateObjectAttributes_objectReference,
    batchUpdateObjectAttributes_attributeUpdates,

    -- * BatchUpdateObjectAttributesResponse
    BatchUpdateObjectAttributesResponse (..),
    newBatchUpdateObjectAttributesResponse,
    batchUpdateObjectAttributesResponse_objectIdentifier,

    -- * BatchWriteOperation
    BatchWriteOperation (..),
    newBatchWriteOperation,
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

    -- * BatchWriteOperationResponse
    BatchWriteOperationResponse (..),
    newBatchWriteOperationResponse,
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

    -- * Directory
    Directory (..),
    newDirectory,
    directory_directoryArn,
    directory_state,
    directory_name,
    directory_creationDateTime,

    -- * Facet
    Facet (..),
    newFacet,
    facet_facetStyle,
    facet_name,
    facet_objectType,

    -- * FacetAttribute
    FacetAttribute (..),
    newFacetAttribute,
    facetAttribute_attributeReference,
    facetAttribute_requiredBehavior,
    facetAttribute_attributeDefinition,
    facetAttribute_name,

    -- * FacetAttributeDefinition
    FacetAttributeDefinition (..),
    newFacetAttributeDefinition,
    facetAttributeDefinition_isImmutable,
    facetAttributeDefinition_rules,
    facetAttributeDefinition_defaultValue,
    facetAttributeDefinition_type,

    -- * FacetAttributeReference
    FacetAttributeReference (..),
    newFacetAttributeReference,
    facetAttributeReference_targetFacetName,
    facetAttributeReference_targetAttributeName,

    -- * FacetAttributeUpdate
    FacetAttributeUpdate (..),
    newFacetAttributeUpdate,
    facetAttributeUpdate_attribute,
    facetAttributeUpdate_action,

    -- * IndexAttachment
    IndexAttachment (..),
    newIndexAttachment,
    indexAttachment_objectIdentifier,
    indexAttachment_indexedAttributes,

    -- * LinkAttributeAction
    LinkAttributeAction (..),
    newLinkAttributeAction,
    linkAttributeAction_attributeUpdateValue,
    linkAttributeAction_attributeActionType,

    -- * LinkAttributeUpdate
    LinkAttributeUpdate (..),
    newLinkAttributeUpdate,
    linkAttributeUpdate_attributeAction,
    linkAttributeUpdate_attributeKey,

    -- * ObjectAttributeAction
    ObjectAttributeAction (..),
    newObjectAttributeAction,
    objectAttributeAction_objectAttributeActionType,
    objectAttributeAction_objectAttributeUpdateValue,

    -- * ObjectAttributeRange
    ObjectAttributeRange (..),
    newObjectAttributeRange,
    objectAttributeRange_range,
    objectAttributeRange_attributeKey,

    -- * ObjectAttributeUpdate
    ObjectAttributeUpdate (..),
    newObjectAttributeUpdate,
    objectAttributeUpdate_objectAttributeAction,
    objectAttributeUpdate_objectAttributeKey,

    -- * ObjectIdentifierAndLinkNameTuple
    ObjectIdentifierAndLinkNameTuple (..),
    newObjectIdentifierAndLinkNameTuple,
    objectIdentifierAndLinkNameTuple_linkName,
    objectIdentifierAndLinkNameTuple_objectIdentifier,

    -- * ObjectReference
    ObjectReference (..),
    newObjectReference,
    objectReference_selector,

    -- * PathToObjectIdentifiers
    PathToObjectIdentifiers (..),
    newPathToObjectIdentifiers,
    pathToObjectIdentifiers_objectIdentifiers,
    pathToObjectIdentifiers_path,

    -- * PolicyAttachment
    PolicyAttachment (..),
    newPolicyAttachment,
    policyAttachment_policyType,
    policyAttachment_objectIdentifier,
    policyAttachment_policyId,

    -- * PolicyToPath
    PolicyToPath (..),
    newPolicyToPath,
    policyToPath_policies,
    policyToPath_path,

    -- * Rule
    Rule (..),
    newRule,
    rule_type,
    rule_parameters,

    -- * SchemaFacet
    SchemaFacet (..),
    newSchemaFacet,
    schemaFacet_schemaArn,
    schemaFacet_facetName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TypedAttributeValue
    TypedAttributeValue (..),
    newTypedAttributeValue,
    typedAttributeValue_stringValue,
    typedAttributeValue_booleanValue,
    typedAttributeValue_binaryValue,
    typedAttributeValue_numberValue,
    typedAttributeValue_datetimeValue,

    -- * TypedAttributeValueRange
    TypedAttributeValueRange (..),
    newTypedAttributeValueRange,
    typedAttributeValueRange_endValue,
    typedAttributeValueRange_startValue,
    typedAttributeValueRange_startMode,
    typedAttributeValueRange_endMode,

    -- * TypedLinkAttributeDefinition
    TypedLinkAttributeDefinition (..),
    newTypedLinkAttributeDefinition,
    typedLinkAttributeDefinition_isImmutable,
    typedLinkAttributeDefinition_rules,
    typedLinkAttributeDefinition_defaultValue,
    typedLinkAttributeDefinition_name,
    typedLinkAttributeDefinition_type,
    typedLinkAttributeDefinition_requiredBehavior,

    -- * TypedLinkAttributeRange
    TypedLinkAttributeRange (..),
    newTypedLinkAttributeRange,
    typedLinkAttributeRange_attributeName,
    typedLinkAttributeRange_range,

    -- * TypedLinkFacet
    TypedLinkFacet (..),
    newTypedLinkFacet,
    typedLinkFacet_name,
    typedLinkFacet_attributes,
    typedLinkFacet_identityAttributeOrder,

    -- * TypedLinkFacetAttributeUpdate
    TypedLinkFacetAttributeUpdate (..),
    newTypedLinkFacetAttributeUpdate,
    typedLinkFacetAttributeUpdate_attribute,
    typedLinkFacetAttributeUpdate_action,

    -- * TypedLinkSchemaAndFacetName
    TypedLinkSchemaAndFacetName (..),
    newTypedLinkSchemaAndFacetName,
    typedLinkSchemaAndFacetName_schemaArn,
    typedLinkSchemaAndFacetName_typedLinkName,

    -- * TypedLinkSpecifier
    TypedLinkSpecifier (..),
    newTypedLinkSpecifier,
    typedLinkSpecifier_typedLinkFacet,
    typedLinkSpecifier_sourceObjectReference,
    typedLinkSpecifier_targetObjectReference,
    typedLinkSpecifier_identityAttributeValues,
  )
where

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
import Network.AWS.CloudDirectory.Types.BatchReadExceptionType
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
import Network.AWS.CloudDirectory.Types.ConsistencyLevel
import Network.AWS.CloudDirectory.Types.Directory
import Network.AWS.CloudDirectory.Types.DirectoryState
import Network.AWS.CloudDirectory.Types.Facet
import Network.AWS.CloudDirectory.Types.FacetAttribute
import Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
import Network.AWS.CloudDirectory.Types.FacetAttributeReference
import Network.AWS.CloudDirectory.Types.FacetAttributeType
import Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
import Network.AWS.CloudDirectory.Types.FacetStyle
import Network.AWS.CloudDirectory.Types.IndexAttachment
import Network.AWS.CloudDirectory.Types.LinkAttributeAction
import Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
import Network.AWS.CloudDirectory.Types.ObjectAttributeAction
import Network.AWS.CloudDirectory.Types.ObjectAttributeRange
import Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
import Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.ObjectType
import Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
import Network.AWS.CloudDirectory.Types.PolicyAttachment
import Network.AWS.CloudDirectory.Types.PolicyToPath
import Network.AWS.CloudDirectory.Types.RangeMode
import Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
import Network.AWS.CloudDirectory.Types.Rule
import Network.AWS.CloudDirectory.Types.RuleType
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
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-11@ of the Amazon CloudDirectory SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "CloudDirectory",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "clouddirectory",
      Prelude._svcVersion = "2017-01-11",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CloudDirectory",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates a failure occurred while performing a check for backward
-- compatibility between the specified schema and the schema that is
-- currently applied to the directory.
_IncompatibleSchemaException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IncompatibleSchemaException =
  Prelude._MatchServiceError
    defaultService
    "IncompatibleSchemaException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that an attempt to make an attachment was invalid. For
-- example, attaching two nodes with a link type that is not applicable to
-- the nodes or attempting to apply a schema to a directory a second time.
_InvalidAttachmentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAttachmentException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAttachmentException"
    Prelude.. Prelude.hasStatus 400

-- | The Facet that you provided was not well formed or could not be
-- validated with the schema.
_FacetValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FacetValidationException =
  Prelude._MatchServiceError
    defaultService
    "FacetValidationException"
    Prelude.. Prelude.hasStatus 400

-- | A @BatchWrite@ exception has occurred.
_BatchWriteException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BatchWriteException =
  Prelude._MatchServiceError
    defaultService
    "BatchWriteException"

-- | Occurs when deleting a facet that contains an attribute that is a target
-- to an attribute reference in a different facet.
_FacetInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FacetInUseException =
  Prelude._MatchServiceError
    defaultService
    "FacetInUseException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided ARN value is not valid.
_InvalidArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidArnException"
    Prelude.. Prelude.hasStatus 400

-- | A directory that has been deleted and to which access has been
-- attempted. Note: The requested resource will eventually cease to exist.
_DirectoryDeletedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryDeletedException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryDeletedException"
    Prelude.. Prelude.hasStatus 400

-- | Occurs when any of the rule parameter keys or values are invalid.
_InvalidRuleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRuleException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRuleException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates a problem that must be resolved by Amazon Web Services. This
-- might be a transient error in which case you can retry your request
-- until it succeeds. Otherwise, go to the
-- <http://status.aws.amazon.com/ AWS Service Health Dashboard> site to see
-- if there are any operational issues with the service.
_InternalServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServiceException =
  Prelude._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Prelude.hasStatus 500

-- | Indicates that the requested operation can only operate on policy
-- objects.
_NotPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotPolicyException =
  Prelude._MatchServiceError
    defaultService
    "NotPolicyException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the @NextToken@ value is not valid.
_InvalidNextTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Prelude.hasStatus 400

-- | Can occur for multiple reasons such as when you tag a resource that
-- doesn’t exist or if you specify a higher number of tags for a resource
-- than the allowed limit. Allowed limit is 50 tags per resource.
_InvalidTaggingRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTaggingRequestException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTaggingRequestException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a link could not be created due to a naming conflict.
-- Choose a different name and then try again.
_LinkNameAlreadyInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LinkNameAlreadyInUseException =
  Prelude._MatchServiceError
    defaultService
    "LinkNameAlreadyInUseException"
    Prelude.. Prelude.hasStatus 400

-- | Cannot list the parents of a Directory root.
_CannotListParentOfRootException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CannotListParentOfRootException =
  Prelude._MatchServiceError
    defaultService
    "CannotListParentOfRootException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the provided @SchemaDoc@ value is not valid.
_InvalidSchemaDocException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSchemaDocException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSchemaDocException"
    Prelude.. Prelude.hasStatus 400

-- | Access denied. Check your permissions.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Prelude.hasStatus 403

-- | Indicates that your request is malformed in some manner. See the
-- exception message.
_ValidationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ValidationException =
  Prelude._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Prelude.hasStatus 400

-- | A facet with the same name already exists.
_FacetAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FacetAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "FacetAlreadyExistsException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the requested operation can only operate on index
-- objects.
_NotIndexException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotIndexException =
  Prelude._MatchServiceError
    defaultService
    "NotIndexException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the requested index type is not supported.
_UnsupportedIndexTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnsupportedIndexTypeException =
  Prelude._MatchServiceError
    defaultService
    "UnsupportedIndexTypeException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that limits are exceeded. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Limits>
-- for more information.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the requested operation cannot be completed because the
-- object has not been detached from the tree.
_ObjectNotDetachedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ObjectNotDetachedException =
  Prelude._MatchServiceError
    defaultService
    "ObjectNotDetachedException"
    Prelude.. Prelude.hasStatus 400

-- | The object could not be deleted because links still exist. Remove the
-- links and then try the operation again.
_StillContainsLinksException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_StillContainsLinksException =
  Prelude._MatchServiceError
    defaultService
    "StillContainsLinksException"
    Prelude.. Prelude.hasStatus 400

-- | The specified Facet could not be found.
_FacetNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FacetNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "FacetNotFoundException"
    Prelude.. Prelude.hasStatus 400

-- | Operations are only permitted on enabled directories.
_DirectoryNotEnabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryNotEnabledException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryNotEnabledException"
    Prelude.. Prelude.hasStatus 400

-- | The specified resource could not be found.
_ResourceNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | An operation can only operate on a disabled directory.
_DirectoryNotDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryNotDisabledException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryNotDisabledException"
    Prelude.. Prelude.hasStatus 400

-- | Occurs when a conflict with a previous successful write is detected. For
-- example, if a write operation occurs on an object and then an attempt is
-- made to read the object using “SERIALIZABLE” consistency, this exception
-- may result. This generally occurs when the previous write did not have
-- time to propagate to the host serving the current request. A retry (with
-- appropriate backoff logic) is the recommended response to this
-- exception.
_RetryableConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RetryableConflictException =
  Prelude._MatchServiceError
    defaultService
    "RetryableConflictException"
    Prelude.. Prelude.hasStatus 409

-- | Indicates that a Directory could not be created due to a naming
-- conflict. Choose a different name and try again.
_DirectoryAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryAlreadyExistsException"
    Prelude.. Prelude.hasStatus 400

-- | An object has been attempted to be attached to an object that does not
-- have the appropriate attribute value.
_IndexedAttributeMissingException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IndexedAttributeMissingException =
  Prelude._MatchServiceError
    defaultService
    "IndexedAttributeMissingException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a schema could not be created due to a naming conflict.
-- Please select a different name and then try again.
_SchemaAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SchemaAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "SchemaAlreadyExistsException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that a schema is already published.
_SchemaAlreadyPublishedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SchemaAlreadyPublishedException =
  Prelude._MatchServiceError
    defaultService
    "SchemaAlreadyPublishedException"
    Prelude.. Prelude.hasStatus 400

-- | Occurs when any invalid operations are performed on an object that is
-- not a node, such as calling @ListObjectChildren@ for a leaf node object.
_NotNodeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotNodeException =
  Prelude._MatchServiceError
    defaultService
    "NotNodeException"
    Prelude.. Prelude.hasStatus 400

-- | Indicates that the object is not attached to the index.
_ObjectAlreadyDetachedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ObjectAlreadyDetachedException =
  Prelude._MatchServiceError
    defaultService
    "ObjectAlreadyDetachedException"
    Prelude.. Prelude.hasStatus 400

-- | An attempt to modify a Facet resulted in an invalid schema exception.
_InvalidFacetUpdateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFacetUpdateException =
  Prelude._MatchServiceError
    defaultService
    "InvalidFacetUpdateException"
    Prelude.. Prelude.hasStatus 400

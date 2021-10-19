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
    _UnsupportedIndexTypeException,
    _NotIndexException,
    _ValidationException,
    _AccessDeniedException,
    _FacetAlreadyExistsException,
    _InvalidSchemaDocException,
    _InvalidAttachmentException,
    _CannotListParentOfRootException,
    _NotPolicyException,
    _InvalidTaggingRequestException,
    _InvalidFacetUpdateException,
    _InvalidRuleException,
    _SchemaAlreadyPublishedException,
    _DirectoryAlreadyExistsException,
    _DirectoryNotDisabledException,
    _BatchWriteException,
    _DirectoryNotEnabledException,
    _FacetInUseException,
    _FacetValidationException,
    _StillContainsLinksException,
    _IncompatibleSchemaException,
    _NotNodeException,
    _InvalidNextTokenException,
    _ObjectAlreadyDetachedException,
    _LinkNameAlreadyInUseException,
    _InternalServiceException,
    _SchemaAlreadyExistsException,
    _IndexedAttributeMissingException,
    _DirectoryDeletedException,
    _RetryableConflictException,
    _InvalidArnException,
    _ResourceNotFoundException,
    _FacetNotFoundException,
    _LimitExceededException,
    _ObjectNotDetachedException,

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
    batchGetObjectInformationResponse_objectIdentifier,
    batchGetObjectInformationResponse_schemaFacets,

    -- * BatchListAttachedIndices
    BatchListAttachedIndices (..),
    newBatchListAttachedIndices,
    batchListAttachedIndices_nextToken,
    batchListAttachedIndices_maxResults,
    batchListAttachedIndices_targetReference,

    -- * BatchListAttachedIndicesResponse
    BatchListAttachedIndicesResponse (..),
    newBatchListAttachedIndicesResponse,
    batchListAttachedIndicesResponse_indexAttachments,
    batchListAttachedIndicesResponse_nextToken,

    -- * BatchListIncomingTypedLinks
    BatchListIncomingTypedLinks (..),
    newBatchListIncomingTypedLinks,
    batchListIncomingTypedLinks_filterAttributeRanges,
    batchListIncomingTypedLinks_nextToken,
    batchListIncomingTypedLinks_filterTypedLink,
    batchListIncomingTypedLinks_maxResults,
    batchListIncomingTypedLinks_objectReference,

    -- * BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse (..),
    newBatchListIncomingTypedLinksResponse,
    batchListIncomingTypedLinksResponse_linkSpecifiers,
    batchListIncomingTypedLinksResponse_nextToken,

    -- * BatchListIndex
    BatchListIndex (..),
    newBatchListIndex,
    batchListIndex_rangesOnIndexedValues,
    batchListIndex_nextToken,
    batchListIndex_maxResults,
    batchListIndex_indexReference,

    -- * BatchListIndexResponse
    BatchListIndexResponse (..),
    newBatchListIndexResponse,
    batchListIndexResponse_indexAttachments,
    batchListIndexResponse_nextToken,

    -- * BatchListObjectAttributes
    BatchListObjectAttributes (..),
    newBatchListObjectAttributes,
    batchListObjectAttributes_facetFilter,
    batchListObjectAttributes_nextToken,
    batchListObjectAttributes_maxResults,
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
    batchListObjectChildrenResponse_children,
    batchListObjectChildrenResponse_nextToken,

    -- * BatchListObjectParentPaths
    BatchListObjectParentPaths (..),
    newBatchListObjectParentPaths,
    batchListObjectParentPaths_nextToken,
    batchListObjectParentPaths_maxResults,
    batchListObjectParentPaths_objectReference,

    -- * BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse (..),
    newBatchListObjectParentPathsResponse,
    batchListObjectParentPathsResponse_pathToObjectIdentifiersList,
    batchListObjectParentPathsResponse_nextToken,

    -- * BatchListObjectParents
    BatchListObjectParents (..),
    newBatchListObjectParents,
    batchListObjectParents_nextToken,
    batchListObjectParents_maxResults,
    batchListObjectParents_objectReference,

    -- * BatchListObjectParentsResponse
    BatchListObjectParentsResponse (..),
    newBatchListObjectParentsResponse,
    batchListObjectParentsResponse_nextToken,
    batchListObjectParentsResponse_parentLinks,

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
    batchListOutgoingTypedLinks_filterAttributeRanges,
    batchListOutgoingTypedLinks_nextToken,
    batchListOutgoingTypedLinks_filterTypedLink,
    batchListOutgoingTypedLinks_maxResults,
    batchListOutgoingTypedLinks_objectReference,

    -- * BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse (..),
    newBatchListOutgoingTypedLinksResponse,
    batchListOutgoingTypedLinksResponse_typedLinkSpecifiers,
    batchListOutgoingTypedLinksResponse_nextToken,

    -- * BatchListPolicyAttachments
    BatchListPolicyAttachments (..),
    newBatchListPolicyAttachments,
    batchListPolicyAttachments_nextToken,
    batchListPolicyAttachments_maxResults,
    batchListPolicyAttachments_policyReference,

    -- * BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse (..),
    newBatchListPolicyAttachmentsResponse,
    batchListPolicyAttachmentsResponse_objectIdentifiers,
    batchListPolicyAttachmentsResponse_nextToken,

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
    batchReadException_type,
    batchReadException_message,

    -- * BatchReadOperation
    BatchReadOperation (..),
    newBatchReadOperation,
    batchReadOperation_listIndex,
    batchReadOperation_getObjectInformation,
    batchReadOperation_listAttachedIndices,
    batchReadOperation_lookupPolicy,
    batchReadOperation_listObjectParentPaths,
    batchReadOperation_listObjectAttributes,
    batchReadOperation_listIncomingTypedLinks,
    batchReadOperation_getLinkAttributes,
    batchReadOperation_getObjectAttributes,
    batchReadOperation_listObjectChildren,
    batchReadOperation_listObjectParents,
    batchReadOperation_listPolicyAttachments,
    batchReadOperation_listOutgoingTypedLinks,
    batchReadOperation_listObjectPolicies,

    -- * BatchReadOperationResponse
    BatchReadOperationResponse (..),
    newBatchReadOperationResponse,
    batchReadOperationResponse_exceptionResponse,
    batchReadOperationResponse_successfulResponse,

    -- * BatchReadSuccessfulResponse
    BatchReadSuccessfulResponse (..),
    newBatchReadSuccessfulResponse,
    batchReadSuccessfulResponse_listIndex,
    batchReadSuccessfulResponse_getObjectInformation,
    batchReadSuccessfulResponse_listAttachedIndices,
    batchReadSuccessfulResponse_lookupPolicy,
    batchReadSuccessfulResponse_listObjectParentPaths,
    batchReadSuccessfulResponse_listObjectAttributes,
    batchReadSuccessfulResponse_listIncomingTypedLinks,
    batchReadSuccessfulResponse_getLinkAttributes,
    batchReadSuccessfulResponse_getObjectAttributes,
    batchReadSuccessfulResponse_listObjectChildren,
    batchReadSuccessfulResponse_listObjectParents,
    batchReadSuccessfulResponse_listPolicyAttachments,
    batchReadSuccessfulResponse_listOutgoingTypedLinks,
    batchReadSuccessfulResponse_listObjectPolicies,

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
    batchWriteOperation_deleteObject,
    batchWriteOperation_detachFromIndex,
    batchWriteOperation_removeFacetFromObject,
    batchWriteOperation_attachObject,
    batchWriteOperation_createObject,
    batchWriteOperation_attachTypedLink,
    batchWriteOperation_detachPolicy,
    batchWriteOperation_createIndex,
    batchWriteOperation_detachObject,
    batchWriteOperation_addFacetToObject,
    batchWriteOperation_detachTypedLink,
    batchWriteOperation_updateObjectAttributes,
    batchWriteOperation_attachPolicy,
    batchWriteOperation_updateLinkAttributes,
    batchWriteOperation_attachToIndex,

    -- * BatchWriteOperationResponse
    BatchWriteOperationResponse (..),
    newBatchWriteOperationResponse,
    batchWriteOperationResponse_deleteObject,
    batchWriteOperationResponse_detachFromIndex,
    batchWriteOperationResponse_removeFacetFromObject,
    batchWriteOperationResponse_attachObject,
    batchWriteOperationResponse_createObject,
    batchWriteOperationResponse_attachTypedLink,
    batchWriteOperationResponse_detachPolicy,
    batchWriteOperationResponse_createIndex,
    batchWriteOperationResponse_detachObject,
    batchWriteOperationResponse_addFacetToObject,
    batchWriteOperationResponse_detachTypedLink,
    batchWriteOperationResponse_updateObjectAttributes,
    batchWriteOperationResponse_attachPolicy,
    batchWriteOperationResponse_updateLinkAttributes,
    batchWriteOperationResponse_attachToIndex,

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
    facet_objectType,
    facet_name,

    -- * FacetAttribute
    FacetAttribute (..),
    newFacetAttribute,
    facetAttribute_attributeReference,
    facetAttribute_attributeDefinition,
    facetAttribute_requiredBehavior,
    facetAttribute_name,

    -- * FacetAttributeDefinition
    FacetAttributeDefinition (..),
    newFacetAttributeDefinition,
    facetAttributeDefinition_rules,
    facetAttributeDefinition_defaultValue,
    facetAttributeDefinition_isImmutable,
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
    indexAttachment_indexedAttributes,
    indexAttachment_objectIdentifier,

    -- * LinkAttributeAction
    LinkAttributeAction (..),
    newLinkAttributeAction,
    linkAttributeAction_attributeActionType,
    linkAttributeAction_attributeUpdateValue,

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
    objectIdentifierAndLinkNameTuple_objectIdentifier,
    objectIdentifierAndLinkNameTuple_linkName,

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
    policyAttachment_policyId,
    policyAttachment_policyType,
    policyAttachment_objectIdentifier,

    -- * PolicyToPath
    PolicyToPath (..),
    newPolicyToPath,
    policyToPath_path,
    policyToPath_policies,

    -- * Rule
    Rule (..),
    newRule,
    rule_parameters,
    rule_type,

    -- * SchemaFacet
    SchemaFacet (..),
    newSchemaFacet,
    schemaFacet_facetName,
    schemaFacet_schemaArn,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TypedAttributeValue
    TypedAttributeValue (..),
    newTypedAttributeValue,
    typedAttributeValue_binaryValue,
    typedAttributeValue_datetimeValue,
    typedAttributeValue_numberValue,
    typedAttributeValue_stringValue,
    typedAttributeValue_booleanValue,

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
    typedLinkAttributeDefinition_rules,
    typedLinkAttributeDefinition_defaultValue,
    typedLinkAttributeDefinition_isImmutable,
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-11@ of the Amazon CloudDirectory SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "CloudDirectory",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "clouddirectory",
      Core._serviceSigningName = "clouddirectory",
      Core._serviceVersion = "2017-01-11",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CloudDirectory",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Indicates that the requested index type is not supported.
_UnsupportedIndexTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedIndexTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedIndexTypeException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested operation can only operate on index
-- objects.
_NotIndexException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotIndexException =
  Core._MatchServiceError
    defaultService
    "NotIndexException"
    Prelude.. Core.hasStatus 400

-- | Indicates that your request is malformed in some manner. See the
-- exception message.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Access denied or directory not found. Either you don\'t have permissions
-- for this directory or the directory does not exist. Try calling
-- ListDirectories and check your permissions.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | A facet with the same name already exists.
_FacetAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "FacetAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided @SchemaDoc@ value is not valid.
_InvalidSchemaDocException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchemaDocException =
  Core._MatchServiceError
    defaultService
    "InvalidSchemaDocException"
    Prelude.. Core.hasStatus 400

-- | Indicates that an attempt to make an attachment was invalid. For
-- example, attaching two nodes with a link type that is not applicable to
-- the nodes or attempting to apply a schema to a directory a second time.
_InvalidAttachmentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAttachmentException =
  Core._MatchServiceError
    defaultService
    "InvalidAttachmentException"
    Prelude.. Core.hasStatus 400

-- | Cannot list the parents of a Directory root.
_CannotListParentOfRootException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotListParentOfRootException =
  Core._MatchServiceError
    defaultService
    "CannotListParentOfRootException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested operation can only operate on policy
-- objects.
_NotPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotPolicyException =
  Core._MatchServiceError
    defaultService
    "NotPolicyException"
    Prelude.. Core.hasStatus 400

-- | Can occur for multiple reasons such as when you tag a resource that
-- doesn’t exist or if you specify a higher number of tags for a resource
-- than the allowed limit. Allowed limit is 50 tags per resource.
_InvalidTaggingRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTaggingRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidTaggingRequestException"
    Prelude.. Core.hasStatus 400

-- | An attempt to modify a Facet resulted in an invalid schema exception.
_InvalidFacetUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFacetUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidFacetUpdateException"
    Prelude.. Core.hasStatus 400

-- | Occurs when any of the rule parameter keys or values are invalid.
_InvalidRuleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRuleException =
  Core._MatchServiceError
    defaultService
    "InvalidRuleException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a schema is already published.
_SchemaAlreadyPublishedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchemaAlreadyPublishedException =
  Core._MatchServiceError
    defaultService
    "SchemaAlreadyPublishedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a Directory could not be created due to a naming
-- conflict. Choose a different name and try again.
_DirectoryAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "DirectoryAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | An operation can only operate on a disabled directory.
_DirectoryNotDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNotDisabledException =
  Core._MatchServiceError
    defaultService
    "DirectoryNotDisabledException"
    Prelude.. Core.hasStatus 400

-- | A @BatchWrite@ exception has occurred.
_BatchWriteException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchWriteException =
  Core._MatchServiceError
    defaultService
    "BatchWriteException"

-- | Operations are only permitted on enabled directories.
_DirectoryNotEnabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNotEnabledException =
  Core._MatchServiceError
    defaultService
    "DirectoryNotEnabledException"
    Prelude.. Core.hasStatus 400

-- | Occurs when deleting a facet that contains an attribute that is a target
-- to an attribute reference in a different facet.
_FacetInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetInUseException =
  Core._MatchServiceError
    defaultService
    "FacetInUseException"
    Prelude.. Core.hasStatus 400

-- | The Facet that you provided was not well formed or could not be
-- validated with the schema.
_FacetValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetValidationException =
  Core._MatchServiceError
    defaultService
    "FacetValidationException"
    Prelude.. Core.hasStatus 400

-- | The object could not be deleted because links still exist. Remove the
-- links and then try the operation again.
_StillContainsLinksException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StillContainsLinksException =
  Core._MatchServiceError
    defaultService
    "StillContainsLinksException"
    Prelude.. Core.hasStatus 400

-- | Indicates a failure occurred while performing a check for backward
-- compatibility between the specified schema and the schema that is
-- currently applied to the directory.
_IncompatibleSchemaException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleSchemaException =
  Core._MatchServiceError
    defaultService
    "IncompatibleSchemaException"
    Prelude.. Core.hasStatus 400

-- | Occurs when any invalid operations are performed on an object that is
-- not a node, such as calling @ListObjectChildren@ for a leaf node object.
_NotNodeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotNodeException =
  Core._MatchServiceError
    defaultService
    "NotNodeException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the @NextToken@ value is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the object is not attached to the index.
_ObjectAlreadyDetachedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ObjectAlreadyDetachedException =
  Core._MatchServiceError
    defaultService
    "ObjectAlreadyDetachedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a link could not be created due to a naming conflict.
-- Choose a different name and then try again.
_LinkNameAlreadyInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LinkNameAlreadyInUseException =
  Core._MatchServiceError
    defaultService
    "LinkNameAlreadyInUseException"
    Prelude.. Core.hasStatus 400

-- | Indicates a problem that must be resolved by Amazon Web Services. This
-- might be a transient error in which case you can retry your request
-- until it succeeds. Otherwise, go to the
-- <http://status.aws.amazon.com/ AWS Service Health Dashboard> site to see
-- if there are any operational issues with the service.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | Indicates that a schema could not be created due to a naming conflict.
-- Please select a different name and then try again.
_SchemaAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchemaAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "SchemaAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | An object has been attempted to be attached to an object that does not
-- have the appropriate attribute value.
_IndexedAttributeMissingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IndexedAttributeMissingException =
  Core._MatchServiceError
    defaultService
    "IndexedAttributeMissingException"
    Prelude.. Core.hasStatus 400

-- | A directory that has been deleted and to which access has been
-- attempted. Note: The requested resource will eventually cease to exist.
_DirectoryDeletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryDeletedException =
  Core._MatchServiceError
    defaultService
    "DirectoryDeletedException"
    Prelude.. Core.hasStatus 400

-- | Occurs when a conflict with a previous successful write is detected. For
-- example, if a write operation occurs on an object and then an attempt is
-- made to read the object using “SERIALIZABLE” consistency, this exception
-- may result. This generally occurs when the previous write did not have
-- time to propagate to the host serving the current request. A retry (with
-- appropriate backoff logic) is the recommended response to this
-- exception.
_RetryableConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RetryableConflictException =
  Core._MatchServiceError
    defaultService
    "RetryableConflictException"
    Prelude.. Core.hasStatus 409

-- | Indicates that the provided ARN value is not valid.
_InvalidArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"
    Prelude.. Core.hasStatus 400

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The specified Facet could not be found.
_FacetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetNotFoundException =
  Core._MatchServiceError
    defaultService
    "FacetNotFoundException"
    Prelude.. Core.hasStatus 400

-- | Indicates that limits are exceeded. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Limits>
-- for more information.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested operation cannot be completed because the
-- object has not been detached from the tree.
_ObjectNotDetachedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ObjectNotDetachedException =
  Core._MatchServiceError
    defaultService
    "ObjectNotDetachedException"
    Prelude.. Core.hasStatus 400

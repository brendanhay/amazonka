{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudDirectory.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _BatchWriteException,
    _CannotListParentOfRootException,
    _DirectoryAlreadyExistsException,
    _DirectoryDeletedException,
    _DirectoryNotDisabledException,
    _DirectoryNotEnabledException,
    _FacetAlreadyExistsException,
    _FacetInUseException,
    _FacetNotFoundException,
    _FacetValidationException,
    _IncompatibleSchemaException,
    _IndexedAttributeMissingException,
    _InternalServiceException,
    _InvalidArnException,
    _InvalidAttachmentException,
    _InvalidFacetUpdateException,
    _InvalidNextTokenException,
    _InvalidRuleException,
    _InvalidSchemaDocException,
    _InvalidTaggingRequestException,
    _LimitExceededException,
    _LinkNameAlreadyInUseException,
    _NotIndexException,
    _NotNodeException,
    _NotPolicyException,
    _ObjectAlreadyDetachedException,
    _ObjectNotDetachedException,
    _ResourceNotFoundException,
    _RetryableConflictException,
    _SchemaAlreadyExistsException,
    _SchemaAlreadyPublishedException,
    _StillContainsLinksException,
    _UnsupportedIndexTypeException,
    _ValidationException,

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
    batchCreateIndex_batchReferenceName,
    batchCreateIndex_linkName,
    batchCreateIndex_parentReference,
    batchCreateIndex_orderedIndexedAttributeList,
    batchCreateIndex_isUnique,

    -- * BatchCreateIndexResponse
    BatchCreateIndexResponse (..),
    newBatchCreateIndexResponse,
    batchCreateIndexResponse_objectIdentifier,

    -- * BatchCreateObject
    BatchCreateObject (..),
    newBatchCreateObject,
    batchCreateObject_batchReferenceName,
    batchCreateObject_linkName,
    batchCreateObject_parentReference,
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
    batchListAttachedIndices_maxResults,
    batchListAttachedIndices_nextToken,
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
    batchListIncomingTypedLinks_filterTypedLink,
    batchListIncomingTypedLinks_maxResults,
    batchListIncomingTypedLinks_nextToken,
    batchListIncomingTypedLinks_objectReference,

    -- * BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse (..),
    newBatchListIncomingTypedLinksResponse,
    batchListIncomingTypedLinksResponse_linkSpecifiers,
    batchListIncomingTypedLinksResponse_nextToken,

    -- * BatchListIndex
    BatchListIndex (..),
    newBatchListIndex,
    batchListIndex_maxResults,
    batchListIndex_nextToken,
    batchListIndex_rangesOnIndexedValues,
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
    batchListObjectAttributes_maxResults,
    batchListObjectAttributes_nextToken,
    batchListObjectAttributes_objectReference,

    -- * BatchListObjectAttributesResponse
    BatchListObjectAttributesResponse (..),
    newBatchListObjectAttributesResponse,
    batchListObjectAttributesResponse_attributes,
    batchListObjectAttributesResponse_nextToken,

    -- * BatchListObjectChildren
    BatchListObjectChildren (..),
    newBatchListObjectChildren,
    batchListObjectChildren_maxResults,
    batchListObjectChildren_nextToken,
    batchListObjectChildren_objectReference,

    -- * BatchListObjectChildrenResponse
    BatchListObjectChildrenResponse (..),
    newBatchListObjectChildrenResponse,
    batchListObjectChildrenResponse_children,
    batchListObjectChildrenResponse_nextToken,

    -- * BatchListObjectParentPaths
    BatchListObjectParentPaths (..),
    newBatchListObjectParentPaths,
    batchListObjectParentPaths_maxResults,
    batchListObjectParentPaths_nextToken,
    batchListObjectParentPaths_objectReference,

    -- * BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse (..),
    newBatchListObjectParentPathsResponse,
    batchListObjectParentPathsResponse_nextToken,
    batchListObjectParentPathsResponse_pathToObjectIdentifiersList,

    -- * BatchListObjectParents
    BatchListObjectParents (..),
    newBatchListObjectParents,
    batchListObjectParents_maxResults,
    batchListObjectParents_nextToken,
    batchListObjectParents_objectReference,

    -- * BatchListObjectParentsResponse
    BatchListObjectParentsResponse (..),
    newBatchListObjectParentsResponse,
    batchListObjectParentsResponse_nextToken,
    batchListObjectParentsResponse_parentLinks,

    -- * BatchListObjectPolicies
    BatchListObjectPolicies (..),
    newBatchListObjectPolicies,
    batchListObjectPolicies_maxResults,
    batchListObjectPolicies_nextToken,
    batchListObjectPolicies_objectReference,

    -- * BatchListObjectPoliciesResponse
    BatchListObjectPoliciesResponse (..),
    newBatchListObjectPoliciesResponse,
    batchListObjectPoliciesResponse_attachedPolicyIds,
    batchListObjectPoliciesResponse_nextToken,

    -- * BatchListOutgoingTypedLinks
    BatchListOutgoingTypedLinks (..),
    newBatchListOutgoingTypedLinks,
    batchListOutgoingTypedLinks_filterAttributeRanges,
    batchListOutgoingTypedLinks_filterTypedLink,
    batchListOutgoingTypedLinks_maxResults,
    batchListOutgoingTypedLinks_nextToken,
    batchListOutgoingTypedLinks_objectReference,

    -- * BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse (..),
    newBatchListOutgoingTypedLinksResponse,
    batchListOutgoingTypedLinksResponse_nextToken,
    batchListOutgoingTypedLinksResponse_typedLinkSpecifiers,

    -- * BatchListPolicyAttachments
    BatchListPolicyAttachments (..),
    newBatchListPolicyAttachments,
    batchListPolicyAttachments_maxResults,
    batchListPolicyAttachments_nextToken,
    batchListPolicyAttachments_policyReference,

    -- * BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse (..),
    newBatchListPolicyAttachmentsResponse,
    batchListPolicyAttachmentsResponse_nextToken,
    batchListPolicyAttachmentsResponse_objectIdentifiers,

    -- * BatchLookupPolicy
    BatchLookupPolicy (..),
    newBatchLookupPolicy,
    batchLookupPolicy_maxResults,
    batchLookupPolicy_nextToken,
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
    batchReadOperation_getLinkAttributes,
    batchReadOperation_getObjectAttributes,
    batchReadOperation_getObjectInformation,
    batchReadOperation_listAttachedIndices,
    batchReadOperation_listIncomingTypedLinks,
    batchReadOperation_listIndex,
    batchReadOperation_listObjectAttributes,
    batchReadOperation_listObjectChildren,
    batchReadOperation_listObjectParentPaths,
    batchReadOperation_listObjectParents,
    batchReadOperation_listObjectPolicies,
    batchReadOperation_listOutgoingTypedLinks,
    batchReadOperation_listPolicyAttachments,
    batchReadOperation_lookupPolicy,

    -- * BatchReadOperationResponse
    BatchReadOperationResponse (..),
    newBatchReadOperationResponse,
    batchReadOperationResponse_exceptionResponse,
    batchReadOperationResponse_successfulResponse,

    -- * BatchReadSuccessfulResponse
    BatchReadSuccessfulResponse (..),
    newBatchReadSuccessfulResponse,
    batchReadSuccessfulResponse_getLinkAttributes,
    batchReadSuccessfulResponse_getObjectAttributes,
    batchReadSuccessfulResponse_getObjectInformation,
    batchReadSuccessfulResponse_listAttachedIndices,
    batchReadSuccessfulResponse_listIncomingTypedLinks,
    batchReadSuccessfulResponse_listIndex,
    batchReadSuccessfulResponse_listObjectAttributes,
    batchReadSuccessfulResponse_listObjectChildren,
    batchReadSuccessfulResponse_listObjectParentPaths,
    batchReadSuccessfulResponse_listObjectParents,
    batchReadSuccessfulResponse_listObjectPolicies,
    batchReadSuccessfulResponse_listOutgoingTypedLinks,
    batchReadSuccessfulResponse_listPolicyAttachments,
    batchReadSuccessfulResponse_lookupPolicy,

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
    batchWriteOperation_addFacetToObject,
    batchWriteOperation_attachObject,
    batchWriteOperation_attachPolicy,
    batchWriteOperation_attachToIndex,
    batchWriteOperation_attachTypedLink,
    batchWriteOperation_createIndex,
    batchWriteOperation_createObject,
    batchWriteOperation_deleteObject,
    batchWriteOperation_detachFromIndex,
    batchWriteOperation_detachObject,
    batchWriteOperation_detachPolicy,
    batchWriteOperation_detachTypedLink,
    batchWriteOperation_removeFacetFromObject,
    batchWriteOperation_updateLinkAttributes,
    batchWriteOperation_updateObjectAttributes,

    -- * BatchWriteOperationResponse
    BatchWriteOperationResponse (..),
    newBatchWriteOperationResponse,
    batchWriteOperationResponse_addFacetToObject,
    batchWriteOperationResponse_attachObject,
    batchWriteOperationResponse_attachPolicy,
    batchWriteOperationResponse_attachToIndex,
    batchWriteOperationResponse_attachTypedLink,
    batchWriteOperationResponse_createIndex,
    batchWriteOperationResponse_createObject,
    batchWriteOperationResponse_deleteObject,
    batchWriteOperationResponse_detachFromIndex,
    batchWriteOperationResponse_detachObject,
    batchWriteOperationResponse_detachPolicy,
    batchWriteOperationResponse_detachTypedLink,
    batchWriteOperationResponse_removeFacetFromObject,
    batchWriteOperationResponse_updateLinkAttributes,
    batchWriteOperationResponse_updateObjectAttributes,

    -- * Directory
    Directory (..),
    newDirectory,
    directory_creationDateTime,
    directory_directoryArn,
    directory_name,
    directory_state,

    -- * Facet
    Facet (..),
    newFacet,
    facet_facetStyle,
    facet_name,
    facet_objectType,

    -- * FacetAttribute
    FacetAttribute (..),
    newFacetAttribute,
    facetAttribute_attributeDefinition,
    facetAttribute_attributeReference,
    facetAttribute_requiredBehavior,
    facetAttribute_name,

    -- * FacetAttributeDefinition
    FacetAttributeDefinition (..),
    newFacetAttributeDefinition,
    facetAttributeDefinition_defaultValue,
    facetAttributeDefinition_isImmutable,
    facetAttributeDefinition_rules,
    facetAttributeDefinition_type,

    -- * FacetAttributeReference
    FacetAttributeReference (..),
    newFacetAttributeReference,
    facetAttributeReference_targetFacetName,
    facetAttributeReference_targetAttributeName,

    -- * FacetAttributeUpdate
    FacetAttributeUpdate (..),
    newFacetAttributeUpdate,
    facetAttributeUpdate_action,
    facetAttributeUpdate_attribute,

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
    objectAttributeRange_attributeKey,
    objectAttributeRange_range,

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
    policyAttachment_objectIdentifier,
    policyAttachment_policyId,
    policyAttachment_policyType,

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
    tag_key,
    tag_value,

    -- * TypedAttributeValue
    TypedAttributeValue (..),
    newTypedAttributeValue,
    typedAttributeValue_binaryValue,
    typedAttributeValue_booleanValue,
    typedAttributeValue_datetimeValue,
    typedAttributeValue_numberValue,
    typedAttributeValue_stringValue,

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
    typedLinkAttributeDefinition_defaultValue,
    typedLinkAttributeDefinition_isImmutable,
    typedLinkAttributeDefinition_rules,
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

import Amazonka.CloudDirectory.Types.AttributeKey
import Amazonka.CloudDirectory.Types.AttributeKeyAndValue
import Amazonka.CloudDirectory.Types.AttributeNameAndValue
import Amazonka.CloudDirectory.Types.BatchAddFacetToObject
import Amazonka.CloudDirectory.Types.BatchAddFacetToObjectResponse
import Amazonka.CloudDirectory.Types.BatchAttachObject
import Amazonka.CloudDirectory.Types.BatchAttachObjectResponse
import Amazonka.CloudDirectory.Types.BatchAttachPolicy
import Amazonka.CloudDirectory.Types.BatchAttachPolicyResponse
import Amazonka.CloudDirectory.Types.BatchAttachToIndex
import Amazonka.CloudDirectory.Types.BatchAttachToIndexResponse
import Amazonka.CloudDirectory.Types.BatchAttachTypedLink
import Amazonka.CloudDirectory.Types.BatchAttachTypedLinkResponse
import Amazonka.CloudDirectory.Types.BatchCreateIndex
import Amazonka.CloudDirectory.Types.BatchCreateIndexResponse
import Amazonka.CloudDirectory.Types.BatchCreateObject
import Amazonka.CloudDirectory.Types.BatchCreateObjectResponse
import Amazonka.CloudDirectory.Types.BatchDeleteObject
import Amazonka.CloudDirectory.Types.BatchDeleteObjectResponse
import Amazonka.CloudDirectory.Types.BatchDetachFromIndex
import Amazonka.CloudDirectory.Types.BatchDetachFromIndexResponse
import Amazonka.CloudDirectory.Types.BatchDetachObject
import Amazonka.CloudDirectory.Types.BatchDetachObjectResponse
import Amazonka.CloudDirectory.Types.BatchDetachPolicy
import Amazonka.CloudDirectory.Types.BatchDetachPolicyResponse
import Amazonka.CloudDirectory.Types.BatchDetachTypedLink
import Amazonka.CloudDirectory.Types.BatchDetachTypedLinkResponse
import Amazonka.CloudDirectory.Types.BatchGetLinkAttributes
import Amazonka.CloudDirectory.Types.BatchGetLinkAttributesResponse
import Amazonka.CloudDirectory.Types.BatchGetObjectAttributes
import Amazonka.CloudDirectory.Types.BatchGetObjectAttributesResponse
import Amazonka.CloudDirectory.Types.BatchGetObjectInformation
import Amazonka.CloudDirectory.Types.BatchGetObjectInformationResponse
import Amazonka.CloudDirectory.Types.BatchListAttachedIndices
import Amazonka.CloudDirectory.Types.BatchListAttachedIndicesResponse
import Amazonka.CloudDirectory.Types.BatchListIncomingTypedLinks
import Amazonka.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
import Amazonka.CloudDirectory.Types.BatchListIndex
import Amazonka.CloudDirectory.Types.BatchListIndexResponse
import Amazonka.CloudDirectory.Types.BatchListObjectAttributes
import Amazonka.CloudDirectory.Types.BatchListObjectAttributesResponse
import Amazonka.CloudDirectory.Types.BatchListObjectChildren
import Amazonka.CloudDirectory.Types.BatchListObjectChildrenResponse
import Amazonka.CloudDirectory.Types.BatchListObjectParentPaths
import Amazonka.CloudDirectory.Types.BatchListObjectParentPathsResponse
import Amazonka.CloudDirectory.Types.BatchListObjectParents
import Amazonka.CloudDirectory.Types.BatchListObjectParentsResponse
import Amazonka.CloudDirectory.Types.BatchListObjectPolicies
import Amazonka.CloudDirectory.Types.BatchListObjectPoliciesResponse
import Amazonka.CloudDirectory.Types.BatchListOutgoingTypedLinks
import Amazonka.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
import Amazonka.CloudDirectory.Types.BatchListPolicyAttachments
import Amazonka.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
import Amazonka.CloudDirectory.Types.BatchLookupPolicy
import Amazonka.CloudDirectory.Types.BatchLookupPolicyResponse
import Amazonka.CloudDirectory.Types.BatchReadException
import Amazonka.CloudDirectory.Types.BatchReadExceptionType
import Amazonka.CloudDirectory.Types.BatchReadOperation
import Amazonka.CloudDirectory.Types.BatchReadOperationResponse
import Amazonka.CloudDirectory.Types.BatchReadSuccessfulResponse
import Amazonka.CloudDirectory.Types.BatchRemoveFacetFromObject
import Amazonka.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
import Amazonka.CloudDirectory.Types.BatchUpdateLinkAttributes
import Amazonka.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
import Amazonka.CloudDirectory.Types.BatchUpdateObjectAttributes
import Amazonka.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
import Amazonka.CloudDirectory.Types.BatchWriteOperation
import Amazonka.CloudDirectory.Types.BatchWriteOperationResponse
import Amazonka.CloudDirectory.Types.ConsistencyLevel
import Amazonka.CloudDirectory.Types.Directory
import Amazonka.CloudDirectory.Types.DirectoryState
import Amazonka.CloudDirectory.Types.Facet
import Amazonka.CloudDirectory.Types.FacetAttribute
import Amazonka.CloudDirectory.Types.FacetAttributeDefinition
import Amazonka.CloudDirectory.Types.FacetAttributeReference
import Amazonka.CloudDirectory.Types.FacetAttributeType
import Amazonka.CloudDirectory.Types.FacetAttributeUpdate
import Amazonka.CloudDirectory.Types.FacetStyle
import Amazonka.CloudDirectory.Types.IndexAttachment
import Amazonka.CloudDirectory.Types.LinkAttributeAction
import Amazonka.CloudDirectory.Types.LinkAttributeUpdate
import Amazonka.CloudDirectory.Types.ObjectAttributeAction
import Amazonka.CloudDirectory.Types.ObjectAttributeRange
import Amazonka.CloudDirectory.Types.ObjectAttributeUpdate
import Amazonka.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.ObjectType
import Amazonka.CloudDirectory.Types.PathToObjectIdentifiers
import Amazonka.CloudDirectory.Types.PolicyAttachment
import Amazonka.CloudDirectory.Types.PolicyToPath
import Amazonka.CloudDirectory.Types.RangeMode
import Amazonka.CloudDirectory.Types.RequiredAttributeBehavior
import Amazonka.CloudDirectory.Types.Rule
import Amazonka.CloudDirectory.Types.RuleType
import Amazonka.CloudDirectory.Types.SchemaFacet
import Amazonka.CloudDirectory.Types.Tag
import Amazonka.CloudDirectory.Types.TypedAttributeValue
import Amazonka.CloudDirectory.Types.TypedAttributeValueRange
import Amazonka.CloudDirectory.Types.TypedLinkAttributeDefinition
import Amazonka.CloudDirectory.Types.TypedLinkAttributeRange
import Amazonka.CloudDirectory.Types.TypedLinkFacet
import Amazonka.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
import Amazonka.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import Amazonka.CloudDirectory.Types.TypedLinkSpecifier
import Amazonka.CloudDirectory.Types.UpdateActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-01-11@ of the Amazon CloudDirectory SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CloudDirectory",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "clouddirectory",
      Core.signingName = "clouddirectory",
      Core.version = "2017-01-11",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CloudDirectory",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Access denied or directory not found. Either you don\'t have permissions
-- for this directory or the directory does not exist. Try calling
-- ListDirectories and check your permissions.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | A @BatchWrite@ exception has occurred.
_BatchWriteException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchWriteException =
  Core._MatchServiceError
    defaultService
    "BatchWriteException"

-- | Cannot list the parents of a Directory root.
_CannotListParentOfRootException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotListParentOfRootException =
  Core._MatchServiceError
    defaultService
    "CannotListParentOfRootException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a Directory could not be created due to a naming
-- conflict. Choose a different name and try again.
_DirectoryAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "DirectoryAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | A directory that has been deleted and to which access has been
-- attempted. Note: The requested resource will eventually cease to exist.
_DirectoryDeletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryDeletedException =
  Core._MatchServiceError
    defaultService
    "DirectoryDeletedException"
    Prelude.. Core.hasStatus 400

-- | An operation can only operate on a disabled directory.
_DirectoryNotDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNotDisabledException =
  Core._MatchServiceError
    defaultService
    "DirectoryNotDisabledException"
    Prelude.. Core.hasStatus 400

-- | Operations are only permitted on enabled directories.
_DirectoryNotEnabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNotEnabledException =
  Core._MatchServiceError
    defaultService
    "DirectoryNotEnabledException"
    Prelude.. Core.hasStatus 400

-- | A facet with the same name already exists.
_FacetAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "FacetAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | Occurs when deleting a facet that contains an attribute that is a target
-- to an attribute reference in a different facet.
_FacetInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetInUseException =
  Core._MatchServiceError
    defaultService
    "FacetInUseException"
    Prelude.. Core.hasStatus 400

-- | The specified Facet could not be found.
_FacetNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetNotFoundException =
  Core._MatchServiceError
    defaultService
    "FacetNotFoundException"
    Prelude.. Core.hasStatus 400

-- | The Facet that you provided was not well formed or could not be
-- validated with the schema.
_FacetValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FacetValidationException =
  Core._MatchServiceError
    defaultService
    "FacetValidationException"
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

-- | An object has been attempted to be attached to an object that does not
-- have the appropriate attribute value.
_IndexedAttributeMissingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IndexedAttributeMissingException =
  Core._MatchServiceError
    defaultService
    "IndexedAttributeMissingException"
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

-- | Indicates that the provided ARN value is not valid.
_InvalidArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidArnException =
  Core._MatchServiceError
    defaultService
    "InvalidArnException"
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

-- | An attempt to modify a Facet resulted in an invalid schema exception.
_InvalidFacetUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFacetUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidFacetUpdateException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the @NextToken@ value is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"
    Prelude.. Core.hasStatus 400

-- | Occurs when any of the rule parameter keys or values are invalid.
_InvalidRuleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRuleException =
  Core._MatchServiceError
    defaultService
    "InvalidRuleException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the provided @SchemaDoc@ value is not valid.
_InvalidSchemaDocException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSchemaDocException =
  Core._MatchServiceError
    defaultService
    "InvalidSchemaDocException"
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

-- | Indicates that limits are exceeded. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Limits>
-- for more information.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a link could not be created due to a naming conflict.
-- Choose a different name and then try again.
_LinkNameAlreadyInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LinkNameAlreadyInUseException =
  Core._MatchServiceError
    defaultService
    "LinkNameAlreadyInUseException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested operation can only operate on index
-- objects.
_NotIndexException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotIndexException =
  Core._MatchServiceError
    defaultService
    "NotIndexException"
    Prelude.. Core.hasStatus 400

-- | Occurs when any invalid operations are performed on an object that is
-- not a node, such as calling @ListObjectChildren@ for a leaf node object.
_NotNodeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotNodeException =
  Core._MatchServiceError
    defaultService
    "NotNodeException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested operation can only operate on policy
-- objects.
_NotPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotPolicyException =
  Core._MatchServiceError
    defaultService
    "NotPolicyException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the object is not attached to the index.
_ObjectAlreadyDetachedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ObjectAlreadyDetachedException =
  Core._MatchServiceError
    defaultService
    "ObjectAlreadyDetachedException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested operation cannot be completed because the
-- object has not been detached from the tree.
_ObjectNotDetachedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ObjectNotDetachedException =
  Core._MatchServiceError
    defaultService
    "ObjectNotDetachedException"
    Prelude.. Core.hasStatus 400

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

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

-- | Indicates that a schema could not be created due to a naming conflict.
-- Please select a different name and then try again.
_SchemaAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchemaAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "SchemaAlreadyExistsException"
    Prelude.. Core.hasStatus 400

-- | Indicates that a schema is already published.
_SchemaAlreadyPublishedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SchemaAlreadyPublishedException =
  Core._MatchServiceError
    defaultService
    "SchemaAlreadyPublishedException"
    Prelude.. Core.hasStatus 400

-- | The object could not be deleted because links still exist. Remove the
-- links and then try the operation again.
_StillContainsLinksException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StillContainsLinksException =
  Core._MatchServiceError
    defaultService
    "StillContainsLinksException"
    Prelude.. Core.hasStatus 400

-- | Indicates that the requested index type is not supported.
_UnsupportedIndexTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedIndexTypeException =
  Core._MatchServiceError
    defaultService
    "UnsupportedIndexTypeException"
    Prelude.. Core.hasStatus 400

-- | Indicates that your request is malformed in some manner. See the
-- exception message.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CloudDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-01-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Cloud Directory
--
-- Amazon Cloud Directory is a component of the AWS Directory Service that
-- simplifies the development and management of cloud-scale web, mobile,
-- and IoT applications. This guide describes the Cloud Directory
-- operations that you can call programmatically and includes detailed
-- information on data types and errors. For information about Cloud
-- Directory features, see
-- <https://aws.amazon.com/directoryservice/ AWS Directory Service> and the
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/what_is_cloud_directory.html Amazon Cloud Directory Developer Guide>.
module Amazonka.CloudDirectory
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BatchWriteException
    _BatchWriteException,

    -- ** CannotListParentOfRootException
    _CannotListParentOfRootException,

    -- ** DirectoryAlreadyExistsException
    _DirectoryAlreadyExistsException,

    -- ** DirectoryDeletedException
    _DirectoryDeletedException,

    -- ** DirectoryNotDisabledException
    _DirectoryNotDisabledException,

    -- ** DirectoryNotEnabledException
    _DirectoryNotEnabledException,

    -- ** FacetAlreadyExistsException
    _FacetAlreadyExistsException,

    -- ** FacetInUseException
    _FacetInUseException,

    -- ** FacetNotFoundException
    _FacetNotFoundException,

    -- ** FacetValidationException
    _FacetValidationException,

    -- ** IncompatibleSchemaException
    _IncompatibleSchemaException,

    -- ** IndexedAttributeMissingException
    _IndexedAttributeMissingException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** InvalidAttachmentException
    _InvalidAttachmentException,

    -- ** InvalidFacetUpdateException
    _InvalidFacetUpdateException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidRuleException
    _InvalidRuleException,

    -- ** InvalidSchemaDocException
    _InvalidSchemaDocException,

    -- ** InvalidTaggingRequestException
    _InvalidTaggingRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** LinkNameAlreadyInUseException
    _LinkNameAlreadyInUseException,

    -- ** NotIndexException
    _NotIndexException,

    -- ** NotNodeException
    _NotNodeException,

    -- ** NotPolicyException
    _NotPolicyException,

    -- ** ObjectAlreadyDetachedException
    _ObjectAlreadyDetachedException,

    -- ** ObjectNotDetachedException
    _ObjectNotDetachedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** RetryableConflictException
    _RetryableConflictException,

    -- ** SchemaAlreadyExistsException
    _SchemaAlreadyExistsException,

    -- ** SchemaAlreadyPublishedException
    _SchemaAlreadyPublishedException,

    -- ** StillContainsLinksException
    _StillContainsLinksException,

    -- ** UnsupportedIndexTypeException
    _UnsupportedIndexTypeException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AddFacetToObject
    AddFacetToObject (AddFacetToObject'),
    newAddFacetToObject,
    AddFacetToObjectResponse (AddFacetToObjectResponse'),
    newAddFacetToObjectResponse,

    -- ** ApplySchema
    ApplySchema (ApplySchema'),
    newApplySchema,
    ApplySchemaResponse (ApplySchemaResponse'),
    newApplySchemaResponse,

    -- ** AttachObject
    AttachObject (AttachObject'),
    newAttachObject,
    AttachObjectResponse (AttachObjectResponse'),
    newAttachObjectResponse,

    -- ** AttachPolicy
    AttachPolicy (AttachPolicy'),
    newAttachPolicy,
    AttachPolicyResponse (AttachPolicyResponse'),
    newAttachPolicyResponse,

    -- ** AttachToIndex
    AttachToIndex (AttachToIndex'),
    newAttachToIndex,
    AttachToIndexResponse (AttachToIndexResponse'),
    newAttachToIndexResponse,

    -- ** AttachTypedLink
    AttachTypedLink (AttachTypedLink'),
    newAttachTypedLink,
    AttachTypedLinkResponse (AttachTypedLinkResponse'),
    newAttachTypedLinkResponse,

    -- ** BatchRead
    BatchRead (BatchRead'),
    newBatchRead,
    BatchReadResponse (BatchReadResponse'),
    newBatchReadResponse,

    -- ** BatchWrite
    BatchWrite (BatchWrite'),
    newBatchWrite,
    BatchWriteResponse (BatchWriteResponse'),
    newBatchWriteResponse,

    -- ** CreateDirectory
    CreateDirectory (CreateDirectory'),
    newCreateDirectory,
    CreateDirectoryResponse (CreateDirectoryResponse'),
    newCreateDirectoryResponse,

    -- ** CreateFacet
    CreateFacet (CreateFacet'),
    newCreateFacet,
    CreateFacetResponse (CreateFacetResponse'),
    newCreateFacetResponse,

    -- ** CreateIndex
    CreateIndex (CreateIndex'),
    newCreateIndex,
    CreateIndexResponse (CreateIndexResponse'),
    newCreateIndexResponse,

    -- ** CreateObject
    CreateObject (CreateObject'),
    newCreateObject,
    CreateObjectResponse (CreateObjectResponse'),
    newCreateObjectResponse,

    -- ** CreateSchema
    CreateSchema (CreateSchema'),
    newCreateSchema,
    CreateSchemaResponse (CreateSchemaResponse'),
    newCreateSchemaResponse,

    -- ** CreateTypedLinkFacet
    CreateTypedLinkFacet (CreateTypedLinkFacet'),
    newCreateTypedLinkFacet,
    CreateTypedLinkFacetResponse (CreateTypedLinkFacetResponse'),
    newCreateTypedLinkFacetResponse,

    -- ** DeleteDirectory
    DeleteDirectory (DeleteDirectory'),
    newDeleteDirectory,
    DeleteDirectoryResponse (DeleteDirectoryResponse'),
    newDeleteDirectoryResponse,

    -- ** DeleteFacet
    DeleteFacet (DeleteFacet'),
    newDeleteFacet,
    DeleteFacetResponse (DeleteFacetResponse'),
    newDeleteFacetResponse,

    -- ** DeleteObject
    DeleteObject (DeleteObject'),
    newDeleteObject,
    DeleteObjectResponse (DeleteObjectResponse'),
    newDeleteObjectResponse,

    -- ** DeleteSchema
    DeleteSchema (DeleteSchema'),
    newDeleteSchema,
    DeleteSchemaResponse (DeleteSchemaResponse'),
    newDeleteSchemaResponse,

    -- ** DeleteTypedLinkFacet
    DeleteTypedLinkFacet (DeleteTypedLinkFacet'),
    newDeleteTypedLinkFacet,
    DeleteTypedLinkFacetResponse (DeleteTypedLinkFacetResponse'),
    newDeleteTypedLinkFacetResponse,

    -- ** DetachFromIndex
    DetachFromIndex (DetachFromIndex'),
    newDetachFromIndex,
    DetachFromIndexResponse (DetachFromIndexResponse'),
    newDetachFromIndexResponse,

    -- ** DetachObject
    DetachObject (DetachObject'),
    newDetachObject,
    DetachObjectResponse (DetachObjectResponse'),
    newDetachObjectResponse,

    -- ** DetachPolicy
    DetachPolicy (DetachPolicy'),
    newDetachPolicy,
    DetachPolicyResponse (DetachPolicyResponse'),
    newDetachPolicyResponse,

    -- ** DetachTypedLink
    DetachTypedLink (DetachTypedLink'),
    newDetachTypedLink,
    DetachTypedLinkResponse (DetachTypedLinkResponse'),
    newDetachTypedLinkResponse,

    -- ** DisableDirectory
    DisableDirectory (DisableDirectory'),
    newDisableDirectory,
    DisableDirectoryResponse (DisableDirectoryResponse'),
    newDisableDirectoryResponse,

    -- ** EnableDirectory
    EnableDirectory (EnableDirectory'),
    newEnableDirectory,
    EnableDirectoryResponse (EnableDirectoryResponse'),
    newEnableDirectoryResponse,

    -- ** GetAppliedSchemaVersion
    GetAppliedSchemaVersion (GetAppliedSchemaVersion'),
    newGetAppliedSchemaVersion,
    GetAppliedSchemaVersionResponse (GetAppliedSchemaVersionResponse'),
    newGetAppliedSchemaVersionResponse,

    -- ** GetDirectory
    GetDirectory (GetDirectory'),
    newGetDirectory,
    GetDirectoryResponse (GetDirectoryResponse'),
    newGetDirectoryResponse,

    -- ** GetFacet
    GetFacet (GetFacet'),
    newGetFacet,
    GetFacetResponse (GetFacetResponse'),
    newGetFacetResponse,

    -- ** GetLinkAttributes
    GetLinkAttributes (GetLinkAttributes'),
    newGetLinkAttributes,
    GetLinkAttributesResponse (GetLinkAttributesResponse'),
    newGetLinkAttributesResponse,

    -- ** GetObjectAttributes
    GetObjectAttributes (GetObjectAttributes'),
    newGetObjectAttributes,
    GetObjectAttributesResponse (GetObjectAttributesResponse'),
    newGetObjectAttributesResponse,

    -- ** GetObjectInformation
    GetObjectInformation (GetObjectInformation'),
    newGetObjectInformation,
    GetObjectInformationResponse (GetObjectInformationResponse'),
    newGetObjectInformationResponse,

    -- ** GetSchemaAsJson
    GetSchemaAsJson (GetSchemaAsJson'),
    newGetSchemaAsJson,
    GetSchemaAsJsonResponse (GetSchemaAsJsonResponse'),
    newGetSchemaAsJsonResponse,

    -- ** GetTypedLinkFacetInformation
    GetTypedLinkFacetInformation (GetTypedLinkFacetInformation'),
    newGetTypedLinkFacetInformation,
    GetTypedLinkFacetInformationResponse (GetTypedLinkFacetInformationResponse'),
    newGetTypedLinkFacetInformationResponse,

    -- ** ListAppliedSchemaArns (Paginated)
    ListAppliedSchemaArns (ListAppliedSchemaArns'),
    newListAppliedSchemaArns,
    ListAppliedSchemaArnsResponse (ListAppliedSchemaArnsResponse'),
    newListAppliedSchemaArnsResponse,

    -- ** ListAttachedIndices (Paginated)
    ListAttachedIndices (ListAttachedIndices'),
    newListAttachedIndices,
    ListAttachedIndicesResponse (ListAttachedIndicesResponse'),
    newListAttachedIndicesResponse,

    -- ** ListDevelopmentSchemaArns (Paginated)
    ListDevelopmentSchemaArns (ListDevelopmentSchemaArns'),
    newListDevelopmentSchemaArns,
    ListDevelopmentSchemaArnsResponse (ListDevelopmentSchemaArnsResponse'),
    newListDevelopmentSchemaArnsResponse,

    -- ** ListDirectories (Paginated)
    ListDirectories (ListDirectories'),
    newListDirectories,
    ListDirectoriesResponse (ListDirectoriesResponse'),
    newListDirectoriesResponse,

    -- ** ListFacetAttributes (Paginated)
    ListFacetAttributes (ListFacetAttributes'),
    newListFacetAttributes,
    ListFacetAttributesResponse (ListFacetAttributesResponse'),
    newListFacetAttributesResponse,

    -- ** ListFacetNames (Paginated)
    ListFacetNames (ListFacetNames'),
    newListFacetNames,
    ListFacetNamesResponse (ListFacetNamesResponse'),
    newListFacetNamesResponse,

    -- ** ListIncomingTypedLinks (Paginated)
    ListIncomingTypedLinks (ListIncomingTypedLinks'),
    newListIncomingTypedLinks,
    ListIncomingTypedLinksResponse (ListIncomingTypedLinksResponse'),
    newListIncomingTypedLinksResponse,

    -- ** ListIndex (Paginated)
    ListIndex (ListIndex'),
    newListIndex,
    ListIndexResponse (ListIndexResponse'),
    newListIndexResponse,

    -- ** ListManagedSchemaArns (Paginated)
    ListManagedSchemaArns (ListManagedSchemaArns'),
    newListManagedSchemaArns,
    ListManagedSchemaArnsResponse (ListManagedSchemaArnsResponse'),
    newListManagedSchemaArnsResponse,

    -- ** ListObjectAttributes (Paginated)
    ListObjectAttributes (ListObjectAttributes'),
    newListObjectAttributes,
    ListObjectAttributesResponse (ListObjectAttributesResponse'),
    newListObjectAttributesResponse,

    -- ** ListObjectChildren
    ListObjectChildren (ListObjectChildren'),
    newListObjectChildren,
    ListObjectChildrenResponse (ListObjectChildrenResponse'),
    newListObjectChildrenResponse,

    -- ** ListObjectParentPaths (Paginated)
    ListObjectParentPaths (ListObjectParentPaths'),
    newListObjectParentPaths,
    ListObjectParentPathsResponse (ListObjectParentPathsResponse'),
    newListObjectParentPathsResponse,

    -- ** ListObjectParents
    ListObjectParents (ListObjectParents'),
    newListObjectParents,
    ListObjectParentsResponse (ListObjectParentsResponse'),
    newListObjectParentsResponse,

    -- ** ListObjectPolicies (Paginated)
    ListObjectPolicies (ListObjectPolicies'),
    newListObjectPolicies,
    ListObjectPoliciesResponse (ListObjectPoliciesResponse'),
    newListObjectPoliciesResponse,

    -- ** ListOutgoingTypedLinks (Paginated)
    ListOutgoingTypedLinks (ListOutgoingTypedLinks'),
    newListOutgoingTypedLinks,
    ListOutgoingTypedLinksResponse (ListOutgoingTypedLinksResponse'),
    newListOutgoingTypedLinksResponse,

    -- ** ListPolicyAttachments (Paginated)
    ListPolicyAttachments (ListPolicyAttachments'),
    newListPolicyAttachments,
    ListPolicyAttachmentsResponse (ListPolicyAttachmentsResponse'),
    newListPolicyAttachmentsResponse,

    -- ** ListPublishedSchemaArns (Paginated)
    ListPublishedSchemaArns (ListPublishedSchemaArns'),
    newListPublishedSchemaArns,
    ListPublishedSchemaArnsResponse (ListPublishedSchemaArnsResponse'),
    newListPublishedSchemaArnsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTypedLinkFacetAttributes (Paginated)
    ListTypedLinkFacetAttributes (ListTypedLinkFacetAttributes'),
    newListTypedLinkFacetAttributes,
    ListTypedLinkFacetAttributesResponse (ListTypedLinkFacetAttributesResponse'),
    newListTypedLinkFacetAttributesResponse,

    -- ** ListTypedLinkFacetNames (Paginated)
    ListTypedLinkFacetNames (ListTypedLinkFacetNames'),
    newListTypedLinkFacetNames,
    ListTypedLinkFacetNamesResponse (ListTypedLinkFacetNamesResponse'),
    newListTypedLinkFacetNamesResponse,

    -- ** LookupPolicy (Paginated)
    LookupPolicy (LookupPolicy'),
    newLookupPolicy,
    LookupPolicyResponse (LookupPolicyResponse'),
    newLookupPolicyResponse,

    -- ** PublishSchema
    PublishSchema (PublishSchema'),
    newPublishSchema,
    PublishSchemaResponse (PublishSchemaResponse'),
    newPublishSchemaResponse,

    -- ** PutSchemaFromJson
    PutSchemaFromJson (PutSchemaFromJson'),
    newPutSchemaFromJson,
    PutSchemaFromJsonResponse (PutSchemaFromJsonResponse'),
    newPutSchemaFromJsonResponse,

    -- ** RemoveFacetFromObject
    RemoveFacetFromObject (RemoveFacetFromObject'),
    newRemoveFacetFromObject,
    RemoveFacetFromObjectResponse (RemoveFacetFromObjectResponse'),
    newRemoveFacetFromObjectResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateFacet
    UpdateFacet (UpdateFacet'),
    newUpdateFacet,
    UpdateFacetResponse (UpdateFacetResponse'),
    newUpdateFacetResponse,

    -- ** UpdateLinkAttributes
    UpdateLinkAttributes (UpdateLinkAttributes'),
    newUpdateLinkAttributes,
    UpdateLinkAttributesResponse (UpdateLinkAttributesResponse'),
    newUpdateLinkAttributesResponse,

    -- ** UpdateObjectAttributes
    UpdateObjectAttributes (UpdateObjectAttributes'),
    newUpdateObjectAttributes,
    UpdateObjectAttributesResponse (UpdateObjectAttributesResponse'),
    newUpdateObjectAttributesResponse,

    -- ** UpdateSchema
    UpdateSchema (UpdateSchema'),
    newUpdateSchema,
    UpdateSchemaResponse (UpdateSchemaResponse'),
    newUpdateSchemaResponse,

    -- ** UpdateTypedLinkFacet
    UpdateTypedLinkFacet (UpdateTypedLinkFacet'),
    newUpdateTypedLinkFacet,
    UpdateTypedLinkFacetResponse (UpdateTypedLinkFacetResponse'),
    newUpdateTypedLinkFacetResponse,

    -- ** UpgradeAppliedSchema
    UpgradeAppliedSchema (UpgradeAppliedSchema'),
    newUpgradeAppliedSchema,
    UpgradeAppliedSchemaResponse (UpgradeAppliedSchemaResponse'),
    newUpgradeAppliedSchemaResponse,

    -- ** UpgradePublishedSchema
    UpgradePublishedSchema (UpgradePublishedSchema'),
    newUpgradePublishedSchema,
    UpgradePublishedSchemaResponse (UpgradePublishedSchemaResponse'),
    newUpgradePublishedSchemaResponse,

    -- * Types

    -- ** BatchReadExceptionType
    BatchReadExceptionType (..),

    -- ** ConsistencyLevel
    ConsistencyLevel (..),

    -- ** DirectoryState
    DirectoryState (..),

    -- ** FacetAttributeType
    FacetAttributeType (..),

    -- ** FacetStyle
    FacetStyle (..),

    -- ** ObjectType
    ObjectType (..),

    -- ** RangeMode
    RangeMode (..),

    -- ** RequiredAttributeBehavior
    RequiredAttributeBehavior (..),

    -- ** RuleType
    RuleType (..),

    -- ** UpdateActionType
    UpdateActionType (..),

    -- ** AttributeKey
    AttributeKey (AttributeKey'),
    newAttributeKey,

    -- ** AttributeKeyAndValue
    AttributeKeyAndValue (AttributeKeyAndValue'),
    newAttributeKeyAndValue,

    -- ** AttributeNameAndValue
    AttributeNameAndValue (AttributeNameAndValue'),
    newAttributeNameAndValue,

    -- ** BatchAddFacetToObject
    BatchAddFacetToObject (BatchAddFacetToObject'),
    newBatchAddFacetToObject,

    -- ** BatchAddFacetToObjectResponse
    BatchAddFacetToObjectResponse (BatchAddFacetToObjectResponse'),
    newBatchAddFacetToObjectResponse,

    -- ** BatchAttachObject
    BatchAttachObject (BatchAttachObject'),
    newBatchAttachObject,

    -- ** BatchAttachObjectResponse
    BatchAttachObjectResponse (BatchAttachObjectResponse'),
    newBatchAttachObjectResponse,

    -- ** BatchAttachPolicy
    BatchAttachPolicy (BatchAttachPolicy'),
    newBatchAttachPolicy,

    -- ** BatchAttachPolicyResponse
    BatchAttachPolicyResponse (BatchAttachPolicyResponse'),
    newBatchAttachPolicyResponse,

    -- ** BatchAttachToIndex
    BatchAttachToIndex (BatchAttachToIndex'),
    newBatchAttachToIndex,

    -- ** BatchAttachToIndexResponse
    BatchAttachToIndexResponse (BatchAttachToIndexResponse'),
    newBatchAttachToIndexResponse,

    -- ** BatchAttachTypedLink
    BatchAttachTypedLink (BatchAttachTypedLink'),
    newBatchAttachTypedLink,

    -- ** BatchAttachTypedLinkResponse
    BatchAttachTypedLinkResponse (BatchAttachTypedLinkResponse'),
    newBatchAttachTypedLinkResponse,

    -- ** BatchCreateIndex
    BatchCreateIndex (BatchCreateIndex'),
    newBatchCreateIndex,

    -- ** BatchCreateIndexResponse
    BatchCreateIndexResponse (BatchCreateIndexResponse'),
    newBatchCreateIndexResponse,

    -- ** BatchCreateObject
    BatchCreateObject (BatchCreateObject'),
    newBatchCreateObject,

    -- ** BatchCreateObjectResponse
    BatchCreateObjectResponse (BatchCreateObjectResponse'),
    newBatchCreateObjectResponse,

    -- ** BatchDeleteObject
    BatchDeleteObject (BatchDeleteObject'),
    newBatchDeleteObject,

    -- ** BatchDeleteObjectResponse
    BatchDeleteObjectResponse (BatchDeleteObjectResponse'),
    newBatchDeleteObjectResponse,

    -- ** BatchDetachFromIndex
    BatchDetachFromIndex (BatchDetachFromIndex'),
    newBatchDetachFromIndex,

    -- ** BatchDetachFromIndexResponse
    BatchDetachFromIndexResponse (BatchDetachFromIndexResponse'),
    newBatchDetachFromIndexResponse,

    -- ** BatchDetachObject
    BatchDetachObject (BatchDetachObject'),
    newBatchDetachObject,

    -- ** BatchDetachObjectResponse
    BatchDetachObjectResponse (BatchDetachObjectResponse'),
    newBatchDetachObjectResponse,

    -- ** BatchDetachPolicy
    BatchDetachPolicy (BatchDetachPolicy'),
    newBatchDetachPolicy,

    -- ** BatchDetachPolicyResponse
    BatchDetachPolicyResponse (BatchDetachPolicyResponse'),
    newBatchDetachPolicyResponse,

    -- ** BatchDetachTypedLink
    BatchDetachTypedLink (BatchDetachTypedLink'),
    newBatchDetachTypedLink,

    -- ** BatchDetachTypedLinkResponse
    BatchDetachTypedLinkResponse (BatchDetachTypedLinkResponse'),
    newBatchDetachTypedLinkResponse,

    -- ** BatchGetLinkAttributes
    BatchGetLinkAttributes (BatchGetLinkAttributes'),
    newBatchGetLinkAttributes,

    -- ** BatchGetLinkAttributesResponse
    BatchGetLinkAttributesResponse (BatchGetLinkAttributesResponse'),
    newBatchGetLinkAttributesResponse,

    -- ** BatchGetObjectAttributes
    BatchGetObjectAttributes (BatchGetObjectAttributes'),
    newBatchGetObjectAttributes,

    -- ** BatchGetObjectAttributesResponse
    BatchGetObjectAttributesResponse (BatchGetObjectAttributesResponse'),
    newBatchGetObjectAttributesResponse,

    -- ** BatchGetObjectInformation
    BatchGetObjectInformation (BatchGetObjectInformation'),
    newBatchGetObjectInformation,

    -- ** BatchGetObjectInformationResponse
    BatchGetObjectInformationResponse (BatchGetObjectInformationResponse'),
    newBatchGetObjectInformationResponse,

    -- ** BatchListAttachedIndices
    BatchListAttachedIndices (BatchListAttachedIndices'),
    newBatchListAttachedIndices,

    -- ** BatchListAttachedIndicesResponse
    BatchListAttachedIndicesResponse (BatchListAttachedIndicesResponse'),
    newBatchListAttachedIndicesResponse,

    -- ** BatchListIncomingTypedLinks
    BatchListIncomingTypedLinks (BatchListIncomingTypedLinks'),
    newBatchListIncomingTypedLinks,

    -- ** BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse (BatchListIncomingTypedLinksResponse'),
    newBatchListIncomingTypedLinksResponse,

    -- ** BatchListIndex
    BatchListIndex (BatchListIndex'),
    newBatchListIndex,

    -- ** BatchListIndexResponse
    BatchListIndexResponse (BatchListIndexResponse'),
    newBatchListIndexResponse,

    -- ** BatchListObjectAttributes
    BatchListObjectAttributes (BatchListObjectAttributes'),
    newBatchListObjectAttributes,

    -- ** BatchListObjectAttributesResponse
    BatchListObjectAttributesResponse (BatchListObjectAttributesResponse'),
    newBatchListObjectAttributesResponse,

    -- ** BatchListObjectChildren
    BatchListObjectChildren (BatchListObjectChildren'),
    newBatchListObjectChildren,

    -- ** BatchListObjectChildrenResponse
    BatchListObjectChildrenResponse (BatchListObjectChildrenResponse'),
    newBatchListObjectChildrenResponse,

    -- ** BatchListObjectParentPaths
    BatchListObjectParentPaths (BatchListObjectParentPaths'),
    newBatchListObjectParentPaths,

    -- ** BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse (BatchListObjectParentPathsResponse'),
    newBatchListObjectParentPathsResponse,

    -- ** BatchListObjectParents
    BatchListObjectParents (BatchListObjectParents'),
    newBatchListObjectParents,

    -- ** BatchListObjectParentsResponse
    BatchListObjectParentsResponse (BatchListObjectParentsResponse'),
    newBatchListObjectParentsResponse,

    -- ** BatchListObjectPolicies
    BatchListObjectPolicies (BatchListObjectPolicies'),
    newBatchListObjectPolicies,

    -- ** BatchListObjectPoliciesResponse
    BatchListObjectPoliciesResponse (BatchListObjectPoliciesResponse'),
    newBatchListObjectPoliciesResponse,

    -- ** BatchListOutgoingTypedLinks
    BatchListOutgoingTypedLinks (BatchListOutgoingTypedLinks'),
    newBatchListOutgoingTypedLinks,

    -- ** BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse (BatchListOutgoingTypedLinksResponse'),
    newBatchListOutgoingTypedLinksResponse,

    -- ** BatchListPolicyAttachments
    BatchListPolicyAttachments (BatchListPolicyAttachments'),
    newBatchListPolicyAttachments,

    -- ** BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse (BatchListPolicyAttachmentsResponse'),
    newBatchListPolicyAttachmentsResponse,

    -- ** BatchLookupPolicy
    BatchLookupPolicy (BatchLookupPolicy'),
    newBatchLookupPolicy,

    -- ** BatchLookupPolicyResponse
    BatchLookupPolicyResponse (BatchLookupPolicyResponse'),
    newBatchLookupPolicyResponse,

    -- ** BatchReadException
    BatchReadException (BatchReadException'),
    newBatchReadException,

    -- ** BatchReadOperation
    BatchReadOperation (BatchReadOperation'),
    newBatchReadOperation,

    -- ** BatchReadOperationResponse
    BatchReadOperationResponse (BatchReadOperationResponse'),
    newBatchReadOperationResponse,

    -- ** BatchReadSuccessfulResponse
    BatchReadSuccessfulResponse (BatchReadSuccessfulResponse'),
    newBatchReadSuccessfulResponse,

    -- ** BatchRemoveFacetFromObject
    BatchRemoveFacetFromObject (BatchRemoveFacetFromObject'),
    newBatchRemoveFacetFromObject,

    -- ** BatchRemoveFacetFromObjectResponse
    BatchRemoveFacetFromObjectResponse (BatchRemoveFacetFromObjectResponse'),
    newBatchRemoveFacetFromObjectResponse,

    -- ** BatchUpdateLinkAttributes
    BatchUpdateLinkAttributes (BatchUpdateLinkAttributes'),
    newBatchUpdateLinkAttributes,

    -- ** BatchUpdateLinkAttributesResponse
    BatchUpdateLinkAttributesResponse (BatchUpdateLinkAttributesResponse'),
    newBatchUpdateLinkAttributesResponse,

    -- ** BatchUpdateObjectAttributes
    BatchUpdateObjectAttributes (BatchUpdateObjectAttributes'),
    newBatchUpdateObjectAttributes,

    -- ** BatchUpdateObjectAttributesResponse
    BatchUpdateObjectAttributesResponse (BatchUpdateObjectAttributesResponse'),
    newBatchUpdateObjectAttributesResponse,

    -- ** BatchWriteOperation
    BatchWriteOperation (BatchWriteOperation'),
    newBatchWriteOperation,

    -- ** BatchWriteOperationResponse
    BatchWriteOperationResponse (BatchWriteOperationResponse'),
    newBatchWriteOperationResponse,

    -- ** Directory
    Directory (Directory'),
    newDirectory,

    -- ** Facet
    Facet (Facet'),
    newFacet,

    -- ** FacetAttribute
    FacetAttribute (FacetAttribute'),
    newFacetAttribute,

    -- ** FacetAttributeDefinition
    FacetAttributeDefinition (FacetAttributeDefinition'),
    newFacetAttributeDefinition,

    -- ** FacetAttributeReference
    FacetAttributeReference (FacetAttributeReference'),
    newFacetAttributeReference,

    -- ** FacetAttributeUpdate
    FacetAttributeUpdate (FacetAttributeUpdate'),
    newFacetAttributeUpdate,

    -- ** IndexAttachment
    IndexAttachment (IndexAttachment'),
    newIndexAttachment,

    -- ** LinkAttributeAction
    LinkAttributeAction (LinkAttributeAction'),
    newLinkAttributeAction,

    -- ** LinkAttributeUpdate
    LinkAttributeUpdate (LinkAttributeUpdate'),
    newLinkAttributeUpdate,

    -- ** ObjectAttributeAction
    ObjectAttributeAction (ObjectAttributeAction'),
    newObjectAttributeAction,

    -- ** ObjectAttributeRange
    ObjectAttributeRange (ObjectAttributeRange'),
    newObjectAttributeRange,

    -- ** ObjectAttributeUpdate
    ObjectAttributeUpdate (ObjectAttributeUpdate'),
    newObjectAttributeUpdate,

    -- ** ObjectIdentifierAndLinkNameTuple
    ObjectIdentifierAndLinkNameTuple (ObjectIdentifierAndLinkNameTuple'),
    newObjectIdentifierAndLinkNameTuple,

    -- ** ObjectReference
    ObjectReference (ObjectReference'),
    newObjectReference,

    -- ** PathToObjectIdentifiers
    PathToObjectIdentifiers (PathToObjectIdentifiers'),
    newPathToObjectIdentifiers,

    -- ** PolicyAttachment
    PolicyAttachment (PolicyAttachment'),
    newPolicyAttachment,

    -- ** PolicyToPath
    PolicyToPath (PolicyToPath'),
    newPolicyToPath,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** SchemaFacet
    SchemaFacet (SchemaFacet'),
    newSchemaFacet,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TypedAttributeValue
    TypedAttributeValue (TypedAttributeValue'),
    newTypedAttributeValue,

    -- ** TypedAttributeValueRange
    TypedAttributeValueRange (TypedAttributeValueRange'),
    newTypedAttributeValueRange,

    -- ** TypedLinkAttributeDefinition
    TypedLinkAttributeDefinition (TypedLinkAttributeDefinition'),
    newTypedLinkAttributeDefinition,

    -- ** TypedLinkAttributeRange
    TypedLinkAttributeRange (TypedLinkAttributeRange'),
    newTypedLinkAttributeRange,

    -- ** TypedLinkFacet
    TypedLinkFacet (TypedLinkFacet'),
    newTypedLinkFacet,

    -- ** TypedLinkFacetAttributeUpdate
    TypedLinkFacetAttributeUpdate (TypedLinkFacetAttributeUpdate'),
    newTypedLinkFacetAttributeUpdate,

    -- ** TypedLinkSchemaAndFacetName
    TypedLinkSchemaAndFacetName (TypedLinkSchemaAndFacetName'),
    newTypedLinkSchemaAndFacetName,

    -- ** TypedLinkSpecifier
    TypedLinkSpecifier (TypedLinkSpecifier'),
    newTypedLinkSpecifier,
  )
where

import Amazonka.CloudDirectory.AddFacetToObject
import Amazonka.CloudDirectory.ApplySchema
import Amazonka.CloudDirectory.AttachObject
import Amazonka.CloudDirectory.AttachPolicy
import Amazonka.CloudDirectory.AttachToIndex
import Amazonka.CloudDirectory.AttachTypedLink
import Amazonka.CloudDirectory.BatchRead
import Amazonka.CloudDirectory.BatchWrite
import Amazonka.CloudDirectory.CreateDirectory
import Amazonka.CloudDirectory.CreateFacet
import Amazonka.CloudDirectory.CreateIndex
import Amazonka.CloudDirectory.CreateObject
import Amazonka.CloudDirectory.CreateSchema
import Amazonka.CloudDirectory.CreateTypedLinkFacet
import Amazonka.CloudDirectory.DeleteDirectory
import Amazonka.CloudDirectory.DeleteFacet
import Amazonka.CloudDirectory.DeleteObject
import Amazonka.CloudDirectory.DeleteSchema
import Amazonka.CloudDirectory.DeleteTypedLinkFacet
import Amazonka.CloudDirectory.DetachFromIndex
import Amazonka.CloudDirectory.DetachObject
import Amazonka.CloudDirectory.DetachPolicy
import Amazonka.CloudDirectory.DetachTypedLink
import Amazonka.CloudDirectory.DisableDirectory
import Amazonka.CloudDirectory.EnableDirectory
import Amazonka.CloudDirectory.GetAppliedSchemaVersion
import Amazonka.CloudDirectory.GetDirectory
import Amazonka.CloudDirectory.GetFacet
import Amazonka.CloudDirectory.GetLinkAttributes
import Amazonka.CloudDirectory.GetObjectAttributes
import Amazonka.CloudDirectory.GetObjectInformation
import Amazonka.CloudDirectory.GetSchemaAsJson
import Amazonka.CloudDirectory.GetTypedLinkFacetInformation
import Amazonka.CloudDirectory.Lens
import Amazonka.CloudDirectory.ListAppliedSchemaArns
import Amazonka.CloudDirectory.ListAttachedIndices
import Amazonka.CloudDirectory.ListDevelopmentSchemaArns
import Amazonka.CloudDirectory.ListDirectories
import Amazonka.CloudDirectory.ListFacetAttributes
import Amazonka.CloudDirectory.ListFacetNames
import Amazonka.CloudDirectory.ListIncomingTypedLinks
import Amazonka.CloudDirectory.ListIndex
import Amazonka.CloudDirectory.ListManagedSchemaArns
import Amazonka.CloudDirectory.ListObjectAttributes
import Amazonka.CloudDirectory.ListObjectChildren
import Amazonka.CloudDirectory.ListObjectParentPaths
import Amazonka.CloudDirectory.ListObjectParents
import Amazonka.CloudDirectory.ListObjectPolicies
import Amazonka.CloudDirectory.ListOutgoingTypedLinks
import Amazonka.CloudDirectory.ListPolicyAttachments
import Amazonka.CloudDirectory.ListPublishedSchemaArns
import Amazonka.CloudDirectory.ListTagsForResource
import Amazonka.CloudDirectory.ListTypedLinkFacetAttributes
import Amazonka.CloudDirectory.ListTypedLinkFacetNames
import Amazonka.CloudDirectory.LookupPolicy
import Amazonka.CloudDirectory.PublishSchema
import Amazonka.CloudDirectory.PutSchemaFromJson
import Amazonka.CloudDirectory.RemoveFacetFromObject
import Amazonka.CloudDirectory.TagResource
import Amazonka.CloudDirectory.Types
import Amazonka.CloudDirectory.UntagResource
import Amazonka.CloudDirectory.UpdateFacet
import Amazonka.CloudDirectory.UpdateLinkAttributes
import Amazonka.CloudDirectory.UpdateObjectAttributes
import Amazonka.CloudDirectory.UpdateSchema
import Amazonka.CloudDirectory.UpdateTypedLinkFacet
import Amazonka.CloudDirectory.UpgradeAppliedSchema
import Amazonka.CloudDirectory.UpgradePublishedSchema
import Amazonka.CloudDirectory.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CloudDirectory'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

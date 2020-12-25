{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Cloud Directory__
--
-- Amazon Cloud Directory is a component of the AWS Directory Service that simplifies the development and management of cloud-scale web, mobile, and IoT applications. This guide describes the Cloud Directory operations that you can call programmatically and includes detailed information on data types and errors. For information about Cloud Directory features, see <https://aws.amazon.com/directoryservice/ AWS Directory Service> and the <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/what_is_cloud_directory.html Amazon Cloud Directory Developer Guide> .
module Network.AWS.CloudDirectory
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    -- $errors

    -- ** UnsupportedIndexTypeException
    _UnsupportedIndexTypeException,

    -- ** NotIndexException
    _NotIndexException,

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** FacetAlreadyExistsException
    _FacetAlreadyExistsException,

    -- ** InvalidSchemaDocException
    _InvalidSchemaDocException,

    -- ** InvalidAttachmentException
    _InvalidAttachmentException,

    -- ** CannotListParentOfRootException
    _CannotListParentOfRootException,

    -- ** NotPolicyException
    _NotPolicyException,

    -- ** InvalidTaggingRequestException
    _InvalidTaggingRequestException,

    -- ** InvalidFacetUpdateException
    _InvalidFacetUpdateException,

    -- ** InvalidRuleException
    _InvalidRuleException,

    -- ** SchemaAlreadyPublishedException
    _SchemaAlreadyPublishedException,

    -- ** DirectoryAlreadyExistsException
    _DirectoryAlreadyExistsException,

    -- ** DirectoryNotDisabledException
    _DirectoryNotDisabledException,

    -- ** BatchWriteException
    _BatchWriteException,

    -- ** DirectoryNotEnabledException
    _DirectoryNotEnabledException,

    -- ** FacetInUseException
    _FacetInUseException,

    -- ** FacetValidationException
    _FacetValidationException,

    -- ** StillContainsLinksException
    _StillContainsLinksException,

    -- ** IncompatibleSchemaException
    _IncompatibleSchemaException,

    -- ** NotNodeException
    _NotNodeException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** ObjectAlreadyDetachedException
    _ObjectAlreadyDetachedException,

    -- ** LinkNameAlreadyInUseException
    _LinkNameAlreadyInUseException,

    -- ** InternalServiceException
    _InternalServiceException,

    -- ** SchemaAlreadyExistsException
    _SchemaAlreadyExistsException,

    -- ** IndexedAttributeMissingException
    _IndexedAttributeMissingException,

    -- ** DirectoryDeletedException
    _DirectoryDeletedException,

    -- ** RetryableConflictException
    _RetryableConflictException,

    -- ** InvalidArnException
    _InvalidArnException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** FacetNotFoundException
    _FacetNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ObjectNotDetachedException
    _ObjectNotDetachedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListTypedLinkFacetAttributes (Paginated)
    module Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes,

    -- ** DeleteObject
    module Network.AWS.CloudDirectory.DeleteObject,

    -- ** ListIndex (Paginated)
    module Network.AWS.CloudDirectory.ListIndex,

    -- ** UpgradeAppliedSchema
    module Network.AWS.CloudDirectory.UpgradeAppliedSchema,

    -- ** GetDirectory
    module Network.AWS.CloudDirectory.GetDirectory,

    -- ** GetObjectInformation
    module Network.AWS.CloudDirectory.GetObjectInformation,

    -- ** ListAttachedIndices (Paginated)
    module Network.AWS.CloudDirectory.ListAttachedIndices,

    -- ** DetachFromIndex
    module Network.AWS.CloudDirectory.DetachFromIndex,

    -- ** LookupPolicy (Paginated)
    module Network.AWS.CloudDirectory.LookupPolicy,

    -- ** ListTagsForResource (Paginated)
    module Network.AWS.CloudDirectory.ListTagsForResource,

    -- ** ListPublishedSchemaArns (Paginated)
    module Network.AWS.CloudDirectory.ListPublishedSchemaArns,

    -- ** ListDirectories (Paginated)
    module Network.AWS.CloudDirectory.ListDirectories,

    -- ** CreateTypedLinkFacet
    module Network.AWS.CloudDirectory.CreateTypedLinkFacet,

    -- ** ListObjectParentPaths (Paginated)
    module Network.AWS.CloudDirectory.ListObjectParentPaths,

    -- ** DisableDirectory
    module Network.AWS.CloudDirectory.DisableDirectory,

    -- ** CreateDirectory
    module Network.AWS.CloudDirectory.CreateDirectory,

    -- ** ListFacetAttributes (Paginated)
    module Network.AWS.CloudDirectory.ListFacetAttributes,

    -- ** ListManagedSchemaArns (Paginated)
    module Network.AWS.CloudDirectory.ListManagedSchemaArns,

    -- ** UpdateTypedLinkFacet
    module Network.AWS.CloudDirectory.UpdateTypedLinkFacet,

    -- ** DeleteTypedLinkFacet
    module Network.AWS.CloudDirectory.DeleteTypedLinkFacet,

    -- ** GetAppliedSchemaVersion
    module Network.AWS.CloudDirectory.GetAppliedSchemaVersion,

    -- ** RemoveFacetFromObject
    module Network.AWS.CloudDirectory.RemoveFacetFromObject,

    -- ** EnableDirectory
    module Network.AWS.CloudDirectory.EnableDirectory,

    -- ** ListObjectAttributes (Paginated)
    module Network.AWS.CloudDirectory.ListObjectAttributes,

    -- ** ListAppliedSchemaArns (Paginated)
    module Network.AWS.CloudDirectory.ListAppliedSchemaArns,

    -- ** ListIncomingTypedLinks (Paginated)
    module Network.AWS.CloudDirectory.ListIncomingTypedLinks,

    -- ** GetFacet
    module Network.AWS.CloudDirectory.GetFacet,

    -- ** GetTypedLinkFacetInformation
    module Network.AWS.CloudDirectory.GetTypedLinkFacetInformation,

    -- ** ListDevelopmentSchemaArns (Paginated)
    module Network.AWS.CloudDirectory.ListDevelopmentSchemaArns,

    -- ** AttachObject
    module Network.AWS.CloudDirectory.AttachObject,

    -- ** BatchWrite
    module Network.AWS.CloudDirectory.BatchWrite,

    -- ** CreateObject
    module Network.AWS.CloudDirectory.CreateObject,

    -- ** UpgradePublishedSchema
    module Network.AWS.CloudDirectory.UpgradePublishedSchema,

    -- ** CreateFacet
    module Network.AWS.CloudDirectory.CreateFacet,

    -- ** GetLinkAttributes
    module Network.AWS.CloudDirectory.GetLinkAttributes,

    -- ** GetObjectAttributes
    module Network.AWS.CloudDirectory.GetObjectAttributes,

    -- ** DeleteFacet
    module Network.AWS.CloudDirectory.DeleteFacet,

    -- ** UpdateFacet
    module Network.AWS.CloudDirectory.UpdateFacet,

    -- ** ListObjectChildren
    module Network.AWS.CloudDirectory.ListObjectChildren,

    -- ** ListTypedLinkFacetNames (Paginated)
    module Network.AWS.CloudDirectory.ListTypedLinkFacetNames,

    -- ** AttachTypedLink
    module Network.AWS.CloudDirectory.AttachTypedLink,

    -- ** DetachPolicy
    module Network.AWS.CloudDirectory.DetachPolicy,

    -- ** CreateIndex
    module Network.AWS.CloudDirectory.CreateIndex,

    -- ** DetachObject
    module Network.AWS.CloudDirectory.DetachObject,

    -- ** AddFacetToObject
    module Network.AWS.CloudDirectory.AddFacetToObject,

    -- ** ApplySchema
    module Network.AWS.CloudDirectory.ApplySchema,

    -- ** CreateSchema
    module Network.AWS.CloudDirectory.CreateSchema,

    -- ** GetSchemaAsJson
    module Network.AWS.CloudDirectory.GetSchemaAsJson,

    -- ** PublishSchema
    module Network.AWS.CloudDirectory.PublishSchema,

    -- ** DeleteDirectory
    module Network.AWS.CloudDirectory.DeleteDirectory,

    -- ** ListObjectParents
    module Network.AWS.CloudDirectory.ListObjectParents,

    -- ** ListPolicyAttachments (Paginated)
    module Network.AWS.CloudDirectory.ListPolicyAttachments,

    -- ** TagResource
    module Network.AWS.CloudDirectory.TagResource,

    -- ** UpdateSchema
    module Network.AWS.CloudDirectory.UpdateSchema,

    -- ** DeleteSchema
    module Network.AWS.CloudDirectory.DeleteSchema,

    -- ** DetachTypedLink
    module Network.AWS.CloudDirectory.DetachTypedLink,

    -- ** ListFacetNames (Paginated)
    module Network.AWS.CloudDirectory.ListFacetNames,

    -- ** UntagResource
    module Network.AWS.CloudDirectory.UntagResource,

    -- ** ListOutgoingTypedLinks (Paginated)
    module Network.AWS.CloudDirectory.ListOutgoingTypedLinks,

    -- ** UpdateObjectAttributes
    module Network.AWS.CloudDirectory.UpdateObjectAttributes,

    -- ** AttachPolicy
    module Network.AWS.CloudDirectory.AttachPolicy,

    -- ** BatchRead
    module Network.AWS.CloudDirectory.BatchRead,

    -- ** PutSchemaFromJson
    module Network.AWS.CloudDirectory.PutSchemaFromJson,

    -- ** UpdateLinkAttributes
    module Network.AWS.CloudDirectory.UpdateLinkAttributes,

    -- ** AttachToIndex
    module Network.AWS.CloudDirectory.AttachToIndex,

    -- ** ListObjectPolicies (Paginated)
    module Network.AWS.CloudDirectory.ListObjectPolicies,

    -- * Types

    -- ** BatchListObjectAttributes
    BatchListObjectAttributes (..),
    mkBatchListObjectAttributes,
    bloaObjectReference,
    bloaFacetFilter,
    bloaMaxResults,
    bloaNextToken,

    -- ** BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse (..),
    mkBatchListIncomingTypedLinksResponse,
    blitlrLinkSpecifiers,
    blitlrNextToken,

    -- ** DirectoryArn
    DirectoryArn (..),

    -- ** ExceptionMessage
    ExceptionMessage (..),

    -- ** TypedAttributeValueRange
    TypedAttributeValueRange (..),
    mkTypedAttributeValueRange,
    tavrStartMode,
    tavrEndMode,
    tavrEndValue,
    tavrStartValue,

    -- ** FacetAttributeDefinition
    FacetAttributeDefinition (..),
    mkFacetAttributeDefinition,
    fadType,
    fadDefaultValue,
    fadIsImmutable,
    fadRules,

    -- ** SchemaFacet
    SchemaFacet (..),
    mkSchemaFacet,
    sfFacetName,
    sfSchemaArn,

    -- ** BatchRemoveFacetFromObject
    BatchRemoveFacetFromObject (..),
    mkBatchRemoveFacetFromObject,
    brffoSchemaFacet,
    brffoObjectReference,

    -- ** BatchListObjectAttributesResponse
    BatchListObjectAttributesResponse (..),
    mkBatchListObjectAttributesResponse,
    bloarAttributes,
    bloarNextToken,

    -- ** IndexAttachment
    IndexAttachment (..),
    mkIndexAttachment,
    iaIndexedAttributes,
    iaObjectIdentifier,

    -- ** BatchRemoveFacetFromObjectResponse
    BatchRemoveFacetFromObjectResponse (..),
    mkBatchRemoveFacetFromObjectResponse,

    -- ** BatchListIncomingTypedLinks
    BatchListIncomingTypedLinks (..),
    mkBatchListIncomingTypedLinks,
    blitlObjectReference,
    blitlFilterAttributeRanges,
    blitlFilterTypedLink,
    blitlMaxResults,
    blitlNextToken,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** StringAttributeValue
    StringAttributeValue (..),

    -- ** FacetStyle
    FacetStyle (..),

    -- ** BatchUpdateLinkAttributes
    BatchUpdateLinkAttributes (..),
    mkBatchUpdateLinkAttributes,
    bulaTypedLinkSpecifier,
    bulaAttributeUpdates,

    -- ** FacetName
    FacetName (..),

    -- ** BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse (..),
    mkBatchListObjectParentPathsResponse,
    blopprNextToken,
    blopprPathToObjectIdentifiersList,

    -- ** BatchAttachToIndex
    BatchAttachToIndex (..),
    mkBatchAttachToIndex,
    batiIndexReference,
    batiTargetReference,

    -- ** ObjectIdentifierAndLinkNameTuple
    ObjectIdentifierAndLinkNameTuple (..),
    mkObjectIdentifierAndLinkNameTuple,
    oialntLinkName,
    oialntObjectIdentifier,

    -- ** ObjectType
    ObjectType (..),

    -- ** TypedLinkAttributeDefinition
    TypedLinkAttributeDefinition (..),
    mkTypedLinkAttributeDefinition,
    tladName,
    tladType,
    tladRequiredBehavior,
    tladDefaultValue,
    tladIsImmutable,
    tladRules,

    -- ** BatchCreateObject
    BatchCreateObject (..),
    mkBatchCreateObject,
    bcoSchemaFacet,
    bcoObjectAttributeList,
    bcoBatchReferenceName,
    bcoLinkName,
    bcoParentReference,

    -- ** Arn
    Arn (..),

    -- ** BatchGetLinkAttributes
    BatchGetLinkAttributes (..),
    mkBatchGetLinkAttributes,
    bglaTypedLinkSpecifier,
    bglaAttributeNames,

    -- ** BatchLookupPolicyResponse
    BatchLookupPolicyResponse (..),
    mkBatchLookupPolicyResponse,
    blprNextToken,
    blprPolicyToPathList,

    -- ** BatchGetObjectInformationResponse
    BatchGetObjectInformationResponse (..),
    mkBatchGetObjectInformationResponse,
    bgoirObjectIdentifier,
    bgoirSchemaFacets,

    -- ** BatchDeleteObject
    BatchDeleteObject (..),
    mkBatchDeleteObject,
    bdoObjectReference,

    -- ** BatchListAttachedIndicesResponse
    BatchListAttachedIndicesResponse (..),
    mkBatchListAttachedIndicesResponse,
    blairIndexAttachments,
    blairNextToken,

    -- ** ConsistencyLevel
    ConsistencyLevel (..),

    -- ** RuleParameterKey
    RuleParameterKey (..),

    -- ** BatchListIndex
    BatchListIndex (..),
    mkBatchListIndex,
    bliIndexReference,
    bliMaxResults,
    bliNextToken,
    bliRangesOnIndexedValues,

    -- ** BatchReadOperationResponse
    BatchReadOperationResponse (..),
    mkBatchReadOperationResponse,
    brorExceptionResponse,
    brorSuccessfulResponse,

    -- ** PolicyType
    PolicyType (..),

    -- ** PathString
    PathString (..),

    -- ** BatchDetachFromIndexResponse
    BatchDetachFromIndexResponse (..),
    mkBatchDetachFromIndexResponse,
    bdfirDetachedObjectIdentifier,

    -- ** BatchDetachFromIndex
    BatchDetachFromIndex (..),
    mkBatchDetachFromIndex,
    bdfiIndexReference,
    bdfiTargetReference,

    -- ** AttributeNameAndValue
    AttributeNameAndValue (..),
    mkAttributeNameAndValue,
    anavAttributeName,
    anavValue,

    -- ** BatchListAttachedIndices
    BatchListAttachedIndices (..),
    mkBatchListAttachedIndices,
    blaiTargetReference,
    blaiMaxResults,
    blaiNextToken,

    -- ** BatchListIndexResponse
    BatchListIndexResponse (..),
    mkBatchListIndexResponse,
    blirIndexAttachments,
    blirNextToken,

    -- ** BatchDeleteObjectResponse
    BatchDeleteObjectResponse (..),
    mkBatchDeleteObjectResponse,

    -- ** TypedAttributeValue
    TypedAttributeValue (..),
    mkTypedAttributeValue,
    tavBinaryValue,
    tavBooleanValue,
    tavDatetimeValue,
    tavNumberValue,
    tavStringValue,

    -- ** PathToObjectIdentifiers
    PathToObjectIdentifiers (..),
    mkPathToObjectIdentifiers,
    ptoiObjectIdentifiers,
    ptoiPath,

    -- ** BatchReadOperation
    BatchReadOperation (..),
    mkBatchReadOperation,
    broGetLinkAttributes,
    broGetObjectAttributes,
    broGetObjectInformation,
    broListAttachedIndices,
    broListIncomingTypedLinks,
    broListIndex,
    broListObjectAttributes,
    broListObjectChildren,
    broListObjectParentPaths,
    broListObjectParents,
    broListObjectPolicies,
    broListOutgoingTypedLinks,
    broListPolicyAttachments,
    broLookupPolicy,

    -- ** AttributeKeyAndValue
    AttributeKeyAndValue (..),
    mkAttributeKeyAndValue,
    akavKey,
    akavValue,

    -- ** BatchGetObjectInformation
    BatchGetObjectInformation (..),
    mkBatchGetObjectInformation,
    bgoiObjectReference,

    -- ** ObjectIdentifier
    ObjectIdentifier (..),

    -- ** SchemaJsonDocument
    SchemaJsonDocument (..),

    -- ** BatchListObjectPoliciesResponse
    BatchListObjectPoliciesResponse (..),
    mkBatchListObjectPoliciesResponse,
    bAttachedPolicyIds,
    bNextToken,

    -- ** PolicyToPath
    PolicyToPath (..),
    mkPolicyToPath,
    ptpPath,
    ptpPolicies,

    -- ** SchemaName
    SchemaName (..),

    -- ** BatchCreateIndex
    BatchCreateIndex (..),
    mkBatchCreateIndex,
    bciOrderedIndexedAttributeList,
    bciIsUnique,
    bciBatchReferenceName,
    bciLinkName,
    bciParentReference,

    -- ** BatchDetachObject
    BatchDetachObject (..),
    mkBatchDetachObject,
    bdoParentReference,
    bdoLinkName,
    bdoBatchReferenceName,

    -- ** Rule
    Rule (..),
    mkRule,
    rParameters,
    rType,

    -- ** BatchAddFacetToObject
    BatchAddFacetToObject (..),
    mkBatchAddFacetToObject,
    baftoSchemaFacet,
    baftoObjectAttributeList,
    baftoObjectReference,

    -- ** FacetAttribute
    FacetAttribute (..),
    mkFacetAttribute,
    faName,
    faAttributeDefinition,
    faAttributeReference,
    faRequiredBehavior,

    -- ** RuleKey
    RuleKey (..),

    -- ** BatchAttachToIndexResponse
    BatchAttachToIndexResponse (..),
    mkBatchAttachToIndexResponse,
    batirAttachedObjectIdentifier,

    -- ** BatchUpdateLinkAttributesResponse
    BatchUpdateLinkAttributesResponse (..),
    mkBatchUpdateLinkAttributesResponse,

    -- ** RequiredAttributeBehavior
    RequiredAttributeBehavior (..),

    -- ** BatchReadException
    BatchReadException (..),
    mkBatchReadException,
    breMessage,
    breType,

    -- ** BatchListObjectParentPaths
    BatchListObjectParentPaths (..),
    mkBatchListObjectParentPaths,
    bloppObjectReference,
    bloppMaxResults,
    bloppNextToken,

    -- ** BatchAttachPolicyResponse
    BatchAttachPolicyResponse (..),
    mkBatchAttachPolicyResponse,

    -- ** TypedLinkSchemaAndFacetName
    TypedLinkSchemaAndFacetName (..),
    mkTypedLinkSchemaAndFacetName,
    tlsafnSchemaArn,
    tlsafnTypedLinkName,

    -- ** RangeMode
    RangeMode (..),

    -- ** NextToken
    NextToken (..),

    -- ** BatchUpdateObjectAttributesResponse
    BatchUpdateObjectAttributesResponse (..),
    mkBatchUpdateObjectAttributesResponse,
    buoarObjectIdentifier,

    -- ** TypedLinkFacetAttributeUpdate
    TypedLinkFacetAttributeUpdate (..),
    mkTypedLinkFacetAttributeUpdate,
    tlfauAttribute,
    tlfauAction,

    -- ** BatchListPolicyAttachments
    BatchListPolicyAttachments (..),
    mkBatchListPolicyAttachments,
    blpaPolicyReference,
    blpaMaxResults,
    blpaNextToken,

    -- ** BatchDetachTypedLinkResponse
    BatchDetachTypedLinkResponse (..),
    mkBatchDetachTypedLinkResponse,

    -- ** BatchListObjectParents
    BatchListObjectParents (..),
    mkBatchListObjectParents,
    blopObjectReference,
    blopMaxResults,
    blopNextToken,

    -- ** BatchWriteOperation
    BatchWriteOperation (..),
    mkBatchWriteOperation,
    bwoAddFacetToObject,
    bwoAttachObject,
    bwoAttachPolicy,
    bwoAttachToIndex,
    bwoAttachTypedLink,
    bwoCreateIndex,
    bwoCreateObject,
    bwoDeleteObject,
    bwoDetachFromIndex,
    bwoDetachObject,
    bwoDetachPolicy,
    bwoDetachTypedLink,
    bwoRemoveFacetFromObject,
    bwoUpdateLinkAttributes,
    bwoUpdateObjectAttributes,

    -- ** RuleParameterValue
    RuleParameterValue (..),

    -- ** BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse (..),
    mkBatchListOutgoingTypedLinksResponse,
    blotlrNextToken,
    blotlrTypedLinkSpecifiers,

    -- ** BatchDetachTypedLink
    BatchDetachTypedLink (..),
    mkBatchDetachTypedLink,
    bdtlTypedLinkSpecifier,

    -- ** BatchAttachPolicy
    BatchAttachPolicy (..),
    mkBatchAttachPolicy,
    bapPolicyReference,
    bapObjectReference,

    -- ** ObjectAttributeAction
    ObjectAttributeAction (..),
    mkObjectAttributeAction,
    oaaObjectAttributeActionType,
    oaaObjectAttributeUpdateValue,

    -- ** BatchListOutgoingTypedLinks
    BatchListOutgoingTypedLinks (..),
    mkBatchListOutgoingTypedLinks,
    blotlObjectReference,
    blotlFilterAttributeRanges,
    blotlFilterTypedLink,
    blotlMaxResults,
    blotlNextToken,

    -- ** BatchListObjectParentsResponse
    BatchListObjectParentsResponse (..),
    mkBatchListObjectParentsResponse,
    bloprNextToken,
    bloprParentLinks,

    -- ** BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse (..),
    mkBatchListPolicyAttachmentsResponse,
    blparNextToken,
    blparObjectIdentifiers,

    -- ** UpdateActionType
    UpdateActionType (..),

    -- ** BatchUpdateObjectAttributes
    BatchUpdateObjectAttributes (..),
    mkBatchUpdateObjectAttributes,
    buoaObjectReference,
    buoaAttributeUpdates,

    -- ** TypedLinkAttributeRange
    TypedLinkAttributeRange (..),
    mkTypedLinkAttributeRange,
    tlarRange,
    tlarAttributeName,

    -- ** LinkName
    LinkName (..),

    -- ** BatchWriteOperationResponse
    BatchWriteOperationResponse (..),
    mkBatchWriteOperationResponse,
    bworAddFacetToObject,
    bworAttachObject,
    bworAttachPolicy,
    bworAttachToIndex,
    bworAttachTypedLink,
    bworCreateIndex,
    bworCreateObject,
    bworDeleteObject,
    bworDetachFromIndex,
    bworDetachObject,
    bworDetachPolicy,
    bworDetachTypedLink,
    bworRemoveFacetFromObject,
    bworUpdateLinkAttributes,
    bworUpdateObjectAttributes,

    -- ** BatchAttachObject
    BatchAttachObject (..),
    mkBatchAttachObject,
    baoParentReference,
    baoChildReference,
    baoLinkName,

    -- ** Version
    Version (..),

    -- ** LinkAttributeAction
    LinkAttributeAction (..),
    mkLinkAttributeAction,
    laaAttributeActionType,
    laaAttributeUpdateValue,

    -- ** FacetAttributeReference
    FacetAttributeReference (..),
    mkFacetAttributeReference,
    farTargetFacetName,
    farTargetAttributeName,

    -- ** BatchListObjectPolicies
    BatchListObjectPolicies (..),
    mkBatchListObjectPolicies,
    blopsObjectReference,
    blopsMaxResults,
    blopsNextToken,

    -- ** BatchAddFacetToObjectResponse
    BatchAddFacetToObjectResponse (..),
    mkBatchAddFacetToObjectResponse,

    -- ** Facet
    Facet (..),
    mkFacet,
    fFacetStyle,
    fName,
    fObjectType,

    -- ** LinkAttributeUpdate
    LinkAttributeUpdate (..),
    mkLinkAttributeUpdate,
    lauAttributeAction,
    lauAttributeKey,

    -- ** BatchDetachObjectResponse
    BatchDetachObjectResponse (..),
    mkBatchDetachObjectResponse,
    bdorDetachedObjectIdentifier,

    -- ** BatchCreateIndexResponse
    BatchCreateIndexResponse (..),
    mkBatchCreateIndexResponse,
    bcirObjectIdentifier,

    -- ** TagKey
    TagKey (..),

    -- ** BatchReferenceName
    BatchReferenceName (..),

    -- ** BatchListObjectChildren
    BatchListObjectChildren (..),
    mkBatchListObjectChildren,
    blocObjectReference,
    blocMaxResults,
    blocNextToken,

    -- ** ObjectAttributeUpdate
    ObjectAttributeUpdate (..),
    mkObjectAttributeUpdate,
    oauObjectAttributeAction,
    oauObjectAttributeKey,

    -- ** FacetAttributeUpdate
    FacetAttributeUpdate (..),
    mkFacetAttributeUpdate,
    fauAction,
    fauAttribute,

    -- ** BatchAttachTypedLinkResponse
    BatchAttachTypedLinkResponse (..),
    mkBatchAttachTypedLinkResponse,
    batlrTypedLinkSpecifier,

    -- ** BatchDetachPolicyResponse
    BatchDetachPolicyResponse (..),
    mkBatchDetachPolicyResponse,

    -- ** TypedLinkSpecifier
    TypedLinkSpecifier (..),
    mkTypedLinkSpecifier,
    tlsTypedLinkFacet,
    tlsSourceObjectReference,
    tlsTargetObjectReference,
    tlsIdentityAttributeValues,

    -- ** BatchGetObjectAttributes
    BatchGetObjectAttributes (..),
    mkBatchGetObjectAttributes,
    bgoaObjectReference,
    bgoaSchemaFacet,
    bgoaAttributeNames,

    -- ** TypedLinkName
    TypedLinkName (..),

    -- ** SelectorObjectReference
    SelectorObjectReference (..),

    -- ** AttributeKey
    AttributeKey (..),
    mkAttributeKey,
    akSchemaArn,
    akFacetName,
    akName,

    -- ** BatchAttachTypedLink
    BatchAttachTypedLink (..),
    mkBatchAttachTypedLink,
    batlSourceObjectReference,
    batlTargetObjectReference,
    batlTypedLinkFacet,
    batlAttributes,

    -- ** BatchGetObjectAttributesResponse
    BatchGetObjectAttributesResponse (..),
    mkBatchGetObjectAttributesResponse,
    bgoarAttributes,

    -- ** AttributeName
    AttributeName (..),

    -- ** BatchListObjectChildrenResponse
    BatchListObjectChildrenResponse (..),
    mkBatchListObjectChildrenResponse,
    blocrChildren,
    blocrNextToken,

    -- ** BatchDetachPolicy
    BatchDetachPolicy (..),
    mkBatchDetachPolicy,
    bdpPolicyReference,
    bdpObjectReference,

    -- ** BatchLookupPolicy
    BatchLookupPolicy (..),
    mkBatchLookupPolicy,
    blpObjectReference,
    blpMaxResults,
    blpNextToken,

    -- ** BatchCreateObjectResponse
    BatchCreateObjectResponse (..),
    mkBatchCreateObjectResponse,
    bcorObjectIdentifier,

    -- ** RuleType
    RuleType (..),

    -- ** DirectoryName
    DirectoryName (..),

    -- ** ObjectAttributeRange
    ObjectAttributeRange (..),
    mkObjectAttributeRange,
    oarAttributeKey,
    oarRange,

    -- ** BatchGetLinkAttributesResponse
    BatchGetLinkAttributesResponse (..),
    mkBatchGetLinkAttributesResponse,
    bglarAttributes,

    -- ** DirectoryState
    DirectoryState (..),

    -- ** ObjectReference
    ObjectReference (..),
    mkObjectReference,
    orSelector,

    -- ** BatchReadSuccessfulResponse
    BatchReadSuccessfulResponse (..),
    mkBatchReadSuccessfulResponse,
    brsrGetLinkAttributes,
    brsrGetObjectAttributes,
    brsrGetObjectInformation,
    brsrListAttachedIndices,
    brsrListIncomingTypedLinks,
    brsrListIndex,
    brsrListObjectAttributes,
    brsrListObjectChildren,
    brsrListObjectParentPaths,
    brsrListObjectParents,
    brsrListObjectPolicies,
    brsrListOutgoingTypedLinks,
    brsrListPolicyAttachments,
    brsrLookupPolicy,

    -- ** BatchAttachObjectResponse
    BatchAttachObjectResponse (..),
    mkBatchAttachObjectResponse,
    baorAttachedObjectIdentifier,

    -- ** FacetAttributeType
    FacetAttributeType (..),

    -- ** Directory
    Directory (..),
    mkDirectory,
    dCreationDateTime,
    dDirectoryArn,
    dName,
    dState,

    -- ** BatchReadExceptionType
    BatchReadExceptionType (..),

    -- ** TypedLinkFacet
    TypedLinkFacet (..),
    mkTypedLinkFacet,
    tlfName,
    tlfAttributes,
    tlfIdentityAttributeOrder,

    -- ** PolicyAttachment
    PolicyAttachment (..),
    mkPolicyAttachment,
    paObjectIdentifier,
    paPolicyId,
    paPolicyType,

    -- ** SchemaArn
    SchemaArn (..),

    -- ** Name
    Name (..),

    -- ** DetachedObjectIdentifier
    DetachedObjectIdentifier (..),

    -- ** UpgradedSchemaArn
    UpgradedSchemaArn (..),

    -- ** Key
    Key (..),

    -- ** Value
    Value (..),

    -- ** AttachedObjectIdentifier
    AttachedObjectIdentifier (..),

    -- ** MinorVersion
    MinorVersion (..),

    -- ** NumberValue
    NumberValue (..),

    -- ** TargetAttributeName
    TargetAttributeName (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.UntagResource
import Network.AWS.CloudDirectory.UpdateFacet
import Network.AWS.CloudDirectory.UpdateLinkAttributes
import Network.AWS.CloudDirectory.UpdateObjectAttributes
import Network.AWS.CloudDirectory.UpdateSchema
import Network.AWS.CloudDirectory.UpdateTypedLinkFacet
import Network.AWS.CloudDirectory.UpgradeAppliedSchema
import Network.AWS.CloudDirectory.UpgradePublishedSchema
import Network.AWS.CloudDirectory.Waiters
import qualified Network.AWS.Prelude as Lude

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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    cloudDirectoryService,

    -- * Errors
    -- $errors

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

    -- ** ListPublishedSchemaARNs (Paginated)
    module Network.AWS.CloudDirectory.ListPublishedSchemaARNs,

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

    -- ** ListManagedSchemaARNs (Paginated)
    module Network.AWS.CloudDirectory.ListManagedSchemaARNs,

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

    -- ** ListAppliedSchemaARNs (Paginated)
    module Network.AWS.CloudDirectory.ListAppliedSchemaARNs,

    -- ** ListIncomingTypedLinks (Paginated)
    module Network.AWS.CloudDirectory.ListIncomingTypedLinks,

    -- ** GetFacet
    module Network.AWS.CloudDirectory.GetFacet,

    -- ** GetTypedLinkFacetInformation
    module Network.AWS.CloudDirectory.GetTypedLinkFacetInformation,

    -- ** ListDevelopmentSchemaARNs (Paginated)
    module Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs,

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

    -- ** GetSchemaAsJSON
    module Network.AWS.CloudDirectory.GetSchemaAsJSON,

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

    -- ** PutSchemaFromJSON
    module Network.AWS.CloudDirectory.PutSchemaFromJSON,

    -- ** UpdateLinkAttributes
    module Network.AWS.CloudDirectory.UpdateLinkAttributes,

    -- ** AttachToIndex
    module Network.AWS.CloudDirectory.AttachToIndex,

    -- ** ListObjectPolicies (Paginated)
    module Network.AWS.CloudDirectory.ListObjectPolicies,

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
    AttributeKey (..),
    mkAttributeKey,
    akSchemaARN,
    akFacetName,
    akName,

    -- ** AttributeKeyAndValue
    AttributeKeyAndValue (..),
    mkAttributeKeyAndValue,
    akavKey,
    akavValue,

    -- ** AttributeNameAndValue
    AttributeNameAndValue (..),
    mkAttributeNameAndValue,
    anavAttributeName,
    anavValue,

    -- ** BatchAddFacetToObject
    BatchAddFacetToObject (..),
    mkBatchAddFacetToObject,
    baftoSchemaFacet,
    baftoObjectAttributeList,
    baftoObjectReference,

    -- ** BatchAddFacetToObjectResponse
    BatchAddFacetToObjectResponse (..),
    mkBatchAddFacetToObjectResponse,

    -- ** BatchAttachObject
    BatchAttachObject (..),
    mkBatchAttachObject,
    baoParentReference,
    baoChildReference,
    baoLinkName,

    -- ** BatchAttachObjectResponse
    BatchAttachObjectResponse (..),
    mkBatchAttachObjectResponse,
    baoAttachedObjectIdentifier,

    -- ** BatchAttachPolicy
    BatchAttachPolicy (..),
    mkBatchAttachPolicy,
    bapPolicyReference,
    bapObjectReference,

    -- ** BatchAttachPolicyResponse
    BatchAttachPolicyResponse (..),
    mkBatchAttachPolicyResponse,

    -- ** BatchAttachToIndex
    BatchAttachToIndex (..),
    mkBatchAttachToIndex,
    batiIndexReference,
    batiTargetReference,

    -- ** BatchAttachToIndexResponse
    BatchAttachToIndexResponse (..),
    mkBatchAttachToIndexResponse,
    batiAttachedObjectIdentifier,

    -- ** BatchAttachTypedLink
    BatchAttachTypedLink (..),
    mkBatchAttachTypedLink,
    batlSourceObjectReference,
    batlTargetObjectReference,
    batlTypedLinkFacet,
    batlAttributes,

    -- ** BatchAttachTypedLinkResponse
    BatchAttachTypedLinkResponse (..),
    mkBatchAttachTypedLinkResponse,
    batlTypedLinkSpecifier,

    -- ** BatchCreateIndex
    BatchCreateIndex (..),
    mkBatchCreateIndex,
    bciParentReference,
    bciLinkName,
    bciBatchReferenceName,
    bciOrderedIndexedAttributeList,
    bciIsUnique,

    -- ** BatchCreateIndexResponse
    BatchCreateIndexResponse (..),
    mkBatchCreateIndexResponse,
    bciObjectIdentifier,

    -- ** BatchCreateObject
    BatchCreateObject (..),
    mkBatchCreateObject,
    bcoParentReference,
    bcoLinkName,
    bcoBatchReferenceName,
    bcoSchemaFacet,
    bcoObjectAttributeList,

    -- ** BatchCreateObjectResponse
    BatchCreateObjectResponse (..),
    mkBatchCreateObjectResponse,
    bcoObjectIdentifier,

    -- ** BatchDeleteObject
    BatchDeleteObject (..),
    mkBatchDeleteObject,
    bdoObjectReference,

    -- ** BatchDeleteObjectResponse
    BatchDeleteObjectResponse (..),
    mkBatchDeleteObjectResponse,

    -- ** BatchDetachFromIndex
    BatchDetachFromIndex (..),
    mkBatchDetachFromIndex,
    bdfiIndexReference,
    bdfiTargetReference,

    -- ** BatchDetachFromIndexResponse
    BatchDetachFromIndexResponse (..),
    mkBatchDetachFromIndexResponse,
    bdfiDetachedObjectIdentifier,

    -- ** BatchDetachObject
    BatchDetachObject (..),
    mkBatchDetachObject,
    bdoBatchReferenceName,
    bdoParentReference,
    bdoLinkName,

    -- ** BatchDetachObjectResponse
    BatchDetachObjectResponse (..),
    mkBatchDetachObjectResponse,
    bdoDetachedObjectIdentifier,

    -- ** BatchDetachPolicy
    BatchDetachPolicy (..),
    mkBatchDetachPolicy,
    bdpPolicyReference,
    bdpObjectReference,

    -- ** BatchDetachPolicyResponse
    BatchDetachPolicyResponse (..),
    mkBatchDetachPolicyResponse,

    -- ** BatchDetachTypedLink
    BatchDetachTypedLink (..),
    mkBatchDetachTypedLink,
    bdtlTypedLinkSpecifier,

    -- ** BatchDetachTypedLinkResponse
    BatchDetachTypedLinkResponse (..),
    mkBatchDetachTypedLinkResponse,

    -- ** BatchGetLinkAttributes
    BatchGetLinkAttributes (..),
    mkBatchGetLinkAttributes,
    bglaTypedLinkSpecifier,
    bglaAttributeNames,

    -- ** BatchGetLinkAttributesResponse
    BatchGetLinkAttributesResponse (..),
    mkBatchGetLinkAttributesResponse,
    bglaAttributes,

    -- ** BatchGetObjectAttributes
    BatchGetObjectAttributes (..),
    mkBatchGetObjectAttributes,
    bgoaObjectReference,
    bgoaSchemaFacet,
    bgoaAttributeNames,

    -- ** BatchGetObjectAttributesResponse
    BatchGetObjectAttributesResponse (..),
    mkBatchGetObjectAttributesResponse,
    bgoaAttributes,

    -- ** BatchGetObjectInformation
    BatchGetObjectInformation (..),
    mkBatchGetObjectInformation,
    bgoiObjectReference,

    -- ** BatchGetObjectInformationResponse
    BatchGetObjectInformationResponse (..),
    mkBatchGetObjectInformationResponse,
    bgoiObjectIdentifier,
    bgoiSchemaFacets,

    -- ** BatchListAttachedIndices
    BatchListAttachedIndices (..),
    mkBatchListAttachedIndices,
    blaisNextToken,
    blaisMaxResults,
    blaisTargetReference,

    -- ** BatchListAttachedIndicesResponse
    BatchListAttachedIndicesResponse (..),
    mkBatchListAttachedIndicesResponse,
    blaiIndexAttachments,
    blaiNextToken,

    -- ** BatchListIncomingTypedLinks
    BatchListIncomingTypedLinks (..),
    mkBatchListIncomingTypedLinks,
    blitlsFilterAttributeRanges,
    blitlsNextToken,
    blitlsFilterTypedLink,
    blitlsMaxResults,
    blitlsObjectReference,

    -- ** BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse (..),
    mkBatchListIncomingTypedLinksResponse,
    blitlLinkSpecifiers,
    blitlNextToken,

    -- ** BatchListIndex
    BatchListIndex (..),
    mkBatchListIndex,
    batRangesOnIndexedValues,
    batNextToken,
    batMaxResults,
    batIndexReference,

    -- ** BatchListIndexResponse
    BatchListIndexResponse (..),
    mkBatchListIndexResponse,
    bliIndexAttachments,
    bliNextToken,

    -- ** BatchListObjectAttributes
    BatchListObjectAttributes (..),
    mkBatchListObjectAttributes,
    bloaFacetFilter,
    bloaNextToken,
    bloaMaxResults,
    bloaObjectReference,

    -- ** BatchListObjectAttributesResponse
    BatchListObjectAttributesResponse (..),
    mkBatchListObjectAttributesResponse,
    bNextToken,
    bAttributes,

    -- ** BatchListObjectChildren
    BatchListObjectChildren (..),
    mkBatchListObjectChildren,
    bloclNextToken,
    bloclMaxResults,
    bloclObjectReference,

    -- ** BatchListObjectChildrenResponse
    BatchListObjectChildrenResponse (..),
    mkBatchListObjectChildrenResponse,
    blocChildren,
    blocNextToken,

    -- ** BatchListObjectParentPaths
    BatchListObjectParentPaths (..),
    mkBatchListObjectParentPaths,
    bloppsNextToken,
    bloppsMaxResults,
    bloppsObjectReference,

    -- ** BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse (..),
    mkBatchListObjectParentPathsResponse,
    bloppPathToObjectIdentifiersList,
    bloppNextToken,

    -- ** BatchListObjectParents
    BatchListObjectParents (..),
    mkBatchListObjectParents,
    bloplNextToken,
    bloplMaxResults,
    bloplObjectReference,

    -- ** BatchListObjectParentsResponse
    BatchListObjectParentsResponse (..),
    mkBatchListObjectParentsResponse,
    blopNextToken,
    blopParentLinks,

    -- ** BatchListObjectPolicies
    BatchListObjectPolicies (..),
    mkBatchListObjectPolicies,
    bbNextToken,
    bbMaxResults,
    bbObjectReference,

    -- ** BatchListObjectPoliciesResponse
    BatchListObjectPoliciesResponse (..),
    mkBatchListObjectPoliciesResponse,
    blopsNextToken,
    blopsAttachedPolicyIds,

    -- ** BatchListOutgoingTypedLinks
    BatchListOutgoingTypedLinks (..),
    mkBatchListOutgoingTypedLinks,
    blotlsFilterAttributeRanges,
    blotlsNextToken,
    blotlsFilterTypedLink,
    blotlsMaxResults,
    blotlsObjectReference,

    -- ** BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse (..),
    mkBatchListOutgoingTypedLinksResponse,
    blotlTypedLinkSpecifiers,
    blotlNextToken,

    -- ** BatchListPolicyAttachments
    BatchListPolicyAttachments (..),
    mkBatchListPolicyAttachments,
    blpasNextToken,
    blpasMaxResults,
    blpasPolicyReference,

    -- ** BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse (..),
    mkBatchListPolicyAttachmentsResponse,
    blpaObjectIdentifiers,
    blpaNextToken,

    -- ** BatchLookupPolicy
    BatchLookupPolicy (..),
    mkBatchLookupPolicy,
    blplNextToken,
    blplMaxResults,
    blplObjectReference,

    -- ** BatchLookupPolicyResponse
    BatchLookupPolicyResponse (..),
    mkBatchLookupPolicyResponse,
    blpNextToken,
    blpPolicyToPathList,

    -- ** BatchReadException
    BatchReadException (..),
    mkBatchReadException,
    breType,
    breMessage,

    -- ** BatchReadOperation
    BatchReadOperation (..),
    mkBatchReadOperation,
    broListIndex,
    broGetObjectInformation,
    broListAttachedIndices,
    broLookupPolicy,
    broListObjectParentPaths,
    broListObjectAttributes,
    broListIncomingTypedLinks,
    broGetLinkAttributes,
    broGetObjectAttributes,
    broListObjectChildren,
    broListObjectParents,
    broListPolicyAttachments,
    broListOutgoingTypedLinks,
    broListObjectPolicies,

    -- ** BatchReadOperationResponse
    BatchReadOperationResponse (..),
    mkBatchReadOperationResponse,
    broExceptionResponse,
    broSuccessfulResponse,

    -- ** BatchReadSuccessfulResponse
    BatchReadSuccessfulResponse (..),
    mkBatchReadSuccessfulResponse,
    brsListIndex,
    brsGetObjectInformation,
    brsListAttachedIndices,
    brsLookupPolicy,
    brsListObjectParentPaths,
    brsListObjectAttributes,
    brsListIncomingTypedLinks,
    brsGetLinkAttributes,
    brsGetObjectAttributes,
    brsListObjectChildren,
    brsListObjectParents,
    brsListPolicyAttachments,
    brsListOutgoingTypedLinks,
    brsListObjectPolicies,

    -- ** BatchRemoveFacetFromObject
    BatchRemoveFacetFromObject (..),
    mkBatchRemoveFacetFromObject,
    brffoSchemaFacet,
    brffoObjectReference,

    -- ** BatchRemoveFacetFromObjectResponse
    BatchRemoveFacetFromObjectResponse (..),
    mkBatchRemoveFacetFromObjectResponse,

    -- ** BatchUpdateLinkAttributes
    BatchUpdateLinkAttributes (..),
    mkBatchUpdateLinkAttributes,
    bulaTypedLinkSpecifier,
    bulaAttributeUpdates,

    -- ** BatchUpdateLinkAttributesResponse
    BatchUpdateLinkAttributesResponse (..),
    mkBatchUpdateLinkAttributesResponse,

    -- ** BatchUpdateObjectAttributes
    BatchUpdateObjectAttributes (..),
    mkBatchUpdateObjectAttributes,
    buoaObjectReference,
    buoaAttributeUpdates,

    -- ** BatchUpdateObjectAttributesResponse
    BatchUpdateObjectAttributesResponse (..),
    mkBatchUpdateObjectAttributesResponse,
    buoaObjectIdentifier,

    -- ** BatchWriteOperation
    BatchWriteOperation (..),
    mkBatchWriteOperation,
    bDeleteObject,
    bDetachFromIndex,
    bRemoveFacetFromObject,
    bAttachObject,
    bCreateObject,
    bAttachTypedLink,
    bDetachPolicy,
    bCreateIndex,
    bDetachObject,
    bAddFacetToObject,
    bDetachTypedLink,
    bUpdateObjectAttributes,
    bAttachPolicy,
    bUpdateLinkAttributes,
    bAttachToIndex,

    -- ** BatchWriteOperationResponse
    BatchWriteOperationResponse (..),
    mkBatchWriteOperationResponse,
    bwoDeleteObject,
    bwoDetachFromIndex,
    bwoRemoveFacetFromObject,
    bwoAttachObject,
    bwoCreateObject,
    bwoAttachTypedLink,
    bwoDetachPolicy,
    bwoCreateIndex,
    bwoDetachObject,
    bwoAddFacetToObject,
    bwoDetachTypedLink,
    bwoUpdateObjectAttributes,
    bwoAttachPolicy,
    bwoUpdateLinkAttributes,
    bwoAttachToIndex,

    -- ** Directory
    Directory (..),
    mkDirectory,
    dDirectoryARN,
    dState,
    dName,
    dCreationDateTime,

    -- ** Facet
    Facet (..),
    mkFacet,
    fFacetStyle,
    fObjectType,
    fName,

    -- ** FacetAttribute
    FacetAttribute (..),
    mkFacetAttribute,
    faAttributeReference,
    faAttributeDefinition,
    faRequiredBehavior,
    faName,

    -- ** FacetAttributeDefinition
    FacetAttributeDefinition (..),
    mkFacetAttributeDefinition,
    fadRules,
    fadDefaultValue,
    fadIsImmutable,
    fadType,

    -- ** FacetAttributeReference
    FacetAttributeReference (..),
    mkFacetAttributeReference,
    farTargetFacetName,
    farTargetAttributeName,

    -- ** FacetAttributeUpdate
    FacetAttributeUpdate (..),
    mkFacetAttributeUpdate,
    fauAttribute,
    fauAction,

    -- ** IndexAttachment
    IndexAttachment (..),
    mkIndexAttachment,
    iaIndexedAttributes,
    iaObjectIdentifier,

    -- ** LinkAttributeAction
    LinkAttributeAction (..),
    mkLinkAttributeAction,
    laaAttributeActionType,
    laaAttributeUpdateValue,

    -- ** LinkAttributeUpdate
    LinkAttributeUpdate (..),
    mkLinkAttributeUpdate,
    lauAttributeAction,
    lauAttributeKey,

    -- ** ObjectAttributeAction
    ObjectAttributeAction (..),
    mkObjectAttributeAction,
    oaaObjectAttributeActionType,
    oaaObjectAttributeUpdateValue,

    -- ** ObjectAttributeRange
    ObjectAttributeRange (..),
    mkObjectAttributeRange,
    oarRange,
    oarAttributeKey,

    -- ** ObjectAttributeUpdate
    ObjectAttributeUpdate (..),
    mkObjectAttributeUpdate,
    oauObjectAttributeAction,
    oauObjectAttributeKey,

    -- ** ObjectIdentifierAndLinkNameTuple
    ObjectIdentifierAndLinkNameTuple (..),
    mkObjectIdentifierAndLinkNameTuple,
    oialntObjectIdentifier,
    oialntLinkName,

    -- ** ObjectReference
    ObjectReference (..),
    mkObjectReference,
    orSelector,

    -- ** PathToObjectIdentifiers
    PathToObjectIdentifiers (..),
    mkPathToObjectIdentifiers,
    ptoiObjectIdentifiers,
    ptoiPath,

    -- ** PolicyAttachment
    PolicyAttachment (..),
    mkPolicyAttachment,
    paPolicyId,
    paPolicyType,
    paObjectIdentifier,

    -- ** PolicyToPath
    PolicyToPath (..),
    mkPolicyToPath,
    ptpPath,
    ptpPolicies,

    -- ** Rule
    Rule (..),
    mkRule,
    rParameters,
    rType,

    -- ** SchemaFacet
    SchemaFacet (..),
    mkSchemaFacet,
    sfFacetName,
    sfSchemaARN,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TypedAttributeValue
    TypedAttributeValue (..),
    mkTypedAttributeValue,
    tavBinaryValue,
    tavDatetimeValue,
    tavNumberValue,
    tavStringValue,
    tavBooleanValue,

    -- ** TypedAttributeValueRange
    TypedAttributeValueRange (..),
    mkTypedAttributeValueRange,
    tavrEndValue,
    tavrStartValue,
    tavrStartMode,
    tavrEndMode,

    -- ** TypedLinkAttributeDefinition
    TypedLinkAttributeDefinition (..),
    mkTypedLinkAttributeDefinition,
    tladRules,
    tladDefaultValue,
    tladIsImmutable,
    tladName,
    tladType,
    tladRequiredBehavior,

    -- ** TypedLinkAttributeRange
    TypedLinkAttributeRange (..),
    mkTypedLinkAttributeRange,
    tlarAttributeName,
    tlarRange,

    -- ** TypedLinkFacet
    TypedLinkFacet (..),
    mkTypedLinkFacet,
    tlfName,
    tlfAttributes,
    tlfIdentityAttributeOrder,

    -- ** TypedLinkFacetAttributeUpdate
    TypedLinkFacetAttributeUpdate (..),
    mkTypedLinkFacetAttributeUpdate,
    tlfauAttribute,
    tlfauAction,

    -- ** TypedLinkSchemaAndFacetName
    TypedLinkSchemaAndFacetName (..),
    mkTypedLinkSchemaAndFacetName,
    tlsafnSchemaARN,
    tlsafnTypedLinkName,

    -- ** TypedLinkSpecifier
    TypedLinkSpecifier (..),
    mkTypedLinkSpecifier,
    tlsTypedLinkFacet,
    tlsSourceObjectReference,
    tlsTargetObjectReference,
    tlsIdentityAttributeValues,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import Network.AWS.CloudDirectory.GetSchemaAsJSON
import Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
import Network.AWS.CloudDirectory.ListAppliedSchemaARNs
import Network.AWS.CloudDirectory.ListAttachedIndices
import Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs
import Network.AWS.CloudDirectory.ListDirectories
import Network.AWS.CloudDirectory.ListFacetAttributes
import Network.AWS.CloudDirectory.ListFacetNames
import Network.AWS.CloudDirectory.ListIncomingTypedLinks
import Network.AWS.CloudDirectory.ListIndex
import Network.AWS.CloudDirectory.ListManagedSchemaARNs
import Network.AWS.CloudDirectory.ListObjectAttributes
import Network.AWS.CloudDirectory.ListObjectChildren
import Network.AWS.CloudDirectory.ListObjectParentPaths
import Network.AWS.CloudDirectory.ListObjectParents
import Network.AWS.CloudDirectory.ListObjectPolicies
import Network.AWS.CloudDirectory.ListOutgoingTypedLinks
import Network.AWS.CloudDirectory.ListPolicyAttachments
import Network.AWS.CloudDirectory.ListPublishedSchemaARNs
import Network.AWS.CloudDirectory.ListTagsForResource
import Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
import Network.AWS.CloudDirectory.ListTypedLinkFacetNames
import Network.AWS.CloudDirectory.LookupPolicy
import Network.AWS.CloudDirectory.PublishSchema
import Network.AWS.CloudDirectory.PutSchemaFromJSON
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

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types
  ( -- * Service configuration
    cloudDirectoryService,

    -- * Errors

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
    mkAttributeKey,
    akSchemaARN,
    akFacetName,
    akName,

    -- * AttributeKeyAndValue
    AttributeKeyAndValue (..),
    mkAttributeKeyAndValue,
    akavKey,
    akavValue,

    -- * AttributeNameAndValue
    AttributeNameAndValue (..),
    mkAttributeNameAndValue,
    anavAttributeName,
    anavValue,

    -- * BatchAddFacetToObject
    BatchAddFacetToObject (..),
    mkBatchAddFacetToObject,
    baftoSchemaFacet,
    baftoObjectAttributeList,
    baftoObjectReference,

    -- * BatchAddFacetToObjectResponse
    BatchAddFacetToObjectResponse (..),
    mkBatchAddFacetToObjectResponse,

    -- * BatchAttachObject
    BatchAttachObject (..),
    mkBatchAttachObject,
    baoParentReference,
    baoChildReference,
    baoLinkName,

    -- * BatchAttachObjectResponse
    BatchAttachObjectResponse (..),
    mkBatchAttachObjectResponse,
    baoAttachedObjectIdentifier,

    -- * BatchAttachPolicy
    BatchAttachPolicy (..),
    mkBatchAttachPolicy,
    bapPolicyReference,
    bapObjectReference,

    -- * BatchAttachPolicyResponse
    BatchAttachPolicyResponse (..),
    mkBatchAttachPolicyResponse,

    -- * BatchAttachToIndex
    BatchAttachToIndex (..),
    mkBatchAttachToIndex,
    batiIndexReference,
    batiTargetReference,

    -- * BatchAttachToIndexResponse
    BatchAttachToIndexResponse (..),
    mkBatchAttachToIndexResponse,
    batiAttachedObjectIdentifier,

    -- * BatchAttachTypedLink
    BatchAttachTypedLink (..),
    mkBatchAttachTypedLink,
    batlSourceObjectReference,
    batlTargetObjectReference,
    batlTypedLinkFacet,
    batlAttributes,

    -- * BatchAttachTypedLinkResponse
    BatchAttachTypedLinkResponse (..),
    mkBatchAttachTypedLinkResponse,
    batlTypedLinkSpecifier,

    -- * BatchCreateIndex
    BatchCreateIndex (..),
    mkBatchCreateIndex,
    bciParentReference,
    bciLinkName,
    bciBatchReferenceName,
    bciOrderedIndexedAttributeList,
    bciIsUnique,

    -- * BatchCreateIndexResponse
    BatchCreateIndexResponse (..),
    mkBatchCreateIndexResponse,
    bciObjectIdentifier,

    -- * BatchCreateObject
    BatchCreateObject (..),
    mkBatchCreateObject,
    bcoParentReference,
    bcoLinkName,
    bcoBatchReferenceName,
    bcoSchemaFacet,
    bcoObjectAttributeList,

    -- * BatchCreateObjectResponse
    BatchCreateObjectResponse (..),
    mkBatchCreateObjectResponse,
    bcoObjectIdentifier,

    -- * BatchDeleteObject
    BatchDeleteObject (..),
    mkBatchDeleteObject,
    bdoObjectReference,

    -- * BatchDeleteObjectResponse
    BatchDeleteObjectResponse (..),
    mkBatchDeleteObjectResponse,

    -- * BatchDetachFromIndex
    BatchDetachFromIndex (..),
    mkBatchDetachFromIndex,
    bdfiIndexReference,
    bdfiTargetReference,

    -- * BatchDetachFromIndexResponse
    BatchDetachFromIndexResponse (..),
    mkBatchDetachFromIndexResponse,
    bdfiDetachedObjectIdentifier,

    -- * BatchDetachObject
    BatchDetachObject (..),
    mkBatchDetachObject,
    bdoBatchReferenceName,
    bdoParentReference,
    bdoLinkName,

    -- * BatchDetachObjectResponse
    BatchDetachObjectResponse (..),
    mkBatchDetachObjectResponse,
    bdoDetachedObjectIdentifier,

    -- * BatchDetachPolicy
    BatchDetachPolicy (..),
    mkBatchDetachPolicy,
    bdpPolicyReference,
    bdpObjectReference,

    -- * BatchDetachPolicyResponse
    BatchDetachPolicyResponse (..),
    mkBatchDetachPolicyResponse,

    -- * BatchDetachTypedLink
    BatchDetachTypedLink (..),
    mkBatchDetachTypedLink,
    bdtlTypedLinkSpecifier,

    -- * BatchDetachTypedLinkResponse
    BatchDetachTypedLinkResponse (..),
    mkBatchDetachTypedLinkResponse,

    -- * BatchGetLinkAttributes
    BatchGetLinkAttributes (..),
    mkBatchGetLinkAttributes,
    bglaTypedLinkSpecifier,
    bglaAttributeNames,

    -- * BatchGetLinkAttributesResponse
    BatchGetLinkAttributesResponse (..),
    mkBatchGetLinkAttributesResponse,
    bglaAttributes,

    -- * BatchGetObjectAttributes
    BatchGetObjectAttributes (..),
    mkBatchGetObjectAttributes,
    bgoaObjectReference,
    bgoaSchemaFacet,
    bgoaAttributeNames,

    -- * BatchGetObjectAttributesResponse
    BatchGetObjectAttributesResponse (..),
    mkBatchGetObjectAttributesResponse,
    bgoaAttributes,

    -- * BatchGetObjectInformation
    BatchGetObjectInformation (..),
    mkBatchGetObjectInformation,
    bgoiObjectReference,

    -- * BatchGetObjectInformationResponse
    BatchGetObjectInformationResponse (..),
    mkBatchGetObjectInformationResponse,
    bgoiObjectIdentifier,
    bgoiSchemaFacets,

    -- * BatchListAttachedIndices
    BatchListAttachedIndices (..),
    mkBatchListAttachedIndices,
    blaisNextToken,
    blaisMaxResults,
    blaisTargetReference,

    -- * BatchListAttachedIndicesResponse
    BatchListAttachedIndicesResponse (..),
    mkBatchListAttachedIndicesResponse,
    blaiIndexAttachments,
    blaiNextToken,

    -- * BatchListIncomingTypedLinks
    BatchListIncomingTypedLinks (..),
    mkBatchListIncomingTypedLinks,
    blitlsFilterAttributeRanges,
    blitlsNextToken,
    blitlsFilterTypedLink,
    blitlsMaxResults,
    blitlsObjectReference,

    -- * BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse (..),
    mkBatchListIncomingTypedLinksResponse,
    blitlLinkSpecifiers,
    blitlNextToken,

    -- * BatchListIndex
    BatchListIndex (..),
    mkBatchListIndex,
    batRangesOnIndexedValues,
    batNextToken,
    batMaxResults,
    batIndexReference,

    -- * BatchListIndexResponse
    BatchListIndexResponse (..),
    mkBatchListIndexResponse,
    bliIndexAttachments,
    bliNextToken,

    -- * BatchListObjectAttributes
    BatchListObjectAttributes (..),
    mkBatchListObjectAttributes,
    bloaFacetFilter,
    bloaNextToken,
    bloaMaxResults,
    bloaObjectReference,

    -- * BatchListObjectAttributesResponse
    BatchListObjectAttributesResponse (..),
    mkBatchListObjectAttributesResponse,
    bNextToken,
    bAttributes,

    -- * BatchListObjectChildren
    BatchListObjectChildren (..),
    mkBatchListObjectChildren,
    bloclNextToken,
    bloclMaxResults,
    bloclObjectReference,

    -- * BatchListObjectChildrenResponse
    BatchListObjectChildrenResponse (..),
    mkBatchListObjectChildrenResponse,
    blocChildren,
    blocNextToken,

    -- * BatchListObjectParentPaths
    BatchListObjectParentPaths (..),
    mkBatchListObjectParentPaths,
    bloppsNextToken,
    bloppsMaxResults,
    bloppsObjectReference,

    -- * BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse (..),
    mkBatchListObjectParentPathsResponse,
    bloppPathToObjectIdentifiersList,
    bloppNextToken,

    -- * BatchListObjectParents
    BatchListObjectParents (..),
    mkBatchListObjectParents,
    bloplNextToken,
    bloplMaxResults,
    bloplObjectReference,

    -- * BatchListObjectParentsResponse
    BatchListObjectParentsResponse (..),
    mkBatchListObjectParentsResponse,
    blopNextToken,
    blopParentLinks,

    -- * BatchListObjectPolicies
    BatchListObjectPolicies (..),
    mkBatchListObjectPolicies,
    bbNextToken,
    bbMaxResults,
    bbObjectReference,

    -- * BatchListObjectPoliciesResponse
    BatchListObjectPoliciesResponse (..),
    mkBatchListObjectPoliciesResponse,
    blopsNextToken,
    blopsAttachedPolicyIds,

    -- * BatchListOutgoingTypedLinks
    BatchListOutgoingTypedLinks (..),
    mkBatchListOutgoingTypedLinks,
    blotlsFilterAttributeRanges,
    blotlsNextToken,
    blotlsFilterTypedLink,
    blotlsMaxResults,
    blotlsObjectReference,

    -- * BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse (..),
    mkBatchListOutgoingTypedLinksResponse,
    blotlTypedLinkSpecifiers,
    blotlNextToken,

    -- * BatchListPolicyAttachments
    BatchListPolicyAttachments (..),
    mkBatchListPolicyAttachments,
    blpasNextToken,
    blpasMaxResults,
    blpasPolicyReference,

    -- * BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse (..),
    mkBatchListPolicyAttachmentsResponse,
    blpaObjectIdentifiers,
    blpaNextToken,

    -- * BatchLookupPolicy
    BatchLookupPolicy (..),
    mkBatchLookupPolicy,
    blplNextToken,
    blplMaxResults,
    blplObjectReference,

    -- * BatchLookupPolicyResponse
    BatchLookupPolicyResponse (..),
    mkBatchLookupPolicyResponse,
    blpNextToken,
    blpPolicyToPathList,

    -- * BatchReadException
    BatchReadException (..),
    mkBatchReadException,
    breType,
    breMessage,

    -- * BatchReadOperation
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

    -- * BatchReadOperationResponse
    BatchReadOperationResponse (..),
    mkBatchReadOperationResponse,
    broExceptionResponse,
    broSuccessfulResponse,

    -- * BatchReadSuccessfulResponse
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

    -- * BatchRemoveFacetFromObject
    BatchRemoveFacetFromObject (..),
    mkBatchRemoveFacetFromObject,
    brffoSchemaFacet,
    brffoObjectReference,

    -- * BatchRemoveFacetFromObjectResponse
    BatchRemoveFacetFromObjectResponse (..),
    mkBatchRemoveFacetFromObjectResponse,

    -- * BatchUpdateLinkAttributes
    BatchUpdateLinkAttributes (..),
    mkBatchUpdateLinkAttributes,
    bulaTypedLinkSpecifier,
    bulaAttributeUpdates,

    -- * BatchUpdateLinkAttributesResponse
    BatchUpdateLinkAttributesResponse (..),
    mkBatchUpdateLinkAttributesResponse,

    -- * BatchUpdateObjectAttributes
    BatchUpdateObjectAttributes (..),
    mkBatchUpdateObjectAttributes,
    buoaObjectReference,
    buoaAttributeUpdates,

    -- * BatchUpdateObjectAttributesResponse
    BatchUpdateObjectAttributesResponse (..),
    mkBatchUpdateObjectAttributesResponse,
    buoaObjectIdentifier,

    -- * BatchWriteOperation
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

    -- * BatchWriteOperationResponse
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

    -- * Directory
    Directory (..),
    mkDirectory,
    dDirectoryARN,
    dState,
    dName,
    dCreationDateTime,

    -- * Facet
    Facet (..),
    mkFacet,
    fFacetStyle,
    fObjectType,
    fName,

    -- * FacetAttribute
    FacetAttribute (..),
    mkFacetAttribute,
    faAttributeReference,
    faAttributeDefinition,
    faRequiredBehavior,
    faName,

    -- * FacetAttributeDefinition
    FacetAttributeDefinition (..),
    mkFacetAttributeDefinition,
    fadRules,
    fadDefaultValue,
    fadIsImmutable,
    fadType,

    -- * FacetAttributeReference
    FacetAttributeReference (..),
    mkFacetAttributeReference,
    farTargetFacetName,
    farTargetAttributeName,

    -- * FacetAttributeUpdate
    FacetAttributeUpdate (..),
    mkFacetAttributeUpdate,
    fauAttribute,
    fauAction,

    -- * IndexAttachment
    IndexAttachment (..),
    mkIndexAttachment,
    iaIndexedAttributes,
    iaObjectIdentifier,

    -- * LinkAttributeAction
    LinkAttributeAction (..),
    mkLinkAttributeAction,
    laaAttributeActionType,
    laaAttributeUpdateValue,

    -- * LinkAttributeUpdate
    LinkAttributeUpdate (..),
    mkLinkAttributeUpdate,
    lauAttributeAction,
    lauAttributeKey,

    -- * ObjectAttributeAction
    ObjectAttributeAction (..),
    mkObjectAttributeAction,
    oaaObjectAttributeActionType,
    oaaObjectAttributeUpdateValue,

    -- * ObjectAttributeRange
    ObjectAttributeRange (..),
    mkObjectAttributeRange,
    oarRange,
    oarAttributeKey,

    -- * ObjectAttributeUpdate
    ObjectAttributeUpdate (..),
    mkObjectAttributeUpdate,
    oauObjectAttributeAction,
    oauObjectAttributeKey,

    -- * ObjectIdentifierAndLinkNameTuple
    ObjectIdentifierAndLinkNameTuple (..),
    mkObjectIdentifierAndLinkNameTuple,
    oialntObjectIdentifier,
    oialntLinkName,

    -- * ObjectReference
    ObjectReference (..),
    mkObjectReference,
    orSelector,

    -- * PathToObjectIdentifiers
    PathToObjectIdentifiers (..),
    mkPathToObjectIdentifiers,
    ptoiObjectIdentifiers,
    ptoiPath,

    -- * PolicyAttachment
    PolicyAttachment (..),
    mkPolicyAttachment,
    paPolicyId,
    paPolicyType,
    paObjectIdentifier,

    -- * PolicyToPath
    PolicyToPath (..),
    mkPolicyToPath,
    ptpPath,
    ptpPolicies,

    -- * Rule
    Rule (..),
    mkRule,
    rParameters,
    rType,

    -- * SchemaFacet
    SchemaFacet (..),
    mkSchemaFacet,
    sfFacetName,
    sfSchemaARN,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TypedAttributeValue
    TypedAttributeValue (..),
    mkTypedAttributeValue,
    tavBinaryValue,
    tavDatetimeValue,
    tavNumberValue,
    tavStringValue,
    tavBooleanValue,

    -- * TypedAttributeValueRange
    TypedAttributeValueRange (..),
    mkTypedAttributeValueRange,
    tavrEndValue,
    tavrStartValue,
    tavrStartMode,
    tavrEndMode,

    -- * TypedLinkAttributeDefinition
    TypedLinkAttributeDefinition (..),
    mkTypedLinkAttributeDefinition,
    tladRules,
    tladDefaultValue,
    tladIsImmutable,
    tladName,
    tladType,
    tladRequiredBehavior,

    -- * TypedLinkAttributeRange
    TypedLinkAttributeRange (..),
    mkTypedLinkAttributeRange,
    tlarAttributeName,
    tlarRange,

    -- * TypedLinkFacet
    TypedLinkFacet (..),
    mkTypedLinkFacet,
    tlfName,
    tlfAttributes,
    tlfIdentityAttributeOrder,

    -- * TypedLinkFacetAttributeUpdate
    TypedLinkFacetAttributeUpdate (..),
    mkTypedLinkFacetAttributeUpdate,
    tlfauAttribute,
    tlfauAction,

    -- * TypedLinkSchemaAndFacetName
    TypedLinkSchemaAndFacetName (..),
    mkTypedLinkSchemaAndFacetName,
    tlsafnSchemaARN,
    tlsafnTypedLinkName,

    -- * TypedLinkSpecifier
    TypedLinkSpecifier (..),
    mkTypedLinkSpecifier,
    tlsTypedLinkFacet,
    tlsSourceObjectReference,
    tlsTargetObjectReference,
    tlsIdentityAttributeValues,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-01-11@ of the Amazon CloudDirectory SDK configuration.
cloudDirectoryService :: Lude.Service
cloudDirectoryService =
  Lude.Service
    { Lude._svcAbbrev = "CloudDirectory",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "clouddirectory",
      Lude._svcVersion = "2017-01-11",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudDirectoryService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CloudDirectory",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

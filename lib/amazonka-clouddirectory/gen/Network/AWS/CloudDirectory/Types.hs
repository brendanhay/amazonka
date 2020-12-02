{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types
  ( -- * Service Configuration
    cloudDirectory,

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
    AttributeKey,
    attributeKey,
    akSchemaARN,
    akFacetName,
    akName,

    -- * AttributeKeyAndValue
    AttributeKeyAndValue,
    attributeKeyAndValue,
    akavKey,
    akavValue,

    -- * AttributeNameAndValue
    AttributeNameAndValue,
    attributeNameAndValue,
    anavAttributeName,
    anavValue,

    -- * BatchAddFacetToObject
    BatchAddFacetToObject,
    batchAddFacetToObject,
    baftoSchemaFacet,
    baftoObjectAttributeList,
    baftoObjectReference,

    -- * BatchAddFacetToObjectResponse
    BatchAddFacetToObjectResponse,
    batchAddFacetToObjectResponse,

    -- * BatchAttachObject
    BatchAttachObject,
    batchAttachObject,
    baoParentReference,
    baoChildReference,
    baoLinkName,

    -- * BatchAttachObjectResponse
    BatchAttachObjectResponse,
    batchAttachObjectResponse,
    baoAttachedObjectIdentifier,

    -- * BatchAttachPolicy
    BatchAttachPolicy,
    batchAttachPolicy,
    bapPolicyReference,
    bapObjectReference,

    -- * BatchAttachPolicyResponse
    BatchAttachPolicyResponse,
    batchAttachPolicyResponse,

    -- * BatchAttachToIndex
    BatchAttachToIndex,
    batchAttachToIndex,
    batiIndexReference,
    batiTargetReference,

    -- * BatchAttachToIndexResponse
    BatchAttachToIndexResponse,
    batchAttachToIndexResponse,
    batiAttachedObjectIdentifier,

    -- * BatchAttachTypedLink
    BatchAttachTypedLink,
    batchAttachTypedLink,
    batlSourceObjectReference,
    batlTargetObjectReference,
    batlTypedLinkFacet,
    batlAttributes,

    -- * BatchAttachTypedLinkResponse
    BatchAttachTypedLinkResponse,
    batchAttachTypedLinkResponse,
    batlTypedLinkSpecifier,

    -- * BatchCreateIndex
    BatchCreateIndex,
    batchCreateIndex,
    bciParentReference,
    bciLinkName,
    bciBatchReferenceName,
    bciOrderedIndexedAttributeList,
    bciIsUnique,

    -- * BatchCreateIndexResponse
    BatchCreateIndexResponse,
    batchCreateIndexResponse,
    bciObjectIdentifier,

    -- * BatchCreateObject
    BatchCreateObject,
    batchCreateObject,
    bcoParentReference,
    bcoLinkName,
    bcoBatchReferenceName,
    bcoSchemaFacet,
    bcoObjectAttributeList,

    -- * BatchCreateObjectResponse
    BatchCreateObjectResponse,
    batchCreateObjectResponse,
    bcoObjectIdentifier,

    -- * BatchDeleteObject
    BatchDeleteObject,
    batchDeleteObject,
    bdoObjectReference,

    -- * BatchDeleteObjectResponse
    BatchDeleteObjectResponse,
    batchDeleteObjectResponse,

    -- * BatchDetachFromIndex
    BatchDetachFromIndex,
    batchDetachFromIndex,
    bdfiIndexReference,
    bdfiTargetReference,

    -- * BatchDetachFromIndexResponse
    BatchDetachFromIndexResponse,
    batchDetachFromIndexResponse,
    bdfiDetachedObjectIdentifier,

    -- * BatchDetachObject
    BatchDetachObject,
    batchDetachObject,
    bdoBatchReferenceName,
    bdoParentReference,
    bdoLinkName,

    -- * BatchDetachObjectResponse
    BatchDetachObjectResponse,
    batchDetachObjectResponse,
    bdoDetachedObjectIdentifier,

    -- * BatchDetachPolicy
    BatchDetachPolicy,
    batchDetachPolicy,
    bdpPolicyReference,
    bdpObjectReference,

    -- * BatchDetachPolicyResponse
    BatchDetachPolicyResponse,
    batchDetachPolicyResponse,

    -- * BatchDetachTypedLink
    BatchDetachTypedLink,
    batchDetachTypedLink,
    bdtlTypedLinkSpecifier,

    -- * BatchDetachTypedLinkResponse
    BatchDetachTypedLinkResponse,
    batchDetachTypedLinkResponse,

    -- * BatchGetLinkAttributes
    BatchGetLinkAttributes,
    batchGetLinkAttributes,
    bglaTypedLinkSpecifier,
    bglaAttributeNames,

    -- * BatchGetLinkAttributesResponse
    BatchGetLinkAttributesResponse,
    batchGetLinkAttributesResponse,
    bglaAttributes,

    -- * BatchGetObjectAttributes
    BatchGetObjectAttributes,
    batchGetObjectAttributes,
    bgoaObjectReference,
    bgoaSchemaFacet,
    bgoaAttributeNames,

    -- * BatchGetObjectAttributesResponse
    BatchGetObjectAttributesResponse,
    batchGetObjectAttributesResponse,
    bgoaAttributes,

    -- * BatchGetObjectInformation
    BatchGetObjectInformation,
    batchGetObjectInformation,
    bgoiObjectReference,

    -- * BatchGetObjectInformationResponse
    BatchGetObjectInformationResponse,
    batchGetObjectInformationResponse,
    bgoiObjectIdentifier,
    bgoiSchemaFacets,

    -- * BatchListAttachedIndices
    BatchListAttachedIndices,
    batchListAttachedIndices,
    blaisNextToken,
    blaisMaxResults,
    blaisTargetReference,

    -- * BatchListAttachedIndicesResponse
    BatchListAttachedIndicesResponse,
    batchListAttachedIndicesResponse,
    blaiIndexAttachments,
    blaiNextToken,

    -- * BatchListIncomingTypedLinks
    BatchListIncomingTypedLinks,
    batchListIncomingTypedLinks,
    blitlsFilterAttributeRanges,
    blitlsNextToken,
    blitlsFilterTypedLink,
    blitlsMaxResults,
    blitlsObjectReference,

    -- * BatchListIncomingTypedLinksResponse
    BatchListIncomingTypedLinksResponse,
    batchListIncomingTypedLinksResponse,
    blitlLinkSpecifiers,
    blitlNextToken,

    -- * BatchListIndex
    BatchListIndex,
    batchListIndex,
    batRangesOnIndexedValues,
    batNextToken,
    batMaxResults,
    batIndexReference,

    -- * BatchListIndexResponse
    BatchListIndexResponse,
    batchListIndexResponse,
    bliIndexAttachments,
    bliNextToken,

    -- * BatchListObjectAttributes
    BatchListObjectAttributes,
    batchListObjectAttributes,
    bloaFacetFilter,
    bloaNextToken,
    bloaMaxResults,
    bloaObjectReference,

    -- * BatchListObjectAttributesResponse
    BatchListObjectAttributesResponse,
    batchListObjectAttributesResponse,
    bNextToken,
    bAttributes,

    -- * BatchListObjectChildren
    BatchListObjectChildren,
    batchListObjectChildren,
    bloclNextToken,
    bloclMaxResults,
    bloclObjectReference,

    -- * BatchListObjectChildrenResponse
    BatchListObjectChildrenResponse,
    batchListObjectChildrenResponse,
    blocChildren,
    blocNextToken,

    -- * BatchListObjectParentPaths
    BatchListObjectParentPaths,
    batchListObjectParentPaths,
    bloppsNextToken,
    bloppsMaxResults,
    bloppsObjectReference,

    -- * BatchListObjectParentPathsResponse
    BatchListObjectParentPathsResponse,
    batchListObjectParentPathsResponse,
    bloppPathToObjectIdentifiersList,
    bloppNextToken,

    -- * BatchListObjectParents
    BatchListObjectParents,
    batchListObjectParents,
    bloplNextToken,
    bloplMaxResults,
    bloplObjectReference,

    -- * BatchListObjectParentsResponse
    BatchListObjectParentsResponse,
    batchListObjectParentsResponse,
    blopNextToken,
    blopParentLinks,

    -- * BatchListObjectPolicies
    BatchListObjectPolicies,
    batchListObjectPolicies,
    bbNextToken,
    bbMaxResults,
    bbObjectReference,

    -- * BatchListObjectPoliciesResponse
    BatchListObjectPoliciesResponse,
    batchListObjectPoliciesResponse,
    blopsNextToken,
    blopsAttachedPolicyIds,

    -- * BatchListOutgoingTypedLinks
    BatchListOutgoingTypedLinks,
    batchListOutgoingTypedLinks,
    blotlsFilterAttributeRanges,
    blotlsNextToken,
    blotlsFilterTypedLink,
    blotlsMaxResults,
    blotlsObjectReference,

    -- * BatchListOutgoingTypedLinksResponse
    BatchListOutgoingTypedLinksResponse,
    batchListOutgoingTypedLinksResponse,
    blotlTypedLinkSpecifiers,
    blotlNextToken,

    -- * BatchListPolicyAttachments
    BatchListPolicyAttachments,
    batchListPolicyAttachments,
    blpasNextToken,
    blpasMaxResults,
    blpasPolicyReference,

    -- * BatchListPolicyAttachmentsResponse
    BatchListPolicyAttachmentsResponse,
    batchListPolicyAttachmentsResponse,
    blpaObjectIdentifiers,
    blpaNextToken,

    -- * BatchLookupPolicy
    BatchLookupPolicy,
    batchLookupPolicy,
    blplNextToken,
    blplMaxResults,
    blplObjectReference,

    -- * BatchLookupPolicyResponse
    BatchLookupPolicyResponse,
    batchLookupPolicyResponse,
    blpNextToken,
    blpPolicyToPathList,

    -- * BatchReadException
    BatchReadException,
    batchReadException,
    breType,
    breMessage,

    -- * BatchReadOperation
    BatchReadOperation,
    batchReadOperation,
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
    BatchReadOperationResponse,
    batchReadOperationResponse,
    broExceptionResponse,
    broSuccessfulResponse,

    -- * BatchReadSuccessfulResponse
    BatchReadSuccessfulResponse,
    batchReadSuccessfulResponse,
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
    BatchRemoveFacetFromObject,
    batchRemoveFacetFromObject,
    brffoSchemaFacet,
    brffoObjectReference,

    -- * BatchRemoveFacetFromObjectResponse
    BatchRemoveFacetFromObjectResponse,
    batchRemoveFacetFromObjectResponse,

    -- * BatchUpdateLinkAttributes
    BatchUpdateLinkAttributes,
    batchUpdateLinkAttributes,
    bulaTypedLinkSpecifier,
    bulaAttributeUpdates,

    -- * BatchUpdateLinkAttributesResponse
    BatchUpdateLinkAttributesResponse,
    batchUpdateLinkAttributesResponse,

    -- * BatchUpdateObjectAttributes
    BatchUpdateObjectAttributes,
    batchUpdateObjectAttributes,
    buoaObjectReference,
    buoaAttributeUpdates,

    -- * BatchUpdateObjectAttributesResponse
    BatchUpdateObjectAttributesResponse,
    batchUpdateObjectAttributesResponse,
    buoaObjectIdentifier,

    -- * BatchWriteOperation
    BatchWriteOperation,
    batchWriteOperation,
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
    BatchWriteOperationResponse,
    batchWriteOperationResponse,
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
    Directory,
    directory,
    dDirectoryARN,
    dState,
    dName,
    dCreationDateTime,

    -- * Facet
    Facet,
    facet,
    fFacetStyle,
    fObjectType,
    fName,

    -- * FacetAttribute
    FacetAttribute,
    facetAttribute,
    faAttributeReference,
    faAttributeDefinition,
    faRequiredBehavior,
    faName,

    -- * FacetAttributeDefinition
    FacetAttributeDefinition,
    facetAttributeDefinition,
    fadRules,
    fadDefaultValue,
    fadIsImmutable,
    fadType,

    -- * FacetAttributeReference
    FacetAttributeReference,
    facetAttributeReference,
    farTargetFacetName,
    farTargetAttributeName,

    -- * FacetAttributeUpdate
    FacetAttributeUpdate,
    facetAttributeUpdate,
    fauAttribute,
    fauAction,

    -- * IndexAttachment
    IndexAttachment,
    indexAttachment,
    iaIndexedAttributes,
    iaObjectIdentifier,

    -- * LinkAttributeAction
    LinkAttributeAction,
    linkAttributeAction,
    laaAttributeActionType,
    laaAttributeUpdateValue,

    -- * LinkAttributeUpdate
    LinkAttributeUpdate,
    linkAttributeUpdate,
    lauAttributeAction,
    lauAttributeKey,

    -- * ObjectAttributeAction
    ObjectAttributeAction,
    objectAttributeAction,
    oaaObjectAttributeActionType,
    oaaObjectAttributeUpdateValue,

    -- * ObjectAttributeRange
    ObjectAttributeRange,
    objectAttributeRange,
    oarRange,
    oarAttributeKey,

    -- * ObjectAttributeUpdate
    ObjectAttributeUpdate,
    objectAttributeUpdate,
    oauObjectAttributeAction,
    oauObjectAttributeKey,

    -- * ObjectIdentifierAndLinkNameTuple
    ObjectIdentifierAndLinkNameTuple,
    objectIdentifierAndLinkNameTuple,
    oialntObjectIdentifier,
    oialntLinkName,

    -- * ObjectReference
    ObjectReference,
    objectReference,
    orSelector,

    -- * PathToObjectIdentifiers
    PathToObjectIdentifiers,
    pathToObjectIdentifiers,
    ptoiObjectIdentifiers,
    ptoiPath,

    -- * PolicyAttachment
    PolicyAttachment,
    policyAttachment,
    paPolicyId,
    paPolicyType,
    paObjectIdentifier,

    -- * PolicyToPath
    PolicyToPath,
    policyToPath,
    ptpPath,
    ptpPolicies,

    -- * Rule
    Rule,
    rule,
    rParameters,
    rType,

    -- * SchemaFacet
    SchemaFacet,
    schemaFacet,
    sfFacetName,
    sfSchemaARN,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TypedAttributeValue
    TypedAttributeValue,
    typedAttributeValue,
    tavBinaryValue,
    tavDatetimeValue,
    tavNumberValue,
    tavStringValue,
    tavBooleanValue,

    -- * TypedAttributeValueRange
    TypedAttributeValueRange,
    typedAttributeValueRange,
    tavrEndValue,
    tavrStartValue,
    tavrStartMode,
    tavrEndMode,

    -- * TypedLinkAttributeDefinition
    TypedLinkAttributeDefinition,
    typedLinkAttributeDefinition,
    tladRules,
    tladDefaultValue,
    tladIsImmutable,
    tladName,
    tladType,
    tladRequiredBehavior,

    -- * TypedLinkAttributeRange
    TypedLinkAttributeRange,
    typedLinkAttributeRange,
    tlarAttributeName,
    tlarRange,

    -- * TypedLinkFacet
    TypedLinkFacet,
    typedLinkFacet,
    tlfName,
    tlfAttributes,
    tlfIdentityAttributeOrder,

    -- * TypedLinkFacetAttributeUpdate
    TypedLinkFacetAttributeUpdate,
    typedLinkFacetAttributeUpdate,
    tlfauAttribute,
    tlfauAction,

    -- * TypedLinkSchemaAndFacetName
    TypedLinkSchemaAndFacetName,
    typedLinkSchemaAndFacetName,
    tlsafnSchemaARN,
    tlsafnTypedLinkName,

    -- * TypedLinkSpecifier
    TypedLinkSpecifier,
    typedLinkSpecifier,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-01-11@ of the Amazon CloudDirectory SDK configuration.
cloudDirectory :: Service
cloudDirectory =
  Service
    { _svcAbbrev = "CloudDirectory",
      _svcSigner = v4,
      _svcPrefix = "clouddirectory",
      _svcVersion = "2017-01-11",
      _svcEndpoint = defaultEndpoint cloudDirectory,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CloudDirectory",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

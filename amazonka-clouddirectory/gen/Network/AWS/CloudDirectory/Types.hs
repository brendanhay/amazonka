{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types
    (
    -- * Service Configuration
      cloudDirectory

    -- * Errors
    , _UnsupportedIndexTypeException
    , _NotIndexException
    , _ValidationException
    , _AccessDeniedException
    , _FacetAlreadyExistsException
    , _InvalidSchemaDocException
    , _InvalidAttachmentException
    , _CannotListParentOfRootException
    , _NotPolicyException
    , _InvalidTaggingRequestException
    , _InvalidFacetUpdateException
    , _InvalidRuleException
    , _SchemaAlreadyPublishedException
    , _DirectoryAlreadyExistsException
    , _DirectoryNotDisabledException
    , _BatchWriteException
    , _DirectoryNotEnabledException
    , _FacetInUseException
    , _FacetValidationException
    , _StillContainsLinksException
    , _IncompatibleSchemaException
    , _NotNodeException
    , _InvalidNextTokenException
    , _ObjectAlreadyDetachedException
    , _LinkNameAlreadyInUseException
    , _InternalServiceException
    , _SchemaAlreadyExistsException
    , _IndexedAttributeMissingException
    , _DirectoryDeletedException
    , _RetryableConflictException
    , _InvalidARNException
    , _ResourceNotFoundException
    , _FacetNotFoundException
    , _LimitExceededException
    , _ObjectNotDetachedException

    -- * BatchReadExceptionType
    , BatchReadExceptionType (..)

    -- * ConsistencyLevel
    , ConsistencyLevel (..)

    -- * DirectoryState
    , DirectoryState (..)

    -- * FacetAttributeType
    , FacetAttributeType (..)

    -- * FacetStyle
    , FacetStyle (..)

    -- * ObjectType
    , ObjectType (..)

    -- * RangeMode
    , RangeMode (..)

    -- * RequiredAttributeBehavior
    , RequiredAttributeBehavior (..)

    -- * RuleType
    , RuleType (..)

    -- * UpdateActionType
    , UpdateActionType (..)

    -- * AttributeKey
    , AttributeKey
    , attributeKey
    , akSchemaARN
    , akFacetName
    , akName

    -- * AttributeKeyAndValue
    , AttributeKeyAndValue
    , attributeKeyAndValue
    , akavKey
    , akavValue

    -- * AttributeNameAndValue
    , AttributeNameAndValue
    , attributeNameAndValue
    , anavAttributeName
    , anavValue

    -- * BatchAddFacetToObject
    , BatchAddFacetToObject
    , batchAddFacetToObject
    , baftoSchemaFacet
    , baftoObjectAttributeList
    , baftoObjectReference

    -- * BatchAddFacetToObjectResponse
    , BatchAddFacetToObjectResponse
    , batchAddFacetToObjectResponse

    -- * BatchAttachObject
    , BatchAttachObject
    , batchAttachObject
    , baoParentReference
    , baoChildReference
    , baoLinkName

    -- * BatchAttachObjectResponse
    , BatchAttachObjectResponse
    , batchAttachObjectResponse
    , baoAttachedObjectIdentifier

    -- * BatchAttachPolicy
    , BatchAttachPolicy
    , batchAttachPolicy
    , bapPolicyReference
    , bapObjectReference

    -- * BatchAttachPolicyResponse
    , BatchAttachPolicyResponse
    , batchAttachPolicyResponse

    -- * BatchAttachToIndex
    , BatchAttachToIndex
    , batchAttachToIndex
    , batiIndexReference
    , batiTargetReference

    -- * BatchAttachToIndexResponse
    , BatchAttachToIndexResponse
    , batchAttachToIndexResponse
    , batiAttachedObjectIdentifier

    -- * BatchAttachTypedLink
    , BatchAttachTypedLink
    , batchAttachTypedLink
    , batlSourceObjectReference
    , batlTargetObjectReference
    , batlTypedLinkFacet
    , batlAttributes

    -- * BatchAttachTypedLinkResponse
    , BatchAttachTypedLinkResponse
    , batchAttachTypedLinkResponse
    , batlTypedLinkSpecifier

    -- * BatchCreateIndex
    , BatchCreateIndex
    , batchCreateIndex
    , bciParentReference
    , bciLinkName
    , bciBatchReferenceName
    , bciOrderedIndexedAttributeList
    , bciIsUnique

    -- * BatchCreateIndexResponse
    , BatchCreateIndexResponse
    , batchCreateIndexResponse
    , bciObjectIdentifier

    -- * BatchCreateObject
    , BatchCreateObject
    , batchCreateObject
    , bcoParentReference
    , bcoLinkName
    , bcoBatchReferenceName
    , bcoSchemaFacet
    , bcoObjectAttributeList

    -- * BatchCreateObjectResponse
    , BatchCreateObjectResponse
    , batchCreateObjectResponse
    , bcoObjectIdentifier

    -- * BatchDeleteObject
    , BatchDeleteObject
    , batchDeleteObject
    , bdoObjectReference

    -- * BatchDeleteObjectResponse
    , BatchDeleteObjectResponse
    , batchDeleteObjectResponse

    -- * BatchDetachFromIndex
    , BatchDetachFromIndex
    , batchDetachFromIndex
    , bdfiIndexReference
    , bdfiTargetReference

    -- * BatchDetachFromIndexResponse
    , BatchDetachFromIndexResponse
    , batchDetachFromIndexResponse
    , bdfiDetachedObjectIdentifier

    -- * BatchDetachObject
    , BatchDetachObject
    , batchDetachObject
    , bdoBatchReferenceName
    , bdoParentReference
    , bdoLinkName

    -- * BatchDetachObjectResponse
    , BatchDetachObjectResponse
    , batchDetachObjectResponse
    , bdoDetachedObjectIdentifier

    -- * BatchDetachPolicy
    , BatchDetachPolicy
    , batchDetachPolicy
    , bdpPolicyReference
    , bdpObjectReference

    -- * BatchDetachPolicyResponse
    , BatchDetachPolicyResponse
    , batchDetachPolicyResponse

    -- * BatchDetachTypedLink
    , BatchDetachTypedLink
    , batchDetachTypedLink
    , bdtlTypedLinkSpecifier

    -- * BatchDetachTypedLinkResponse
    , BatchDetachTypedLinkResponse
    , batchDetachTypedLinkResponse

    -- * BatchGetLinkAttributes
    , BatchGetLinkAttributes
    , batchGetLinkAttributes
    , bglaTypedLinkSpecifier
    , bglaAttributeNames

    -- * BatchGetLinkAttributesResponse
    , BatchGetLinkAttributesResponse
    , batchGetLinkAttributesResponse
    , bglaAttributes

    -- * BatchGetObjectAttributes
    , BatchGetObjectAttributes
    , batchGetObjectAttributes
    , bgoaObjectReference
    , bgoaSchemaFacet
    , bgoaAttributeNames

    -- * BatchGetObjectAttributesResponse
    , BatchGetObjectAttributesResponse
    , batchGetObjectAttributesResponse
    , bgoaAttributes

    -- * BatchGetObjectInformation
    , BatchGetObjectInformation
    , batchGetObjectInformation
    , bgoiObjectReference

    -- * BatchGetObjectInformationResponse
    , BatchGetObjectInformationResponse
    , batchGetObjectInformationResponse
    , bgoiObjectIdentifier
    , bgoiSchemaFacets

    -- * BatchListAttachedIndices
    , BatchListAttachedIndices
    , batchListAttachedIndices
    , blaisNextToken
    , blaisMaxResults
    , blaisTargetReference

    -- * BatchListAttachedIndicesResponse
    , BatchListAttachedIndicesResponse
    , batchListAttachedIndicesResponse
    , blaiIndexAttachments
    , blaiNextToken

    -- * BatchListIncomingTypedLinks
    , BatchListIncomingTypedLinks
    , batchListIncomingTypedLinks
    , blitlsFilterAttributeRanges
    , blitlsNextToken
    , blitlsFilterTypedLink
    , blitlsMaxResults
    , blitlsObjectReference

    -- * BatchListIncomingTypedLinksResponse
    , BatchListIncomingTypedLinksResponse
    , batchListIncomingTypedLinksResponse
    , blitlLinkSpecifiers
    , blitlNextToken

    -- * BatchListIndex
    , BatchListIndex
    , batchListIndex
    , batRangesOnIndexedValues
    , batNextToken
    , batMaxResults
    , batIndexReference

    -- * BatchListIndexResponse
    , BatchListIndexResponse
    , batchListIndexResponse
    , bliIndexAttachments
    , bliNextToken

    -- * BatchListObjectAttributes
    , BatchListObjectAttributes
    , batchListObjectAttributes
    , bloaFacetFilter
    , bloaNextToken
    , bloaMaxResults
    , bloaObjectReference

    -- * BatchListObjectAttributesResponse
    , BatchListObjectAttributesResponse
    , batchListObjectAttributesResponse
    , bNextToken
    , bAttributes

    -- * BatchListObjectChildren
    , BatchListObjectChildren
    , batchListObjectChildren
    , bloclNextToken
    , bloclMaxResults
    , bloclObjectReference

    -- * BatchListObjectChildrenResponse
    , BatchListObjectChildrenResponse
    , batchListObjectChildrenResponse
    , blocChildren
    , blocNextToken

    -- * BatchListObjectParentPaths
    , BatchListObjectParentPaths
    , batchListObjectParentPaths
    , bloppsNextToken
    , bloppsMaxResults
    , bloppsObjectReference

    -- * BatchListObjectParentPathsResponse
    , BatchListObjectParentPathsResponse
    , batchListObjectParentPathsResponse
    , bloppPathToObjectIdentifiersList
    , bloppNextToken

    -- * BatchListObjectParents
    , BatchListObjectParents
    , batchListObjectParents
    , bloplNextToken
    , bloplMaxResults
    , bloplObjectReference

    -- * BatchListObjectParentsResponse
    , BatchListObjectParentsResponse
    , batchListObjectParentsResponse
    , blopNextToken
    , blopParentLinks

    -- * BatchListObjectPolicies
    , BatchListObjectPolicies
    , batchListObjectPolicies
    , bbNextToken
    , bbMaxResults
    , bbObjectReference

    -- * BatchListObjectPoliciesResponse
    , BatchListObjectPoliciesResponse
    , batchListObjectPoliciesResponse
    , blopsNextToken
    , blopsAttachedPolicyIds

    -- * BatchListOutgoingTypedLinks
    , BatchListOutgoingTypedLinks
    , batchListOutgoingTypedLinks
    , blotlsFilterAttributeRanges
    , blotlsNextToken
    , blotlsFilterTypedLink
    , blotlsMaxResults
    , blotlsObjectReference

    -- * BatchListOutgoingTypedLinksResponse
    , BatchListOutgoingTypedLinksResponse
    , batchListOutgoingTypedLinksResponse
    , blotlTypedLinkSpecifiers
    , blotlNextToken

    -- * BatchListPolicyAttachments
    , BatchListPolicyAttachments
    , batchListPolicyAttachments
    , blpasNextToken
    , blpasMaxResults
    , blpasPolicyReference

    -- * BatchListPolicyAttachmentsResponse
    , BatchListPolicyAttachmentsResponse
    , batchListPolicyAttachmentsResponse
    , blpaObjectIdentifiers
    , blpaNextToken

    -- * BatchLookupPolicy
    , BatchLookupPolicy
    , batchLookupPolicy
    , blplNextToken
    , blplMaxResults
    , blplObjectReference

    -- * BatchLookupPolicyResponse
    , BatchLookupPolicyResponse
    , batchLookupPolicyResponse
    , blpNextToken
    , blpPolicyToPathList

    -- * BatchReadException
    , BatchReadException
    , batchReadException
    , breType
    , breMessage

    -- * BatchReadOperation
    , BatchReadOperation
    , batchReadOperation
    , broListIndex
    , broGetObjectInformation
    , broListAttachedIndices
    , broLookupPolicy
    , broListObjectParentPaths
    , broListObjectAttributes
    , broListIncomingTypedLinks
    , broGetLinkAttributes
    , broGetObjectAttributes
    , broListObjectChildren
    , broListObjectParents
    , broListPolicyAttachments
    , broListOutgoingTypedLinks
    , broListObjectPolicies

    -- * BatchReadOperationResponse
    , BatchReadOperationResponse
    , batchReadOperationResponse
    , broExceptionResponse
    , broSuccessfulResponse

    -- * BatchReadSuccessfulResponse
    , BatchReadSuccessfulResponse
    , batchReadSuccessfulResponse
    , brsListIndex
    , brsGetObjectInformation
    , brsListAttachedIndices
    , brsLookupPolicy
    , brsListObjectParentPaths
    , brsListObjectAttributes
    , brsListIncomingTypedLinks
    , brsGetLinkAttributes
    , brsGetObjectAttributes
    , brsListObjectChildren
    , brsListObjectParents
    , brsListPolicyAttachments
    , brsListOutgoingTypedLinks
    , brsListObjectPolicies

    -- * BatchRemoveFacetFromObject
    , BatchRemoveFacetFromObject
    , batchRemoveFacetFromObject
    , brffoSchemaFacet
    , brffoObjectReference

    -- * BatchRemoveFacetFromObjectResponse
    , BatchRemoveFacetFromObjectResponse
    , batchRemoveFacetFromObjectResponse

    -- * BatchUpdateLinkAttributes
    , BatchUpdateLinkAttributes
    , batchUpdateLinkAttributes
    , bulaTypedLinkSpecifier
    , bulaAttributeUpdates

    -- * BatchUpdateLinkAttributesResponse
    , BatchUpdateLinkAttributesResponse
    , batchUpdateLinkAttributesResponse

    -- * BatchUpdateObjectAttributes
    , BatchUpdateObjectAttributes
    , batchUpdateObjectAttributes
    , buoaObjectReference
    , buoaAttributeUpdates

    -- * BatchUpdateObjectAttributesResponse
    , BatchUpdateObjectAttributesResponse
    , batchUpdateObjectAttributesResponse
    , buoaObjectIdentifier

    -- * BatchWriteOperation
    , BatchWriteOperation
    , batchWriteOperation
    , bDeleteObject
    , bDetachFromIndex
    , bRemoveFacetFromObject
    , bAttachObject
    , bCreateObject
    , bAttachTypedLink
    , bDetachPolicy
    , bCreateIndex
    , bDetachObject
    , bAddFacetToObject
    , bDetachTypedLink
    , bUpdateObjectAttributes
    , bAttachPolicy
    , bUpdateLinkAttributes
    , bAttachToIndex

    -- * BatchWriteOperationResponse
    , BatchWriteOperationResponse
    , batchWriteOperationResponse
    , bwoDeleteObject
    , bwoDetachFromIndex
    , bwoRemoveFacetFromObject
    , bwoAttachObject
    , bwoCreateObject
    , bwoAttachTypedLink
    , bwoDetachPolicy
    , bwoCreateIndex
    , bwoDetachObject
    , bwoAddFacetToObject
    , bwoDetachTypedLink
    , bwoUpdateObjectAttributes
    , bwoAttachPolicy
    , bwoUpdateLinkAttributes
    , bwoAttachToIndex

    -- * Directory
    , Directory
    , directory
    , dDirectoryARN
    , dState
    , dName
    , dCreationDateTime

    -- * Facet
    , Facet
    , facet
    , fFacetStyle
    , fObjectType
    , fName

    -- * FacetAttribute
    , FacetAttribute
    , facetAttribute
    , faAttributeReference
    , faAttributeDefinition
    , faRequiredBehavior
    , faName

    -- * FacetAttributeDefinition
    , FacetAttributeDefinition
    , facetAttributeDefinition
    , fadRules
    , fadDefaultValue
    , fadIsImmutable
    , fadType

    -- * FacetAttributeReference
    , FacetAttributeReference
    , facetAttributeReference
    , farTargetFacetName
    , farTargetAttributeName

    -- * FacetAttributeUpdate
    , FacetAttributeUpdate
    , facetAttributeUpdate
    , fauAttribute
    , fauAction

    -- * IndexAttachment
    , IndexAttachment
    , indexAttachment
    , iaIndexedAttributes
    , iaObjectIdentifier

    -- * LinkAttributeAction
    , LinkAttributeAction
    , linkAttributeAction
    , laaAttributeActionType
    , laaAttributeUpdateValue

    -- * LinkAttributeUpdate
    , LinkAttributeUpdate
    , linkAttributeUpdate
    , lauAttributeAction
    , lauAttributeKey

    -- * ObjectAttributeAction
    , ObjectAttributeAction
    , objectAttributeAction
    , oaaObjectAttributeActionType
    , oaaObjectAttributeUpdateValue

    -- * ObjectAttributeRange
    , ObjectAttributeRange
    , objectAttributeRange
    , oarRange
    , oarAttributeKey

    -- * ObjectAttributeUpdate
    , ObjectAttributeUpdate
    , objectAttributeUpdate
    , oauObjectAttributeAction
    , oauObjectAttributeKey

    -- * ObjectIdentifierAndLinkNameTuple
    , ObjectIdentifierAndLinkNameTuple
    , objectIdentifierAndLinkNameTuple
    , oialntObjectIdentifier
    , oialntLinkName

    -- * ObjectReference
    , ObjectReference
    , objectReference
    , orSelector

    -- * PathToObjectIdentifiers
    , PathToObjectIdentifiers
    , pathToObjectIdentifiers
    , ptoiObjectIdentifiers
    , ptoiPath

    -- * PolicyAttachment
    , PolicyAttachment
    , policyAttachment
    , paPolicyId
    , paPolicyType
    , paObjectIdentifier

    -- * PolicyToPath
    , PolicyToPath
    , policyToPath
    , ptpPath
    , ptpPolicies

    -- * Rule
    , Rule
    , rule
    , rParameters
    , rType

    -- * SchemaFacet
    , SchemaFacet
    , schemaFacet
    , sfFacetName
    , sfSchemaARN

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TypedAttributeValue
    , TypedAttributeValue
    , typedAttributeValue
    , tavBinaryValue
    , tavDatetimeValue
    , tavNumberValue
    , tavStringValue
    , tavBooleanValue

    -- * TypedAttributeValueRange
    , TypedAttributeValueRange
    , typedAttributeValueRange
    , tavrEndValue
    , tavrStartValue
    , tavrStartMode
    , tavrEndMode

    -- * TypedLinkAttributeDefinition
    , TypedLinkAttributeDefinition
    , typedLinkAttributeDefinition
    , tladRules
    , tladDefaultValue
    , tladIsImmutable
    , tladName
    , tladType
    , tladRequiredBehavior

    -- * TypedLinkAttributeRange
    , TypedLinkAttributeRange
    , typedLinkAttributeRange
    , tlarAttributeName
    , tlarRange

    -- * TypedLinkFacet
    , TypedLinkFacet
    , typedLinkFacet
    , tlfName
    , tlfAttributes
    , tlfIdentityAttributeOrder

    -- * TypedLinkFacetAttributeUpdate
    , TypedLinkFacetAttributeUpdate
    , typedLinkFacetAttributeUpdate
    , tlfauAttribute
    , tlfauAction

    -- * TypedLinkSchemaAndFacetName
    , TypedLinkSchemaAndFacetName
    , typedLinkSchemaAndFacetName
    , tlsafnSchemaARN
    , tlsafnTypedLinkName

    -- * TypedLinkSpecifier
    , TypedLinkSpecifier
    , typedLinkSpecifier
    , tlsTypedLinkFacet
    , tlsSourceObjectReference
    , tlsTargetObjectReference
    , tlsIdentityAttributeValues
    ) where

import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.CloudDirectory.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-01-11@ of the Amazon CloudDirectory SDK configuration.
cloudDirectory :: Service
cloudDirectory =
  Service
    { _svcAbbrev = "CloudDirectory"
    , _svcSigner = v4
    , _svcPrefix = "clouddirectory"
    , _svcVersion = "2017-01-11"
    , _svcEndpoint = defaultEndpoint cloudDirectory
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CloudDirectory"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | Indicates that the requested index type is not supported.
--
--
_UnsupportedIndexTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedIndexTypeException =
  _MatchServiceError cloudDirectory "UnsupportedIndexTypeException" .
  hasStatus 400


-- | Indicates that the requested operation can only operate on index objects.
--
--
_NotIndexException :: AsError a => Getting (First ServiceError) a ServiceError
_NotIndexException =
  _MatchServiceError cloudDirectory "NotIndexException" . hasStatus 400


-- | Indicates that your request is malformed in some manner. See the exception message.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException =
  _MatchServiceError cloudDirectory "ValidationException" . hasStatus 400


-- | Access denied. Check your permissions.
--
--
_AccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_AccessDeniedException =
  _MatchServiceError cloudDirectory "AccessDeniedException" . hasStatus 403


-- | A facet with the same name already exists.
--
--
_FacetAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_FacetAlreadyExistsException =
  _MatchServiceError cloudDirectory "FacetAlreadyExistsException" .
  hasStatus 400


-- | Indicates that the provided @SchemaDoc@ value is not valid.
--
--
_InvalidSchemaDocException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSchemaDocException =
  _MatchServiceError cloudDirectory "InvalidSchemaDocException" . hasStatus 400


-- | Indicates that an attempt to make an attachment was invalid. For example, attaching two nodes with a link type that is not applicable to the nodes or attempting to apply a schema to a directory a second time.
--
--
_InvalidAttachmentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAttachmentException =
  _MatchServiceError cloudDirectory "InvalidAttachmentException" . hasStatus 400


-- | Cannot list the parents of a 'Directory' root.
--
--
_CannotListParentOfRootException :: AsError a => Getting (First ServiceError) a ServiceError
_CannotListParentOfRootException =
  _MatchServiceError cloudDirectory "CannotListParentOfRootException" .
  hasStatus 400


-- | Indicates that the requested operation can only operate on policy objects.
--
--
_NotPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_NotPolicyException =
  _MatchServiceError cloudDirectory "NotPolicyException" . hasStatus 400


-- | Can occur for multiple reasons such as when you tag a resource that doesn

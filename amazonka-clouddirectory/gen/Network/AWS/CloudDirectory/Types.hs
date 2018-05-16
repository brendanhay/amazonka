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

    -- * BatchListObjectPolicies
    , BatchListObjectPolicies
    , batchListObjectPolicies
    , blopsNextToken
    , blopsMaxResults
    , blopsObjectReference

    -- * BatchListObjectPoliciesResponse
    , BatchListObjectPoliciesResponse
    , batchListObjectPoliciesResponse
    , blopNextToken
    , blopAttachedPolicyIds

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
    , broGetObjectAttributes
    , broListObjectChildren
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
    , brsGetObjectAttributes
    , brsListObjectChildren
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

-- | API version @2016-05-10@ of the Amazon CloudDirectory SDK configuration.
cloudDirectory :: Service
cloudDirectory =
  Service
    { _svcAbbrev = "CloudDirectory"
    , _svcSigner = v4
    , _svcPrefix = "clouddirectory"
    , _svcVersion = "2016-05-10"
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


-- | Indicates that an attempt to attach an object with the same link name or to apply a schema with the same name has occurred. Rename the link or the schema and then try again.
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


-- | Can occur for multiple reasons such as when you tag a resource that doesn’t exist or if you specify a higher number of tags for a resource than the allowed limit. Allowed limit is 50 tags per resource.
--
--
_InvalidTaggingRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTaggingRequestException =
  _MatchServiceError cloudDirectory "InvalidTaggingRequestException" .
  hasStatus 400


-- | An attempt to modify a 'Facet' resulted in an invalid schema exception.
--
--
_InvalidFacetUpdateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFacetUpdateException =
  _MatchServiceError cloudDirectory "InvalidFacetUpdateException" .
  hasStatus 400


-- | Occurs when any of the rule parameter keys or values are invalid.
--
--
_InvalidRuleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRuleException =
  _MatchServiceError cloudDirectory "InvalidRuleException" . hasStatus 400


-- | Indicates that a schema is already published.
--
--
_SchemaAlreadyPublishedException :: AsError a => Getting (First ServiceError) a ServiceError
_SchemaAlreadyPublishedException =
  _MatchServiceError cloudDirectory "SchemaAlreadyPublishedException" .
  hasStatus 400


-- | Indicates that a 'Directory' could not be created due to a naming conflict. Choose a different name and try again.
--
--
_DirectoryAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryAlreadyExistsException =
  _MatchServiceError cloudDirectory "DirectoryAlreadyExistsException" .
  hasStatus 400


-- | An operation can only operate on a disabled directory.
--
--
_DirectoryNotDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryNotDisabledException =
  _MatchServiceError cloudDirectory "DirectoryNotDisabledException" .
  hasStatus 400


-- | A @BatchWrite@ exception has occurred.
--
--
_BatchWriteException :: AsError a => Getting (First ServiceError) a ServiceError
_BatchWriteException = _MatchServiceError cloudDirectory "BatchWriteException"


-- | Operations are only permitted on enabled directories.
--
--
_DirectoryNotEnabledException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryNotEnabledException =
  _MatchServiceError cloudDirectory "DirectoryNotEnabledException" .
  hasStatus 400


-- | Occurs when deleting a facet that contains an attribute that is a target to an attribute reference in a different facet.
--
--
_FacetInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_FacetInUseException =
  _MatchServiceError cloudDirectory "FacetInUseException" . hasStatus 400


-- | The 'Facet' that you provided was not well formed or could not be validated with the schema.
--
--
_FacetValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_FacetValidationException =
  _MatchServiceError cloudDirectory "FacetValidationException" . hasStatus 400


-- | The object could not be deleted because links still exist. Remove the links and then try the operation again.
--
--
_StillContainsLinksException :: AsError a => Getting (First ServiceError) a ServiceError
_StillContainsLinksException =
  _MatchServiceError cloudDirectory "StillContainsLinksException" .
  hasStatus 400


-- | Indicates a failure occurred while performing a check for backward compatibility between the specified schema and the schema that is currently applied to the directory.
--
--
_IncompatibleSchemaException :: AsError a => Getting (First ServiceError) a ServiceError
_IncompatibleSchemaException =
  _MatchServiceError cloudDirectory "IncompatibleSchemaException" .
  hasStatus 400


-- | Occurs when any invalid operations are performed on an object that is not a node, such as calling @ListObjectChildren@ for a leaf node object.
--
--
_NotNodeException :: AsError a => Getting (First ServiceError) a ServiceError
_NotNodeException =
  _MatchServiceError cloudDirectory "NotNodeException" . hasStatus 400


-- | Indicates that the @NextToken@ value is not valid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError cloudDirectory "InvalidNextTokenException" . hasStatus 400


-- | Indicates that the object is not attached to the index.
--
--
_ObjectAlreadyDetachedException :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectAlreadyDetachedException =
  _MatchServiceError cloudDirectory "ObjectAlreadyDetachedException" .
  hasStatus 400


-- | Indicates that a link could not be created due to a naming conflict. Choose a different name and then try again.
--
--
_LinkNameAlreadyInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_LinkNameAlreadyInUseException =
  _MatchServiceError cloudDirectory "LinkNameAlreadyInUseException" .
  hasStatus 400


-- | Indicates a problem that must be resolved by Amazon Web Services. This might be a transient error in which case you can retry your request until it succeeds. Otherwise, go to the <http://status.aws.amazon.com/ AWS Service Health Dashboard> site to see if there are any operational issues with the service.
--
--
_InternalServiceException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceException =
  _MatchServiceError cloudDirectory "InternalServiceException" . hasStatus 500


-- | Indicates that a schema could not be created due to a naming conflict. Please select a different name and then try again.
--
--
_SchemaAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_SchemaAlreadyExistsException =
  _MatchServiceError cloudDirectory "SchemaAlreadyExistsException" .
  hasStatus 400


-- | An object has been attempted to be attached to an object that does not have the appropriate attribute value.
--
--
_IndexedAttributeMissingException :: AsError a => Getting (First ServiceError) a ServiceError
_IndexedAttributeMissingException =
  _MatchServiceError cloudDirectory "IndexedAttributeMissingException" .
  hasStatus 400


-- | A directory that has been deleted and to which access has been attempted. Note: The requested resource will eventually cease to exist.
--
--
_DirectoryDeletedException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryDeletedException =
  _MatchServiceError cloudDirectory "DirectoryDeletedException" . hasStatus 400


-- | Occurs when a conflict with a previous successful write is detected. For example, if a write operation occurs on an object and then an attempt is made to read the object using “SERIALIZABLE” consistency, this exception may result. This generally occurs when the previous write did not have time to propagate to the host serving the current request. A retry (with appropriate backoff logic) is the recommended response to this exception.
--
--
_RetryableConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_RetryableConflictException =
  _MatchServiceError cloudDirectory "RetryableConflictException" . hasStatus 409


-- | Indicates that the provided ARN value is not valid.
--
--
_InvalidARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNException =
  _MatchServiceError cloudDirectory "InvalidArnException" . hasStatus 400


-- | The specified resource could not be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError cloudDirectory "ResourceNotFoundException" . hasStatus 404


-- | The specified 'Facet' could not be found.
--
--
_FacetNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_FacetNotFoundException =
  _MatchServiceError cloudDirectory "FacetNotFoundException" . hasStatus 400


-- | Indicates that limits are exceeded. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/limits.html Limits> for more information.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError cloudDirectory "LimitExceededException" . hasStatus 400


-- | Indicates that the requested operation cannot be completed because the object has not been detached from the tree.
--
--
_ObjectNotDetachedException :: AsError a => Getting (First ServiceError) a ServiceError
_ObjectNotDetachedException =
  _MatchServiceError cloudDirectory "ObjectNotDetachedException" . hasStatus 400


-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types
    (
    -- * Service configuration
      mkServiceConfig

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
    , _InvalidArnException
    , _ResourceNotFoundException
    , _FacetNotFoundException
    , _LimitExceededException
    , _ObjectNotDetachedException

    -- * BatchListObjectAttributes
    , BatchListObjectAttributes (..)
    , mkBatchListObjectAttributes
    , bloaObjectReference
    , bloaFacetFilter
    , bloaMaxResults
    , bloaNextToken

    -- * BatchListIncomingTypedLinksResponse
    , BatchListIncomingTypedLinksResponse (..)
    , mkBatchListIncomingTypedLinksResponse
    , blitlrLinkSpecifiers
    , blitlrNextToken

    -- * DirectoryArn
    , DirectoryArn (..)

    -- * ExceptionMessage
    , ExceptionMessage (..)

    -- * TypedAttributeValueRange
    , TypedAttributeValueRange (..)
    , mkTypedAttributeValueRange
    , tavrStartMode
    , tavrEndMode
    , tavrEndValue
    , tavrStartValue

    -- * FacetAttributeDefinition
    , FacetAttributeDefinition (..)
    , mkFacetAttributeDefinition
    , fadType
    , fadDefaultValue
    , fadIsImmutable
    , fadRules

    -- * SchemaFacet
    , SchemaFacet (..)
    , mkSchemaFacet
    , sfFacetName
    , sfSchemaArn

    -- * BatchRemoveFacetFromObject
    , BatchRemoveFacetFromObject (..)
    , mkBatchRemoveFacetFromObject
    , brffoSchemaFacet
    , brffoObjectReference

    -- * BatchListObjectAttributesResponse
    , BatchListObjectAttributesResponse (..)
    , mkBatchListObjectAttributesResponse
    , bloarAttributes
    , bloarNextToken

    -- * IndexAttachment
    , IndexAttachment (..)
    , mkIndexAttachment
    , iaIndexedAttributes
    , iaObjectIdentifier

    -- * BatchRemoveFacetFromObjectResponse
    , BatchRemoveFacetFromObjectResponse (..)
    , mkBatchRemoveFacetFromObjectResponse

    -- * BatchListIncomingTypedLinks
    , BatchListIncomingTypedLinks (..)
    , mkBatchListIncomingTypedLinks
    , blitlObjectReference
    , blitlFilterAttributeRanges
    , blitlFilterTypedLink
    , blitlMaxResults
    , blitlNextToken

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * StringAttributeValue
    , StringAttributeValue (..)

    -- * FacetStyle
    , FacetStyle (..)

    -- * BatchUpdateLinkAttributes
    , BatchUpdateLinkAttributes (..)
    , mkBatchUpdateLinkAttributes
    , bulaTypedLinkSpecifier
    , bulaAttributeUpdates

    -- * FacetName
    , FacetName (..)

    -- * BatchListObjectParentPathsResponse
    , BatchListObjectParentPathsResponse (..)
    , mkBatchListObjectParentPathsResponse
    , blopprNextToken
    , blopprPathToObjectIdentifiersList

    -- * BatchAttachToIndex
    , BatchAttachToIndex (..)
    , mkBatchAttachToIndex
    , batiIndexReference
    , batiTargetReference

    -- * ObjectIdentifierAndLinkNameTuple
    , ObjectIdentifierAndLinkNameTuple (..)
    , mkObjectIdentifierAndLinkNameTuple
    , oialntLinkName
    , oialntObjectIdentifier

    -- * ObjectType
    , ObjectType (..)

    -- * TypedLinkAttributeDefinition
    , TypedLinkAttributeDefinition (..)
    , mkTypedLinkAttributeDefinition
    , tladName
    , tladType
    , tladRequiredBehavior
    , tladDefaultValue
    , tladIsImmutable
    , tladRules

    -- * BatchCreateObject
    , BatchCreateObject (..)
    , mkBatchCreateObject
    , bcoSchemaFacet
    , bcoObjectAttributeList
    , bcoBatchReferenceName
    , bcoLinkName
    , bcoParentReference

    -- * Arn
    , Arn (..)

    -- * BatchGetLinkAttributes
    , BatchGetLinkAttributes (..)
    , mkBatchGetLinkAttributes
    , bglaTypedLinkSpecifier
    , bglaAttributeNames

    -- * BatchLookupPolicyResponse
    , BatchLookupPolicyResponse (..)
    , mkBatchLookupPolicyResponse
    , blprNextToken
    , blprPolicyToPathList

    -- * BatchGetObjectInformationResponse
    , BatchGetObjectInformationResponse (..)
    , mkBatchGetObjectInformationResponse
    , bgoirObjectIdentifier
    , bgoirSchemaFacets

    -- * BatchDeleteObject
    , BatchDeleteObject (..)
    , mkBatchDeleteObject
    , bdoObjectReference

    -- * BatchListAttachedIndicesResponse
    , BatchListAttachedIndicesResponse (..)
    , mkBatchListAttachedIndicesResponse
    , blairIndexAttachments
    , blairNextToken

    -- * ConsistencyLevel
    , ConsistencyLevel (..)

    -- * RuleParameterKey
    , RuleParameterKey (..)

    -- * BatchListIndex
    , BatchListIndex (..)
    , mkBatchListIndex
    , bliIndexReference
    , bliMaxResults
    , bliNextToken
    , bliRangesOnIndexedValues

    -- * BatchReadOperationResponse
    , BatchReadOperationResponse (..)
    , mkBatchReadOperationResponse
    , brorExceptionResponse
    , brorSuccessfulResponse

    -- * PolicyType
    , PolicyType (..)

    -- * PathString
    , PathString (..)

    -- * BatchDetachFromIndexResponse
    , BatchDetachFromIndexResponse (..)
    , mkBatchDetachFromIndexResponse
    , bdfirDetachedObjectIdentifier

    -- * BatchDetachFromIndex
    , BatchDetachFromIndex (..)
    , mkBatchDetachFromIndex
    , bdfiIndexReference
    , bdfiTargetReference

    -- * AttributeNameAndValue
    , AttributeNameAndValue (..)
    , mkAttributeNameAndValue
    , anavAttributeName
    , anavValue

    -- * BatchListAttachedIndices
    , BatchListAttachedIndices (..)
    , mkBatchListAttachedIndices
    , blaiTargetReference
    , blaiMaxResults
    , blaiNextToken

    -- * BatchListIndexResponse
    , BatchListIndexResponse (..)
    , mkBatchListIndexResponse
    , blirIndexAttachments
    , blirNextToken

    -- * BatchDeleteObjectResponse
    , BatchDeleteObjectResponse (..)
    , mkBatchDeleteObjectResponse

    -- * TypedAttributeValue
    , TypedAttributeValue (..)
    , mkTypedAttributeValue
    , tavBinaryValue
    , tavBooleanValue
    , tavDatetimeValue
    , tavNumberValue
    , tavStringValue

    -- * PathToObjectIdentifiers
    , PathToObjectIdentifiers (..)
    , mkPathToObjectIdentifiers
    , ptoiObjectIdentifiers
    , ptoiPath

    -- * BatchReadOperation
    , BatchReadOperation (..)
    , mkBatchReadOperation
    , broGetLinkAttributes
    , broGetObjectAttributes
    , broGetObjectInformation
    , broListAttachedIndices
    , broListIncomingTypedLinks
    , broListIndex
    , broListObjectAttributes
    , broListObjectChildren
    , broListObjectParentPaths
    , broListObjectParents
    , broListObjectPolicies
    , broListOutgoingTypedLinks
    , broListPolicyAttachments
    , broLookupPolicy

    -- * AttributeKeyAndValue
    , AttributeKeyAndValue (..)
    , mkAttributeKeyAndValue
    , akavKey
    , akavValue

    -- * BatchGetObjectInformation
    , BatchGetObjectInformation (..)
    , mkBatchGetObjectInformation
    , bgoiObjectReference

    -- * ObjectIdentifier
    , ObjectIdentifier (..)

    -- * SchemaJsonDocument
    , SchemaJsonDocument (..)

    -- * BatchListObjectPoliciesResponse
    , BatchListObjectPoliciesResponse (..)
    , mkBatchListObjectPoliciesResponse
    , bAttachedPolicyIds
    , bNextToken

    -- * PolicyToPath
    , PolicyToPath (..)
    , mkPolicyToPath
    , ptpPath
    , ptpPolicies

    -- * SchemaName
    , SchemaName (..)

    -- * BatchCreateIndex
    , BatchCreateIndex (..)
    , mkBatchCreateIndex
    , bciOrderedIndexedAttributeList
    , bciIsUnique
    , bciBatchReferenceName
    , bciLinkName
    , bciParentReference

    -- * BatchDetachObject
    , BatchDetachObject (..)
    , mkBatchDetachObject
    , bdoParentReference
    , bdoLinkName
    , bdoBatchReferenceName

    -- * Rule
    , Rule (..)
    , mkRule
    , rParameters
    , rType

    -- * BatchAddFacetToObject
    , BatchAddFacetToObject (..)
    , mkBatchAddFacetToObject
    , baftoSchemaFacet
    , baftoObjectAttributeList
    , baftoObjectReference

    -- * FacetAttribute
    , FacetAttribute (..)
    , mkFacetAttribute
    , faName
    , faAttributeDefinition
    , faAttributeReference
    , faRequiredBehavior

    -- * RuleKey
    , RuleKey (..)

    -- * BatchAttachToIndexResponse
    , BatchAttachToIndexResponse (..)
    , mkBatchAttachToIndexResponse
    , batirAttachedObjectIdentifier

    -- * BatchUpdateLinkAttributesResponse
    , BatchUpdateLinkAttributesResponse (..)
    , mkBatchUpdateLinkAttributesResponse

    -- * RequiredAttributeBehavior
    , RequiredAttributeBehavior (..)

    -- * BatchReadException
    , BatchReadException (..)
    , mkBatchReadException
    , breMessage
    , breType

    -- * BatchListObjectParentPaths
    , BatchListObjectParentPaths (..)
    , mkBatchListObjectParentPaths
    , bloppObjectReference
    , bloppMaxResults
    , bloppNextToken

    -- * BatchAttachPolicyResponse
    , BatchAttachPolicyResponse (..)
    , mkBatchAttachPolicyResponse

    -- * TypedLinkSchemaAndFacetName
    , TypedLinkSchemaAndFacetName (..)
    , mkTypedLinkSchemaAndFacetName
    , tlsafnSchemaArn
    , tlsafnTypedLinkName

    -- * RangeMode
    , RangeMode (..)

    -- * NextToken
    , NextToken (..)

    -- * BatchUpdateObjectAttributesResponse
    , BatchUpdateObjectAttributesResponse (..)
    , mkBatchUpdateObjectAttributesResponse
    , buoarObjectIdentifier

    -- * TypedLinkFacetAttributeUpdate
    , TypedLinkFacetAttributeUpdate (..)
    , mkTypedLinkFacetAttributeUpdate
    , tlfauAttribute
    , tlfauAction

    -- * BatchListPolicyAttachments
    , BatchListPolicyAttachments (..)
    , mkBatchListPolicyAttachments
    , blpaPolicyReference
    , blpaMaxResults
    , blpaNextToken

    -- * BatchDetachTypedLinkResponse
    , BatchDetachTypedLinkResponse (..)
    , mkBatchDetachTypedLinkResponse

    -- * BatchListObjectParents
    , BatchListObjectParents (..)
    , mkBatchListObjectParents
    , blopObjectReference
    , blopMaxResults
    , blopNextToken

    -- * BatchWriteOperation
    , BatchWriteOperation (..)
    , mkBatchWriteOperation
    , bwoAddFacetToObject
    , bwoAttachObject
    , bwoAttachPolicy
    , bwoAttachToIndex
    , bwoAttachTypedLink
    , bwoCreateIndex
    , bwoCreateObject
    , bwoDeleteObject
    , bwoDetachFromIndex
    , bwoDetachObject
    , bwoDetachPolicy
    , bwoDetachTypedLink
    , bwoRemoveFacetFromObject
    , bwoUpdateLinkAttributes
    , bwoUpdateObjectAttributes

    -- * RuleParameterValue
    , RuleParameterValue (..)

    -- * BatchListOutgoingTypedLinksResponse
    , BatchListOutgoingTypedLinksResponse (..)
    , mkBatchListOutgoingTypedLinksResponse
    , blotlrNextToken
    , blotlrTypedLinkSpecifiers

    -- * BatchDetachTypedLink
    , BatchDetachTypedLink (..)
    , mkBatchDetachTypedLink
    , bdtlTypedLinkSpecifier

    -- * BatchAttachPolicy
    , BatchAttachPolicy (..)
    , mkBatchAttachPolicy
    , bapPolicyReference
    , bapObjectReference

    -- * ObjectAttributeAction
    , ObjectAttributeAction (..)
    , mkObjectAttributeAction
    , oaaObjectAttributeActionType
    , oaaObjectAttributeUpdateValue

    -- * BatchListOutgoingTypedLinks
    , BatchListOutgoingTypedLinks (..)
    , mkBatchListOutgoingTypedLinks
    , blotlObjectReference
    , blotlFilterAttributeRanges
    , blotlFilterTypedLink
    , blotlMaxResults
    , blotlNextToken

    -- * BatchListObjectParentsResponse
    , BatchListObjectParentsResponse (..)
    , mkBatchListObjectParentsResponse
    , bloprNextToken
    , bloprParentLinks

    -- * BatchListPolicyAttachmentsResponse
    , BatchListPolicyAttachmentsResponse (..)
    , mkBatchListPolicyAttachmentsResponse
    , blparNextToken
    , blparObjectIdentifiers

    -- * UpdateActionType
    , UpdateActionType (..)

    -- * BatchUpdateObjectAttributes
    , BatchUpdateObjectAttributes (..)
    , mkBatchUpdateObjectAttributes
    , buoaObjectReference
    , buoaAttributeUpdates

    -- * TypedLinkAttributeRange
    , TypedLinkAttributeRange (..)
    , mkTypedLinkAttributeRange
    , tlarRange
    , tlarAttributeName

    -- * LinkName
    , LinkName (..)

    -- * BatchWriteOperationResponse
    , BatchWriteOperationResponse (..)
    , mkBatchWriteOperationResponse
    , bworAddFacetToObject
    , bworAttachObject
    , bworAttachPolicy
    , bworAttachToIndex
    , bworAttachTypedLink
    , bworCreateIndex
    , bworCreateObject
    , bworDeleteObject
    , bworDetachFromIndex
    , bworDetachObject
    , bworDetachPolicy
    , bworDetachTypedLink
    , bworRemoveFacetFromObject
    , bworUpdateLinkAttributes
    , bworUpdateObjectAttributes

    -- * BatchAttachObject
    , BatchAttachObject (..)
    , mkBatchAttachObject
    , baoParentReference
    , baoChildReference
    , baoLinkName

    -- * Version
    , Version (..)

    -- * LinkAttributeAction
    , LinkAttributeAction (..)
    , mkLinkAttributeAction
    , laaAttributeActionType
    , laaAttributeUpdateValue

    -- * FacetAttributeReference
    , FacetAttributeReference (..)
    , mkFacetAttributeReference
    , farTargetFacetName
    , farTargetAttributeName

    -- * BatchListObjectPolicies
    , BatchListObjectPolicies (..)
    , mkBatchListObjectPolicies
    , blopsObjectReference
    , blopsMaxResults
    , blopsNextToken

    -- * BatchAddFacetToObjectResponse
    , BatchAddFacetToObjectResponse (..)
    , mkBatchAddFacetToObjectResponse

    -- * Facet
    , Facet (..)
    , mkFacet
    , fFacetStyle
    , fName
    , fObjectType

    -- * LinkAttributeUpdate
    , LinkAttributeUpdate (..)
    , mkLinkAttributeUpdate
    , lauAttributeAction
    , lauAttributeKey

    -- * BatchDetachObjectResponse
    , BatchDetachObjectResponse (..)
    , mkBatchDetachObjectResponse
    , bdorDetachedObjectIdentifier

    -- * BatchCreateIndexResponse
    , BatchCreateIndexResponse (..)
    , mkBatchCreateIndexResponse
    , bcirObjectIdentifier

    -- * TagKey
    , TagKey (..)

    -- * BatchReferenceName
    , BatchReferenceName (..)

    -- * BatchListObjectChildren
    , BatchListObjectChildren (..)
    , mkBatchListObjectChildren
    , blocObjectReference
    , blocMaxResults
    , blocNextToken

    -- * ObjectAttributeUpdate
    , ObjectAttributeUpdate (..)
    , mkObjectAttributeUpdate
    , oauObjectAttributeAction
    , oauObjectAttributeKey

    -- * FacetAttributeUpdate
    , FacetAttributeUpdate (..)
    , mkFacetAttributeUpdate
    , fauAction
    , fauAttribute

    -- * BatchAttachTypedLinkResponse
    , BatchAttachTypedLinkResponse (..)
    , mkBatchAttachTypedLinkResponse
    , batlrTypedLinkSpecifier

    -- * BatchDetachPolicyResponse
    , BatchDetachPolicyResponse (..)
    , mkBatchDetachPolicyResponse

    -- * TypedLinkSpecifier
    , TypedLinkSpecifier (..)
    , mkTypedLinkSpecifier
    , tlsTypedLinkFacet
    , tlsSourceObjectReference
    , tlsTargetObjectReference
    , tlsIdentityAttributeValues

    -- * BatchGetObjectAttributes
    , BatchGetObjectAttributes (..)
    , mkBatchGetObjectAttributes
    , bgoaObjectReference
    , bgoaSchemaFacet
    , bgoaAttributeNames

    -- * TypedLinkName
    , TypedLinkName (..)

    -- * SelectorObjectReference
    , SelectorObjectReference (..)

    -- * AttributeKey
    , AttributeKey (..)
    , mkAttributeKey
    , akSchemaArn
    , akFacetName
    , akName

    -- * BatchAttachTypedLink
    , BatchAttachTypedLink (..)
    , mkBatchAttachTypedLink
    , batlSourceObjectReference
    , batlTargetObjectReference
    , batlTypedLinkFacet
    , batlAttributes

    -- * BatchGetObjectAttributesResponse
    , BatchGetObjectAttributesResponse (..)
    , mkBatchGetObjectAttributesResponse
    , bgoarAttributes

    -- * AttributeName
    , AttributeName (..)

    -- * BatchListObjectChildrenResponse
    , BatchListObjectChildrenResponse (..)
    , mkBatchListObjectChildrenResponse
    , blocrChildren
    , blocrNextToken

    -- * BatchDetachPolicy
    , BatchDetachPolicy (..)
    , mkBatchDetachPolicy
    , bdpPolicyReference
    , bdpObjectReference

    -- * BatchLookupPolicy
    , BatchLookupPolicy (..)
    , mkBatchLookupPolicy
    , blpObjectReference
    , blpMaxResults
    , blpNextToken

    -- * BatchCreateObjectResponse
    , BatchCreateObjectResponse (..)
    , mkBatchCreateObjectResponse
    , bcorObjectIdentifier

    -- * RuleType
    , RuleType (..)

    -- * DirectoryName
    , DirectoryName (..)

    -- * ObjectAttributeRange
    , ObjectAttributeRange (..)
    , mkObjectAttributeRange
    , oarAttributeKey
    , oarRange

    -- * BatchGetLinkAttributesResponse
    , BatchGetLinkAttributesResponse (..)
    , mkBatchGetLinkAttributesResponse
    , bglarAttributes

    -- * DirectoryState
    , DirectoryState (..)

    -- * ObjectReference
    , ObjectReference (..)
    , mkObjectReference
    , orSelector

    -- * BatchReadSuccessfulResponse
    , BatchReadSuccessfulResponse (..)
    , mkBatchReadSuccessfulResponse
    , brsrGetLinkAttributes
    , brsrGetObjectAttributes
    , brsrGetObjectInformation
    , brsrListAttachedIndices
    , brsrListIncomingTypedLinks
    , brsrListIndex
    , brsrListObjectAttributes
    , brsrListObjectChildren
    , brsrListObjectParentPaths
    , brsrListObjectParents
    , brsrListObjectPolicies
    , brsrListOutgoingTypedLinks
    , brsrListPolicyAttachments
    , brsrLookupPolicy

    -- * BatchAttachObjectResponse
    , BatchAttachObjectResponse (..)
    , mkBatchAttachObjectResponse
    , baorAttachedObjectIdentifier

    -- * FacetAttributeType
    , FacetAttributeType (..)

    -- * Directory
    , Directory (..)
    , mkDirectory
    , dCreationDateTime
    , dDirectoryArn
    , dName
    , dState

    -- * BatchReadExceptionType
    , BatchReadExceptionType (..)

    -- * TypedLinkFacet
    , TypedLinkFacet (..)
    , mkTypedLinkFacet
    , tlfName
    , tlfAttributes
    , tlfIdentityAttributeOrder

    -- * PolicyAttachment
    , PolicyAttachment (..)
    , mkPolicyAttachment
    , paObjectIdentifier
    , paPolicyId
    , paPolicyType

    -- * SchemaArn
    , SchemaArn (..)

    -- * Name
    , Name (..)

    -- * DetachedObjectIdentifier
    , DetachedObjectIdentifier (..)

    -- * UpgradedSchemaArn
    , UpgradedSchemaArn (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * AttachedObjectIdentifier
    , AttachedObjectIdentifier (..)

    -- * MinorVersion
    , MinorVersion (..)

    -- * NumberValue
    , NumberValue (..)

    -- * TargetAttributeName
    , TargetAttributeName (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.CloudDirectory.Types.BatchListObjectAttributes
  
import Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
  
import Network.AWS.CloudDirectory.Types.DirectoryArn
  
import Network.AWS.CloudDirectory.Types.ExceptionMessage
  
  
import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
  
import Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
  
  
  
import Network.AWS.CloudDirectory.Types.SchemaFacet
  
import Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObject
  
  
  
import Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
  
import Network.AWS.CloudDirectory.Types.IndexAttachment
  
  
  
  
import Network.AWS.CloudDirectory.Types.BatchRemoveFacetFromObjectResponse
  
import Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinks
  
  
import Network.AWS.CloudDirectory.Types.Tag
  
import Network.AWS.CloudDirectory.Types.StringAttributeValue
  
import Network.AWS.CloudDirectory.Types.FacetStyle
  
import Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes
  
import Network.AWS.CloudDirectory.Types.FacetName
  
  
import Network.AWS.CloudDirectory.Types.BatchListObjectParentPathsResponse
  
  
import Network.AWS.CloudDirectory.Types.BatchAttachToIndex
  
import Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
  
import Network.AWS.CloudDirectory.Types.ObjectType
  
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
  
import Network.AWS.CloudDirectory.Types.BatchCreateObject
  
import Network.AWS.CloudDirectory.Types.Arn
  
import Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes
  
import Network.AWS.CloudDirectory.Types.BatchLookupPolicyResponse
  
import Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
  
import Network.AWS.CloudDirectory.Types.BatchDeleteObject
  
import Network.AWS.CloudDirectory.Types.BatchListAttachedIndicesResponse
  
import Network.AWS.CloudDirectory.Types.ConsistencyLevel
  
import Network.AWS.CloudDirectory.Types.RuleParameterKey
  
  
import Network.AWS.CloudDirectory.Types.BatchListIndex
  
  
import Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
  
  
import Network.AWS.CloudDirectory.Types.PolicyType
  
import Network.AWS.CloudDirectory.Types.PathString
  
import Network.AWS.CloudDirectory.Types.BatchDetachFromIndexResponse
  
import Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
  
import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
  
import Network.AWS.CloudDirectory.Types.BatchListAttachedIndices
  
import Network.AWS.CloudDirectory.Types.BatchListIndexResponse
  
import Network.AWS.CloudDirectory.Types.BatchDeleteObjectResponse
  
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
  
import Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
  
import Network.AWS.CloudDirectory.Types.BatchReadOperation
  
import Network.AWS.CloudDirectory.Types.AttributeKeyAndValue
  
  
import Network.AWS.CloudDirectory.Types.BatchGetObjectInformation
  
  
import Network.AWS.CloudDirectory.Types.ObjectIdentifier
  
import Network.AWS.CloudDirectory.Types.SchemaJsonDocument
  
import Network.AWS.CloudDirectory.Types.BatchListObjectPoliciesResponse
  
  
  
import Network.AWS.CloudDirectory.Types.PolicyToPath
  
import Network.AWS.CloudDirectory.Types.SchemaName
  
import Network.AWS.CloudDirectory.Types.BatchCreateIndex
  
import Network.AWS.CloudDirectory.Types.BatchDetachObject
  
import Network.AWS.CloudDirectory.Types.Rule
  
import Network.AWS.CloudDirectory.Types.BatchAddFacetToObject
  
import Network.AWS.CloudDirectory.Types.FacetAttribute
  
import Network.AWS.CloudDirectory.Types.RuleKey
  
  
import Network.AWS.CloudDirectory.Types.BatchAttachToIndexResponse
  
import Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributesResponse
  
import Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
  
import Network.AWS.CloudDirectory.Types.BatchReadException
  
  
import Network.AWS.CloudDirectory.Types.BatchListObjectParentPaths
  
import Network.AWS.CloudDirectory.Types.BatchAttachPolicyResponse
  
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
  
import Network.AWS.CloudDirectory.Types.RangeMode
  
import Network.AWS.CloudDirectory.Types.NextToken
  
import Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributesResponse
  
import Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
  
import Network.AWS.CloudDirectory.Types.BatchListPolicyAttachments
  
import Network.AWS.CloudDirectory.Types.BatchDetachTypedLinkResponse
  
import Network.AWS.CloudDirectory.Types.BatchListObjectParents
  
import Network.AWS.CloudDirectory.Types.BatchWriteOperation
  
import Network.AWS.CloudDirectory.Types.RuleParameterValue
  
import Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
  
import Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
  
  
import Network.AWS.CloudDirectory.Types.BatchAttachPolicy
  
import Network.AWS.CloudDirectory.Types.ObjectAttributeAction
  
import Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinks
  
import Network.AWS.CloudDirectory.Types.BatchListObjectParentsResponse
  
import Network.AWS.CloudDirectory.Types.BatchListPolicyAttachmentsResponse
  
import Network.AWS.CloudDirectory.Types.UpdateActionType
  
import Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes
  
import Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
  
import Network.AWS.CloudDirectory.Types.LinkName
  
import Network.AWS.CloudDirectory.Types.BatchWriteOperationResponse
  
  
import Network.AWS.CloudDirectory.Types.BatchAttachObject
  
import Network.AWS.CloudDirectory.Types.Version
  
import Network.AWS.CloudDirectory.Types.LinkAttributeAction
  
  
  
import Network.AWS.CloudDirectory.Types.FacetAttributeReference
  
  
import Network.AWS.CloudDirectory.Types.BatchListObjectPolicies
  
import Network.AWS.CloudDirectory.Types.BatchAddFacetToObjectResponse
  
import Network.AWS.CloudDirectory.Types.Facet
  
import Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
  
import Network.AWS.CloudDirectory.Types.BatchDetachObjectResponse
  
import Network.AWS.CloudDirectory.Types.BatchCreateIndexResponse
  
  
import Network.AWS.CloudDirectory.Types.TagKey
  
import Network.AWS.CloudDirectory.Types.BatchReferenceName
  
import Network.AWS.CloudDirectory.Types.BatchListObjectChildren
  
import Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
  
  
  
import Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
  
import Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
  
import Network.AWS.CloudDirectory.Types.BatchDetachPolicyResponse
  
import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
  
import Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes
  
import Network.AWS.CloudDirectory.Types.TypedLinkName
  
import Network.AWS.CloudDirectory.Types.SelectorObjectReference
  
import Network.AWS.CloudDirectory.Types.AttributeKey
  
import Network.AWS.CloudDirectory.Types.BatchAttachTypedLink
  
import Network.AWS.CloudDirectory.Types.BatchGetObjectAttributesResponse
  
  
  
import Network.AWS.CloudDirectory.Types.AttributeName
  
import Network.AWS.CloudDirectory.Types.BatchListObjectChildrenResponse
  
import Network.AWS.CloudDirectory.Types.BatchDetachPolicy
  
  
import Network.AWS.CloudDirectory.Types.BatchLookupPolicy
  
import Network.AWS.CloudDirectory.Types.BatchCreateObjectResponse
  
import Network.AWS.CloudDirectory.Types.RuleType
  
import Network.AWS.CloudDirectory.Types.DirectoryName
  
import Network.AWS.CloudDirectory.Types.ObjectAttributeRange
  
import Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
  
import Network.AWS.CloudDirectory.Types.DirectoryState
  
import Network.AWS.CloudDirectory.Types.ObjectReference
  
  
import Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse
  
import Network.AWS.CloudDirectory.Types.BatchAttachObjectResponse
  
import Network.AWS.CloudDirectory.Types.FacetAttributeType
  
import Network.AWS.CloudDirectory.Types.Directory
  
import Network.AWS.CloudDirectory.Types.BatchReadExceptionType
  
import Network.AWS.CloudDirectory.Types.TypedLinkFacet
  
  
import Network.AWS.CloudDirectory.Types.PolicyAttachment
  
  
  
import Network.AWS.CloudDirectory.Types.SchemaArn
  
import Network.AWS.CloudDirectory.Types.Name
  
import Network.AWS.CloudDirectory.Types.DetachedObjectIdentifier
  
import Network.AWS.CloudDirectory.Types.UpgradedSchemaArn
  
import Network.AWS.CloudDirectory.Types.Key
  
import Network.AWS.CloudDirectory.Types.Value
  
import Network.AWS.CloudDirectory.Types.AttachedObjectIdentifier
  
import Network.AWS.CloudDirectory.Types.MinorVersion
  
import Network.AWS.CloudDirectory.Types.NumberValue
  
import Network.AWS.CloudDirectory.Types.TargetAttributeName
  

-- | API version @2017-01-11@ of the Amazon CloudDirectory SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "CloudDirectory",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "clouddirectory",
                 Core._svcVersion = "2017-01-11", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "CloudDirectory",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | Indicates that the requested index type is not supported.
_UnsupportedIndexTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedIndexTypeException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedIndexTypeException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _UnsupportedIndexTypeException #-}
{-# DEPRECATED _UnsupportedIndexTypeException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the requested operation can only operate on index objects.
_NotIndexException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotIndexException
  = Core._MatchServiceError mkServiceConfig "NotIndexException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _NotIndexException #-}
{-# DEPRECATED _NotIndexException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that your request is malformed in some manner. See the exception message.
_ValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ValidationException
  = Core._MatchServiceError mkServiceConfig "ValidationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ValidationException #-}
{-# DEPRECATED _ValidationException "Use generic-lens or generic-optics instead"  #-}

-- | Access denied. Check your permissions.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
      Core.. Core.hasStatues 403
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | A facet with the same name already exists.
_FacetAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FacetAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "FacetAlreadyExistsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _FacetAlreadyExistsException #-}
{-# DEPRECATED _FacetAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided @SchemaDoc@ value is not valid.
_InvalidSchemaDocException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSchemaDocException
  = Core._MatchServiceError mkServiceConfig
      "InvalidSchemaDocException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidSchemaDocException #-}
{-# DEPRECATED _InvalidSchemaDocException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that an attempt to make an attachment was invalid. For example, attaching two nodes with a link type that is not applicable to the nodes or attempting to apply a schema to a directory a second time.
_InvalidAttachmentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAttachmentException
  = Core._MatchServiceError mkServiceConfig
      "InvalidAttachmentException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidAttachmentException #-}
{-# DEPRECATED _InvalidAttachmentException "Use generic-lens or generic-optics instead"  #-}

-- | Cannot list the parents of a 'Directory' root.
_CannotListParentOfRootException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CannotListParentOfRootException
  = Core._MatchServiceError mkServiceConfig
      "CannotListParentOfRootException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _CannotListParentOfRootException #-}
{-# DEPRECATED _CannotListParentOfRootException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the requested operation can only operate on policy objects.
_NotPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotPolicyException
  = Core._MatchServiceError mkServiceConfig "NotPolicyException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _NotPolicyException #-}
{-# DEPRECATED _NotPolicyException "Use generic-lens or generic-optics instead"  #-}

-- | Can occur for multiple reasons such as when you tag a resource that doesn’t exist or if you specify a higher number of tags for a resource than the allowed limit. Allowed limit is 50 tags per resource.
_InvalidTaggingRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTaggingRequestException
  = Core._MatchServiceError mkServiceConfig
      "InvalidTaggingRequestException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidTaggingRequestException #-}
{-# DEPRECATED _InvalidTaggingRequestException "Use generic-lens or generic-optics instead"  #-}

-- | An attempt to modify a 'Facet' resulted in an invalid schema exception.
_InvalidFacetUpdateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFacetUpdateException
  = Core._MatchServiceError mkServiceConfig
      "InvalidFacetUpdateException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidFacetUpdateException #-}
{-# DEPRECATED _InvalidFacetUpdateException "Use generic-lens or generic-optics instead"  #-}

-- | Occurs when any of the rule parameter keys or values are invalid.
_InvalidRuleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRuleException
  = Core._MatchServiceError mkServiceConfig "InvalidRuleException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidRuleException #-}
{-# DEPRECATED _InvalidRuleException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a schema is already published.
_SchemaAlreadyPublishedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SchemaAlreadyPublishedException
  = Core._MatchServiceError mkServiceConfig
      "SchemaAlreadyPublishedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SchemaAlreadyPublishedException #-}
{-# DEPRECATED _SchemaAlreadyPublishedException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a 'Directory' could not be created due to a naming conflict. Choose a different name and try again.
_DirectoryAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryAlreadyExistsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DirectoryAlreadyExistsException #-}
{-# DEPRECATED _DirectoryAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | An operation can only operate on a disabled directory.
_DirectoryNotDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryNotDisabledException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryNotDisabledException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DirectoryNotDisabledException #-}
{-# DEPRECATED _DirectoryNotDisabledException "Use generic-lens or generic-optics instead"  #-}

-- | A @BatchWrite@ exception has occurred.
_BatchWriteException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BatchWriteException
  = Core._MatchServiceError mkServiceConfig "BatchWriteException"
{-# INLINEABLE _BatchWriteException #-}
{-# DEPRECATED _BatchWriteException "Use generic-lens or generic-optics instead"  #-}

-- | Operations are only permitted on enabled directories.
_DirectoryNotEnabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryNotEnabledException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryNotEnabledException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DirectoryNotEnabledException #-}
{-# DEPRECATED _DirectoryNotEnabledException "Use generic-lens or generic-optics instead"  #-}

-- | Occurs when deleting a facet that contains an attribute that is a target to an attribute reference in a different facet.
_FacetInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FacetInUseException
  = Core._MatchServiceError mkServiceConfig "FacetInUseException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _FacetInUseException #-}
{-# DEPRECATED _FacetInUseException "Use generic-lens or generic-optics instead"  #-}

-- | The 'Facet' that you provided was not well formed or could not be validated with the schema.
_FacetValidationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FacetValidationException
  = Core._MatchServiceError mkServiceConfig
      "FacetValidationException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _FacetValidationException #-}
{-# DEPRECATED _FacetValidationException "Use generic-lens or generic-optics instead"  #-}

-- | The object could not be deleted because links still exist. Remove the links and then try the operation again.
_StillContainsLinksException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_StillContainsLinksException
  = Core._MatchServiceError mkServiceConfig
      "StillContainsLinksException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _StillContainsLinksException #-}
{-# DEPRECATED _StillContainsLinksException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates a failure occurred while performing a check for backward compatibility between the specified schema and the schema that is currently applied to the directory.
_IncompatibleSchemaException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IncompatibleSchemaException
  = Core._MatchServiceError mkServiceConfig
      "IncompatibleSchemaException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IncompatibleSchemaException #-}
{-# DEPRECATED _IncompatibleSchemaException "Use generic-lens or generic-optics instead"  #-}

-- | Occurs when any invalid operations are performed on an object that is not a node, such as calling @ListObjectChildren@ for a leaf node object.
_NotNodeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotNodeException
  = Core._MatchServiceError mkServiceConfig "NotNodeException" Core..
      Core.hasStatues 400
{-# INLINEABLE _NotNodeException #-}
{-# DEPRECATED _NotNodeException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the @NextToken@ value is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the object is not attached to the index.
_ObjectAlreadyDetachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectAlreadyDetachedException
  = Core._MatchServiceError mkServiceConfig
      "ObjectAlreadyDetachedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ObjectAlreadyDetachedException #-}
{-# DEPRECATED _ObjectAlreadyDetachedException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a link could not be created due to a naming conflict. Choose a different name and then try again.
_LinkNameAlreadyInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LinkNameAlreadyInUseException
  = Core._MatchServiceError mkServiceConfig
      "LinkNameAlreadyInUseException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _LinkNameAlreadyInUseException #-}
{-# DEPRECATED _LinkNameAlreadyInUseException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates a problem that must be resolved by Amazon Web Services. This might be a transient error in which case you can retry your request until it succeeds. Otherwise, go to the <http://status.aws.amazon.com/ AWS Service Health Dashboard> site to see if there are any operational issues with the service.
_InternalServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceException
  = Core._MatchServiceError mkServiceConfig
      "InternalServiceException"
      Core.. Core.hasStatues 500
{-# INLINEABLE _InternalServiceException #-}
{-# DEPRECATED _InternalServiceException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that a schema could not be created due to a naming conflict. Please select a different name and then try again.
_SchemaAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SchemaAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "SchemaAlreadyExistsException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _SchemaAlreadyExistsException #-}
{-# DEPRECATED _SchemaAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | An object has been attempted to be attached to an object that does not have the appropriate attribute value.
_IndexedAttributeMissingException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IndexedAttributeMissingException
  = Core._MatchServiceError mkServiceConfig
      "IndexedAttributeMissingException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _IndexedAttributeMissingException #-}
{-# DEPRECATED _IndexedAttributeMissingException "Use generic-lens or generic-optics instead"  #-}

-- | A directory that has been deleted and to which access has been attempted. Note: The requested resource will eventually cease to exist.
_DirectoryDeletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryDeletedException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryDeletedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DirectoryDeletedException #-}
{-# DEPRECATED _DirectoryDeletedException "Use generic-lens or generic-optics instead"  #-}

-- | Occurs when a conflict with a previous successful write is detected. For example, if a write operation occurs on an object and then an attempt is made to read the object using “SERIALIZABLE” consistency, this exception may result. This generally occurs when the previous write did not have time to propagate to the host serving the current request. A retry (with appropriate backoff logic) is the recommended response to this exception.
_RetryableConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RetryableConflictException
  = Core._MatchServiceError mkServiceConfig
      "RetryableConflictException"
      Core.. Core.hasStatues 409
{-# INLINEABLE _RetryableConflictException #-}
{-# DEPRECATED _RetryableConflictException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the provided ARN value is not valid.
_InvalidArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidArnException
  = Core._MatchServiceError mkServiceConfig "InvalidArnException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidArnException #-}
{-# DEPRECATED _InvalidArnException "Use generic-lens or generic-optics instead"  #-}

-- | The specified resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
      Core.. Core.hasStatues 404
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The specified 'Facet' could not be found.
_FacetNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FacetNotFoundException
  = Core._MatchServiceError mkServiceConfig "FacetNotFoundException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _FacetNotFoundException #-}
{-# DEPRECATED _FacetNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that limits are exceeded. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/limits.html Limits> for more information.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | Indicates that the requested operation cannot be completed because the object has not been detached from the tree.
_ObjectNotDetachedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ObjectNotDetachedException
  = Core._MatchServiceError mkServiceConfig
      "ObjectNotDetachedException"
      Core.. Core.hasStatues 400
{-# INLINEABLE _ObjectNotDetachedException #-}
{-# DEPRECATED _ObjectNotDetachedException "Use generic-lens or generic-optics instead"  #-}

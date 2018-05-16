{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Cloud Directory__
--
-- Amazon Cloud Directory is a component of the AWS Directory Service that simplifies the development and management of cloud-scale web, mobile, and IoT applications. This guide describes the Cloud Directory operations that you can call programmatically and includes detailed information on data types and errors. For information about AWS Directory Services features, see <https://aws.amazon.com/directoryservice/ AWS Directory Service> and the <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html AWS Directory Service Administration Guide> .
--
module Network.AWS.CloudDirectory
    (
    -- * Service Configuration
      cloudDirectory

    -- * Errors
    -- $errors

    -- ** UnsupportedIndexTypeException
    , _UnsupportedIndexTypeException

    -- ** NotIndexException
    , _NotIndexException

    -- ** ValidationException
    , _ValidationException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** FacetAlreadyExistsException
    , _FacetAlreadyExistsException

    -- ** InvalidSchemaDocException
    , _InvalidSchemaDocException

    -- ** InvalidAttachmentException
    , _InvalidAttachmentException

    -- ** CannotListParentOfRootException
    , _CannotListParentOfRootException

    -- ** NotPolicyException
    , _NotPolicyException

    -- ** InvalidTaggingRequestException
    , _InvalidTaggingRequestException

    -- ** InvalidFacetUpdateException
    , _InvalidFacetUpdateException

    -- ** InvalidRuleException
    , _InvalidRuleException

    -- ** SchemaAlreadyPublishedException
    , _SchemaAlreadyPublishedException

    -- ** DirectoryAlreadyExistsException
    , _DirectoryAlreadyExistsException

    -- ** DirectoryNotDisabledException
    , _DirectoryNotDisabledException

    -- ** BatchWriteException
    , _BatchWriteException

    -- ** DirectoryNotEnabledException
    , _DirectoryNotEnabledException

    -- ** FacetInUseException
    , _FacetInUseException

    -- ** FacetValidationException
    , _FacetValidationException

    -- ** StillContainsLinksException
    , _StillContainsLinksException

    -- ** IncompatibleSchemaException
    , _IncompatibleSchemaException

    -- ** NotNodeException
    , _NotNodeException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** ObjectAlreadyDetachedException
    , _ObjectAlreadyDetachedException

    -- ** LinkNameAlreadyInUseException
    , _LinkNameAlreadyInUseException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** SchemaAlreadyExistsException
    , _SchemaAlreadyExistsException

    -- ** IndexedAttributeMissingException
    , _IndexedAttributeMissingException

    -- ** DirectoryDeletedException
    , _DirectoryDeletedException

    -- ** RetryableConflictException
    , _RetryableConflictException

    -- ** InvalidARNException
    , _InvalidARNException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** FacetNotFoundException
    , _FacetNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ObjectNotDetachedException
    , _ObjectNotDetachedException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListTypedLinkFacetAttributes (Paginated)
    , module Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes

    -- ** DeleteObject
    , module Network.AWS.CloudDirectory.DeleteObject

    -- ** ListIndex (Paginated)
    , module Network.AWS.CloudDirectory.ListIndex

    -- ** UpgradeAppliedSchema
    , module Network.AWS.CloudDirectory.UpgradeAppliedSchema

    -- ** GetDirectory
    , module Network.AWS.CloudDirectory.GetDirectory

    -- ** GetObjectInformation
    , module Network.AWS.CloudDirectory.GetObjectInformation

    -- ** ListAttachedIndices (Paginated)
    , module Network.AWS.CloudDirectory.ListAttachedIndices

    -- ** DetachFromIndex
    , module Network.AWS.CloudDirectory.DetachFromIndex

    -- ** LookupPolicy (Paginated)
    , module Network.AWS.CloudDirectory.LookupPolicy

    -- ** ListTagsForResource (Paginated)
    , module Network.AWS.CloudDirectory.ListTagsForResource

    -- ** ListPublishedSchemaARNs (Paginated)
    , module Network.AWS.CloudDirectory.ListPublishedSchemaARNs

    -- ** ListDirectories (Paginated)
    , module Network.AWS.CloudDirectory.ListDirectories

    -- ** CreateTypedLinkFacet
    , module Network.AWS.CloudDirectory.CreateTypedLinkFacet

    -- ** ListObjectParentPaths (Paginated)
    , module Network.AWS.CloudDirectory.ListObjectParentPaths

    -- ** DisableDirectory
    , module Network.AWS.CloudDirectory.DisableDirectory

    -- ** CreateDirectory
    , module Network.AWS.CloudDirectory.CreateDirectory

    -- ** ListFacetAttributes (Paginated)
    , module Network.AWS.CloudDirectory.ListFacetAttributes

    -- ** UpdateTypedLinkFacet
    , module Network.AWS.CloudDirectory.UpdateTypedLinkFacet

    -- ** DeleteTypedLinkFacet
    , module Network.AWS.CloudDirectory.DeleteTypedLinkFacet

    -- ** GetAppliedSchemaVersion
    , module Network.AWS.CloudDirectory.GetAppliedSchemaVersion

    -- ** RemoveFacetFromObject
    , module Network.AWS.CloudDirectory.RemoveFacetFromObject

    -- ** EnableDirectory
    , module Network.AWS.CloudDirectory.EnableDirectory

    -- ** ListObjectAttributes (Paginated)
    , module Network.AWS.CloudDirectory.ListObjectAttributes

    -- ** ListAppliedSchemaARNs (Paginated)
    , module Network.AWS.CloudDirectory.ListAppliedSchemaARNs

    -- ** ListIncomingTypedLinks
    , module Network.AWS.CloudDirectory.ListIncomingTypedLinks

    -- ** GetFacet
    , module Network.AWS.CloudDirectory.GetFacet

    -- ** GetTypedLinkFacetInformation
    , module Network.AWS.CloudDirectory.GetTypedLinkFacetInformation

    -- ** ListDevelopmentSchemaARNs (Paginated)
    , module Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs

    -- ** AttachObject
    , module Network.AWS.CloudDirectory.AttachObject

    -- ** BatchWrite
    , module Network.AWS.CloudDirectory.BatchWrite

    -- ** CreateObject
    , module Network.AWS.CloudDirectory.CreateObject

    -- ** UpgradePublishedSchema
    , module Network.AWS.CloudDirectory.UpgradePublishedSchema

    -- ** CreateFacet
    , module Network.AWS.CloudDirectory.CreateFacet

    -- ** GetObjectAttributes
    , module Network.AWS.CloudDirectory.GetObjectAttributes

    -- ** DeleteFacet
    , module Network.AWS.CloudDirectory.DeleteFacet

    -- ** UpdateFacet
    , module Network.AWS.CloudDirectory.UpdateFacet

    -- ** ListObjectChildren
    , module Network.AWS.CloudDirectory.ListObjectChildren

    -- ** ListTypedLinkFacetNames (Paginated)
    , module Network.AWS.CloudDirectory.ListTypedLinkFacetNames

    -- ** AttachTypedLink
    , module Network.AWS.CloudDirectory.AttachTypedLink

    -- ** DetachPolicy
    , module Network.AWS.CloudDirectory.DetachPolicy

    -- ** CreateIndex
    , module Network.AWS.CloudDirectory.CreateIndex

    -- ** DetachObject
    , module Network.AWS.CloudDirectory.DetachObject

    -- ** AddFacetToObject
    , module Network.AWS.CloudDirectory.AddFacetToObject

    -- ** ApplySchema
    , module Network.AWS.CloudDirectory.ApplySchema

    -- ** CreateSchema
    , module Network.AWS.CloudDirectory.CreateSchema

    -- ** GetSchemaAsJSON
    , module Network.AWS.CloudDirectory.GetSchemaAsJSON

    -- ** PublishSchema
    , module Network.AWS.CloudDirectory.PublishSchema

    -- ** DeleteDirectory
    , module Network.AWS.CloudDirectory.DeleteDirectory

    -- ** ListObjectParents
    , module Network.AWS.CloudDirectory.ListObjectParents

    -- ** ListPolicyAttachments (Paginated)
    , module Network.AWS.CloudDirectory.ListPolicyAttachments

    -- ** TagResource
    , module Network.AWS.CloudDirectory.TagResource

    -- ** UpdateSchema
    , module Network.AWS.CloudDirectory.UpdateSchema

    -- ** DeleteSchema
    , module Network.AWS.CloudDirectory.DeleteSchema

    -- ** DetachTypedLink
    , module Network.AWS.CloudDirectory.DetachTypedLink

    -- ** ListFacetNames (Paginated)
    , module Network.AWS.CloudDirectory.ListFacetNames

    -- ** UntagResource
    , module Network.AWS.CloudDirectory.UntagResource

    -- ** ListOutgoingTypedLinks
    , module Network.AWS.CloudDirectory.ListOutgoingTypedLinks

    -- ** UpdateObjectAttributes
    , module Network.AWS.CloudDirectory.UpdateObjectAttributes

    -- ** AttachPolicy
    , module Network.AWS.CloudDirectory.AttachPolicy

    -- ** BatchRead
    , module Network.AWS.CloudDirectory.BatchRead

    -- ** PutSchemaFromJSON
    , module Network.AWS.CloudDirectory.PutSchemaFromJSON

    -- ** AttachToIndex
    , module Network.AWS.CloudDirectory.AttachToIndex

    -- ** ListObjectPolicies (Paginated)
    , module Network.AWS.CloudDirectory.ListObjectPolicies

    -- * Types

    -- ** BatchReadExceptionType
    , BatchReadExceptionType (..)

    -- ** ConsistencyLevel
    , ConsistencyLevel (..)

    -- ** DirectoryState
    , DirectoryState (..)

    -- ** FacetAttributeType
    , FacetAttributeType (..)

    -- ** ObjectType
    , ObjectType (..)

    -- ** RangeMode
    , RangeMode (..)

    -- ** RequiredAttributeBehavior
    , RequiredAttributeBehavior (..)

    -- ** RuleType
    , RuleType (..)

    -- ** UpdateActionType
    , UpdateActionType (..)

    -- ** AttributeKey
    , AttributeKey
    , attributeKey
    , akSchemaARN
    , akFacetName
    , akName

    -- ** AttributeKeyAndValue
    , AttributeKeyAndValue
    , attributeKeyAndValue
    , akavKey
    , akavValue

    -- ** AttributeNameAndValue
    , AttributeNameAndValue
    , attributeNameAndValue
    , anavAttributeName
    , anavValue

    -- ** BatchAddFacetToObject
    , BatchAddFacetToObject
    , batchAddFacetToObject
    , baftoSchemaFacet
    , baftoObjectAttributeList
    , baftoObjectReference

    -- ** BatchAddFacetToObjectResponse
    , BatchAddFacetToObjectResponse
    , batchAddFacetToObjectResponse

    -- ** BatchAttachObject
    , BatchAttachObject
    , batchAttachObject
    , baoParentReference
    , baoChildReference
    , baoLinkName

    -- ** BatchAttachObjectResponse
    , BatchAttachObjectResponse
    , batchAttachObjectResponse
    , baoAttachedObjectIdentifier

    -- ** BatchAttachPolicy
    , BatchAttachPolicy
    , batchAttachPolicy
    , bapPolicyReference
    , bapObjectReference

    -- ** BatchAttachPolicyResponse
    , BatchAttachPolicyResponse
    , batchAttachPolicyResponse

    -- ** BatchAttachToIndex
    , BatchAttachToIndex
    , batchAttachToIndex
    , batiIndexReference
    , batiTargetReference

    -- ** BatchAttachToIndexResponse
    , BatchAttachToIndexResponse
    , batchAttachToIndexResponse
    , batiAttachedObjectIdentifier

    -- ** BatchAttachTypedLink
    , BatchAttachTypedLink
    , batchAttachTypedLink
    , batlSourceObjectReference
    , batlTargetObjectReference
    , batlTypedLinkFacet
    , batlAttributes

    -- ** BatchAttachTypedLinkResponse
    , BatchAttachTypedLinkResponse
    , batchAttachTypedLinkResponse
    , batlTypedLinkSpecifier

    -- ** BatchCreateIndex
    , BatchCreateIndex
    , batchCreateIndex
    , bciParentReference
    , bciLinkName
    , bciBatchReferenceName
    , bciOrderedIndexedAttributeList
    , bciIsUnique

    -- ** BatchCreateIndexResponse
    , BatchCreateIndexResponse
    , batchCreateIndexResponse
    , bciObjectIdentifier

    -- ** BatchCreateObject
    , BatchCreateObject
    , batchCreateObject
    , bcoParentReference
    , bcoLinkName
    , bcoBatchReferenceName
    , bcoSchemaFacet
    , bcoObjectAttributeList

    -- ** BatchCreateObjectResponse
    , BatchCreateObjectResponse
    , batchCreateObjectResponse
    , bcoObjectIdentifier

    -- ** BatchDeleteObject
    , BatchDeleteObject
    , batchDeleteObject
    , bdoObjectReference

    -- ** BatchDeleteObjectResponse
    , BatchDeleteObjectResponse
    , batchDeleteObjectResponse

    -- ** BatchDetachFromIndex
    , BatchDetachFromIndex
    , batchDetachFromIndex
    , bdfiIndexReference
    , bdfiTargetReference

    -- ** BatchDetachFromIndexResponse
    , BatchDetachFromIndexResponse
    , batchDetachFromIndexResponse
    , bdfiDetachedObjectIdentifier

    -- ** BatchDetachObject
    , BatchDetachObject
    , batchDetachObject
    , bdoBatchReferenceName
    , bdoParentReference
    , bdoLinkName

    -- ** BatchDetachObjectResponse
    , BatchDetachObjectResponse
    , batchDetachObjectResponse
    , bdoDetachedObjectIdentifier

    -- ** BatchDetachPolicy
    , BatchDetachPolicy
    , batchDetachPolicy
    , bdpPolicyReference
    , bdpObjectReference

    -- ** BatchDetachPolicyResponse
    , BatchDetachPolicyResponse
    , batchDetachPolicyResponse

    -- ** BatchDetachTypedLink
    , BatchDetachTypedLink
    , batchDetachTypedLink
    , bdtlTypedLinkSpecifier

    -- ** BatchDetachTypedLinkResponse
    , BatchDetachTypedLinkResponse
    , batchDetachTypedLinkResponse

    -- ** BatchGetObjectAttributes
    , BatchGetObjectAttributes
    , batchGetObjectAttributes
    , bgoaObjectReference
    , bgoaSchemaFacet
    , bgoaAttributeNames

    -- ** BatchGetObjectAttributesResponse
    , BatchGetObjectAttributesResponse
    , batchGetObjectAttributesResponse
    , bgoaAttributes

    -- ** BatchGetObjectInformation
    , BatchGetObjectInformation
    , batchGetObjectInformation
    , bgoiObjectReference

    -- ** BatchGetObjectInformationResponse
    , BatchGetObjectInformationResponse
    , batchGetObjectInformationResponse
    , bgoiObjectIdentifier
    , bgoiSchemaFacets

    -- ** BatchListAttachedIndices
    , BatchListAttachedIndices
    , batchListAttachedIndices
    , blaisNextToken
    , blaisMaxResults
    , blaisTargetReference

    -- ** BatchListAttachedIndicesResponse
    , BatchListAttachedIndicesResponse
    , batchListAttachedIndicesResponse
    , blaiIndexAttachments
    , blaiNextToken

    -- ** BatchListIncomingTypedLinks
    , BatchListIncomingTypedLinks
    , batchListIncomingTypedLinks
    , blitlsFilterAttributeRanges
    , blitlsNextToken
    , blitlsFilterTypedLink
    , blitlsMaxResults
    , blitlsObjectReference

    -- ** BatchListIncomingTypedLinksResponse
    , BatchListIncomingTypedLinksResponse
    , batchListIncomingTypedLinksResponse
    , blitlLinkSpecifiers
    , blitlNextToken

    -- ** BatchListIndex
    , BatchListIndex
    , batchListIndex
    , batRangesOnIndexedValues
    , batNextToken
    , batMaxResults
    , batIndexReference

    -- ** BatchListIndexResponse
    , BatchListIndexResponse
    , batchListIndexResponse
    , bliIndexAttachments
    , bliNextToken

    -- ** BatchListObjectAttributes
    , BatchListObjectAttributes
    , batchListObjectAttributes
    , bloaFacetFilter
    , bloaNextToken
    , bloaMaxResults
    , bloaObjectReference

    -- ** BatchListObjectAttributesResponse
    , BatchListObjectAttributesResponse
    , batchListObjectAttributesResponse
    , bNextToken
    , bAttributes

    -- ** BatchListObjectChildren
    , BatchListObjectChildren
    , batchListObjectChildren
    , bloclNextToken
    , bloclMaxResults
    , bloclObjectReference

    -- ** BatchListObjectChildrenResponse
    , BatchListObjectChildrenResponse
    , batchListObjectChildrenResponse
    , blocChildren
    , blocNextToken

    -- ** BatchListObjectParentPaths
    , BatchListObjectParentPaths
    , batchListObjectParentPaths
    , bloppsNextToken
    , bloppsMaxResults
    , bloppsObjectReference

    -- ** BatchListObjectParentPathsResponse
    , BatchListObjectParentPathsResponse
    , batchListObjectParentPathsResponse
    , bloppPathToObjectIdentifiersList
    , bloppNextToken

    -- ** BatchListObjectPolicies
    , BatchListObjectPolicies
    , batchListObjectPolicies
    , blopsNextToken
    , blopsMaxResults
    , blopsObjectReference

    -- ** BatchListObjectPoliciesResponse
    , BatchListObjectPoliciesResponse
    , batchListObjectPoliciesResponse
    , blopNextToken
    , blopAttachedPolicyIds

    -- ** BatchListOutgoingTypedLinks
    , BatchListOutgoingTypedLinks
    , batchListOutgoingTypedLinks
    , blotlsFilterAttributeRanges
    , blotlsNextToken
    , blotlsFilterTypedLink
    , blotlsMaxResults
    , blotlsObjectReference

    -- ** BatchListOutgoingTypedLinksResponse
    , BatchListOutgoingTypedLinksResponse
    , batchListOutgoingTypedLinksResponse
    , blotlTypedLinkSpecifiers
    , blotlNextToken

    -- ** BatchListPolicyAttachments
    , BatchListPolicyAttachments
    , batchListPolicyAttachments
    , blpasNextToken
    , blpasMaxResults
    , blpasPolicyReference

    -- ** BatchListPolicyAttachmentsResponse
    , BatchListPolicyAttachmentsResponse
    , batchListPolicyAttachmentsResponse
    , blpaObjectIdentifiers
    , blpaNextToken

    -- ** BatchLookupPolicy
    , BatchLookupPolicy
    , batchLookupPolicy
    , blplNextToken
    , blplMaxResults
    , blplObjectReference

    -- ** BatchLookupPolicyResponse
    , BatchLookupPolicyResponse
    , batchLookupPolicyResponse
    , blpNextToken
    , blpPolicyToPathList

    -- ** BatchReadException
    , BatchReadException
    , batchReadException
    , breType
    , breMessage

    -- ** BatchReadOperation
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

    -- ** BatchReadOperationResponse
    , BatchReadOperationResponse
    , batchReadOperationResponse
    , broExceptionResponse
    , broSuccessfulResponse

    -- ** BatchReadSuccessfulResponse
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

    -- ** BatchRemoveFacetFromObject
    , BatchRemoveFacetFromObject
    , batchRemoveFacetFromObject
    , brffoSchemaFacet
    , brffoObjectReference

    -- ** BatchRemoveFacetFromObjectResponse
    , BatchRemoveFacetFromObjectResponse
    , batchRemoveFacetFromObjectResponse

    -- ** BatchUpdateObjectAttributes
    , BatchUpdateObjectAttributes
    , batchUpdateObjectAttributes
    , buoaObjectReference
    , buoaAttributeUpdates

    -- ** BatchUpdateObjectAttributesResponse
    , BatchUpdateObjectAttributesResponse
    , batchUpdateObjectAttributesResponse
    , buoaObjectIdentifier

    -- ** BatchWriteOperation
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

    -- ** BatchWriteOperationResponse
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

    -- ** Directory
    , Directory
    , directory
    , dDirectoryARN
    , dState
    , dName
    , dCreationDateTime

    -- ** Facet
    , Facet
    , facet
    , fObjectType
    , fName

    -- ** FacetAttribute
    , FacetAttribute
    , facetAttribute
    , faAttributeReference
    , faAttributeDefinition
    , faRequiredBehavior
    , faName

    -- ** FacetAttributeDefinition
    , FacetAttributeDefinition
    , facetAttributeDefinition
    , fadRules
    , fadDefaultValue
    , fadIsImmutable
    , fadType

    -- ** FacetAttributeReference
    , FacetAttributeReference
    , facetAttributeReference
    , farTargetFacetName
    , farTargetAttributeName

    -- ** FacetAttributeUpdate
    , FacetAttributeUpdate
    , facetAttributeUpdate
    , fauAttribute
    , fauAction

    -- ** IndexAttachment
    , IndexAttachment
    , indexAttachment
    , iaIndexedAttributes
    , iaObjectIdentifier

    -- ** ObjectAttributeAction
    , ObjectAttributeAction
    , objectAttributeAction
    , oaaObjectAttributeActionType
    , oaaObjectAttributeUpdateValue

    -- ** ObjectAttributeRange
    , ObjectAttributeRange
    , objectAttributeRange
    , oarRange
    , oarAttributeKey

    -- ** ObjectAttributeUpdate
    , ObjectAttributeUpdate
    , objectAttributeUpdate
    , oauObjectAttributeAction
    , oauObjectAttributeKey

    -- ** ObjectReference
    , ObjectReference
    , objectReference
    , orSelector

    -- ** PathToObjectIdentifiers
    , PathToObjectIdentifiers
    , pathToObjectIdentifiers
    , ptoiObjectIdentifiers
    , ptoiPath

    -- ** PolicyAttachment
    , PolicyAttachment
    , policyAttachment
    , paPolicyId
    , paPolicyType
    , paObjectIdentifier

    -- ** PolicyToPath
    , PolicyToPath
    , policyToPath
    , ptpPath
    , ptpPolicies

    -- ** Rule
    , Rule
    , rule
    , rParameters
    , rType

    -- ** SchemaFacet
    , SchemaFacet
    , schemaFacet
    , sfFacetName
    , sfSchemaARN

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TypedAttributeValue
    , TypedAttributeValue
    , typedAttributeValue
    , tavBinaryValue
    , tavDatetimeValue
    , tavNumberValue
    , tavStringValue
    , tavBooleanValue

    -- ** TypedAttributeValueRange
    , TypedAttributeValueRange
    , typedAttributeValueRange
    , tavrEndValue
    , tavrStartValue
    , tavrStartMode
    , tavrEndMode

    -- ** TypedLinkAttributeDefinition
    , TypedLinkAttributeDefinition
    , typedLinkAttributeDefinition
    , tladRules
    , tladDefaultValue
    , tladIsImmutable
    , tladName
    , tladType
    , tladRequiredBehavior

    -- ** TypedLinkAttributeRange
    , TypedLinkAttributeRange
    , typedLinkAttributeRange
    , tlarAttributeName
    , tlarRange

    -- ** TypedLinkFacet
    , TypedLinkFacet
    , typedLinkFacet
    , tlfName
    , tlfAttributes
    , tlfIdentityAttributeOrder

    -- ** TypedLinkFacetAttributeUpdate
    , TypedLinkFacetAttributeUpdate
    , typedLinkFacetAttributeUpdate
    , tlfauAttribute
    , tlfauAction

    -- ** TypedLinkSchemaAndFacetName
    , TypedLinkSchemaAndFacetName
    , typedLinkSchemaAndFacetName
    , tlsafnSchemaARN
    , tlsafnTypedLinkName

    -- ** TypedLinkSpecifier
    , TypedLinkSpecifier
    , typedLinkSpecifier
    , tlsTypedLinkFacet
    , tlsSourceObjectReference
    , tlsTargetObjectReference
    , tlsIdentityAttributeValues
    ) where

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
import Network.AWS.CloudDirectory.UpdateObjectAttributes
import Network.AWS.CloudDirectory.UpdateSchema
import Network.AWS.CloudDirectory.UpdateTypedLinkFacet
import Network.AWS.CloudDirectory.UpgradeAppliedSchema
import Network.AWS.CloudDirectory.UpgradePublishedSchema
import Network.AWS.CloudDirectory.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CloudDirectory'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

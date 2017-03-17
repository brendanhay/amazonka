{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon Cloud Directory__
--
-- Amazon Cloud Directory is a component of the AWS Directory Service that simplifies the development and management of cloud-scale web, mobile and IoT applications. This guide describes the Cloud Directory operations that you can call programatically and includes detailed information on data types and errors. For information about AWS Directory Services features, see <https://aws.amazon.com/directoryservice/ AWS Directory Service> and the <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html AWS Directory Service Administration Guide> .
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

    -- ** DeleteObject
    , module Network.AWS.CloudDirectory.DeleteObject

    -- ** ListIndex
    , module Network.AWS.CloudDirectory.ListIndex

    -- ** GetDirectory
    , module Network.AWS.CloudDirectory.GetDirectory

    -- ** GetObjectInformation
    , module Network.AWS.CloudDirectory.GetObjectInformation

    -- ** ListAttachedIndices
    , module Network.AWS.CloudDirectory.ListAttachedIndices

    -- ** DetachFromIndex
    , module Network.AWS.CloudDirectory.DetachFromIndex

    -- ** LookupPolicy
    , module Network.AWS.CloudDirectory.LookupPolicy

    -- ** ListTagsForResource
    , module Network.AWS.CloudDirectory.ListTagsForResource

    -- ** ListPublishedSchemaARNs
    , module Network.AWS.CloudDirectory.ListPublishedSchemaARNs

    -- ** ListDirectories
    , module Network.AWS.CloudDirectory.ListDirectories

    -- ** ListObjectParentPaths
    , module Network.AWS.CloudDirectory.ListObjectParentPaths

    -- ** DisableDirectory
    , module Network.AWS.CloudDirectory.DisableDirectory

    -- ** CreateDirectory
    , module Network.AWS.CloudDirectory.CreateDirectory

    -- ** ListFacetAttributes
    , module Network.AWS.CloudDirectory.ListFacetAttributes

    -- ** RemoveFacetFromObject
    , module Network.AWS.CloudDirectory.RemoveFacetFromObject

    -- ** EnableDirectory
    , module Network.AWS.CloudDirectory.EnableDirectory

    -- ** ListObjectAttributes
    , module Network.AWS.CloudDirectory.ListObjectAttributes

    -- ** ListAppliedSchemaARNs
    , module Network.AWS.CloudDirectory.ListAppliedSchemaARNs

    -- ** GetFacet
    , module Network.AWS.CloudDirectory.GetFacet

    -- ** ListDevelopmentSchemaARNs
    , module Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs

    -- ** AttachObject
    , module Network.AWS.CloudDirectory.AttachObject

    -- ** BatchWrite
    , module Network.AWS.CloudDirectory.BatchWrite

    -- ** CreateObject
    , module Network.AWS.CloudDirectory.CreateObject

    -- ** CreateFacet
    , module Network.AWS.CloudDirectory.CreateFacet

    -- ** DeleteFacet
    , module Network.AWS.CloudDirectory.DeleteFacet

    -- ** UpdateFacet
    , module Network.AWS.CloudDirectory.UpdateFacet

    -- ** ListObjectChildren
    , module Network.AWS.CloudDirectory.ListObjectChildren

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

    -- ** ListPolicyAttachments
    , module Network.AWS.CloudDirectory.ListPolicyAttachments

    -- ** TagResource
    , module Network.AWS.CloudDirectory.TagResource

    -- ** UpdateSchema
    , module Network.AWS.CloudDirectory.UpdateSchema

    -- ** DeleteSchema
    , module Network.AWS.CloudDirectory.DeleteSchema

    -- ** ListFacetNames
    , module Network.AWS.CloudDirectory.ListFacetNames

    -- ** UntagResource
    , module Network.AWS.CloudDirectory.UntagResource

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

    -- ** ListObjectPolicies
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

    -- ** BatchCreateObject
    , BatchCreateObject
    , batchCreateObject
    , bcoSchemaFacet
    , bcoObjectAttributeList
    , bcoParentReference
    , bcoLinkName
    , bcoBatchReferenceName

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

    -- ** BatchDetachObject
    , BatchDetachObject
    , batchDetachObject
    , bdoParentReference
    , bdoLinkName
    , bdoBatchReferenceName

    -- ** BatchDetachObjectResponse
    , BatchDetachObjectResponse
    , batchDetachObjectResponse
    , bdoDetachedObjectIdentifier

    -- ** BatchListObjectAttributes
    , BatchListObjectAttributes
    , batchListObjectAttributes
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
    , batNextToken
    , batMaxResults
    , batObjectReference

    -- ** BatchListObjectChildrenResponse
    , BatchListObjectChildrenResponse
    , batchListObjectChildrenResponse
    , blocChildren
    , blocNextToken

    -- ** BatchReadException
    , BatchReadException
    , batchReadException
    , breType
    , breMessage

    -- ** BatchReadOperation
    , BatchReadOperation
    , batchReadOperation
    , broListObjectAttributes
    , broListObjectChildren

    -- ** BatchReadOperationResponse
    , BatchReadOperationResponse
    , batchReadOperationResponse
    , broExceptionResponse
    , broSuccessfulResponse

    -- ** BatchReadSuccessfulResponse
    , BatchReadSuccessfulResponse
    , batchReadSuccessfulResponse
    , brsListObjectAttributes
    , brsListObjectChildren

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
    , bRemoveFacetFromObject
    , bAttachObject
    , bCreateObject
    , bDetachObject
    , bAddFacetToObject
    , bUpdateObjectAttributes

    -- ** BatchWriteOperationResponse
    , BatchWriteOperationResponse
    , batchWriteOperationResponse
    , bwoDeleteObject
    , bwoRemoveFacetFromObject
    , bwoAttachObject
    , bwoCreateObject
    , bwoDetachObject
    , bwoAddFacetToObject
    , bwoUpdateObjectAttributes

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
    ) where

import           Network.AWS.CloudDirectory.AddFacetToObject
import           Network.AWS.CloudDirectory.ApplySchema
import           Network.AWS.CloudDirectory.AttachObject
import           Network.AWS.CloudDirectory.AttachPolicy
import           Network.AWS.CloudDirectory.AttachToIndex
import           Network.AWS.CloudDirectory.BatchRead
import           Network.AWS.CloudDirectory.BatchWrite
import           Network.AWS.CloudDirectory.CreateDirectory
import           Network.AWS.CloudDirectory.CreateFacet
import           Network.AWS.CloudDirectory.CreateIndex
import           Network.AWS.CloudDirectory.CreateObject
import           Network.AWS.CloudDirectory.CreateSchema
import           Network.AWS.CloudDirectory.DeleteDirectory
import           Network.AWS.CloudDirectory.DeleteFacet
import           Network.AWS.CloudDirectory.DeleteObject
import           Network.AWS.CloudDirectory.DeleteSchema
import           Network.AWS.CloudDirectory.DetachFromIndex
import           Network.AWS.CloudDirectory.DetachObject
import           Network.AWS.CloudDirectory.DetachPolicy
import           Network.AWS.CloudDirectory.DisableDirectory
import           Network.AWS.CloudDirectory.EnableDirectory
import           Network.AWS.CloudDirectory.GetDirectory
import           Network.AWS.CloudDirectory.GetFacet
import           Network.AWS.CloudDirectory.GetObjectInformation
import           Network.AWS.CloudDirectory.GetSchemaAsJSON
import           Network.AWS.CloudDirectory.ListAppliedSchemaARNs
import           Network.AWS.CloudDirectory.ListAttachedIndices
import           Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs
import           Network.AWS.CloudDirectory.ListDirectories
import           Network.AWS.CloudDirectory.ListFacetAttributes
import           Network.AWS.CloudDirectory.ListFacetNames
import           Network.AWS.CloudDirectory.ListIndex
import           Network.AWS.CloudDirectory.ListObjectAttributes
import           Network.AWS.CloudDirectory.ListObjectChildren
import           Network.AWS.CloudDirectory.ListObjectParentPaths
import           Network.AWS.CloudDirectory.ListObjectParents
import           Network.AWS.CloudDirectory.ListObjectPolicies
import           Network.AWS.CloudDirectory.ListPolicyAttachments
import           Network.AWS.CloudDirectory.ListPublishedSchemaARNs
import           Network.AWS.CloudDirectory.ListTagsForResource
import           Network.AWS.CloudDirectory.LookupPolicy
import           Network.AWS.CloudDirectory.PublishSchema
import           Network.AWS.CloudDirectory.PutSchemaFromJSON
import           Network.AWS.CloudDirectory.RemoveFacetFromObject
import           Network.AWS.CloudDirectory.TagResource
import           Network.AWS.CloudDirectory.Types
import           Network.AWS.CloudDirectory.UntagResource
import           Network.AWS.CloudDirectory.UpdateFacet
import           Network.AWS.CloudDirectory.UpdateObjectAttributes
import           Network.AWS.CloudDirectory.UpdateSchema
import           Network.AWS.CloudDirectory.Waiters

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

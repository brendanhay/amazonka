{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon SimpleDB is a web service providing the core database functions
-- of data indexing and querying in the cloud. By offloading the time and
-- effort associated with building and operating a web-scale database,
-- SimpleDB provides developers the freedom to focus on application
-- development.
--
-- A traditional, clustered relational database requires a sizable upfront
-- capital outlay, is complex to design, and often requires extensive and
-- repetitive database administration. Amazon SimpleDB is dramatically
-- simpler, requiring no schema, automatically indexing your data and
-- providing a simple API for storage and access. This approach eliminates
-- the administrative burden of data modeling, index maintenance, and
-- performance tuning. Developers gain access to this functionality within
-- Amazon\'s proven computing environment, are able to scale instantly, and
-- pay only for what they use.
--
-- Visit <http://aws.amazon.com/simpledb/> for more information.
module Network.AWS.SDB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NumberDomainAttributesExceeded
    _NumberDomainAttributesExceeded,

    -- ** NumberSubmittedItemsExceeded
    _NumberSubmittedItemsExceeded,

    -- ** NoSuchDomain
    _NoSuchDomain,

    -- ** NumberDomainBytesExceeded
    _NumberDomainBytesExceeded,

    -- ** NumberDomainsExceeded
    _NumberDomainsExceeded,

    -- ** InvalidNumberPredicates
    _InvalidNumberPredicates,

    -- ** DuplicateItemName
    _DuplicateItemName,

    -- ** AttributeDoesNotExist
    _AttributeDoesNotExist,

    -- ** InvalidNumberValueTests
    _InvalidNumberValueTests,

    -- ** InvalidQueryExpression
    _InvalidQueryExpression,

    -- ** NumberSubmittedAttributesExceeded
    _NumberSubmittedAttributesExceeded,

    -- ** InvalidParameterValue
    _InvalidParameterValue,

    -- ** TooManyRequestedAttributes
    _TooManyRequestedAttributes,

    -- ** MissingParameter
    _MissingParameter,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** RequestTimeout
    _RequestTimeout,

    -- ** NumberItemAttributesExceeded
    _NumberItemAttributesExceeded,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

    -- ** DeleteAttributes
    DeleteAttributes (DeleteAttributes'),
    newDeleteAttributes,
    DeleteAttributesResponse (DeleteAttributesResponse'),
    newDeleteAttributesResponse,

    -- ** DomainMetadata
    DomainMetadata (DomainMetadata'),
    newDomainMetadata,
    DomainMetadataResponse (DomainMetadataResponse'),
    newDomainMetadataResponse,

    -- ** BatchPutAttributes
    BatchPutAttributes (BatchPutAttributes'),
    newBatchPutAttributes,
    BatchPutAttributesResponse (BatchPutAttributesResponse'),
    newBatchPutAttributesResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** GetAttributes
    GetAttributes (GetAttributes'),
    newGetAttributes,
    GetAttributesResponse (GetAttributesResponse'),
    newGetAttributesResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** PutAttributes
    PutAttributes (PutAttributes'),
    newPutAttributes,
    PutAttributesResponse (PutAttributesResponse'),
    newPutAttributesResponse,

    -- ** Select (Paginated)
    Select (Select'),
    newSelect,
    SelectResponse (SelectResponse'),
    newSelectResponse,

    -- ** BatchDeleteAttributes
    BatchDeleteAttributes (BatchDeleteAttributes'),
    newBatchDeleteAttributes,
    BatchDeleteAttributesResponse (BatchDeleteAttributesResponse'),
    newBatchDeleteAttributesResponse,

    -- * Types

    -- ** Attribute
    Attribute (Attribute'),
    newAttribute,

    -- ** DeletableItem
    DeletableItem (DeletableItem'),
    newDeletableItem,

    -- ** Item
    Item (Item'),
    newItem,

    -- ** ReplaceableAttribute
    ReplaceableAttribute (ReplaceableAttribute'),
    newReplaceableAttribute,

    -- ** ReplaceableItem
    ReplaceableItem (ReplaceableItem'),
    newReplaceableItem,

    -- ** UpdateCondition
    UpdateCondition (UpdateCondition'),
    newUpdateCondition,
  )
where

import Network.AWS.SDB.BatchDeleteAttributes
import Network.AWS.SDB.BatchPutAttributes
import Network.AWS.SDB.CreateDomain
import Network.AWS.SDB.DeleteAttributes
import Network.AWS.SDB.DeleteDomain
import Network.AWS.SDB.DomainMetadata
import Network.AWS.SDB.GetAttributes
import Network.AWS.SDB.Lens
import Network.AWS.SDB.ListDomains
import Network.AWS.SDB.PutAttributes
import Network.AWS.SDB.Select
import Network.AWS.SDB.Types
import Network.AWS.SDB.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SDB'.

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

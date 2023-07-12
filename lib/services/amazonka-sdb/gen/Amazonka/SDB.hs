{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SDB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2009-04-15@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.SDB
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AttributeDoesNotExist
    _AttributeDoesNotExist,

    -- ** DuplicateItemName
    _DuplicateItemName,

    -- ** InvalidNextToken
    _InvalidNextToken,

    -- ** InvalidNumberPredicates
    _InvalidNumberPredicates,

    -- ** InvalidNumberValueTests
    _InvalidNumberValueTests,

    -- ** InvalidParameterValue
    _InvalidParameterValue,

    -- ** InvalidQueryExpression
    _InvalidQueryExpression,

    -- ** MissingParameter
    _MissingParameter,

    -- ** NoSuchDomain
    _NoSuchDomain,

    -- ** NumberDomainAttributesExceeded
    _NumberDomainAttributesExceeded,

    -- ** NumberDomainBytesExceeded
    _NumberDomainBytesExceeded,

    -- ** NumberDomainsExceeded
    _NumberDomainsExceeded,

    -- ** NumberItemAttributesExceeded
    _NumberItemAttributesExceeded,

    -- ** NumberSubmittedAttributesExceeded
    _NumberSubmittedAttributesExceeded,

    -- ** NumberSubmittedItemsExceeded
    _NumberSubmittedItemsExceeded,

    -- ** RequestTimeout
    _RequestTimeout,

    -- ** TooManyRequestedAttributes
    _TooManyRequestedAttributes,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDeleteAttributes
    BatchDeleteAttributes (BatchDeleteAttributes'),
    newBatchDeleteAttributes,
    BatchDeleteAttributesResponse (BatchDeleteAttributesResponse'),
    newBatchDeleteAttributesResponse,

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

    -- ** DeleteAttributes
    DeleteAttributes (DeleteAttributes'),
    newDeleteAttributes,
    DeleteAttributesResponse (DeleteAttributesResponse'),
    newDeleteAttributesResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DomainMetadata
    DomainMetadata (DomainMetadata'),
    newDomainMetadata,
    DomainMetadataResponse (DomainMetadataResponse'),
    newDomainMetadataResponse,

    -- ** GetAttributes
    GetAttributes (GetAttributes'),
    newGetAttributes,
    GetAttributesResponse (GetAttributesResponse'),
    newGetAttributesResponse,

    -- ** ListDomains (Paginated)
    ListDomains (ListDomains'),
    newListDomains,
    ListDomainsResponse (ListDomainsResponse'),
    newListDomainsResponse,

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

import Amazonka.SDB.BatchDeleteAttributes
import Amazonka.SDB.BatchPutAttributes
import Amazonka.SDB.CreateDomain
import Amazonka.SDB.DeleteAttributes
import Amazonka.SDB.DeleteDomain
import Amazonka.SDB.DomainMetadata
import Amazonka.SDB.GetAttributes
import Amazonka.SDB.Lens
import Amazonka.SDB.ListDomains
import Amazonka.SDB.PutAttributes
import Amazonka.SDB.Select
import Amazonka.SDB.Types
import Amazonka.SDB.Waiters

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

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon SimpleDB is a web service providing the core database functions of data indexing and querying in the cloud. By offloading the time and effort associated with building and operating a web-scale database, SimpleDB provides developers the freedom to focus on application development. A traditional, clustered relational database requires a sizable upfront capital outlay, is complex to design, and often requires extensive and repetitive database administration. Amazon SimpleDB is dramatically simpler, requiring no schema, automatically indexing your data and providing a simple API for storage and access. This approach eliminates the administrative burden of data modeling, index maintenance, and performance tuning. Developers gain access to this functionality within Amazon's proven computing environment, are able to scale instantly, and pay only for what they use.
--
--
-- Visit <http://aws.amazon.com/simpledb/ http://aws.amazon.com/simpledb/> for more information.
--
module Network.AWS.SDB
    (
    -- * Service Configuration
      sdb

    -- * Errors
    -- $errors

    -- ** InvalidNumberValueTests
    , _InvalidNumberValueTests

    -- ** NoSuchDomain
    , _NoSuchDomain

    -- ** NumberSubmittedItemsExceeded
    , _NumberSubmittedItemsExceeded

    -- ** AttributeDoesNotExist
    , _AttributeDoesNotExist

    -- ** NumberDomainAttributesExceeded
    , _NumberDomainAttributesExceeded

    -- ** DuplicateItemName
    , _DuplicateItemName

    -- ** MissingParameter
    , _MissingParameter

    -- ** InvalidNextToken
    , _InvalidNextToken

    -- ** InvalidParameterValue
    , _InvalidParameterValue

    -- ** NumberItemAttributesExceeded
    , _NumberItemAttributesExceeded

    -- ** RequestTimeout
    , _RequestTimeout

    -- ** TooManyRequestedAttributes
    , _TooManyRequestedAttributes

    -- ** InvalidNumberPredicates
    , _InvalidNumberPredicates

    -- ** NumberDomainsExceeded
    , _NumberDomainsExceeded

    -- ** NumberSubmittedAttributesExceeded
    , _NumberSubmittedAttributesExceeded

    -- ** NumberDomainBytesExceeded
    , _NumberDomainBytesExceeded

    -- ** InvalidQueryExpression
    , _InvalidQueryExpression

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDeleteAttributes
    , module Network.AWS.SDB.BatchDeleteAttributes

    -- ** BatchPutAttributes
    , module Network.AWS.SDB.BatchPutAttributes

    -- ** GetAttributes
    , module Network.AWS.SDB.GetAttributes

    -- ** CreateDomain
    , module Network.AWS.SDB.CreateDomain

    -- ** DomainMetadata
    , module Network.AWS.SDB.DomainMetadata

    -- ** Select (Paginated)
    , module Network.AWS.SDB.Select

    -- ** DeleteAttributes
    , module Network.AWS.SDB.DeleteAttributes

    -- ** PutAttributes
    , module Network.AWS.SDB.PutAttributes

    -- ** DeleteDomain
    , module Network.AWS.SDB.DeleteDomain

    -- ** ListDomains (Paginated)
    , module Network.AWS.SDB.ListDomains

    -- * Types

    -- ** Attribute
    , Attribute
    , attribute
    , aAlternateValueEncoding
    , aAlternateNameEncoding
    , aName
    , aValue

    -- ** DeletableItem
    , DeletableItem
    , deletableItem
    , diAttributes
    , diName

    -- ** Item
    , Item
    , item
    , iAlternateNameEncoding
    , iName
    , iAttributes

    -- ** ReplaceableAttribute
    , ReplaceableAttribute
    , replaceableAttribute
    , raReplace
    , raName
    , raValue

    -- ** ReplaceableItem
    , ReplaceableItem
    , replaceableItem
    , riName
    , riAttributes

    -- ** UpdateCondition
    , UpdateCondition
    , updateCondition
    , ucExists
    , ucValue
    , ucName
    ) where

import Network.AWS.SDB.BatchDeleteAttributes
import Network.AWS.SDB.BatchPutAttributes
import Network.AWS.SDB.CreateDomain
import Network.AWS.SDB.DeleteAttributes
import Network.AWS.SDB.DeleteDomain
import Network.AWS.SDB.DomainMetadata
import Network.AWS.SDB.GetAttributes
import Network.AWS.SDB.ListDomains
import Network.AWS.SDB.PutAttributes
import Network.AWS.SDB.Select
import Network.AWS.SDB.Types
import Network.AWS.SDB.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'SDB'.
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

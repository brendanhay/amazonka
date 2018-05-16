{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStoreData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An AWS Elemental MediaStore asset is an object, similar to an object in the Amazon S3 service. Objects are the fundamental entities that are stored in AWS Elemental MediaStore.
--
--
module Network.AWS.MediaStoreData
    (
    -- * Service Configuration
      mediaStoreData

    -- * Errors
    -- $errors

    -- ** RequestedRangeNotSatisfiableException
    , _RequestedRangeNotSatisfiableException

    -- ** InternalServerError
    , _InternalServerError

    -- ** ContainerNotFoundException
    , _ContainerNotFoundException

    -- ** ObjectNotFoundException
    , _ObjectNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutObject
    , module Network.AWS.MediaStoreData.PutObject

    -- ** DeleteObject
    , module Network.AWS.MediaStoreData.DeleteObject

    -- ** DescribeObject
    , module Network.AWS.MediaStoreData.DescribeObject

    -- ** GetObject
    , module Network.AWS.MediaStoreData.GetObject

    -- ** ListItems
    , module Network.AWS.MediaStoreData.ListItems

    -- * Types

    -- ** ItemType
    , ItemType (..)

    -- ** StorageClass
    , StorageClass (..)

    -- ** Item
    , Item
    , item
    , iETag
    , iContentLength
    , iName
    , iType
    , iLastModified
    , iContentType
    ) where

import Network.AWS.MediaStoreData.DeleteObject
import Network.AWS.MediaStoreData.DescribeObject
import Network.AWS.MediaStoreData.GetObject
import Network.AWS.MediaStoreData.ListItems
import Network.AWS.MediaStoreData.PutObject
import Network.AWS.MediaStoreData.Types
import Network.AWS.MediaStoreData.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MediaStoreData'.
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

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An AWS Elemental MediaStore container is a namespace that holds folders and objects. You use a container endpoint to create, read, and delete objects.
--
--
module Network.AWS.MediaStore
    (
    -- * Service Configuration
      mediaStore

    -- * Errors
    -- $errors

    -- ** PolicyNotFoundException
    , _PolicyNotFoundException

    -- ** CORSPolicyNotFoundException
    , _CORSPolicyNotFoundException

    -- ** ContainerInUseException
    , _ContainerInUseException

    -- ** InternalServerError
    , _InternalServerError

    -- ** ContainerNotFoundException
    , _ContainerNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateContainer
    , module Network.AWS.MediaStore.CreateContainer

    -- ** ListContainers
    , module Network.AWS.MediaStore.ListContainers

    -- ** DeleteContainer
    , module Network.AWS.MediaStore.DeleteContainer

    -- ** PutCORSPolicy
    , module Network.AWS.MediaStore.PutCORSPolicy

    -- ** DeleteCORSPolicy
    , module Network.AWS.MediaStore.DeleteCORSPolicy

    -- ** DescribeContainer
    , module Network.AWS.MediaStore.DescribeContainer

    -- ** GetCORSPolicy
    , module Network.AWS.MediaStore.GetCORSPolicy

    -- ** DeleteContainerPolicy
    , module Network.AWS.MediaStore.DeleteContainerPolicy

    -- ** PutContainerPolicy
    , module Network.AWS.MediaStore.PutContainerPolicy

    -- ** GetContainerPolicy
    , module Network.AWS.MediaStore.GetContainerPolicy

    -- * Types

    -- ** ContainerStatus
    , ContainerStatus (..)

    -- ** MethodName
    , MethodName (..)

    -- ** CORSRule
    , CORSRule
    , corsRule
    , crAllowedMethods
    , crMaxAgeSeconds
    , crAllowedHeaders
    , crAllowedOrigins
    , crExposeHeaders

    -- ** Container
    , Container
    , container
    , cCreationTime
    , cStatus
    , cARN
    , cName
    , cEndpoint
    ) where

import Network.AWS.MediaStore.CreateContainer
import Network.AWS.MediaStore.DeleteContainer
import Network.AWS.MediaStore.DeleteContainerPolicy
import Network.AWS.MediaStore.DeleteCORSPolicy
import Network.AWS.MediaStore.DescribeContainer
import Network.AWS.MediaStore.GetContainerPolicy
import Network.AWS.MediaStore.GetCORSPolicy
import Network.AWS.MediaStore.ListContainers
import Network.AWS.MediaStore.PutContainerPolicy
import Network.AWS.MediaStore.PutCORSPolicy
import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MediaStore'.
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

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTDataPlane
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS IoT__
--
-- AWS IoT-Data enables secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. It implements a broker for applications and things to publish messages over HTTP (Publish) and retrieve, update, and delete thing shadows. A thing shadow is a persistent representation of your things and their state in the AWS cloud.
--
module Network.AWS.IoTDataPlane
    (
    -- * Service Configuration
      ioTDataPlane

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** ConflictException
    , _ConflictException

    -- ** RequestEntityTooLargeException
    , _RequestEntityTooLargeException

    -- ** ThrottlingException
    , _ThrottlingException

    -- ** MethodNotAllowedException
    , _MethodNotAllowedException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** UnsupportedDocumentEncodingException
    , _UnsupportedDocumentEncodingException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetThingShadow
    , module Network.AWS.IoTDataPlane.GetThingShadow

    -- ** DeleteThingShadow
    , module Network.AWS.IoTDataPlane.DeleteThingShadow

    -- ** UpdateThingShadow
    , module Network.AWS.IoTDataPlane.UpdateThingShadow

    -- ** Publish
    , module Network.AWS.IoTDataPlane.Publish

    -- * Types
    ) where

import Network.AWS.IoTDataPlane.DeleteThingShadow
import Network.AWS.IoTDataPlane.GetThingShadow
import Network.AWS.IoTDataPlane.Publish
import Network.AWS.IoTDataPlane.Types
import Network.AWS.IoTDataPlane.UpdateThingShadow
import Network.AWS.IoTDataPlane.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'IoTDataPlane'.
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

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS IoT
--
-- AWS IoT-Data enables secure, bi-directional communication between
-- Internet-connected things (such as sensors, actuators, embedded devices,
-- or smart appliances) and the AWS cloud. It implements a broker for
-- applications and things to publish messages over HTTP (Publish) and
-- retrieve, update, and delete shadows. A shadow is a persistent
-- representation of your things and their state in the AWS cloud.
--
-- Find the endpoint address for actions in the AWS IoT data plane by
-- running this CLI command:
--
-- @aws iot describe-endpoint --endpoint-type iot:Data-ATS@
--
-- The service name used by
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html AWS Signature Version 4>
-- to sign requests is: /iotdevicegateway/.
module Network.AWS.IoTData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** UnsupportedDocumentEncodingException
    _UnsupportedDocumentEncodingException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** RequestEntityTooLargeException
    _RequestEntityTooLargeException,

    -- ** ConflictException
    _ConflictException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetThingShadow
    GetThingShadow (GetThingShadow'),
    newGetThingShadow,
    GetThingShadowResponse (GetThingShadowResponse'),
    newGetThingShadowResponse,

    -- ** Publish
    Publish (Publish'),
    newPublish,
    PublishResponse (PublishResponse'),
    newPublishResponse,

    -- ** ListNamedShadowsForThing
    ListNamedShadowsForThing (ListNamedShadowsForThing'),
    newListNamedShadowsForThing,
    ListNamedShadowsForThingResponse (ListNamedShadowsForThingResponse'),
    newListNamedShadowsForThingResponse,

    -- ** UpdateThingShadow
    UpdateThingShadow (UpdateThingShadow'),
    newUpdateThingShadow,
    UpdateThingShadowResponse (UpdateThingShadowResponse'),
    newUpdateThingShadowResponse,

    -- ** DeleteThingShadow
    DeleteThingShadow (DeleteThingShadow'),
    newDeleteThingShadow,
    DeleteThingShadowResponse (DeleteThingShadowResponse'),
    newDeleteThingShadowResponse,

    -- * Types
  )
where

import Network.AWS.IoTData.DeleteThingShadow
import Network.AWS.IoTData.GetThingShadow
import Network.AWS.IoTData.Lens
import Network.AWS.IoTData.ListNamedShadowsForThing
import Network.AWS.IoTData.Publish
import Network.AWS.IoTData.Types
import Network.AWS.IoTData.UpdateThingShadow
import Network.AWS.IoTData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTData'.

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

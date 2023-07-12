{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-05-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- IoT data
--
-- IoT data enables secure, bi-directional communication between
-- Internet-connected things (such as sensors, actuators, embedded devices,
-- or smart appliances) and the Amazon Web Services cloud. It implements a
-- broker for applications and things to publish messages over HTTP
-- (Publish) and retrieve, update, and delete shadows. A shadow is a
-- persistent representation of your things and their state in the Amazon
-- Web Services cloud.
--
-- Find the endpoint address for actions in IoT data by running this CLI
-- command:
--
-- @aws iot describe-endpoint --endpoint-type iot:Data-ATS@
--
-- The service name used by
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Amazon Web ServicesSignature Version 4>
-- to sign requests is: /iotdevicegateway/.
module Amazonka.IoTData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** MethodNotAllowedException
    _MethodNotAllowedException,

    -- ** RequestEntityTooLargeException
    _RequestEntityTooLargeException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** UnsupportedDocumentEncodingException
    _UnsupportedDocumentEncodingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteThingShadow
    DeleteThingShadow (DeleteThingShadow'),
    newDeleteThingShadow,
    DeleteThingShadowResponse (DeleteThingShadowResponse'),
    newDeleteThingShadowResponse,

    -- ** GetRetainedMessage
    GetRetainedMessage (GetRetainedMessage'),
    newGetRetainedMessage,
    GetRetainedMessageResponse (GetRetainedMessageResponse'),
    newGetRetainedMessageResponse,

    -- ** GetThingShadow
    GetThingShadow (GetThingShadow'),
    newGetThingShadow,
    GetThingShadowResponse (GetThingShadowResponse'),
    newGetThingShadowResponse,

    -- ** ListNamedShadowsForThing
    ListNamedShadowsForThing (ListNamedShadowsForThing'),
    newListNamedShadowsForThing,
    ListNamedShadowsForThingResponse (ListNamedShadowsForThingResponse'),
    newListNamedShadowsForThingResponse,

    -- ** ListRetainedMessages (Paginated)
    ListRetainedMessages (ListRetainedMessages'),
    newListRetainedMessages,
    ListRetainedMessagesResponse (ListRetainedMessagesResponse'),
    newListRetainedMessagesResponse,

    -- ** Publish
    Publish (Publish'),
    newPublish,
    PublishResponse (PublishResponse'),
    newPublishResponse,

    -- ** UpdateThingShadow
    UpdateThingShadow (UpdateThingShadow'),
    newUpdateThingShadow,
    UpdateThingShadowResponse (UpdateThingShadowResponse'),
    newUpdateThingShadowResponse,

    -- * Types

    -- ** PayloadFormatIndicator
    PayloadFormatIndicator (..),

    -- ** RetainedMessageSummary
    RetainedMessageSummary (RetainedMessageSummary'),
    newRetainedMessageSummary,
  )
where

import Amazonka.IoTData.DeleteThingShadow
import Amazonka.IoTData.GetRetainedMessage
import Amazonka.IoTData.GetThingShadow
import Amazonka.IoTData.Lens
import Amazonka.IoTData.ListNamedShadowsForThing
import Amazonka.IoTData.ListRetainedMessages
import Amazonka.IoTData.Publish
import Amazonka.IoTData.Types
import Amazonka.IoTData.UpdateThingShadow
import Amazonka.IoTData.Waiters

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

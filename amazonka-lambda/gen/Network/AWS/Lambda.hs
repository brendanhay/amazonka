{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Lambda
--
-- __Overview__
--
-- This is the /AWS Lambda API Reference/. The AWS Lambda Developer Guide
-- provides additional information. For the service overview, go to
-- <http://docs.aws.amazon.com/lambda/latest/dg/welcome.html What is AWS Lambda>,
-- and for information about how the service works, go to
-- <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works>
-- in the /AWS Lambda Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_Reference.html AWS API Reference>
module Network.AWS.Lambda
    (
    -- * Service Description
      Lambda

    -- * Error Matchers
    -- $errors

    -- ** PolicyLengthExceededException
    , _PolicyLengthExceededException

    -- ** UnsupportedMediaTypeException
    , _UnsupportedMediaTypeException

    -- ** InvalidRequestContentException
    , _InvalidRequestContentException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** RequestTooLargeException
    , _RequestTooLargeException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ServiceException
    , _ServiceException

    -- ** CodeStorageExceededException
    , _CodeStorageExceededException

    -- ** ResourceConflictException
    , _ResourceConflictException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetFunctionConfiguration 
    , module Network.AWS.Lambda.GetFunctionConfiguration

    -- ** UpdateEventSourceMapping 
    , module Network.AWS.Lambda.UpdateEventSourceMapping

    -- ** DeleteEventSourceMapping 
    , module Network.AWS.Lambda.DeleteEventSourceMapping

    -- ** RemovePermission 
    , module Network.AWS.Lambda.RemovePermission

    -- ** Invoke 
    , module Network.AWS.Lambda.Invoke

    -- ** GetEventSourceMapping 
    , module Network.AWS.Lambda.GetEventSourceMapping

    -- ** CreateFunction 
    , module Network.AWS.Lambda.CreateFunction

    -- ** CreateEventSourceMapping 
    , module Network.AWS.Lambda.CreateEventSourceMapping

    -- ** GetFunction 
    , module Network.AWS.Lambda.GetFunction

    -- ** ListEventSourceMappings (Paginated)
    , module Network.AWS.Lambda.ListEventSourceMappings
    -- $pager

    -- ** AddPermission 
    , module Network.AWS.Lambda.AddPermission

    -- ** DeleteFunction 
    , module Network.AWS.Lambda.DeleteFunction

    -- ** UpdateFunctionConfiguration 
    , module Network.AWS.Lambda.UpdateFunctionConfiguration

    -- ** ListFunctions (Paginated)
    , module Network.AWS.Lambda.ListFunctions
    -- $pager

    -- ** UpdateFunctionCode 
    , module Network.AWS.Lambda.UpdateFunctionCode

    -- ** GetPolicy 
    , module Network.AWS.Lambda.GetPolicy

    -- * Types

    -- ** EventSourcePosition
    , EventSourcePosition (..)

    -- ** InvocationType
    , InvocationType (..)

    -- ** LogType
    , LogType (..)

    -- ** Runtime
    , Runtime (..)

    -- ** EventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    , eventSourceMappingConfiguration
    , esmcEventSourceARN
    , esmcFunctionARN
    , esmcState
    , esmcUUId
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified

    -- ** FunctionCode
    , FunctionCode
    , functionCode
    , fcS3ObjectVersion
    , fcS3Key
    , fcZipFile
    , fcS3Bucket

    -- ** FunctionCodeLocation
    , FunctionCodeLocation
    , functionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- ** FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcRuntime
    , fcMemorySize
    , fcFunctionARN
    , fcRole
    , fcFunctionName
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcDescription
    ) where

import Network.AWS.Lambda.AddPermission
import Network.AWS.Lambda.CreateEventSourceMapping
import Network.AWS.Lambda.CreateFunction
import Network.AWS.Lambda.DeleteEventSourceMapping
import Network.AWS.Lambda.DeleteFunction
import Network.AWS.Lambda.GetEventSourceMapping
import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.GetPolicy
import Network.AWS.Lambda.Invoke
import Network.AWS.Lambda.ListEventSourceMappings
import Network.AWS.Lambda.ListFunctions
import Network.AWS.Lambda.RemovePermission
import Network.AWS.Lambda.Types
import Network.AWS.Lambda.UpdateEventSourceMapping
import Network.AWS.Lambda.UpdateFunctionCode
import Network.AWS.Lambda.UpdateFunctionConfiguration
import Network.AWS.Lambda.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Lambda'.
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

{- $pager
This operation can return paginated results.
-}

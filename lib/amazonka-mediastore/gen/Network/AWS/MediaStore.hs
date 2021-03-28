{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An AWS Elemental MediaStore container is a namespace that holds folders and objects. You use a container endpoint to create, read, and delete objects. 
module Network.AWS.MediaStore
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** PolicyNotFoundException
    , _PolicyNotFoundException

    -- ** CorsPolicyNotFoundException
    , _CorsPolicyNotFoundException

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

    -- ** StopAccessLogging 
    , module Network.AWS.MediaStore.StopAccessLogging

    -- ** PutLifecyclePolicy 
    , module Network.AWS.MediaStore.PutLifecyclePolicy

    -- ** DeleteLifecyclePolicy 
    , module Network.AWS.MediaStore.DeleteLifecyclePolicy

    -- ** ListTagsForResource 
    , module Network.AWS.MediaStore.ListTagsForResource

    -- ** CreateContainer 
    , module Network.AWS.MediaStore.CreateContainer

    -- ** ListContainers (Paginated)
    , module Network.AWS.MediaStore.ListContainers

    -- ** DeleteContainer 
    , module Network.AWS.MediaStore.DeleteContainer

    -- ** PutCorsPolicy 
    , module Network.AWS.MediaStore.PutCorsPolicy

    -- ** DeleteCorsPolicy 
    , module Network.AWS.MediaStore.DeleteCorsPolicy

    -- ** StartAccessLogging 
    , module Network.AWS.MediaStore.StartAccessLogging

    -- ** DescribeContainer 
    , module Network.AWS.MediaStore.DescribeContainer

    -- ** GetMetricPolicy 
    , module Network.AWS.MediaStore.GetMetricPolicy

    -- ** DeleteMetricPolicy 
    , module Network.AWS.MediaStore.DeleteMetricPolicy

    -- ** PutMetricPolicy 
    , module Network.AWS.MediaStore.PutMetricPolicy

    -- ** GetLifecyclePolicy 
    , module Network.AWS.MediaStore.GetLifecyclePolicy

    -- ** TagResource 
    , module Network.AWS.MediaStore.TagResource

    -- ** GetCorsPolicy 
    , module Network.AWS.MediaStore.GetCorsPolicy

    -- ** UntagResource 
    , module Network.AWS.MediaStore.UntagResource

    -- ** DeleteContainerPolicy 
    , module Network.AWS.MediaStore.DeleteContainerPolicy

    -- ** PutContainerPolicy 
    , module Network.AWS.MediaStore.PutContainerPolicy

    -- ** GetContainerPolicy 
    , module Network.AWS.MediaStore.GetContainerPolicy

    -- * Types

    -- ** Origin
    , Origin (..)

    -- ** MetricPolicy
    , MetricPolicy (..)
    , mkMetricPolicy
    , mpContainerLevelMetrics
    , mpMetricPolicyRules

    -- ** PaginationToken
    , PaginationToken (..)

    -- ** ContainerPolicy
    , ContainerPolicy (..)

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** ObjectGroup
    , ObjectGroup (..)

    -- ** ContainerARN
    , ContainerARN (..)

    -- ** ContainerName
    , ContainerName (..)

    -- ** MethodName
    , MethodName (..)

    -- ** CorsRule
    , CorsRule (..)
    , mkCorsRule
    , crAllowedOrigins
    , crAllowedHeaders
    , crAllowedMethods
    , crExposeHeaders
    , crMaxAgeSeconds

    -- ** ContainerLevelMetrics
    , ContainerLevelMetrics (..)

    -- ** Container
    , Container (..)
    , mkContainer
    , cARN
    , cAccessLoggingEnabled
    , cCreationTime
    , cEndpoint
    , cName
    , cStatus

    -- ** Header
    , Header (..)

    -- ** MetricPolicyRule
    , MetricPolicyRule (..)
    , mkMetricPolicyRule
    , mprObjectGroup
    , mprObjectGroupName

    -- ** ObjectGroupName
    , ObjectGroupName (..)

    -- ** TagKey
    , TagKey (..)

    -- ** Endpoint
    , Endpoint (..)

    -- ** ContainerStatus
    , ContainerStatus (..)

    -- ** LifecyclePolicy
    , LifecyclePolicy (..)

    -- ** Key
    , Key (..)

    -- ** Value
    , Value (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.MediaStore.Types
import Network.AWS.MediaStore.Waiters
import Network.AWS.MediaStore.StopAccessLogging
import Network.AWS.MediaStore.PutLifecyclePolicy
import Network.AWS.MediaStore.DeleteLifecyclePolicy
import Network.AWS.MediaStore.ListTagsForResource
import Network.AWS.MediaStore.CreateContainer
import Network.AWS.MediaStore.ListContainers
import Network.AWS.MediaStore.DeleteContainer
import Network.AWS.MediaStore.PutCorsPolicy
import Network.AWS.MediaStore.DeleteCorsPolicy
import Network.AWS.MediaStore.StartAccessLogging
import Network.AWS.MediaStore.DescribeContainer
import Network.AWS.MediaStore.GetMetricPolicy
import Network.AWS.MediaStore.DeleteMetricPolicy
import Network.AWS.MediaStore.PutMetricPolicy
import Network.AWS.MediaStore.GetLifecyclePolicy
import Network.AWS.MediaStore.TagResource
import Network.AWS.MediaStore.GetCorsPolicy
import Network.AWS.MediaStore.UntagResource
import Network.AWS.MediaStore.DeleteContainerPolicy
import Network.AWS.MediaStore.PutContainerPolicy
import Network.AWS.MediaStore.GetContainerPolicy
import qualified Network.AWS.Prelude as Lude

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

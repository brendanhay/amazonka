{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
--
--
--
module Network.AWS.KinesisVideo
    (
    -- * Service Configuration
      kinesisVideo

    -- * Errors
    -- $errors

    -- ** InvalidArgumentException
    , _InvalidArgumentException

    -- ** TagsPerResourceExceededLimitException
    , _TagsPerResourceExceededLimitException

    -- ** NotAuthorizedException
    , _NotAuthorizedException

    -- ** ClientLimitExceededException
    , _ClientLimitExceededException

    -- ** InvalidDeviceException
    , _InvalidDeviceException

    -- ** VersionMismatchException
    , _VersionMismatchException

    -- ** AccountStreamLimitExceededException
    , _AccountStreamLimitExceededException

    -- ** InvalidResourceFormatException
    , _InvalidResourceFormatException

    -- ** DeviceStreamLimitExceededException
    , _DeviceStreamLimitExceededException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UntagStream
    , module Network.AWS.KinesisVideo.UntagStream

    -- ** UpdateDataRetention
    , module Network.AWS.KinesisVideo.UpdateDataRetention

    -- ** GetDataEndpoint
    , module Network.AWS.KinesisVideo.GetDataEndpoint

    -- ** ListTagsForStream
    , module Network.AWS.KinesisVideo.ListTagsForStream

    -- ** UpdateStream
    , module Network.AWS.KinesisVideo.UpdateStream

    -- ** DeleteStream
    , module Network.AWS.KinesisVideo.DeleteStream

    -- ** ListStreams
    , module Network.AWS.KinesisVideo.ListStreams

    -- ** CreateStream
    , module Network.AWS.KinesisVideo.CreateStream

    -- ** TagStream
    , module Network.AWS.KinesisVideo.TagStream

    -- ** DescribeStream
    , module Network.AWS.KinesisVideo.DescribeStream

    -- * Types

    -- ** APIName
    , APIName (..)

    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- ** StreamStatus
    , StreamStatus (..)

    -- ** UpdateDataRetentionOperation
    , UpdateDataRetentionOperation (..)

    -- ** StreamInfo
    , StreamInfo
    , streamInfo
    , siCreationTime
    , siStatus
    , siMediaType
    , siDataRetentionInHours
    , siStreamARN
    , siKMSKeyId
    , siDeviceName
    , siVersion
    , siStreamName

    -- ** StreamNameCondition
    , StreamNameCondition
    , streamNameCondition
    , sncComparisonOperator
    , sncComparisonValue
    ) where

import Network.AWS.KinesisVideo.CreateStream
import Network.AWS.KinesisVideo.DeleteStream
import Network.AWS.KinesisVideo.DescribeStream
import Network.AWS.KinesisVideo.GetDataEndpoint
import Network.AWS.KinesisVideo.ListStreams
import Network.AWS.KinesisVideo.ListTagsForStream
import Network.AWS.KinesisVideo.TagStream
import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.UntagStream
import Network.AWS.KinesisVideo.UpdateDataRetention
import Network.AWS.KinesisVideo.UpdateStream
import Network.AWS.KinesisVideo.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'KinesisVideo'.
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

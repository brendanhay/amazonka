{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Elemental MediaPackage
module Network.AWS.MediaPackage
    (
    -- * Service Configuration
      mediaPackage

    -- * Errors
    -- $errors

    -- ** UnprocessableEntityException
    , _UnprocessableEntityException

    -- ** ForbiddenException
    , _ForbiddenException

    -- ** NotFoundException
    , _NotFoundException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalServerErrorException
    , _InternalServerErrorException

    -- ** ServiceUnavailableException
    , _ServiceUnavailableException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** RotateChannelCredentials
    , module Network.AWS.MediaPackage.RotateChannelCredentials

    -- ** DescribeOriginEndpoint
    , module Network.AWS.MediaPackage.DescribeOriginEndpoint

    -- ** ListChannels (Paginated)
    , module Network.AWS.MediaPackage.ListChannels

    -- ** DeleteChannel
    , module Network.AWS.MediaPackage.DeleteChannel

    -- ** UpdateChannel
    , module Network.AWS.MediaPackage.UpdateChannel

    -- ** CreateOriginEndpoint
    , module Network.AWS.MediaPackage.CreateOriginEndpoint

    -- ** ListOriginEndpoints (Paginated)
    , module Network.AWS.MediaPackage.ListOriginEndpoints

    -- ** CreateChannel
    , module Network.AWS.MediaPackage.CreateChannel

    -- ** DescribeChannel
    , module Network.AWS.MediaPackage.DescribeChannel

    -- ** DeleteOriginEndpoint
    , module Network.AWS.MediaPackage.DeleteOriginEndpoint

    -- ** UpdateOriginEndpoint
    , module Network.AWS.MediaPackage.UpdateOriginEndpoint

    -- * Types

    -- ** AdMarkers
    , AdMarkers (..)

    -- ** EncryptionMethod
    , EncryptionMethod (..)

    -- ** PlaylistType
    , PlaylistType (..)

    -- ** Profile
    , Profile (..)

    -- ** StreamOrder
    , StreamOrder (..)

    -- ** Channel
    , Channel
    , channel
    , cHlsIngest
    , cARN
    , cId
    , cDescription

    -- ** CmafEncryption
    , CmafEncryption
    , cmafEncryption
    , ceKeyRotationIntervalSeconds
    , ceSpekeKeyProvider

    -- ** CmafPackage
    , CmafPackage
    , cmafPackage
    , cpHlsManifests
    , cpSegmentDurationSeconds
    , cpStreamSelection
    , cpEncryption
    , cpSegmentPrefix

    -- ** CmafPackageCreateOrUpdateParameters
    , CmafPackageCreateOrUpdateParameters
    , cmafPackageCreateOrUpdateParameters
    , cpcoupHlsManifests
    , cpcoupSegmentDurationSeconds
    , cpcoupStreamSelection
    , cpcoupEncryption
    , cpcoupSegmentPrefix

    -- ** DashEncryption
    , DashEncryption
    , dashEncryption
    , deKeyRotationIntervalSeconds
    , deSpekeKeyProvider

    -- ** DashPackage
    , DashPackage
    , dashPackage
    , dpMinBufferTimeSeconds
    , dpProfile
    , dpSegmentDurationSeconds
    , dpStreamSelection
    , dpEncryption
    , dpMinUpdatePeriodSeconds
    , dpSuggestedPresentationDelaySeconds
    , dpManifestWindowSeconds

    -- ** HlsEncryption
    , HlsEncryption
    , hlsEncryption
    , heEncryptionMethod
    , heKeyRotationIntervalSeconds
    , heConstantInitializationVector
    , heRepeatExtXKey
    , heSpekeKeyProvider

    -- ** HlsIngest
    , HlsIngest
    , hlsIngest
    , hiIngestEndpoints

    -- ** HlsManifest
    , HlsManifest
    , hlsManifest
    , hmManifestName
    , hmURL
    , hmPlaylistType
    , hmProgramDateTimeIntervalSeconds
    , hmAdMarkers
    , hmIncludeIframeOnlyStream
    , hmPlaylistWindowSeconds
    , hmId

    -- ** HlsManifestCreateOrUpdateParameters
    , HlsManifestCreateOrUpdateParameters
    , hlsManifestCreateOrUpdateParameters
    , hmcoupManifestName
    , hmcoupPlaylistType
    , hmcoupProgramDateTimeIntervalSeconds
    , hmcoupAdMarkers
    , hmcoupIncludeIframeOnlyStream
    , hmcoupPlaylistWindowSeconds
    , hmcoupId

    -- ** HlsPackage
    , HlsPackage
    , hlsPackage
    , hpUseAudioRenditionGroup
    , hpPlaylistType
    , hpSegmentDurationSeconds
    , hpProgramDateTimeIntervalSeconds
    , hpStreamSelection
    , hpAdMarkers
    , hpEncryption
    , hpIncludeIframeOnlyStream
    , hpPlaylistWindowSeconds

    -- ** IngestEndpoint
    , IngestEndpoint
    , ingestEndpoint
    , ieURL
    , ieUsername
    , iePassword

    -- ** MssEncryption
    , MssEncryption
    , mssEncryption
    , meSpekeKeyProvider

    -- ** MssPackage
    , MssPackage
    , mssPackage
    , mpSegmentDurationSeconds
    , mpStreamSelection
    , mpEncryption
    , mpManifestWindowSeconds

    -- ** OriginEndpoint
    , OriginEndpoint
    , originEndpoint
    , oeWhitelist
    , oeHlsPackage
    , oeARN
    , oeManifestName
    , oeURL
    , oeChannelId
    , oeStartoverWindowSeconds
    , oeDashPackage
    , oeMssPackage
    , oeId
    , oeTimeDelaySeconds
    , oeCmafPackage
    , oeDescription

    -- ** SpekeKeyProvider
    , SpekeKeyProvider
    , spekeKeyProvider
    , skpURL
    , skpResourceId
    , skpRoleARN
    , skpSystemIds

    -- ** StreamSelection
    , StreamSelection
    , streamSelection
    , ssStreamOrder
    , ssMinVideoBitsPerSecond
    , ssMaxVideoBitsPerSecond
    ) where

import Network.AWS.MediaPackage.CreateChannel
import Network.AWS.MediaPackage.CreateOriginEndpoint
import Network.AWS.MediaPackage.DeleteChannel
import Network.AWS.MediaPackage.DeleteOriginEndpoint
import Network.AWS.MediaPackage.DescribeChannel
import Network.AWS.MediaPackage.DescribeOriginEndpoint
import Network.AWS.MediaPackage.ListChannels
import Network.AWS.MediaPackage.ListOriginEndpoints
import Network.AWS.MediaPackage.RotateChannelCredentials
import Network.AWS.MediaPackage.Types
import Network.AWS.MediaPackage.UpdateChannel
import Network.AWS.MediaPackage.UpdateOriginEndpoint
import Network.AWS.MediaPackage.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'MediaPackage'.
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

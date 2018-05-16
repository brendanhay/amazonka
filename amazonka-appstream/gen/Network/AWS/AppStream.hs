{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon AppStream 2.0__
--
-- You can use Amazon AppStream 2.0 to stream desktop applications to any device running a web browser, without rewriting them.
--
module Network.AWS.AppStream
    (
    -- * Service Configuration
      appStream

    -- * Errors
    -- $errors

    -- ** InvalidRoleException
    , _InvalidRoleException

    -- ** ResourceAlreadyExistsException
    , _ResourceAlreadyExistsException

    -- ** IncompatibleImageException
    , _IncompatibleImageException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** OperationNotPermittedException
    , _OperationNotPermittedException

    -- ** InvalidAccountStatusException
    , _InvalidAccountStatusException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidParameterCombinationException
    , _InvalidParameterCombinationException

    -- ** ResourceNotAvailableException
    , _ResourceNotAvailableException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- ** FleetStopped
    , fleetStopped

    -- ** FleetStarted
    , fleetStarted

    -- * Operations
    -- $operations

    -- ** DisassociateFleet
    , module Network.AWS.AppStream.DisassociateFleet

    -- ** ListAssociatedFleets
    , module Network.AWS.AppStream.ListAssociatedFleets

    -- ** DeleteStack
    , module Network.AWS.AppStream.DeleteStack

    -- ** UpdateStack
    , module Network.AWS.AppStream.UpdateStack

    -- ** CreateDirectoryConfig
    , module Network.AWS.AppStream.CreateDirectoryConfig

    -- ** ListAssociatedStacks
    , module Network.AWS.AppStream.ListAssociatedStacks

    -- ** DeleteFleet
    , module Network.AWS.AppStream.DeleteFleet

    -- ** UpdateFleet
    , module Network.AWS.AppStream.UpdateFleet

    -- ** DeleteImageBuilder
    , module Network.AWS.AppStream.DeleteImageBuilder

    -- ** AssociateFleet
    , module Network.AWS.AppStream.AssociateFleet

    -- ** CreateImageBuilder
    , module Network.AWS.AppStream.CreateImageBuilder

    -- ** ListTagsForResource
    , module Network.AWS.AppStream.ListTagsForResource

    -- ** DescribeDirectoryConfigs
    , module Network.AWS.AppStream.DescribeDirectoryConfigs

    -- ** CreateImageBuilderStreamingURL
    , module Network.AWS.AppStream.CreateImageBuilderStreamingURL

    -- ** DescribeSessions
    , module Network.AWS.AppStream.DescribeSessions

    -- ** DescribeStacks
    , module Network.AWS.AppStream.DescribeStacks

    -- ** DescribeFleets
    , module Network.AWS.AppStream.DescribeFleets

    -- ** DescribeImageBuilders
    , module Network.AWS.AppStream.DescribeImageBuilders

    -- ** StopFleet
    , module Network.AWS.AppStream.StopFleet

    -- ** StartImageBuilder
    , module Network.AWS.AppStream.StartImageBuilder

    -- ** DeleteDirectoryConfig
    , module Network.AWS.AppStream.DeleteDirectoryConfig

    -- ** UpdateDirectoryConfig
    , module Network.AWS.AppStream.UpdateDirectoryConfig

    -- ** CreateFleet
    , module Network.AWS.AppStream.CreateFleet

    -- ** CreateStack
    , module Network.AWS.AppStream.CreateStack

    -- ** CopyImage
    , module Network.AWS.AppStream.CopyImage

    -- ** ExpireSession
    , module Network.AWS.AppStream.ExpireSession

    -- ** TagResource
    , module Network.AWS.AppStream.TagResource

    -- ** CreateStreamingURL
    , module Network.AWS.AppStream.CreateStreamingURL

    -- ** UntagResource
    , module Network.AWS.AppStream.UntagResource

    -- ** StartFleet
    , module Network.AWS.AppStream.StartFleet

    -- ** StopImageBuilder
    , module Network.AWS.AppStream.StopImageBuilder

    -- ** DeleteImage
    , module Network.AWS.AppStream.DeleteImage

    -- ** DescribeImages
    , module Network.AWS.AppStream.DescribeImages

    -- * Types

    -- ** AuthenticationType
    , AuthenticationType (..)

    -- ** FleetAttribute
    , FleetAttribute (..)

    -- ** FleetErrorCode
    , FleetErrorCode (..)

    -- ** FleetState
    , FleetState (..)

    -- ** FleetType
    , FleetType (..)

    -- ** ImageBuilderState
    , ImageBuilderState (..)

    -- ** ImageBuilderStateChangeReasonCode
    , ImageBuilderStateChangeReasonCode (..)

    -- ** ImageState
    , ImageState (..)

    -- ** ImageStateChangeReasonCode
    , ImageStateChangeReasonCode (..)

    -- ** PlatformType
    , PlatformType (..)

    -- ** SessionState
    , SessionState (..)

    -- ** StackAttribute
    , StackAttribute (..)

    -- ** StackErrorCode
    , StackErrorCode (..)

    -- ** StorageConnectorType
    , StorageConnectorType (..)

    -- ** VisibilityType
    , VisibilityType (..)

    -- ** Application
    , Application
    , application
    , aEnabled
    , aLaunchPath
    , aLaunchParameters
    , aName
    , aDisplayName
    , aMetadata
    , aIconURL

    -- ** ComputeCapacity
    , ComputeCapacity
    , computeCapacity
    , ccDesiredInstances

    -- ** ComputeCapacityStatus
    , ComputeCapacityStatus
    , computeCapacityStatus
    , ccsInUse
    , ccsRunning
    , ccsAvailable
    , ccsDesired

    -- ** DirectoryConfig
    , DirectoryConfig
    , directoryConfig
    , dcCreatedTime
    , dcServiceAccountCredentials
    , dcOrganizationalUnitDistinguishedNames
    , dcDirectoryName

    -- ** DomainJoinInfo
    , DomainJoinInfo
    , domainJoinInfo
    , djiOrganizationalUnitDistinguishedName
    , djiDirectoryName

    -- ** Fleet
    , Fleet
    , fleet
    , fDomainJoinInfo
    , fDisconnectTimeoutInSeconds
    , fMaxUserDurationInSeconds
    , fCreatedTime
    , fFleetType
    , fVPCConfig
    , fFleetErrors
    , fDisplayName
    , fEnableDefaultInternetAccess
    , fDescription
    , fARN
    , fName
    , fImageName
    , fInstanceType
    , fComputeCapacityStatus
    , fState

    -- ** FleetError
    , FleetError
    , fleetError
    , feErrorCode
    , feErrorMessage

    -- ** Image
    , Image
    , image
    , iState
    , iPlatform
    , iPublicBaseImageReleasedDate
    , iStateChangeReason
    , iARN
    , iCreatedTime
    , iImageBuilderSupported
    , iVisibility
    , iBaseImageARN
    , iDisplayName
    , iDescription
    , iAppstreamAgentVersion
    , iApplications
    , iName

    -- ** ImageBuilder
    , ImageBuilder
    , imageBuilder
    , ibDomainJoinInfo
    , ibState
    , ibPlatform
    , ibStateChangeReason
    , ibARN
    , ibCreatedTime
    , ibImageBuilderErrors
    , ibInstanceType
    , ibVPCConfig
    , ibImageARN
    , ibDisplayName
    , ibEnableDefaultInternetAccess
    , ibDescription
    , ibAppstreamAgentVersion
    , ibName

    -- ** ImageBuilderStateChangeReason
    , ImageBuilderStateChangeReason
    , imageBuilderStateChangeReason
    , ibscrCode
    , ibscrMessage

    -- ** ImageStateChangeReason
    , ImageStateChangeReason
    , imageStateChangeReason
    , iscrCode
    , iscrMessage

    -- ** ResourceError
    , ResourceError
    , resourceError
    , reErrorCode
    , reErrorMessage
    , reErrorTimestamp

    -- ** ServiceAccountCredentials
    , ServiceAccountCredentials
    , serviceAccountCredentials
    , sacAccountName
    , sacAccountPassword

    -- ** Session
    , Session
    , session
    , sAuthenticationType
    , sId
    , sUserId
    , sStackName
    , sFleetName
    , sState

    -- ** Stack
    , Stack
    , stack
    , sFeedbackURL
    , sARN
    , sCreatedTime
    , sStorageConnectors
    , sDisplayName
    , sStackErrors
    , sDescription
    , sRedirectURL
    , sName

    -- ** StackError
    , StackError
    , stackError
    , seErrorCode
    , seErrorMessage

    -- ** StorageConnector
    , StorageConnector
    , storageConnector
    , scResourceIdentifier
    , scConnectorType

    -- ** VPCConfig
    , VPCConfig
    , vpcConfig
    , vcSecurityGroupIds
    , vcSubnetIds
    ) where

import Network.AWS.AppStream.AssociateFleet
import Network.AWS.AppStream.CopyImage
import Network.AWS.AppStream.CreateDirectoryConfig
import Network.AWS.AppStream.CreateFleet
import Network.AWS.AppStream.CreateImageBuilder
import Network.AWS.AppStream.CreateImageBuilderStreamingURL
import Network.AWS.AppStream.CreateStack
import Network.AWS.AppStream.CreateStreamingURL
import Network.AWS.AppStream.DeleteDirectoryConfig
import Network.AWS.AppStream.DeleteFleet
import Network.AWS.AppStream.DeleteImage
import Network.AWS.AppStream.DeleteImageBuilder
import Network.AWS.AppStream.DeleteStack
import Network.AWS.AppStream.DescribeDirectoryConfigs
import Network.AWS.AppStream.DescribeFleets
import Network.AWS.AppStream.DescribeImageBuilders
import Network.AWS.AppStream.DescribeImages
import Network.AWS.AppStream.DescribeSessions
import Network.AWS.AppStream.DescribeStacks
import Network.AWS.AppStream.DisassociateFleet
import Network.AWS.AppStream.ExpireSession
import Network.AWS.AppStream.ListAssociatedFleets
import Network.AWS.AppStream.ListAssociatedStacks
import Network.AWS.AppStream.ListTagsForResource
import Network.AWS.AppStream.StartFleet
import Network.AWS.AppStream.StartImageBuilder
import Network.AWS.AppStream.StopFleet
import Network.AWS.AppStream.StopImageBuilder
import Network.AWS.AppStream.TagResource
import Network.AWS.AppStream.Types
import Network.AWS.AppStream.UntagResource
import Network.AWS.AppStream.UpdateDirectoryConfig
import Network.AWS.AppStream.UpdateFleet
import Network.AWS.AppStream.UpdateStack
import Network.AWS.AppStream.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AppStream'.
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

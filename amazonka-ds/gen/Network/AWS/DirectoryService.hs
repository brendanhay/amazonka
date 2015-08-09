{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service
--
-- This is the /AWS Directory Service API Reference/. This guide provides
-- detailed information about AWS Directory Service operations, data types,
-- parameters, and errors.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/welcome.html AWS API Reference>
module Network.AWS.DirectoryService
    (
    -- * Service
      DirectoryService

    -- * Errors
    -- $errors

    -- ** AuthenticationFailedException
    , _AuthenticationFailedException

    -- ** DirectoryUnavailableException
    , _DirectoryUnavailableException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** UnsupportedOperationException
    , _UnsupportedOperationException

    -- ** EntityAlreadyExistsException
    , _EntityAlreadyExistsException

    -- ** DirectoryLimitExceededException
    , _DirectoryLimitExceededException

    -- ** EntityDoesNotExistException
    , _EntityDoesNotExistException

    -- ** InsufficientPermissionsException
    , _InsufficientPermissionsException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** ServiceException
    , _ServiceException

    -- ** SnapshotLimitExceededException
    , _SnapshotLimitExceededException

    -- ** ClientException
    , _ClientException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetSnapshotLimits
    , module Network.AWS.DirectoryService.GetSnapshotLimits

    -- ** ConnectDirectory
    , module Network.AWS.DirectoryService.ConnectDirectory

    -- ** DescribeDirectories
    , module Network.AWS.DirectoryService.DescribeDirectories

    -- ** CreateAlias
    , module Network.AWS.DirectoryService.CreateAlias

    -- ** CreateDirectory
    , module Network.AWS.DirectoryService.CreateDirectory

    -- ** EnableSSO
    , module Network.AWS.DirectoryService.EnableSSO

    -- ** EnableRadius
    , module Network.AWS.DirectoryService.EnableRadius

    -- ** DisableRadius
    , module Network.AWS.DirectoryService.DisableRadius

    -- ** DescribeSnapshots
    , module Network.AWS.DirectoryService.DescribeSnapshots

    -- ** RestoreFromSnapshot
    , module Network.AWS.DirectoryService.RestoreFromSnapshot

    -- ** DeleteSnapshot
    , module Network.AWS.DirectoryService.DeleteSnapshot

    -- ** CreateSnapshot
    , module Network.AWS.DirectoryService.CreateSnapshot

    -- ** DeleteDirectory
    , module Network.AWS.DirectoryService.DeleteDirectory

    -- ** DisableSSO
    , module Network.AWS.DirectoryService.DisableSSO

    -- ** CreateComputer
    , module Network.AWS.DirectoryService.CreateComputer

    -- ** GetDirectoryLimits
    , module Network.AWS.DirectoryService.GetDirectoryLimits

    -- ** UpdateRadius
    , module Network.AWS.DirectoryService.UpdateRadius

    -- * Types

    -- ** DirectorySize
    , DirectorySize (..)

    -- ** DirectoryStage
    , DirectoryStage (..)

    -- ** DirectoryType
    , DirectoryType (..)

    -- ** RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- ** RadiusStatus
    , RadiusStatus (..)

    -- ** SnapshotStatus
    , SnapshotStatus (..)

    -- ** SnapshotType
    , SnapshotType (..)

    -- ** Attribute
    , Attribute
    , attribute
    , aValue
    , aName

    -- ** Computer
    , Computer
    , computer
    , cComputerId
    , cComputerAttributes
    , cComputerName

    -- ** DirectoryConnectSettings
    , DirectoryConnectSettings
    , directoryConnectSettings
    , dcsVPCId
    , dcsSubnetIds
    , dcsCustomerDNSIPs
    , dcsCustomerUserName

    -- ** DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription
    , directoryConnectSettingsDescription
    , dcsdCustomerUserName
    , dcsdSubnetIds
    , dcsdVPCId
    , dcsdConnectIPs
    , dcsdSecurityGroupId
    , dcsdAvailabilityZones

    -- ** DirectoryDescription
    , DirectoryDescription
    , directoryDescription
    , ddRadiusStatus
    , ddDirectoryId
    , ddStage
    , ddAccessURL
    , ddShortName
    , ddSize
    , ddRadiusSettings
    , ddLaunchTime
    , ddAlias
    , ddName
    , ddSSOEnabled
    , ddStageLastUpdatedDateTime
    , ddStageReason
    , ddDNSIPAddrs
    , ddVPCSettings
    , ddType
    , ddConnectSettings
    , ddDescription

    -- ** DirectoryLimits
    , DirectoryLimits
    , directoryLimits
    , dlConnectedDirectoriesCurrentCount
    , dlConnectedDirectoriesLimit
    , dlConnectedDirectoriesLimitReached
    , dlCloudOnlyDirectoriesLimit
    , dlCloudOnlyDirectoriesCurrentCount
    , dlCloudOnlyDirectoriesLimitReached

    -- ** DirectoryVPCSettings
    , DirectoryVPCSettings
    , directoryVPCSettings
    , dvsVPCId
    , dvsSubnetIds

    -- ** DirectoryVPCSettingsDescription
    , DirectoryVPCSettingsDescription
    , directoryVPCSettingsDescription
    , dvsdSubnetIds
    , dvsdVPCId
    , dvsdSecurityGroupId
    , dvsdAvailabilityZones

    -- ** RadiusSettings
    , RadiusSettings
    , radiusSettings
    , rsDisplayLabel
    , rsRadiusServers
    , rsRadiusRetries
    , rsAuthenticationProtocol
    , rsUseSameUsername
    , rsSharedSecret
    , rsRadiusTimeout
    , rsRadiusPort

    -- ** Snapshot
    , Snapshot
    , snapshot
    , sDirectoryId
    , sStatus
    , sStartTime
    , sName
    , sType
    , sSnapshotId

    -- ** SnapshotLimits
    , SnapshotLimits
    , snapshotLimits
    , slManualSnapshotsLimitReached
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit
    ) where

import           Network.AWS.DirectoryService.ConnectDirectory
import           Network.AWS.DirectoryService.CreateAlias
import           Network.AWS.DirectoryService.CreateComputer
import           Network.AWS.DirectoryService.CreateDirectory
import           Network.AWS.DirectoryService.CreateSnapshot
import           Network.AWS.DirectoryService.DeleteDirectory
import           Network.AWS.DirectoryService.DeleteSnapshot
import           Network.AWS.DirectoryService.DescribeDirectories
import           Network.AWS.DirectoryService.DescribeSnapshots
import           Network.AWS.DirectoryService.DisableRadius
import           Network.AWS.DirectoryService.DisableSSO
import           Network.AWS.DirectoryService.EnableRadius
import           Network.AWS.DirectoryService.EnableSSO
import           Network.AWS.DirectoryService.GetDirectoryLimits
import           Network.AWS.DirectoryService.GetSnapshotLimits
import           Network.AWS.DirectoryService.RestoreFromSnapshot
import           Network.AWS.DirectoryService.Types
import           Network.AWS.DirectoryService.UpdateRadius
import           Network.AWS.DirectoryService.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DirectoryService'.
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

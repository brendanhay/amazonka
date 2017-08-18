{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Lightsail is the easiest way to get started with AWS for developers who just need virtual private servers. Lightsail includes everything you need to launch your project quickly - a virtual machine, SSD-based storage, data transfer, DNS management, and a static IP - for a low, predictable price. You manage those Lightsail servers through the Lightsail console or by using the API or command-line interface (CLI).
--
--
-- For more information about Lightsail concepts and tasks, see the <https://lightsail.aws.amazon.com/ls/docs/all Lightsail Dev Guide> .
--
-- To use the Lightsail API or the CLI, you will need to use AWS Identity and Access Management (IAM) to generate access keys. For details about how to set this up, see the <http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli Lightsail Dev Guide> .
--
module Network.AWS.Lightsail
    (
    -- * Service Configuration
      lightsail

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** AccountSetupInProgressException
    , _AccountSetupInProgressException

    -- ** NotFoundException
    , _NotFoundException

    -- ** OperationFailureException
    , _OperationFailureException

    -- ** ServiceException
    , _ServiceException

    -- ** UnauthenticatedException
    , _UnauthenticatedException

    -- ** InvalidInputException
    , _InvalidInputException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CloseInstancePublicPorts
    , module Network.AWS.Lightsail.CloseInstancePublicPorts

    -- ** AllocateStaticIP
    , module Network.AWS.Lightsail.AllocateStaticIP

    -- ** DeleteKeyPair
    , module Network.AWS.Lightsail.DeleteKeyPair

    -- ** DeleteInstanceSnapshot
    , module Network.AWS.Lightsail.DeleteInstanceSnapshot

    -- ** GetInstances (Paginated)
    , module Network.AWS.Lightsail.GetInstances

    -- ** GetInstance
    , module Network.AWS.Lightsail.GetInstance

    -- ** AttachStaticIP
    , module Network.AWS.Lightsail.AttachStaticIP

    -- ** DownloadDefaultKeyPair
    , module Network.AWS.Lightsail.DownloadDefaultKeyPair

    -- ** GetDomains (Paginated)
    , module Network.AWS.Lightsail.GetDomains

    -- ** CreateDomainEntry
    , module Network.AWS.Lightsail.CreateDomainEntry

    -- ** ImportKeyPair
    , module Network.AWS.Lightsail.ImportKeyPair

    -- ** GetInstanceSnapshots (Paginated)
    , module Network.AWS.Lightsail.GetInstanceSnapshots

    -- ** ReleaseStaticIP
    , module Network.AWS.Lightsail.ReleaseStaticIP

    -- ** DeleteInstance
    , module Network.AWS.Lightsail.DeleteInstance

    -- ** RebootInstance
    , module Network.AWS.Lightsail.RebootInstance

    -- ** GetInstanceSnapshot
    , module Network.AWS.Lightsail.GetInstanceSnapshot

    -- ** GetDomain
    , module Network.AWS.Lightsail.GetDomain

    -- ** GetActiveNames (Paginated)
    , module Network.AWS.Lightsail.GetActiveNames

    -- ** GetInstanceAccessDetails
    , module Network.AWS.Lightsail.GetInstanceAccessDetails

    -- ** StopInstance
    , module Network.AWS.Lightsail.StopInstance

    -- ** CreateInstanceSnapshot
    , module Network.AWS.Lightsail.CreateInstanceSnapshot

    -- ** IsVPCPeered
    , module Network.AWS.Lightsail.IsVPCPeered

    -- ** GetStaticIPs (Paginated)
    , module Network.AWS.Lightsail.GetStaticIPs

    -- ** UnpeerVPC
    , module Network.AWS.Lightsail.UnpeerVPC

    -- ** CreateInstancesFromSnapshot
    , module Network.AWS.Lightsail.CreateInstancesFromSnapshot

    -- ** CreateDomain
    , module Network.AWS.Lightsail.CreateDomain

    -- ** PeerVPC
    , module Network.AWS.Lightsail.PeerVPC

    -- ** GetStaticIP
    , module Network.AWS.Lightsail.GetStaticIP

    -- ** GetBlueprints (Paginated)
    , module Network.AWS.Lightsail.GetBlueprints

    -- ** GetInstancePortStates
    , module Network.AWS.Lightsail.GetInstancePortStates

    -- ** DeleteDomainEntry
    , module Network.AWS.Lightsail.DeleteDomainEntry

    -- ** UpdateDomainEntry
    , module Network.AWS.Lightsail.UpdateDomainEntry

    -- ** GetRegions
    , module Network.AWS.Lightsail.GetRegions

    -- ** GetInstanceState
    , module Network.AWS.Lightsail.GetInstanceState

    -- ** GetKeyPairs (Paginated)
    , module Network.AWS.Lightsail.GetKeyPairs

    -- ** GetOperations (Paginated)
    , module Network.AWS.Lightsail.GetOperations

    -- ** GetOperation
    , module Network.AWS.Lightsail.GetOperation

    -- ** GetInstanceMetricData
    , module Network.AWS.Lightsail.GetInstanceMetricData

    -- ** GetKeyPair
    , module Network.AWS.Lightsail.GetKeyPair

    -- ** PutInstancePublicPorts
    , module Network.AWS.Lightsail.PutInstancePublicPorts

    -- ** DetachStaticIP
    , module Network.AWS.Lightsail.DetachStaticIP

    -- ** CreateInstances
    , module Network.AWS.Lightsail.CreateInstances

    -- ** OpenInstancePublicPorts
    , module Network.AWS.Lightsail.OpenInstancePublicPorts

    -- ** GetBundles (Paginated)
    , module Network.AWS.Lightsail.GetBundles

    -- ** DeleteDomain
    , module Network.AWS.Lightsail.DeleteDomain

    -- ** GetOperationsForResource
    , module Network.AWS.Lightsail.GetOperationsForResource

    -- ** CreateKeyPair
    , module Network.AWS.Lightsail.CreateKeyPair

    -- ** StartInstance
    , module Network.AWS.Lightsail.StartInstance

    -- * Types

    -- ** AccessDirection
    , AccessDirection (..)

    -- ** BlueprintType
    , BlueprintType (..)

    -- ** InstanceAccessProtocol
    , InstanceAccessProtocol (..)

    -- ** InstanceMetricName
    , InstanceMetricName (..)

    -- ** InstanceSnapshotState
    , InstanceSnapshotState (..)

    -- ** MetricStatistic
    , MetricStatistic (..)

    -- ** MetricUnit
    , MetricUnit (..)

    -- ** NetworkProtocol
    , NetworkProtocol (..)

    -- ** OperationStatus
    , OperationStatus (..)

    -- ** OperationType
    , OperationType (..)

    -- ** PortAccessType
    , PortAccessType (..)

    -- ** PortState
    , PortState (..)

    -- ** RegionName
    , RegionName (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azState
    , azZoneName

    -- ** Blueprint
    , Blueprint
    , blueprint
    , bVersionCode
    , bGroup
    , bMinPower
    , bProductURL
    , bLicenseURL
    , bName
    , bVersion
    , bBlueprintId
    , bType
    , bIsActive
    , bDescription

    -- ** Bundle
    , Bundle
    , bundle
    , bunCpuCount
    , bunTransferPerMonthInGb
    , bunBundleId
    , bunInstanceType
    , bunName
    , bunPower
    , bunDiskSizeInGb
    , bunPrice
    , bunIsActive
    , bunRamSizeInGb

    -- ** Disk
    , Disk
    , disk
    , dResourceType
    , dArn
    , dPath
    , dCreatedAt
    , dLocation
    , dIops
    , dIsAttached
    , dAttachmentState
    , dName
    , dSizeInGb
    , dSupportCode
    , dIsSystemDisk
    , dAttachedTo
    , dGbInUse

    -- ** Domain
    , Domain
    , domain
    , domResourceType
    , domDomainEntries
    , domArn
    , domCreatedAt
    , domLocation
    , domName
    , domSupportCode

    -- ** DomainEntry
    , DomainEntry
    , domainEntry
    , deName
    , deId
    , deOptions
    , deType
    , deTarget

    -- ** Instance
    , Instance
    , instance'
    , iState
    , iIpv6Address
    , iResourceType
    , iArn
    , iCreatedAt
    , iLocation
    , iSshKeyName
    , iUsername
    , iNetworking
    , iBundleId
    , iName
    , iSupportCode
    , iBlueprintId
    , iPrivateIPAddress
    , iBlueprintName
    , iIsStaticIP
    , iPublicIPAddress
    , iHardware

    -- ** InstanceAccessDetails
    , InstanceAccessDetails
    , instanceAccessDetails
    , iadCertKey
    , iadIpAddress
    , iadPrivateKey
    , iadExpiresAt
    , iadUsername
    , iadProtocol
    , iadPassword
    , iadInstanceName

    -- ** InstanceHardware
    , InstanceHardware
    , instanceHardware
    , ihCpuCount
    , ihDisks
    , ihRamSizeInGb

    -- ** InstanceNetworking
    , InstanceNetworking
    , instanceNetworking
    , inMonthlyTransfer
    , inPorts

    -- ** InstancePortInfo
    , InstancePortInfo
    , instancePortInfo
    , ipiFromPort
    , ipiCommonName
    , ipiProtocol
    , ipiAccessDirection
    , ipiAccessType
    , ipiToPort
    , ipiAccessFrom

    -- ** InstancePortState
    , InstancePortState
    , instancePortState
    , ipsFromPort
    , ipsState
    , ipsProtocol
    , ipsToPort

    -- ** InstanceSnapshot
    , InstanceSnapshot
    , instanceSnapshot
    , insFromBlueprintId
    , insState
    , insResourceType
    , insArn
    , insCreatedAt
    , insLocation
    , insProgress
    , insName
    , insFromBundleId
    , insSizeInGb
    , insSupportCode
    , insFromInstanceARN
    , insFromInstanceName

    -- ** InstanceState
    , InstanceState
    , instanceState
    , isName
    , isCode

    -- ** KeyPair
    , KeyPair
    , keyPair
    , kpResourceType
    , kpArn
    , kpCreatedAt
    , kpLocation
    , kpFingerprint
    , kpName
    , kpSupportCode

    -- ** MetricDatapoint
    , MetricDatapoint
    , metricDatapoint
    , mdSampleCount
    , mdMaximum
    , mdAverage
    , mdMinimum
    , mdSum
    , mdTimestamp
    , mdUnit

    -- ** MonthlyTransfer
    , MonthlyTransfer
    , monthlyTransfer
    , mtGbPerMonthAllocated

    -- ** Operation
    , Operation
    , operation
    , oStatus
    , oOperationDetails
    , oResourceType
    , oCreatedAt
    , oResourceName
    , oLocation
    , oStatusChangedAt
    , oErrorDetails
    , oErrorCode
    , oId
    , oOperationType
    , oIsTerminal

    -- ** PortInfo
    , PortInfo
    , portInfo
    , piFromPort
    , piProtocol
    , piToPort

    -- ** RegionInfo
    , RegionInfo
    , regionInfo
    , riAvailabilityZones
    , riName
    , riDisplayName
    , riContinentCode
    , riDescription

    -- ** ResourceLocation
    , ResourceLocation
    , resourceLocation
    , rlRegionName
    , rlAvailabilityZone

    -- ** StaticIP
    , StaticIP
    , staticIP
    , siIpAddress
    , siResourceType
    , siArn
    , siCreatedAt
    , siLocation
    , siIsAttached
    , siName
    , siSupportCode
    , siAttachedTo
    ) where

import           Network.AWS.Lightsail.AllocateStaticIP
import           Network.AWS.Lightsail.AttachStaticIP
import           Network.AWS.Lightsail.CloseInstancePublicPorts
import           Network.AWS.Lightsail.CreateDomain
import           Network.AWS.Lightsail.CreateDomainEntry
import           Network.AWS.Lightsail.CreateInstances
import           Network.AWS.Lightsail.CreateInstancesFromSnapshot
import           Network.AWS.Lightsail.CreateInstanceSnapshot
import           Network.AWS.Lightsail.CreateKeyPair
import           Network.AWS.Lightsail.DeleteDomain
import           Network.AWS.Lightsail.DeleteDomainEntry
import           Network.AWS.Lightsail.DeleteInstance
import           Network.AWS.Lightsail.DeleteInstanceSnapshot
import           Network.AWS.Lightsail.DeleteKeyPair
import           Network.AWS.Lightsail.DetachStaticIP
import           Network.AWS.Lightsail.DownloadDefaultKeyPair
import           Network.AWS.Lightsail.GetActiveNames
import           Network.AWS.Lightsail.GetBlueprints
import           Network.AWS.Lightsail.GetBundles
import           Network.AWS.Lightsail.GetDomain
import           Network.AWS.Lightsail.GetDomains
import           Network.AWS.Lightsail.GetInstance
import           Network.AWS.Lightsail.GetInstanceAccessDetails
import           Network.AWS.Lightsail.GetInstanceMetricData
import           Network.AWS.Lightsail.GetInstancePortStates
import           Network.AWS.Lightsail.GetInstances
import           Network.AWS.Lightsail.GetInstanceSnapshot
import           Network.AWS.Lightsail.GetInstanceSnapshots
import           Network.AWS.Lightsail.GetInstanceState
import           Network.AWS.Lightsail.GetKeyPair
import           Network.AWS.Lightsail.GetKeyPairs
import           Network.AWS.Lightsail.GetOperation
import           Network.AWS.Lightsail.GetOperations
import           Network.AWS.Lightsail.GetOperationsForResource
import           Network.AWS.Lightsail.GetRegions
import           Network.AWS.Lightsail.GetStaticIP
import           Network.AWS.Lightsail.GetStaticIPs
import           Network.AWS.Lightsail.ImportKeyPair
import           Network.AWS.Lightsail.IsVPCPeered
import           Network.AWS.Lightsail.OpenInstancePublicPorts
import           Network.AWS.Lightsail.PeerVPC
import           Network.AWS.Lightsail.PutInstancePublicPorts
import           Network.AWS.Lightsail.RebootInstance
import           Network.AWS.Lightsail.ReleaseStaticIP
import           Network.AWS.Lightsail.StartInstance
import           Network.AWS.Lightsail.StopInstance
import           Network.AWS.Lightsail.Types
import           Network.AWS.Lightsail.UnpeerVPC
import           Network.AWS.Lightsail.UpdateDomainEntry
import           Network.AWS.Lightsail.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Lightsail'.
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

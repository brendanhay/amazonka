{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assess, monitor, manage, and remediate security issues across your AWS infrastructure, applications, and data.
module Network.AWS.GuardDuty
    (
    -- * Service Configuration
      guardDuty

    -- * Errors
    -- $errors

    -- ** InternalServerErrorException
    , _InternalServerErrorException

    -- ** BadRequestException
    , _BadRequestException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateFilter
    , module Network.AWS.GuardDuty.CreateFilter

    -- ** ListFindings (Paginated)
    , module Network.AWS.GuardDuty.ListFindings

    -- ** CreateIPSet
    , module Network.AWS.GuardDuty.CreateIPSet

    -- ** DeleteThreatIntelSet
    , module Network.AWS.GuardDuty.DeleteThreatIntelSet

    -- ** UpdateThreatIntelSet
    , module Network.AWS.GuardDuty.UpdateThreatIntelSet

    -- ** StopMonitoringMembers
    , module Network.AWS.GuardDuty.StopMonitoringMembers

    -- ** ListThreatIntelSets (Paginated)
    , module Network.AWS.GuardDuty.ListThreatIntelSets

    -- ** CreateThreatIntelSet
    , module Network.AWS.GuardDuty.CreateThreatIntelSet

    -- ** DeleteMembers
    , module Network.AWS.GuardDuty.DeleteMembers

    -- ** GetFindingsStatistics
    , module Network.AWS.GuardDuty.GetFindingsStatistics

    -- ** GetIPSet
    , module Network.AWS.GuardDuty.GetIPSet

    -- ** ListInvitations (Paginated)
    , module Network.AWS.GuardDuty.ListInvitations

    -- ** GetThreatIntelSet
    , module Network.AWS.GuardDuty.GetThreatIntelSet

    -- ** DeleteInvitations
    , module Network.AWS.GuardDuty.DeleteInvitations

    -- ** GetMasterAccount
    , module Network.AWS.GuardDuty.GetMasterAccount

    -- ** CreateDetector
    , module Network.AWS.GuardDuty.CreateDetector

    -- ** DeclineInvitations
    , module Network.AWS.GuardDuty.DeclineInvitations

    -- ** UpdateFilter
    , module Network.AWS.GuardDuty.UpdateFilter

    -- ** DeleteFilter
    , module Network.AWS.GuardDuty.DeleteFilter

    -- ** DisassociateMembers
    , module Network.AWS.GuardDuty.DisassociateMembers

    -- ** DisassociateFromMasterAccount
    , module Network.AWS.GuardDuty.DisassociateFromMasterAccount

    -- ** AcceptInvitation
    , module Network.AWS.GuardDuty.AcceptInvitation

    -- ** ListFilters (Paginated)
    , module Network.AWS.GuardDuty.ListFilters

    -- ** ListMembers (Paginated)
    , module Network.AWS.GuardDuty.ListMembers

    -- ** GetDetector
    , module Network.AWS.GuardDuty.GetDetector

    -- ** CreateSampleFindings
    , module Network.AWS.GuardDuty.CreateSampleFindings

    -- ** ArchiveFindings
    , module Network.AWS.GuardDuty.ArchiveFindings

    -- ** CreateMembers
    , module Network.AWS.GuardDuty.CreateMembers

    -- ** UnarchiveFindings
    , module Network.AWS.GuardDuty.UnarchiveFindings

    -- ** GetInvitationsCount
    , module Network.AWS.GuardDuty.GetInvitationsCount

    -- ** StartMonitoringMembers
    , module Network.AWS.GuardDuty.StartMonitoringMembers

    -- ** InviteMembers
    , module Network.AWS.GuardDuty.InviteMembers

    -- ** DeleteIPSet
    , module Network.AWS.GuardDuty.DeleteIPSet

    -- ** UpdateIPSet
    , module Network.AWS.GuardDuty.UpdateIPSet

    -- ** ListIPSets (Paginated)
    , module Network.AWS.GuardDuty.ListIPSets

    -- ** GetMembers
    , module Network.AWS.GuardDuty.GetMembers

    -- ** GetFindings
    , module Network.AWS.GuardDuty.GetFindings

    -- ** ListDetectors (Paginated)
    , module Network.AWS.GuardDuty.ListDetectors

    -- ** UpdateDetector
    , module Network.AWS.GuardDuty.UpdateDetector

    -- ** DeleteDetector
    , module Network.AWS.GuardDuty.DeleteDetector

    -- ** UpdateFindingsFeedback
    , module Network.AWS.GuardDuty.UpdateFindingsFeedback

    -- ** GetFilter
    , module Network.AWS.GuardDuty.GetFilter

    -- * Types

    -- ** DetectorStatus
    , DetectorStatus (..)

    -- ** Feedback
    , Feedback (..)

    -- ** FilterAction
    , FilterAction (..)

    -- ** FindingStatisticType
    , FindingStatisticType (..)

    -- ** IPSetFormat
    , IPSetFormat (..)

    -- ** IPSetStatus
    , IPSetStatus (..)

    -- ** OrderBy
    , OrderBy (..)

    -- ** ThreatIntelSetFormat
    , ThreatIntelSetFormat (..)

    -- ** ThreatIntelSetStatus
    , ThreatIntelSetStatus (..)

    -- ** AWSAPICallAction
    , AWSAPICallAction
    , awsAPICallAction
    , aacaRemoteIPDetails
    , aacaCallerType
    , aacaDomainDetails
    , aacaServiceName
    , aacaAPI

    -- ** AccessKeyDetails
    , AccessKeyDetails
    , accessKeyDetails
    , akdPrincipalId
    , akdUserName
    , akdAccessKeyId
    , akdUserType

    -- ** AccountDetail
    , AccountDetail
    , accountDetail
    , adEmail
    , adAccountId

    -- ** Action
    , Action
    , action
    , aNetworkConnectionAction
    , aPortProbeAction
    , aActionType
    , aDNSRequestAction
    , aAWSAPICallAction

    -- ** City
    , City
    , city
    , cCityName

    -- ** Condition
    , Condition
    , condition
    , cEQ
    , cLte
    , cGT
    , cNeq
    , cLT
    , cGte

    -- ** Country
    , Country
    , country
    , cCountryName
    , cCountryCode

    -- ** DNSRequestAction
    , DNSRequestAction
    , dnsRequestAction
    , draDomain

    -- ** DomainDetails
    , DomainDetails
    , domainDetails

    -- ** Finding
    , Finding
    , finding
    , fService
    , fConfidence
    , fPartition
    , fTitle
    , fDescription
    , fAccountId
    , fSchemaVersion
    , fCreatedAt
    , fResource
    , fSeverity
    , fUpdatedAt
    , fType
    , fRegion
    , fId
    , fARN

    -- ** FindingCriteria
    , FindingCriteria
    , findingCriteria
    , fcCriterion

    -- ** FindingStatistics
    , FindingStatistics
    , findingStatistics
    , fsCountBySeverity

    -- ** GeoLocation
    , GeoLocation
    , geoLocation
    , glLat
    , glLon

    -- ** IAMInstanceProfile
    , IAMInstanceProfile
    , iamInstanceProfile
    , iapARN
    , iapId

    -- ** InstanceDetails
    , InstanceDetails
    , instanceDetails
    , idInstanceId
    , idPlatform
    , idLaunchTime
    , idNetworkInterfaces
    , idInstanceType
    , idAvailabilityZone
    , idIAMInstanceProfile
    , idImageId
    , idProductCodes
    , idInstanceState
    , idTags
    , idImageDescription

    -- ** Invitation
    , Invitation
    , invitation
    , iInvitedAt
    , iRelationshipStatus
    , iInvitationId
    , iAccountId

    -- ** LocalPortDetails
    , LocalPortDetails
    , localPortDetails
    , lpdPortName
    , lpdPort

    -- ** Master
    , Master
    , master
    , masInvitedAt
    , masRelationshipStatus
    , masInvitationId
    , masAccountId

    -- ** Member
    , Member
    , member
    , mInvitedAt
    , mDetectorId
    , mEmail
    , mAccountId
    , mMasterId
    , mUpdatedAt
    , mRelationshipStatus

    -- ** NetworkConnectionAction
    , NetworkConnectionAction
    , networkConnectionAction
    , ncaRemoteIPDetails
    , ncaProtocol
    , ncaRemotePortDetails
    , ncaBlocked
    , ncaConnectionDirection
    , ncaLocalPortDetails

    -- ** NetworkInterface
    , NetworkInterface
    , networkInterface
    , niPrivateIPAddresses
    , niPublicDNSName
    , niSecurityGroups
    , niVPCId
    , niNetworkInterfaceId
    , niSubnetId
    , niPrivateIPAddress
    , niPublicIP
    , niPrivateDNSName
    , niIPv6Addresses

    -- ** Organization
    , Organization
    , organization
    , oOrg
    , oASNOrg
    , oASN
    , oIsp

    -- ** PortProbeAction
    , PortProbeAction
    , portProbeAction
    , ppaPortProbeDetails
    , ppaBlocked

    -- ** PortProbeDetail
    , PortProbeDetail
    , portProbeDetail
    , ppdRemoteIPDetails
    , ppdLocalPortDetails

    -- ** PrivateIPAddressDetails
    , PrivateIPAddressDetails
    , privateIPAddressDetails
    , piadPrivateIPAddress
    , piadPrivateDNSName

    -- ** ProductCode
    , ProductCode
    , productCode
    , pcProductType
    , pcCode

    -- ** RemoteIPDetails
    , RemoteIPDetails
    , remoteIPDetails
    , ridCountry
    , ridCity
    , ridIPAddressV4
    , ridGeoLocation
    , ridOrganization

    -- ** RemotePortDetails
    , RemotePortDetails
    , remotePortDetails
    , rpdPortName
    , rpdPort

    -- ** Resource
    , Resource
    , resource
    , rResourceType
    , rInstanceDetails
    , rAccessKeyDetails

    -- ** SecurityGroup
    , SecurityGroup
    , securityGroup
    , sgGroupId
    , sgGroupName

    -- ** ServiceInfo
    , ServiceInfo
    , serviceInfo
    , siCount
    , siEventFirstSeen
    , siAction
    , siDetectorId
    , siServiceName
    , siUserFeedback
    , siEventLastSeen
    , siResourceRole
    , siArchived

    -- ** SortCriteria
    , SortCriteria
    , sortCriteria
    , scOrderBy
    , scAttributeName

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** UnprocessedAccount
    , UnprocessedAccount
    , unprocessedAccount
    , uaAccountId
    , uaResult
    ) where

import Network.AWS.GuardDuty.AcceptInvitation
import Network.AWS.GuardDuty.ArchiveFindings
import Network.AWS.GuardDuty.CreateDetector
import Network.AWS.GuardDuty.CreateFilter
import Network.AWS.GuardDuty.CreateIPSet
import Network.AWS.GuardDuty.CreateMembers
import Network.AWS.GuardDuty.CreateSampleFindings
import Network.AWS.GuardDuty.CreateThreatIntelSet
import Network.AWS.GuardDuty.DeclineInvitations
import Network.AWS.GuardDuty.DeleteDetector
import Network.AWS.GuardDuty.DeleteFilter
import Network.AWS.GuardDuty.DeleteInvitations
import Network.AWS.GuardDuty.DeleteIPSet
import Network.AWS.GuardDuty.DeleteMembers
import Network.AWS.GuardDuty.DeleteThreatIntelSet
import Network.AWS.GuardDuty.DisassociateFromMasterAccount
import Network.AWS.GuardDuty.DisassociateMembers
import Network.AWS.GuardDuty.GetDetector
import Network.AWS.GuardDuty.GetFilter
import Network.AWS.GuardDuty.GetFindings
import Network.AWS.GuardDuty.GetFindingsStatistics
import Network.AWS.GuardDuty.GetInvitationsCount
import Network.AWS.GuardDuty.GetIPSet
import Network.AWS.GuardDuty.GetMasterAccount
import Network.AWS.GuardDuty.GetMembers
import Network.AWS.GuardDuty.GetThreatIntelSet
import Network.AWS.GuardDuty.InviteMembers
import Network.AWS.GuardDuty.ListDetectors
import Network.AWS.GuardDuty.ListFilters
import Network.AWS.GuardDuty.ListFindings
import Network.AWS.GuardDuty.ListInvitations
import Network.AWS.GuardDuty.ListIPSets
import Network.AWS.GuardDuty.ListMembers
import Network.AWS.GuardDuty.ListThreatIntelSets
import Network.AWS.GuardDuty.StartMonitoringMembers
import Network.AWS.GuardDuty.StopMonitoringMembers
import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.UnarchiveFindings
import Network.AWS.GuardDuty.UpdateDetector
import Network.AWS.GuardDuty.UpdateFilter
import Network.AWS.GuardDuty.UpdateFindingsFeedback
import Network.AWS.GuardDuty.UpdateIPSet
import Network.AWS.GuardDuty.UpdateThreatIntelSet
import Network.AWS.GuardDuty.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'GuardDuty'.
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

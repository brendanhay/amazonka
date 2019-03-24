{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Alexa for Business helps you use Alexa in your organization. Alexa for Business provides you with the tools to manage Alexa devices, enroll your users, and assign skills, at scale. You can build your own context-aware voice skills using the Alexa Skills Kit and the Alexa for Business API operations. You can also make these available as private skills for your organization. Alexa for Business makes it efficient to voice-enable your products and services, thus providing context-aware voice experiences for your customers. Device makers building with the Alexa Voice Service (AVS) can create fully integrated solutions, register their products with Alexa for Business, and manage them as shared devices in their organization.
--
--
module Network.AWS.AlexaBusiness
    (
    -- * Service Configuration
      alexaBusiness

    -- * Errors
    -- $errors

    -- ** SkillNotLinkedException
    , _SkillNotLinkedException

    -- ** InvalidCertificateAuthorityException
    , _InvalidCertificateAuthorityException

    -- ** DeviceNotRegisteredException
    , _DeviceNotRegisteredException

    -- ** InvalidUserStatusException
    , _InvalidUserStatusException

    -- ** InvalidDeviceException
    , _InvalidDeviceException

    -- ** NotFoundException
    , _NotFoundException

    -- ** NameInUseException
    , _NameInUseException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** UnauthorizedException
    , _UnauthorizedException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SearchUsers (Paginated)
    , module Network.AWS.AlexaBusiness.SearchUsers

    -- ** PutConferencePreference
    , module Network.AWS.AlexaBusiness.PutConferencePreference

    -- ** UpdateBusinessReportSchedule
    , module Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule

    -- ** DeleteBusinessReportSchedule
    , module Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule

    -- ** AssociateSkillGroupWithRoom
    , module Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom

    -- ** ListSmartHomeAppliances (Paginated)
    , module Network.AWS.AlexaBusiness.ListSmartHomeAppliances

    -- ** DeleteProfile
    , module Network.AWS.AlexaBusiness.DeleteProfile

    -- ** UpdateProfile
    , module Network.AWS.AlexaBusiness.UpdateProfile

    -- ** SearchRooms (Paginated)
    , module Network.AWS.AlexaBusiness.SearchRooms

    -- ** AssociateSkillWithUsers
    , module Network.AWS.AlexaBusiness.AssociateSkillWithUsers

    -- ** RegisterAVSDevice
    , module Network.AWS.AlexaBusiness.RegisterAVSDevice

    -- ** ForgetSmartHomeAppliances
    , module Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances

    -- ** PutInvitationConfiguration
    , module Network.AWS.AlexaBusiness.PutInvitationConfiguration

    -- ** DisassociateContactFromAddressBook
    , module Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook

    -- ** GetConferencePreference
    , module Network.AWS.AlexaBusiness.GetConferencePreference

    -- ** DisassociateSkillFromSkillGroup
    , module Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup

    -- ** CreateAddressBook
    , module Network.AWS.AlexaBusiness.CreateAddressBook

    -- ** DeleteAddressBook
    , module Network.AWS.AlexaBusiness.DeleteAddressBook

    -- ** UpdateAddressBook
    , module Network.AWS.AlexaBusiness.UpdateAddressBook

    -- ** UpdateRoom
    , module Network.AWS.AlexaBusiness.UpdateRoom

    -- ** DeleteRoom
    , module Network.AWS.AlexaBusiness.DeleteRoom

    -- ** GetDevice
    , module Network.AWS.AlexaBusiness.GetDevice

    -- ** ListSkillsStoreSkillsByCategory (Paginated)
    , module Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory

    -- ** DeleteConferenceProvider
    , module Network.AWS.AlexaBusiness.DeleteConferenceProvider

    -- ** UpdateConferenceProvider
    , module Network.AWS.AlexaBusiness.UpdateConferenceProvider

    -- ** GetContact
    , module Network.AWS.AlexaBusiness.GetContact

    -- ** ApproveSkill
    , module Network.AWS.AlexaBusiness.ApproveSkill

    -- ** AssociateDeviceWithRoom
    , module Network.AWS.AlexaBusiness.AssociateDeviceWithRoom

    -- ** GetRoomSkillParameter
    , module Network.AWS.AlexaBusiness.GetRoomSkillParameter

    -- ** CreateBusinessReportSchedule
    , module Network.AWS.AlexaBusiness.CreateBusinessReportSchedule

    -- ** DeleteContact
    , module Network.AWS.AlexaBusiness.DeleteContact

    -- ** UpdateContact
    , module Network.AWS.AlexaBusiness.UpdateContact

    -- ** GetAddressBook
    , module Network.AWS.AlexaBusiness.GetAddressBook

    -- ** ListBusinessReportSchedules (Paginated)
    , module Network.AWS.AlexaBusiness.ListBusinessReportSchedules

    -- ** CreateContact
    , module Network.AWS.AlexaBusiness.CreateContact

    -- ** CreateProfile
    , module Network.AWS.AlexaBusiness.CreateProfile

    -- ** DeleteSkillGroup
    , module Network.AWS.AlexaBusiness.DeleteSkillGroup

    -- ** UpdateSkillGroup
    , module Network.AWS.AlexaBusiness.UpdateSkillGroup

    -- ** StartDeviceSync
    , module Network.AWS.AlexaBusiness.StartDeviceSync

    -- ** GetInvitationConfiguration
    , module Network.AWS.AlexaBusiness.GetInvitationConfiguration

    -- ** DisassociateSkillFromUsers
    , module Network.AWS.AlexaBusiness.DisassociateSkillFromUsers

    -- ** SearchAddressBooks
    , module Network.AWS.AlexaBusiness.SearchAddressBooks

    -- ** CreateSkillGroup
    , module Network.AWS.AlexaBusiness.CreateSkillGroup

    -- ** GetProfile
    , module Network.AWS.AlexaBusiness.GetProfile

    -- ** DisassociateSkillGroupFromRoom
    , module Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom

    -- ** SendInvitation
    , module Network.AWS.AlexaBusiness.SendInvitation

    -- ** ListDeviceEvents (Paginated)
    , module Network.AWS.AlexaBusiness.ListDeviceEvents

    -- ** CreateUser
    , module Network.AWS.AlexaBusiness.CreateUser

    -- ** SearchDevices (Paginated)
    , module Network.AWS.AlexaBusiness.SearchDevices

    -- ** SearchContacts
    , module Network.AWS.AlexaBusiness.SearchContacts

    -- ** DeleteUser
    , module Network.AWS.AlexaBusiness.DeleteUser

    -- ** GetSkillGroup
    , module Network.AWS.AlexaBusiness.GetSkillGroup

    -- ** ListSkills (Paginated)
    , module Network.AWS.AlexaBusiness.ListSkills

    -- ** TagResource
    , module Network.AWS.AlexaBusiness.TagResource

    -- ** DisassociateDeviceFromRoom
    , module Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom

    -- ** SearchSkillGroups (Paginated)
    , module Network.AWS.AlexaBusiness.SearchSkillGroups

    -- ** PutSkillAuthorization
    , module Network.AWS.AlexaBusiness.PutSkillAuthorization

    -- ** ListTags (Paginated)
    , module Network.AWS.AlexaBusiness.ListTags

    -- ** DeleteSkillAuthorization
    , module Network.AWS.AlexaBusiness.DeleteSkillAuthorization

    -- ** UntagResource
    , module Network.AWS.AlexaBusiness.UntagResource

    -- ** CreateConferenceProvider
    , module Network.AWS.AlexaBusiness.CreateConferenceProvider

    -- ** ResolveRoom
    , module Network.AWS.AlexaBusiness.ResolveRoom

    -- ** CreateRoom
    , module Network.AWS.AlexaBusiness.CreateRoom

    -- ** DeleteRoomSkillParameter
    , module Network.AWS.AlexaBusiness.DeleteRoomSkillParameter

    -- ** PutRoomSkillParameter
    , module Network.AWS.AlexaBusiness.PutRoomSkillParameter

    -- ** SearchProfiles (Paginated)
    , module Network.AWS.AlexaBusiness.SearchProfiles

    -- ** RejectSkill
    , module Network.AWS.AlexaBusiness.RejectSkill

    -- ** ListConferenceProviders (Paginated)
    , module Network.AWS.AlexaBusiness.ListConferenceProviders

    -- ** RevokeInvitation
    , module Network.AWS.AlexaBusiness.RevokeInvitation

    -- ** DeleteDevice
    , module Network.AWS.AlexaBusiness.DeleteDevice

    -- ** UpdateDevice
    , module Network.AWS.AlexaBusiness.UpdateDevice

    -- ** AssociateSkillWithSkillGroup
    , module Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup

    -- ** GetConferenceProvider
    , module Network.AWS.AlexaBusiness.GetConferenceProvider

    -- ** GetRoom
    , module Network.AWS.AlexaBusiness.GetRoom

    -- ** ListSkillsStoreCategories (Paginated)
    , module Network.AWS.AlexaBusiness.ListSkillsStoreCategories

    -- ** StartSmartHomeApplianceDiscovery
    , module Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery

    -- ** AssociateContactWithAddressBook
    , module Network.AWS.AlexaBusiness.AssociateContactWithAddressBook

    -- * Types

    -- ** BusinessReportFailureCode
    , BusinessReportFailureCode (..)

    -- ** BusinessReportFormat
    , BusinessReportFormat (..)

    -- ** BusinessReportInterval
    , BusinessReportInterval (..)

    -- ** BusinessReportStatus
    , BusinessReportStatus (..)

    -- ** CommsProtocol
    , CommsProtocol (..)

    -- ** ConferenceProviderType
    , ConferenceProviderType (..)

    -- ** ConnectionStatus
    , ConnectionStatus (..)

    -- ** DeviceEventType
    , DeviceEventType (..)

    -- ** DeviceStatus
    , DeviceStatus (..)

    -- ** DeviceStatusDetailCode
    , DeviceStatusDetailCode (..)

    -- ** DistanceUnit
    , DistanceUnit (..)

    -- ** EnablementType
    , EnablementType (..)

    -- ** EnablementTypeFilter
    , EnablementTypeFilter (..)

    -- ** EnrollmentStatus
    , EnrollmentStatus (..)

    -- ** Feature
    , Feature (..)

    -- ** RequirePin
    , RequirePin (..)

    -- ** SkillType
    , SkillType (..)

    -- ** SkillTypeFilter
    , SkillTypeFilter (..)

    -- ** SortValue
    , SortValue (..)

    -- ** TemperatureUnit
    , TemperatureUnit (..)

    -- ** WakeWord
    , WakeWord (..)

    -- ** AddressBook
    , AddressBook
    , addressBook
    , abAddressBookARN
    , abName
    , abDescription

    -- ** AddressBookData
    , AddressBookData
    , addressBookData
    , abdAddressBookARN
    , abdName
    , abdDescription

    -- ** BusinessReport
    , BusinessReport
    , businessReport
    , brStatus
    , brFailureCode
    , brDeliveryTime
    , brDownloadURL
    , brS3Location

    -- ** BusinessReportContentRange
    , BusinessReportContentRange
    , businessReportContentRange
    , brcrInterval

    -- ** BusinessReportRecurrence
    , BusinessReportRecurrence
    , businessReportRecurrence
    , brrStartDate

    -- ** BusinessReportS3Location
    , BusinessReportS3Location
    , businessReportS3Location
    , brslPath
    , brslBucketName

    -- ** BusinessReportSchedule
    , BusinessReportSchedule
    , businessReportSchedule
    , brsS3KeyPrefix
    , brsLastBusinessReport
    , brsFormat
    , brsRecurrence
    , brsScheduleName
    , brsScheduleARN
    , brsContentRange
    , brsS3BucketName

    -- ** Category
    , Category
    , category
    , cCategoryName
    , cCategoryId

    -- ** ConferencePreference
    , ConferencePreference
    , conferencePreference
    , cpDefaultConferenceProviderARN

    -- ** ConferenceProvider
    , ConferenceProvider
    , conferenceProvider
    , cpMeetingSetting
    , cpARN
    , cpPSTNDialIn
    , cpName
    , cpType
    , cpIPDialIn

    -- ** Contact
    , Contact
    , contact
    , cLastName
    , cContactARN
    , cPhoneNumber
    , cFirstName
    , cDisplayName

    -- ** ContactData
    , ContactData
    , contactData
    , cdLastName
    , cdContactARN
    , cdPhoneNumber
    , cdFirstName
    , cdDisplayName

    -- ** DeveloperInfo
    , DeveloperInfo
    , developerInfo
    , diEmail
    , diURL
    , diPrivacyPolicy
    , diDeveloperName

    -- ** Device
    , Device
    , device
    , dDeviceStatus
    , dDeviceStatusInfo
    , dDeviceARN
    , dMACAddress
    , dDeviceName
    , dRoomARN
    , dSoftwareVersion
    , dDeviceType
    , dDeviceSerialNumber

    -- ** DeviceData
    , DeviceData
    , deviceData
    , ddDeviceStatus
    , ddDeviceStatusInfo
    , ddDeviceARN
    , ddMACAddress
    , ddDeviceName
    , ddRoomARN
    , ddSoftwareVersion
    , ddDeviceType
    , ddRoomName
    , ddDeviceSerialNumber

    -- ** DeviceEvent
    , DeviceEvent
    , deviceEvent
    , deValue
    , deType
    , deTimestamp

    -- ** DeviceStatusDetail
    , DeviceStatusDetail
    , deviceStatusDetail
    , dsdCode

    -- ** DeviceStatusInfo
    , DeviceStatusInfo
    , deviceStatusInfo
    , dsiDeviceStatusDetails
    , dsiConnectionStatus

    -- ** Filter
    , Filter
    , filter'
    , fKey
    , fValues

    -- ** IPDialIn
    , IPDialIn
    , ipDialIn
    , idiEndpoint
    , idiCommsProtocol

    -- ** MeetingSetting
    , MeetingSetting
    , meetingSetting
    , msRequirePin

    -- ** PSTNDialIn
    , PSTNDialIn
    , pSTNDialIn
    , pstndiCountryCode
    , pstndiPhoneNumber
    , pstndiOneClickIdDelay
    , pstndiOneClickPinDelay

    -- ** Profile
    , Profile
    , profile
    , pSetupModeDisabled
    , pPSTNEnabled
    , pAddressBookARN
    , pDistanceUnit
    , pAddress
    , pProfileARN
    , pWakeWord
    , pProfileName
    , pTemperatureUnit
    , pTimezone
    , pMaxVolumeLimit
    , pIsDefault

    -- ** ProfileData
    , ProfileData
    , profileData
    , pdDistanceUnit
    , pdAddress
    , pdProfileARN
    , pdWakeWord
    , pdProfileName
    , pdTemperatureUnit
    , pdTimezone
    , pdIsDefault

    -- ** Room
    , Room
    , room
    , rProfileARN
    , rProviderCalendarId
    , rRoomARN
    , rRoomName
    , rDescription

    -- ** RoomData
    , RoomData
    , roomData
    , rdProfileARN
    , rdProviderCalendarId
    , rdProfileName
    , rdRoomARN
    , rdRoomName
    , rdDescription

    -- ** RoomSkillParameter
    , RoomSkillParameter
    , roomSkillParameter
    , rspParameterKey
    , rspParameterValue

    -- ** SkillDetails
    , SkillDetails
    , skillDetails
    , sdSkillTypes
    , sdProductDescription
    , sdInvocationPhrase
    , sdDeveloperInfo
    , sdEndUserLicenseAgreement
    , sdGenericKeywords
    , sdReviews
    , sdReleaseDate
    , sdNewInThisVersionBulletPoints
    , sdBulletPoints

    -- ** SkillGroup
    , SkillGroup
    , skillGroup
    , sgSkillGroupARN
    , sgDescription
    , sgSkillGroupName

    -- ** SkillGroupData
    , SkillGroupData
    , skillGroupData
    , sgdSkillGroupARN
    , sgdDescription
    , sgdSkillGroupName

    -- ** SkillSummary
    , SkillSummary
    , skillSummary
    , ssSkillId
    , ssSupportsLinking
    , ssSkillType
    , ssEnablementType
    , ssSkillName

    -- ** SkillsStoreSkill
    , SkillsStoreSkill
    , skillsStoreSkill
    , sssSkillId
    , sssSupportsLinking
    , sssSampleUtterances
    , sssShortDescription
    , sssIconURL
    , sssSkillDetails
    , sssSkillName

    -- ** SmartHomeAppliance
    , SmartHomeAppliance
    , smartHomeAppliance
    , shaFriendlyName
    , shaManufacturerName
    , shaDescription

    -- ** Sort
    , Sort
    , sort
    , sKey
    , sValue

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** UserData
    , UserData
    , userData
    , udEmail
    , udLastName
    , udEnrollmentId
    , udUserARN
    , udFirstName
    , udEnrollmentStatus
    ) where

import Network.AWS.AlexaBusiness.ApproveSkill
import Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
import Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
import Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
import Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
import Network.AWS.AlexaBusiness.AssociateSkillWithUsers
import Network.AWS.AlexaBusiness.CreateAddressBook
import Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
import Network.AWS.AlexaBusiness.CreateConferenceProvider
import Network.AWS.AlexaBusiness.CreateContact
import Network.AWS.AlexaBusiness.CreateProfile
import Network.AWS.AlexaBusiness.CreateRoom
import Network.AWS.AlexaBusiness.CreateSkillGroup
import Network.AWS.AlexaBusiness.CreateUser
import Network.AWS.AlexaBusiness.DeleteAddressBook
import Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule
import Network.AWS.AlexaBusiness.DeleteConferenceProvider
import Network.AWS.AlexaBusiness.DeleteContact
import Network.AWS.AlexaBusiness.DeleteDevice
import Network.AWS.AlexaBusiness.DeleteProfile
import Network.AWS.AlexaBusiness.DeleteRoom
import Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
import Network.AWS.AlexaBusiness.DeleteSkillAuthorization
import Network.AWS.AlexaBusiness.DeleteSkillGroup
import Network.AWS.AlexaBusiness.DeleteUser
import Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
import Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
import Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
import Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
import Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
import Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
import Network.AWS.AlexaBusiness.GetAddressBook
import Network.AWS.AlexaBusiness.GetConferencePreference
import Network.AWS.AlexaBusiness.GetConferenceProvider
import Network.AWS.AlexaBusiness.GetContact
import Network.AWS.AlexaBusiness.GetDevice
import Network.AWS.AlexaBusiness.GetInvitationConfiguration
import Network.AWS.AlexaBusiness.GetProfile
import Network.AWS.AlexaBusiness.GetRoom
import Network.AWS.AlexaBusiness.GetRoomSkillParameter
import Network.AWS.AlexaBusiness.GetSkillGroup
import Network.AWS.AlexaBusiness.ListBusinessReportSchedules
import Network.AWS.AlexaBusiness.ListConferenceProviders
import Network.AWS.AlexaBusiness.ListDeviceEvents
import Network.AWS.AlexaBusiness.ListSkills
import Network.AWS.AlexaBusiness.ListSkillsStoreCategories
import Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
import Network.AWS.AlexaBusiness.ListSmartHomeAppliances
import Network.AWS.AlexaBusiness.ListTags
import Network.AWS.AlexaBusiness.PutConferencePreference
import Network.AWS.AlexaBusiness.PutInvitationConfiguration
import Network.AWS.AlexaBusiness.PutRoomSkillParameter
import Network.AWS.AlexaBusiness.PutSkillAuthorization
import Network.AWS.AlexaBusiness.RegisterAVSDevice
import Network.AWS.AlexaBusiness.RejectSkill
import Network.AWS.AlexaBusiness.ResolveRoom
import Network.AWS.AlexaBusiness.RevokeInvitation
import Network.AWS.AlexaBusiness.SearchAddressBooks
import Network.AWS.AlexaBusiness.SearchContacts
import Network.AWS.AlexaBusiness.SearchDevices
import Network.AWS.AlexaBusiness.SearchProfiles
import Network.AWS.AlexaBusiness.SearchRooms
import Network.AWS.AlexaBusiness.SearchSkillGroups
import Network.AWS.AlexaBusiness.SearchUsers
import Network.AWS.AlexaBusiness.SendInvitation
import Network.AWS.AlexaBusiness.StartDeviceSync
import Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
import Network.AWS.AlexaBusiness.TagResource
import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.UntagResource
import Network.AWS.AlexaBusiness.UpdateAddressBook
import Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
import Network.AWS.AlexaBusiness.UpdateConferenceProvider
import Network.AWS.AlexaBusiness.UpdateContact
import Network.AWS.AlexaBusiness.UpdateDevice
import Network.AWS.AlexaBusiness.UpdateProfile
import Network.AWS.AlexaBusiness.UpdateRoom
import Network.AWS.AlexaBusiness.UpdateSkillGroup
import Network.AWS.AlexaBusiness.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'AlexaBusiness'.
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

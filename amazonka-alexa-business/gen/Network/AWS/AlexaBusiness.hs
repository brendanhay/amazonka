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
-- Alexa for Business makes it easy for you to use Alexa in your organization. Alexa for Business gives you the tools you need for managing Alexa devices, enroll your users, and assign skills, at scale. You can build your own context-aware voice skills using the Alexa Skills Kit and the Alexa for Business API operations. You can make also these available as private skills for your organization. Alexa for Business makes it easy to voice-enable your products and services, providing context-aware voice experiences for your customers.
--
--
module Network.AWS.AlexaBusiness
    (
    -- * Service Configuration
      alexaBusiness

    -- * Errors
    -- $errors

    -- ** InvalidUserStatusException
    , _InvalidUserStatusException

    -- ** NotFoundException
    , _NotFoundException

    -- ** NameInUseException
    , _NameInUseException

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

    -- ** AssociateSkillGroupWithRoom
    , module Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom

    -- ** DeleteProfile
    , module Network.AWS.AlexaBusiness.DeleteProfile

    -- ** UpdateProfile
    , module Network.AWS.AlexaBusiness.UpdateProfile

    -- ** SearchRooms (Paginated)
    , module Network.AWS.AlexaBusiness.SearchRooms

    -- ** DisassociateContactFromAddressBook
    , module Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook

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

    -- ** GetContact
    , module Network.AWS.AlexaBusiness.GetContact

    -- ** AssociateDeviceWithRoom
    , module Network.AWS.AlexaBusiness.AssociateDeviceWithRoom

    -- ** GetRoomSkillParameter
    , module Network.AWS.AlexaBusiness.GetRoomSkillParameter

    -- ** DeleteContact
    , module Network.AWS.AlexaBusiness.DeleteContact

    -- ** UpdateContact
    , module Network.AWS.AlexaBusiness.UpdateContact

    -- ** GetAddressBook
    , module Network.AWS.AlexaBusiness.GetAddressBook

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

    -- ** ListDeviceEvents
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

    -- ** ListTags (Paginated)
    , module Network.AWS.AlexaBusiness.ListTags

    -- ** UntagResource
    , module Network.AWS.AlexaBusiness.UntagResource

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

    -- ** RevokeInvitation
    , module Network.AWS.AlexaBusiness.RevokeInvitation

    -- ** UpdateDevice
    , module Network.AWS.AlexaBusiness.UpdateDevice

    -- ** GetRoom
    , module Network.AWS.AlexaBusiness.GetRoom

    -- ** AssociateContactWithAddressBook
    , module Network.AWS.AlexaBusiness.AssociateContactWithAddressBook

    -- * Types

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

    -- ** EnrollmentStatus
    , EnrollmentStatus (..)

    -- ** Feature
    , Feature (..)

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

    -- ** Profile
    , Profile
    , profile
    , pSetupModeDisabled
    , pPSTNEnabled
    , pDistanceUnit
    , pAddress
    , pProfileARN
    , pWakeWord
    , pProfileName
    , pTemperatureUnit
    , pTimezone
    , pMaxVolumeLimit

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
    , ssSkillName

    -- ** Sort
    , Sort
    , sort
    , sKey
    , sValue

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

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

import Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
import Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
import Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
import Network.AWS.AlexaBusiness.CreateAddressBook
import Network.AWS.AlexaBusiness.CreateContact
import Network.AWS.AlexaBusiness.CreateProfile
import Network.AWS.AlexaBusiness.CreateRoom
import Network.AWS.AlexaBusiness.CreateSkillGroup
import Network.AWS.AlexaBusiness.CreateUser
import Network.AWS.AlexaBusiness.DeleteAddressBook
import Network.AWS.AlexaBusiness.DeleteContact
import Network.AWS.AlexaBusiness.DeleteProfile
import Network.AWS.AlexaBusiness.DeleteRoom
import Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
import Network.AWS.AlexaBusiness.DeleteSkillGroup
import Network.AWS.AlexaBusiness.DeleteUser
import Network.AWS.AlexaBusiness.DisassociateContactFromAddressBook
import Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
import Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
import Network.AWS.AlexaBusiness.GetAddressBook
import Network.AWS.AlexaBusiness.GetContact
import Network.AWS.AlexaBusiness.GetDevice
import Network.AWS.AlexaBusiness.GetProfile
import Network.AWS.AlexaBusiness.GetRoom
import Network.AWS.AlexaBusiness.GetRoomSkillParameter
import Network.AWS.AlexaBusiness.GetSkillGroup
import Network.AWS.AlexaBusiness.ListDeviceEvents
import Network.AWS.AlexaBusiness.ListSkills
import Network.AWS.AlexaBusiness.ListTags
import Network.AWS.AlexaBusiness.PutRoomSkillParameter
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
import Network.AWS.AlexaBusiness.TagResource
import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.UntagResource
import Network.AWS.AlexaBusiness.UpdateAddressBook
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

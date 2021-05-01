{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Lens
  ( -- * Operations

    -- ** StartDeviceSync
    startDeviceSync_deviceArn,
    startDeviceSync_roomArn,
    startDeviceSync_features,
    startDeviceSyncResponse_httpStatus,

    -- ** CreateProfile
    createProfile_locale,
    createProfile_setupModeDisabled,
    createProfile_pSTNEnabled,
    createProfile_tags,
    createProfile_maxVolumeLimit,
    createProfile_meetingRoomConfiguration,
    createProfile_clientRequestToken,
    createProfile_profileName,
    createProfile_timezone,
    createProfile_address,
    createProfile_distanceUnit,
    createProfile_temperatureUnit,
    createProfile_wakeWord,
    createProfileResponse_profileArn,
    createProfileResponse_httpStatus,

    -- ** CreateContact
    createContact_phoneNumber,
    createContact_phoneNumbers,
    createContact_tags,
    createContact_clientRequestToken,
    createContact_displayName,
    createContact_lastName,
    createContact_sipAddresses,
    createContact_firstName,
    createContactResponse_contactArn,
    createContactResponse_httpStatus,

    -- ** DeleteBusinessReportSchedule
    deleteBusinessReportSchedule_scheduleArn,
    deleteBusinessReportScheduleResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_certificateAuthorityArn,
    updateNetworkProfile_trustAnchors,
    updateNetworkProfile_currentPassword,
    updateNetworkProfile_networkProfileName,
    updateNetworkProfile_description,
    updateNetworkProfile_nextPassword,
    updateNetworkProfile_networkProfileArn,
    updateNetworkProfileResponse_httpStatus,

    -- ** ListBusinessReportSchedules
    listBusinessReportSchedules_nextToken,
    listBusinessReportSchedules_maxResults,
    listBusinessReportSchedulesResponse_nextToken,
    listBusinessReportSchedulesResponse_businessReportSchedules,
    listBusinessReportSchedulesResponse_httpStatus,

    -- ** DeleteNetworkProfile
    deleteNetworkProfile_networkProfileArn,
    deleteNetworkProfileResponse_httpStatus,

    -- ** UpdateBusinessReportSchedule
    updateBusinessReportSchedule_format,
    updateBusinessReportSchedule_s3KeyPrefix,
    updateBusinessReportSchedule_recurrence,
    updateBusinessReportSchedule_s3BucketName,
    updateBusinessReportSchedule_scheduleName,
    updateBusinessReportSchedule_scheduleArn,
    updateBusinessReportScheduleResponse_httpStatus,

    -- ** DeleteDeviceUsageData
    deleteDeviceUsageData_deviceArn,
    deleteDeviceUsageData_deviceUsageType,
    deleteDeviceUsageDataResponse_httpStatus,

    -- ** GetConferenceProvider
    getConferenceProvider_conferenceProviderArn,
    getConferenceProviderResponse_conferenceProvider,
    getConferenceProviderResponse_httpStatus,

    -- ** GetGatewayGroup
    getGatewayGroup_gatewayGroupArn,
    getGatewayGroupResponse_gatewayGroup,
    getGatewayGroupResponse_httpStatus,

    -- ** GetRoom
    getRoom_roomArn,
    getRoomResponse_room,
    getRoomResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_certificateAuthorityArn,
    createNetworkProfile_trustAnchors,
    createNetworkProfile_currentPassword,
    createNetworkProfile_eapMethod,
    createNetworkProfile_tags,
    createNetworkProfile_description,
    createNetworkProfile_nextPassword,
    createNetworkProfile_networkProfileName,
    createNetworkProfile_ssid,
    createNetworkProfile_securityType,
    createNetworkProfile_clientRequestToken,
    createNetworkProfileResponse_networkProfileArn,
    createNetworkProfileResponse_httpStatus,

    -- ** ListSkillsStoreCategories
    listSkillsStoreCategories_nextToken,
    listSkillsStoreCategories_maxResults,
    listSkillsStoreCategoriesResponse_nextToken,
    listSkillsStoreCategoriesResponse_categoryList,
    listSkillsStoreCategoriesResponse_httpStatus,

    -- ** CreateBusinessReportSchedule
    createBusinessReportSchedule_s3KeyPrefix,
    createBusinessReportSchedule_recurrence,
    createBusinessReportSchedule_tags,
    createBusinessReportSchedule_s3BucketName,
    createBusinessReportSchedule_clientRequestToken,
    createBusinessReportSchedule_scheduleName,
    createBusinessReportSchedule_format,
    createBusinessReportSchedule_contentRange,
    createBusinessReportScheduleResponse_scheduleArn,
    createBusinessReportScheduleResponse_httpStatus,

    -- ** GetAddressBook
    getAddressBook_addressBookArn,
    getAddressBookResponse_addressBook,
    getAddressBookResponse_httpStatus,

    -- ** AssociateContactWithAddressBook
    associateContactWithAddressBook_contactArn,
    associateContactWithAddressBook_addressBookArn,
    associateContactWithAddressBookResponse_httpStatus,

    -- ** GetDevice
    getDevice_deviceArn,
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,

    -- ** DeleteRoomSkillParameter
    deleteRoomSkillParameter_roomArn,
    deleteRoomSkillParameter_skillId,
    deleteRoomSkillParameter_parameterKey,
    deleteRoomSkillParameterResponse_httpStatus,

    -- ** ListSkillsStoreSkillsByCategory
    listSkillsStoreSkillsByCategory_nextToken,
    listSkillsStoreSkillsByCategory_maxResults,
    listSkillsStoreSkillsByCategory_categoryId,
    listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills,
    listSkillsStoreSkillsByCategoryResponse_nextToken,
    listSkillsStoreSkillsByCategoryResponse_httpStatus,

    -- ** SearchProfiles
    searchProfiles_nextToken,
    searchProfiles_sortCriteria,
    searchProfiles_maxResults,
    searchProfiles_filters,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_profiles,
    searchProfilesResponse_totalCount,
    searchProfilesResponse_httpStatus,

    -- ** DeleteAddressBook
    deleteAddressBook_addressBookArn,
    deleteAddressBookResponse_httpStatus,

    -- ** UpdateAddressBook
    updateAddressBook_name,
    updateAddressBook_description,
    updateAddressBook_addressBookArn,
    updateAddressBookResponse_httpStatus,

    -- ** SearchSkillGroups
    searchSkillGroups_nextToken,
    searchSkillGroups_sortCriteria,
    searchSkillGroups_maxResults,
    searchSkillGroups_filters,
    searchSkillGroupsResponse_nextToken,
    searchSkillGroupsResponse_totalCount,
    searchSkillGroupsResponse_skillGroups,
    searchSkillGroupsResponse_httpStatus,

    -- ** ResolveRoom
    resolveRoom_userId,
    resolveRoom_skillId,
    resolveRoomResponse_roomSkillParameters,
    resolveRoomResponse_roomArn,
    resolveRoomResponse_roomName,
    resolveRoomResponse_httpStatus,

    -- ** PutSkillAuthorization
    putSkillAuthorization_roomArn,
    putSkillAuthorization_authorizationResult,
    putSkillAuthorization_skillId,
    putSkillAuthorizationResponse_httpStatus,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DisassociateContactFromAddressBook
    disassociateContactFromAddressBook_contactArn,
    disassociateContactFromAddressBook_addressBookArn,
    disassociateContactFromAddressBookResponse_httpStatus,

    -- ** AssociateDeviceWithNetworkProfile
    associateDeviceWithNetworkProfile_deviceArn,
    associateDeviceWithNetworkProfile_networkProfileArn,
    associateDeviceWithNetworkProfileResponse_httpStatus,

    -- ** SearchNetworkProfiles
    searchNetworkProfiles_nextToken,
    searchNetworkProfiles_sortCriteria,
    searchNetworkProfiles_maxResults,
    searchNetworkProfiles_filters,
    searchNetworkProfilesResponse_nextToken,
    searchNetworkProfilesResponse_networkProfiles,
    searchNetworkProfilesResponse_totalCount,
    searchNetworkProfilesResponse_httpStatus,

    -- ** GetSkillGroup
    getSkillGroup_skillGroupArn,
    getSkillGroupResponse_skillGroup,
    getSkillGroupResponse_httpStatus,

    -- ** PutInvitationConfiguration
    putInvitationConfiguration_contactEmail,
    putInvitationConfiguration_privateSkillIds,
    putInvitationConfiguration_organizationName,
    putInvitationConfigurationResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListDeviceEvents
    listDeviceEvents_nextToken,
    listDeviceEvents_eventType,
    listDeviceEvents_maxResults,
    listDeviceEvents_deviceArn,
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_deviceEvents,
    listDeviceEventsResponse_httpStatus,

    -- ** SendAnnouncement
    sendAnnouncement_timeToLiveInSeconds,
    sendAnnouncement_roomFilters,
    sendAnnouncement_content,
    sendAnnouncement_clientRequestToken,
    sendAnnouncementResponse_announcementArn,
    sendAnnouncementResponse_httpStatus,

    -- ** DisassociateSkillGroupFromRoom
    disassociateSkillGroupFromRoom_roomArn,
    disassociateSkillGroupFromRoom_skillGroupArn,
    disassociateSkillGroupFromRoomResponse_httpStatus,

    -- ** GetProfile
    getProfile_profileArn,
    getProfileResponse_profile,
    getProfileResponse_httpStatus,

    -- ** CreateUser
    createUser_email,
    createUser_tags,
    createUser_clientRequestToken,
    createUser_firstName,
    createUser_lastName,
    createUser_userId,
    createUserResponse_userArn,
    createUserResponse_httpStatus,

    -- ** SearchContacts
    searchContacts_nextToken,
    searchContacts_sortCriteria,
    searchContacts_maxResults,
    searchContacts_filters,
    searchContactsResponse_nextToken,
    searchContactsResponse_totalCount,
    searchContactsResponse_contacts,
    searchContactsResponse_httpStatus,

    -- ** RegisterAVSDevice
    registerAVSDevice_roomArn,
    registerAVSDevice_tags,
    registerAVSDevice_deviceSerialNumber,
    registerAVSDevice_clientId,
    registerAVSDevice_userCode,
    registerAVSDevice_productId,
    registerAVSDevice_amazonId,
    registerAVSDeviceResponse_deviceArn,
    registerAVSDeviceResponse_httpStatus,

    -- ** SendInvitation
    sendInvitation_userArn,
    sendInvitationResponse_httpStatus,

    -- ** ForgetSmartHomeAppliances
    forgetSmartHomeAppliances_roomArn,
    forgetSmartHomeAppliancesResponse_httpStatus,

    -- ** AssociateSkillWithUsers
    associateSkillWithUsers_skillId,
    associateSkillWithUsersResponse_httpStatus,

    -- ** GetInvitationConfiguration
    getInvitationConfigurationResponse_organizationName,
    getInvitationConfigurationResponse_contactEmail,
    getInvitationConfigurationResponse_privateSkillIds,
    getInvitationConfigurationResponse_httpStatus,

    -- ** DisassociateSkillFromUsers
    disassociateSkillFromUsers_skillId,
    disassociateSkillFromUsersResponse_httpStatus,

    -- ** DeleteSkillGroup
    deleteSkillGroup_skillGroupArn,
    deleteSkillGroupResponse_httpStatus,

    -- ** UpdateSkillGroup
    updateSkillGroup_skillGroupName,
    updateSkillGroup_description,
    updateSkillGroup_skillGroupArn,
    updateSkillGroupResponse_httpStatus,

    -- ** AssociateSkillGroupWithRoom
    associateSkillGroupWithRoom_roomArn,
    associateSkillGroupWithRoom_skillGroupArn,
    associateSkillGroupWithRoomResponse_httpStatus,

    -- ** SearchUsers
    searchUsers_nextToken,
    searchUsers_sortCriteria,
    searchUsers_maxResults,
    searchUsers_filters,
    searchUsersResponse_nextToken,
    searchUsersResponse_totalCount,
    searchUsersResponse_users,
    searchUsersResponse_httpStatus,

    -- ** PutConferencePreference
    putConferencePreference_conferencePreference,
    putConferencePreferenceResponse_httpStatus,

    -- ** UpdateGateway
    updateGateway_name,
    updateGateway_description,
    updateGateway_softwareVersion,
    updateGateway_gatewayArn,
    updateGatewayResponse_httpStatus,

    -- ** DeleteDevice
    deleteDevice_deviceArn,
    deleteDeviceResponse_httpStatus,

    -- ** RevokeInvitation
    revokeInvitation_userArn,
    revokeInvitation_enrollmentId,
    revokeInvitationResponse_httpStatus,

    -- ** GetRoomSkillParameter
    getRoomSkillParameter_roomArn,
    getRoomSkillParameter_skillId,
    getRoomSkillParameter_parameterKey,
    getRoomSkillParameterResponse_roomSkillParameter,
    getRoomSkillParameterResponse_httpStatus,

    -- ** UpdateContact
    updateContact_phoneNumber,
    updateContact_phoneNumbers,
    updateContact_displayName,
    updateContact_firstName,
    updateContact_lastName,
    updateContact_sipAddresses,
    updateContact_contactArn,
    updateContactResponse_httpStatus,

    -- ** StartSmartHomeApplianceDiscovery
    startSmartHomeApplianceDiscovery_roomArn,
    startSmartHomeApplianceDiscoveryResponse_httpStatus,

    -- ** DeleteContact
    deleteContact_contactArn,
    deleteContactResponse_httpStatus,

    -- ** UpdateDevice
    updateDevice_deviceArn,
    updateDevice_deviceName,
    updateDeviceResponse_httpStatus,

    -- ** AssociateDeviceWithRoom
    associateDeviceWithRoom_deviceArn,
    associateDeviceWithRoom_roomArn,
    associateDeviceWithRoomResponse_httpStatus,

    -- ** AssociateSkillWithSkillGroup
    associateSkillWithSkillGroup_skillGroupArn,
    associateSkillWithSkillGroup_skillId,
    associateSkillWithSkillGroupResponse_httpStatus,

    -- ** ListGateways
    listGateways_nextToken,
    listGateways_maxResults,
    listGateways_gatewayGroupArn,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,

    -- ** DeleteRoom
    deleteRoom_roomArn,
    deleteRoomResponse_httpStatus,

    -- ** ListConferenceProviders
    listConferenceProviders_nextToken,
    listConferenceProviders_maxResults,
    listConferenceProvidersResponse_nextToken,
    listConferenceProvidersResponse_conferenceProviders,
    listConferenceProvidersResponse_httpStatus,

    -- ** DeleteGatewayGroup
    deleteGatewayGroup_gatewayGroupArn,
    deleteGatewayGroupResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_roomArn,
    updateRoom_providerCalendarId,
    updateRoom_profileArn,
    updateRoom_description,
    updateRoom_roomName,
    updateRoomResponse_httpStatus,

    -- ** DeleteConferenceProvider
    deleteConferenceProvider_conferenceProviderArn,
    deleteConferenceProviderResponse_httpStatus,

    -- ** GetGateway
    getGateway_gatewayArn,
    getGatewayResponse_gateway,
    getGatewayResponse_httpStatus,

    -- ** UpdateConferenceProvider
    updateConferenceProvider_iPDialIn,
    updateConferenceProvider_pSTNDialIn,
    updateConferenceProvider_conferenceProviderArn,
    updateConferenceProvider_conferenceProviderType,
    updateConferenceProvider_meetingSetting,
    updateConferenceProviderResponse_httpStatus,

    -- ** UpdateGatewayGroup
    updateGatewayGroup_name,
    updateGatewayGroup_description,
    updateGatewayGroup_gatewayGroupArn,
    updateGatewayGroupResponse_httpStatus,

    -- ** ListGatewayGroups
    listGatewayGroups_nextToken,
    listGatewayGroups_maxResults,
    listGatewayGroupsResponse_nextToken,
    listGatewayGroupsResponse_gatewayGroups,
    listGatewayGroupsResponse_httpStatus,

    -- ** ApproveSkill
    approveSkill_skillId,
    approveSkillResponse_httpStatus,

    -- ** GetContact
    getContact_contactArn,
    getContactResponse_contact,
    getContactResponse_httpStatus,

    -- ** RejectSkill
    rejectSkill_skillId,
    rejectSkillResponse_httpStatus,

    -- ** PutRoomSkillParameter
    putRoomSkillParameter_roomArn,
    putRoomSkillParameter_skillId,
    putRoomSkillParameter_roomSkillParameter,
    putRoomSkillParameterResponse_httpStatus,

    -- ** DisassociateDeviceFromRoom
    disassociateDeviceFromRoom_deviceArn,
    disassociateDeviceFromRoomResponse_httpStatus,

    -- ** CreateAddressBook
    createAddressBook_tags,
    createAddressBook_description,
    createAddressBook_clientRequestToken,
    createAddressBook_name,
    createAddressBookResponse_addressBookArn,
    createAddressBookResponse_httpStatus,

    -- ** CreateRoom
    createRoom_tags,
    createRoom_providerCalendarId,
    createRoom_profileArn,
    createRoom_description,
    createRoom_clientRequestToken,
    createRoom_roomName,
    createRoomResponse_roomArn,
    createRoomResponse_httpStatus,

    -- ** CreateConferenceProvider
    createConferenceProvider_iPDialIn,
    createConferenceProvider_tags,
    createConferenceProvider_pSTNDialIn,
    createConferenceProvider_clientRequestToken,
    createConferenceProvider_conferenceProviderName,
    createConferenceProvider_conferenceProviderType,
    createConferenceProvider_meetingSetting,
    createConferenceProviderResponse_conferenceProviderArn,
    createConferenceProviderResponse_httpStatus,

    -- ** GetNetworkProfile
    getNetworkProfile_networkProfileArn,
    getNetworkProfileResponse_networkProfile,
    getNetworkProfileResponse_httpStatus,

    -- ** GetConferencePreference
    getConferencePreferenceResponse_preference,
    getConferencePreferenceResponse_httpStatus,

    -- ** DeleteSkillAuthorization
    deleteSkillAuthorization_roomArn,
    deleteSkillAuthorization_skillId,
    deleteSkillAuthorizationResponse_httpStatus,

    -- ** CreateGatewayGroup
    createGatewayGroup_tags,
    createGatewayGroup_description,
    createGatewayGroup_name,
    createGatewayGroup_clientRequestToken,
    createGatewayGroupResponse_gatewayGroupArn,
    createGatewayGroupResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_arn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DisassociateSkillFromSkillGroup
    disassociateSkillFromSkillGroup_skillGroupArn,
    disassociateSkillFromSkillGroup_skillId,
    disassociateSkillFromSkillGroupResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userArn,
    deleteUser_enrollmentId,
    deleteUserResponse_httpStatus,

    -- ** ListSkills
    listSkills_nextToken,
    listSkills_maxResults,
    listSkills_skillType,
    listSkills_skillGroupArn,
    listSkills_enablementType,
    listSkillsResponse_nextToken,
    listSkillsResponse_skillSummaries,
    listSkillsResponse_httpStatus,

    -- ** SearchDevices
    searchDevices_nextToken,
    searchDevices_sortCriteria,
    searchDevices_maxResults,
    searchDevices_filters,
    searchDevicesResponse_nextToken,
    searchDevicesResponse_devices,
    searchDevicesResponse_totalCount,
    searchDevicesResponse_httpStatus,

    -- ** SearchRooms
    searchRooms_nextToken,
    searchRooms_sortCriteria,
    searchRooms_maxResults,
    searchRooms_filters,
    searchRoomsResponse_nextToken,
    searchRoomsResponse_rooms,
    searchRoomsResponse_totalCount,
    searchRoomsResponse_httpStatus,

    -- ** SearchAddressBooks
    searchAddressBooks_nextToken,
    searchAddressBooks_sortCriteria,
    searchAddressBooks_maxResults,
    searchAddressBooks_filters,
    searchAddressBooksResponse_nextToken,
    searchAddressBooksResponse_addressBooks,
    searchAddressBooksResponse_totalCount,
    searchAddressBooksResponse_httpStatus,

    -- ** ListSmartHomeAppliances
    listSmartHomeAppliances_nextToken,
    listSmartHomeAppliances_maxResults,
    listSmartHomeAppliances_roomArn,
    listSmartHomeAppliancesResponse_nextToken,
    listSmartHomeAppliancesResponse_smartHomeAppliances,
    listSmartHomeAppliancesResponse_httpStatus,

    -- ** DeleteProfile
    deleteProfile_profileArn,
    deleteProfileResponse_httpStatus,

    -- ** CreateSkillGroup
    createSkillGroup_tags,
    createSkillGroup_description,
    createSkillGroup_clientRequestToken,
    createSkillGroup_skillGroupName,
    createSkillGroupResponse_skillGroupArn,
    createSkillGroupResponse_httpStatus,

    -- ** UpdateProfile
    updateProfile_profileName,
    updateProfile_isDefault,
    updateProfile_address,
    updateProfile_locale,
    updateProfile_temperatureUnit,
    updateProfile_setupModeDisabled,
    updateProfile_pSTNEnabled,
    updateProfile_maxVolumeLimit,
    updateProfile_meetingRoomConfiguration,
    updateProfile_wakeWord,
    updateProfile_profileArn,
    updateProfile_timezone,
    updateProfile_distanceUnit,
    updateProfileResponse_httpStatus,

    -- * Types

    -- ** AddressBook
    addressBook_addressBookArn,
    addressBook_name,
    addressBook_description,

    -- ** AddressBookData
    addressBookData_addressBookArn,
    addressBookData_name,
    addressBookData_description,

    -- ** Audio
    audio_locale,
    audio_location,

    -- ** BusinessReport
    businessReport_downloadUrl,
    businessReport_status,
    businessReport_deliveryTime,
    businessReport_failureCode,
    businessReport_s3Location,

    -- ** BusinessReportContentRange
    businessReportContentRange_interval,

    -- ** BusinessReportRecurrence
    businessReportRecurrence_startDate,

    -- ** BusinessReportS3Location
    businessReportS3Location_bucketName,
    businessReportS3Location_path,

    -- ** BusinessReportSchedule
    businessReportSchedule_contentRange,
    businessReportSchedule_format,
    businessReportSchedule_scheduleArn,
    businessReportSchedule_lastBusinessReport,
    businessReportSchedule_s3KeyPrefix,
    businessReportSchedule_recurrence,
    businessReportSchedule_s3BucketName,
    businessReportSchedule_scheduleName,

    -- ** Category
    category_categoryId,
    category_categoryName,

    -- ** ConferencePreference
    conferencePreference_defaultConferenceProviderArn,

    -- ** ConferenceProvider
    conferenceProvider_meetingSetting,
    conferenceProvider_iPDialIn,
    conferenceProvider_arn,
    conferenceProvider_name,
    conferenceProvider_pSTNDialIn,
    conferenceProvider_type,

    -- ** Contact
    contact_phoneNumber,
    contact_phoneNumbers,
    contact_displayName,
    contact_contactArn,
    contact_firstName,
    contact_lastName,
    contact_sipAddresses,

    -- ** ContactData
    contactData_phoneNumber,
    contactData_phoneNumbers,
    contactData_displayName,
    contactData_contactArn,
    contactData_firstName,
    contactData_lastName,
    contactData_sipAddresses,

    -- ** Content
    content_textList,
    content_ssmlList,
    content_audioList,

    -- ** CreateEndOfMeetingReminder
    createEndOfMeetingReminder_reminderAtMinutes,
    createEndOfMeetingReminder_reminderType,
    createEndOfMeetingReminder_enabled,

    -- ** CreateInstantBooking
    createInstantBooking_durationInMinutes,
    createInstantBooking_enabled,

    -- ** CreateMeetingRoomConfiguration
    createMeetingRoomConfiguration_roomUtilizationMetricsEnabled,
    createMeetingRoomConfiguration_endOfMeetingReminder,
    createMeetingRoomConfiguration_instantBooking,
    createMeetingRoomConfiguration_requireCheckIn,

    -- ** CreateRequireCheckIn
    createRequireCheckIn_releaseAfterMinutes,
    createRequireCheckIn_enabled,

    -- ** DeveloperInfo
    developerInfo_developerName,
    developerInfo_email,
    developerInfo_privacyPolicy,
    developerInfo_url,

    -- ** Device
    device_deviceStatus,
    device_macAddress,
    device_deviceArn,
    device_roomArn,
    device_deviceStatusInfo,
    device_deviceName,
    device_networkProfileInfo,
    device_deviceSerialNumber,
    device_deviceType,
    device_softwareVersion,

    -- ** DeviceData
    deviceData_deviceStatus,
    deviceData_macAddress,
    deviceData_createdTime,
    deviceData_deviceArn,
    deviceData_roomArn,
    deviceData_networkProfileName,
    deviceData_deviceStatusInfo,
    deviceData_deviceName,
    deviceData_deviceSerialNumber,
    deviceData_roomName,
    deviceData_deviceType,
    deviceData_networkProfileArn,
    deviceData_softwareVersion,

    -- ** DeviceEvent
    deviceEvent_timestamp,
    deviceEvent_value,
    deviceEvent_type,

    -- ** DeviceNetworkProfileInfo
    deviceNetworkProfileInfo_certificateExpirationTime,
    deviceNetworkProfileInfo_certificateArn,
    deviceNetworkProfileInfo_networkProfileArn,

    -- ** DeviceStatusDetail
    deviceStatusDetail_code,
    deviceStatusDetail_feature,

    -- ** DeviceStatusInfo
    deviceStatusInfo_deviceStatusDetails,
    deviceStatusInfo_connectionStatusUpdatedTime,
    deviceStatusInfo_connectionStatus,

    -- ** EndOfMeetingReminder
    endOfMeetingReminder_reminderType,
    endOfMeetingReminder_reminderAtMinutes,
    endOfMeetingReminder_enabled,

    -- ** Filter
    filter_key,
    filter_values,

    -- ** Gateway
    gateway_arn,
    gateway_gatewayGroupArn,
    gateway_name,
    gateway_description,
    gateway_softwareVersion,

    -- ** GatewayGroup
    gatewayGroup_arn,
    gatewayGroup_name,
    gatewayGroup_description,

    -- ** GatewayGroupSummary
    gatewayGroupSummary_arn,
    gatewayGroupSummary_name,
    gatewayGroupSummary_description,

    -- ** GatewaySummary
    gatewaySummary_arn,
    gatewaySummary_gatewayGroupArn,
    gatewaySummary_name,
    gatewaySummary_description,
    gatewaySummary_softwareVersion,

    -- ** IPDialIn
    iPDialIn_endpoint,
    iPDialIn_commsProtocol,

    -- ** InstantBooking
    instantBooking_durationInMinutes,
    instantBooking_enabled,

    -- ** MeetingRoomConfiguration
    meetingRoomConfiguration_roomUtilizationMetricsEnabled,
    meetingRoomConfiguration_endOfMeetingReminder,
    meetingRoomConfiguration_instantBooking,
    meetingRoomConfiguration_requireCheckIn,

    -- ** MeetingSetting
    meetingSetting_requirePin,

    -- ** NetworkProfile
    networkProfile_certificateAuthorityArn,
    networkProfile_trustAnchors,
    networkProfile_currentPassword,
    networkProfile_eapMethod,
    networkProfile_networkProfileName,
    networkProfile_securityType,
    networkProfile_description,
    networkProfile_nextPassword,
    networkProfile_networkProfileArn,
    networkProfile_ssid,

    -- ** NetworkProfileData
    networkProfileData_certificateAuthorityArn,
    networkProfileData_eapMethod,
    networkProfileData_networkProfileName,
    networkProfileData_securityType,
    networkProfileData_description,
    networkProfileData_networkProfileArn,
    networkProfileData_ssid,

    -- ** PSTNDialIn
    pSTNDialIn_countryCode,
    pSTNDialIn_phoneNumber,
    pSTNDialIn_oneClickIdDelay,
    pSTNDialIn_oneClickPinDelay,

    -- ** PhoneNumber
    phoneNumber_number,
    phoneNumber_type,

    -- ** Profile
    profile_profileName,
    profile_isDefault,
    profile_address,
    profile_locale,
    profile_temperatureUnit,
    profile_addressBookArn,
    profile_setupModeDisabled,
    profile_pSTNEnabled,
    profile_maxVolumeLimit,
    profile_meetingRoomConfiguration,
    profile_wakeWord,
    profile_profileArn,
    profile_timezone,
    profile_distanceUnit,

    -- ** ProfileData
    profileData_profileName,
    profileData_isDefault,
    profileData_address,
    profileData_locale,
    profileData_temperatureUnit,
    profileData_wakeWord,
    profileData_profileArn,
    profileData_timezone,
    profileData_distanceUnit,

    -- ** RequireCheckIn
    requireCheckIn_releaseAfterMinutes,
    requireCheckIn_enabled,

    -- ** Room
    room_roomArn,
    room_providerCalendarId,
    room_profileArn,
    room_description,
    room_roomName,

    -- ** RoomData
    roomData_profileName,
    roomData_roomArn,
    roomData_providerCalendarId,
    roomData_profileArn,
    roomData_description,
    roomData_roomName,

    -- ** RoomSkillParameter
    roomSkillParameter_parameterKey,
    roomSkillParameter_parameterValue,

    -- ** SipAddress
    sipAddress_uri,
    sipAddress_type,

    -- ** SkillDetails
    skillDetails_newInThisVersionBulletPoints,
    skillDetails_skillTypes,
    skillDetails_reviews,
    skillDetails_bulletPoints,
    skillDetails_genericKeywords,
    skillDetails_endUserLicenseAgreement,
    skillDetails_developerInfo,
    skillDetails_productDescription,
    skillDetails_invocationPhrase,
    skillDetails_releaseDate,

    -- ** SkillGroup
    skillGroup_skillGroupName,
    skillGroup_description,
    skillGroup_skillGroupArn,

    -- ** SkillGroupData
    skillGroupData_skillGroupName,
    skillGroupData_description,
    skillGroupData_skillGroupArn,

    -- ** SkillSummary
    skillSummary_skillId,
    skillSummary_supportsLinking,
    skillSummary_skillType,
    skillSummary_skillName,
    skillSummary_enablementType,

    -- ** SkillsStoreSkill
    skillsStoreSkill_iconUrl,
    skillsStoreSkill_skillId,
    skillsStoreSkill_shortDescription,
    skillsStoreSkill_supportsLinking,
    skillsStoreSkill_skillName,
    skillsStoreSkill_sampleUtterances,
    skillsStoreSkill_skillDetails,

    -- ** SmartHomeAppliance
    smartHomeAppliance_friendlyName,
    smartHomeAppliance_description,
    smartHomeAppliance_manufacturerName,

    -- ** Sort
    sort_key,
    sort_value,

    -- ** Ssml
    ssml_locale,
    ssml_value,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TextMessage
    textMessage_locale,
    textMessage_value,

    -- ** UpdateEndOfMeetingReminder
    updateEndOfMeetingReminder_reminderType,
    updateEndOfMeetingReminder_reminderAtMinutes,
    updateEndOfMeetingReminder_enabled,

    -- ** UpdateInstantBooking
    updateInstantBooking_durationInMinutes,
    updateInstantBooking_enabled,

    -- ** UpdateMeetingRoomConfiguration
    updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled,
    updateMeetingRoomConfiguration_endOfMeetingReminder,
    updateMeetingRoomConfiguration_instantBooking,
    updateMeetingRoomConfiguration_requireCheckIn,

    -- ** UpdateRequireCheckIn
    updateRequireCheckIn_releaseAfterMinutes,
    updateRequireCheckIn_enabled,

    -- ** UserData
    userData_userArn,
    userData_enrollmentId,
    userData_email,
    userData_enrollmentStatus,
    userData_firstName,
    userData_lastName,
  )
where

import Network.AWS.AlexaBusiness.ApproveSkill
import Network.AWS.AlexaBusiness.AssociateContactWithAddressBook
import Network.AWS.AlexaBusiness.AssociateDeviceWithNetworkProfile
import Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
import Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
import Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
import Network.AWS.AlexaBusiness.AssociateSkillWithUsers
import Network.AWS.AlexaBusiness.CreateAddressBook
import Network.AWS.AlexaBusiness.CreateBusinessReportSchedule
import Network.AWS.AlexaBusiness.CreateConferenceProvider
import Network.AWS.AlexaBusiness.CreateContact
import Network.AWS.AlexaBusiness.CreateGatewayGroup
import Network.AWS.AlexaBusiness.CreateNetworkProfile
import Network.AWS.AlexaBusiness.CreateProfile
import Network.AWS.AlexaBusiness.CreateRoom
import Network.AWS.AlexaBusiness.CreateSkillGroup
import Network.AWS.AlexaBusiness.CreateUser
import Network.AWS.AlexaBusiness.DeleteAddressBook
import Network.AWS.AlexaBusiness.DeleteBusinessReportSchedule
import Network.AWS.AlexaBusiness.DeleteConferenceProvider
import Network.AWS.AlexaBusiness.DeleteContact
import Network.AWS.AlexaBusiness.DeleteDevice
import Network.AWS.AlexaBusiness.DeleteDeviceUsageData
import Network.AWS.AlexaBusiness.DeleteGatewayGroup
import Network.AWS.AlexaBusiness.DeleteNetworkProfile
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
import Network.AWS.AlexaBusiness.GetGateway
import Network.AWS.AlexaBusiness.GetGatewayGroup
import Network.AWS.AlexaBusiness.GetInvitationConfiguration
import Network.AWS.AlexaBusiness.GetNetworkProfile
import Network.AWS.AlexaBusiness.GetProfile
import Network.AWS.AlexaBusiness.GetRoom
import Network.AWS.AlexaBusiness.GetRoomSkillParameter
import Network.AWS.AlexaBusiness.GetSkillGroup
import Network.AWS.AlexaBusiness.ListBusinessReportSchedules
import Network.AWS.AlexaBusiness.ListConferenceProviders
import Network.AWS.AlexaBusiness.ListDeviceEvents
import Network.AWS.AlexaBusiness.ListGatewayGroups
import Network.AWS.AlexaBusiness.ListGateways
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
import Network.AWS.AlexaBusiness.SearchNetworkProfiles
import Network.AWS.AlexaBusiness.SearchProfiles
import Network.AWS.AlexaBusiness.SearchRooms
import Network.AWS.AlexaBusiness.SearchSkillGroups
import Network.AWS.AlexaBusiness.SearchUsers
import Network.AWS.AlexaBusiness.SendAnnouncement
import Network.AWS.AlexaBusiness.SendInvitation
import Network.AWS.AlexaBusiness.StartDeviceSync
import Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
import Network.AWS.AlexaBusiness.TagResource
import Network.AWS.AlexaBusiness.Types.AddressBook
import Network.AWS.AlexaBusiness.Types.AddressBookData
import Network.AWS.AlexaBusiness.Types.Audio
import Network.AWS.AlexaBusiness.Types.BusinessReport
import Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
import Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
import Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
import Network.AWS.AlexaBusiness.Types.BusinessReportSchedule
import Network.AWS.AlexaBusiness.Types.Category
import Network.AWS.AlexaBusiness.Types.ConferencePreference
import Network.AWS.AlexaBusiness.Types.ConferenceProvider
import Network.AWS.AlexaBusiness.Types.Contact
import Network.AWS.AlexaBusiness.Types.ContactData
import Network.AWS.AlexaBusiness.Types.Content
import Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.CreateInstantBooking
import Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.DeveloperInfo
import Network.AWS.AlexaBusiness.Types.Device
import Network.AWS.AlexaBusiness.Types.DeviceData
import Network.AWS.AlexaBusiness.Types.DeviceEvent
import Network.AWS.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
import Network.AWS.AlexaBusiness.Types.DeviceStatusInfo
import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.Filter
import Network.AWS.AlexaBusiness.Types.Gateway
import Network.AWS.AlexaBusiness.Types.GatewayGroup
import Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
import Network.AWS.AlexaBusiness.Types.GatewaySummary
import Network.AWS.AlexaBusiness.Types.IPDialIn
import Network.AWS.AlexaBusiness.Types.InstantBooking
import Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.MeetingSetting
import Network.AWS.AlexaBusiness.Types.NetworkProfile
import Network.AWS.AlexaBusiness.Types.NetworkProfileData
import Network.AWS.AlexaBusiness.Types.PSTNDialIn
import Network.AWS.AlexaBusiness.Types.PhoneNumber
import Network.AWS.AlexaBusiness.Types.Profile
import Network.AWS.AlexaBusiness.Types.ProfileData
import Network.AWS.AlexaBusiness.Types.RequireCheckIn
import Network.AWS.AlexaBusiness.Types.Room
import Network.AWS.AlexaBusiness.Types.RoomData
import Network.AWS.AlexaBusiness.Types.RoomSkillParameter
import Network.AWS.AlexaBusiness.Types.SipAddress
import Network.AWS.AlexaBusiness.Types.SkillDetails
import Network.AWS.AlexaBusiness.Types.SkillGroup
import Network.AWS.AlexaBusiness.Types.SkillGroupData
import Network.AWS.AlexaBusiness.Types.SkillSummary
import Network.AWS.AlexaBusiness.Types.SkillsStoreSkill
import Network.AWS.AlexaBusiness.Types.SmartHomeAppliance
import Network.AWS.AlexaBusiness.Types.Sort
import Network.AWS.AlexaBusiness.Types.Ssml
import Network.AWS.AlexaBusiness.Types.Tag
import Network.AWS.AlexaBusiness.Types.TextMessage
import Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
import Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
import Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
import Network.AWS.AlexaBusiness.Types.UserData
import Network.AWS.AlexaBusiness.UntagResource
import Network.AWS.AlexaBusiness.UpdateAddressBook
import Network.AWS.AlexaBusiness.UpdateBusinessReportSchedule
import Network.AWS.AlexaBusiness.UpdateConferenceProvider
import Network.AWS.AlexaBusiness.UpdateContact
import Network.AWS.AlexaBusiness.UpdateDevice
import Network.AWS.AlexaBusiness.UpdateGateway
import Network.AWS.AlexaBusiness.UpdateGatewayGroup
import Network.AWS.AlexaBusiness.UpdateNetworkProfile
import Network.AWS.AlexaBusiness.UpdateProfile
import Network.AWS.AlexaBusiness.UpdateRoom
import Network.AWS.AlexaBusiness.UpdateSkillGroup

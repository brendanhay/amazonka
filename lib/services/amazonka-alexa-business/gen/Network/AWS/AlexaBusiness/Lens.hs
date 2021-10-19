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

    -- ** SearchUsers
    searchUsers_filters,
    searchUsers_sortCriteria,
    searchUsers_nextToken,
    searchUsers_maxResults,
    searchUsersResponse_users,
    searchUsersResponse_nextToken,
    searchUsersResponse_totalCount,
    searchUsersResponse_httpStatus,

    -- ** PutConferencePreference
    putConferencePreference_conferencePreference,
    putConferencePreferenceResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_networkProfileName,
    updateNetworkProfile_currentPassword,
    updateNetworkProfile_nextPassword,
    updateNetworkProfile_description,
    updateNetworkProfile_trustAnchors,
    updateNetworkProfile_certificateAuthorityArn,
    updateNetworkProfile_networkProfileArn,
    updateNetworkProfileResponse_httpStatus,

    -- ** DeleteNetworkProfile
    deleteNetworkProfile_networkProfileArn,
    deleteNetworkProfileResponse_httpStatus,

    -- ** UpdateBusinessReportSchedule
    updateBusinessReportSchedule_s3KeyPrefix,
    updateBusinessReportSchedule_format,
    updateBusinessReportSchedule_recurrence,
    updateBusinessReportSchedule_scheduleName,
    updateBusinessReportSchedule_s3BucketName,
    updateBusinessReportSchedule_scheduleArn,
    updateBusinessReportScheduleResponse_httpStatus,

    -- ** DeleteBusinessReportSchedule
    deleteBusinessReportSchedule_scheduleArn,
    deleteBusinessReportScheduleResponse_httpStatus,

    -- ** AssociateSkillGroupWithRoom
    associateSkillGroupWithRoom_skillGroupArn,
    associateSkillGroupWithRoom_roomArn,
    associateSkillGroupWithRoomResponse_httpStatus,

    -- ** ListSmartHomeAppliances
    listSmartHomeAppliances_nextToken,
    listSmartHomeAppliances_maxResults,
    listSmartHomeAppliances_roomArn,
    listSmartHomeAppliancesResponse_smartHomeAppliances,
    listSmartHomeAppliancesResponse_nextToken,
    listSmartHomeAppliancesResponse_httpStatus,

    -- ** DeleteProfile
    deleteProfile_profileArn,
    deleteProfileResponse_httpStatus,

    -- ** UpdateProfile
    updateProfile_setupModeDisabled,
    updateProfile_pSTNEnabled,
    updateProfile_distanceUnit,
    updateProfile_locale,
    updateProfile_address,
    updateProfile_profileArn,
    updateProfile_wakeWord,
    updateProfile_meetingRoomConfiguration,
    updateProfile_profileName,
    updateProfile_temperatureUnit,
    updateProfile_dataRetentionOptIn,
    updateProfile_timezone,
    updateProfile_maxVolumeLimit,
    updateProfile_isDefault,
    updateProfileResponse_httpStatus,

    -- ** SearchRooms
    searchRooms_filters,
    searchRooms_sortCriteria,
    searchRooms_nextToken,
    searchRooms_maxResults,
    searchRoomsResponse_rooms,
    searchRoomsResponse_nextToken,
    searchRoomsResponse_totalCount,
    searchRoomsResponse_httpStatus,

    -- ** AssociateSkillWithUsers
    associateSkillWithUsers_skillId,
    associateSkillWithUsersResponse_httpStatus,

    -- ** RegisterAVSDevice
    registerAVSDevice_roomArn,
    registerAVSDevice_deviceSerialNumber,
    registerAVSDevice_tags,
    registerAVSDevice_clientId,
    registerAVSDevice_userCode,
    registerAVSDevice_productId,
    registerAVSDevice_amazonId,
    registerAVSDeviceResponse_deviceArn,
    registerAVSDeviceResponse_httpStatus,

    -- ** ForgetSmartHomeAppliances
    forgetSmartHomeAppliances_roomArn,
    forgetSmartHomeAppliancesResponse_httpStatus,

    -- ** PutInvitationConfiguration
    putInvitationConfiguration_contactEmail,
    putInvitationConfiguration_privateSkillIds,
    putInvitationConfiguration_organizationName,
    putInvitationConfigurationResponse_httpStatus,

    -- ** DisassociateContactFromAddressBook
    disassociateContactFromAddressBook_contactArn,
    disassociateContactFromAddressBook_addressBookArn,
    disassociateContactFromAddressBookResponse_httpStatus,

    -- ** GetNetworkProfile
    getNetworkProfile_networkProfileArn,
    getNetworkProfileResponse_networkProfile,
    getNetworkProfileResponse_httpStatus,

    -- ** GetConferencePreference
    getConferencePreferenceResponse_preference,
    getConferencePreferenceResponse_httpStatus,

    -- ** DisassociateSkillFromSkillGroup
    disassociateSkillFromSkillGroup_skillGroupArn,
    disassociateSkillFromSkillGroup_skillId,
    disassociateSkillFromSkillGroupResponse_httpStatus,

    -- ** CreateAddressBook
    createAddressBook_clientRequestToken,
    createAddressBook_description,
    createAddressBook_tags,
    createAddressBook_name,
    createAddressBookResponse_addressBookArn,
    createAddressBookResponse_httpStatus,

    -- ** DeleteAddressBook
    deleteAddressBook_addressBookArn,
    deleteAddressBookResponse_httpStatus,

    -- ** UpdateAddressBook
    updateAddressBook_name,
    updateAddressBook_description,
    updateAddressBook_addressBookArn,
    updateAddressBookResponse_httpStatus,

    -- ** DeleteGatewayGroup
    deleteGatewayGroup_gatewayGroupArn,
    deleteGatewayGroupResponse_httpStatus,

    -- ** UpdateGatewayGroup
    updateGatewayGroup_name,
    updateGatewayGroup_description,
    updateGatewayGroup_gatewayGroupArn,
    updateGatewayGroupResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_profileArn,
    updateRoom_providerCalendarId,
    updateRoom_roomArn,
    updateRoom_roomName,
    updateRoom_description,
    updateRoomResponse_httpStatus,

    -- ** DeleteRoom
    deleteRoom_roomArn,
    deleteRoomResponse_httpStatus,

    -- ** GetDevice
    getDevice_deviceArn,
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,

    -- ** GetGateway
    getGateway_gatewayArn,
    getGatewayResponse_gateway,
    getGatewayResponse_httpStatus,

    -- ** ListSkillsStoreSkillsByCategory
    listSkillsStoreSkillsByCategory_nextToken,
    listSkillsStoreSkillsByCategory_maxResults,
    listSkillsStoreSkillsByCategory_categoryId,
    listSkillsStoreSkillsByCategoryResponse_nextToken,
    listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills,
    listSkillsStoreSkillsByCategoryResponse_httpStatus,

    -- ** DeleteConferenceProvider
    deleteConferenceProvider_conferenceProviderArn,
    deleteConferenceProviderResponse_httpStatus,

    -- ** UpdateConferenceProvider
    updateConferenceProvider_pSTNDialIn,
    updateConferenceProvider_iPDialIn,
    updateConferenceProvider_conferenceProviderArn,
    updateConferenceProvider_conferenceProviderType,
    updateConferenceProvider_meetingSetting,
    updateConferenceProviderResponse_httpStatus,

    -- ** GetContact
    getContact_contactArn,
    getContactResponse_contact,
    getContactResponse_httpStatus,

    -- ** ApproveSkill
    approveSkill_skillId,
    approveSkillResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_currentPassword,
    createNetworkProfile_nextPassword,
    createNetworkProfile_eapMethod,
    createNetworkProfile_description,
    createNetworkProfile_trustAnchors,
    createNetworkProfile_certificateAuthorityArn,
    createNetworkProfile_tags,
    createNetworkProfile_networkProfileName,
    createNetworkProfile_ssid,
    createNetworkProfile_securityType,
    createNetworkProfile_clientRequestToken,
    createNetworkProfileResponse_networkProfileArn,
    createNetworkProfileResponse_httpStatus,

    -- ** AssociateDeviceWithRoom
    associateDeviceWithRoom_deviceArn,
    associateDeviceWithRoom_roomArn,
    associateDeviceWithRoomResponse_httpStatus,

    -- ** GetRoomSkillParameter
    getRoomSkillParameter_roomArn,
    getRoomSkillParameter_skillId,
    getRoomSkillParameter_parameterKey,
    getRoomSkillParameterResponse_roomSkillParameter,
    getRoomSkillParameterResponse_httpStatus,

    -- ** UpdateGateway
    updateGateway_name,
    updateGateway_softwareVersion,
    updateGateway_description,
    updateGateway_gatewayArn,
    updateGatewayResponse_httpStatus,

    -- ** CreateBusinessReportSchedule
    createBusinessReportSchedule_s3KeyPrefix,
    createBusinessReportSchedule_recurrence,
    createBusinessReportSchedule_scheduleName,
    createBusinessReportSchedule_clientRequestToken,
    createBusinessReportSchedule_s3BucketName,
    createBusinessReportSchedule_tags,
    createBusinessReportSchedule_format,
    createBusinessReportSchedule_contentRange,
    createBusinessReportScheduleResponse_scheduleArn,
    createBusinessReportScheduleResponse_httpStatus,

    -- ** DeleteContact
    deleteContact_contactArn,
    deleteContactResponse_httpStatus,

    -- ** UpdateContact
    updateContact_lastName,
    updateContact_phoneNumbers,
    updateContact_phoneNumber,
    updateContact_sipAddresses,
    updateContact_firstName,
    updateContact_displayName,
    updateContact_contactArn,
    updateContactResponse_httpStatus,

    -- ** GetAddressBook
    getAddressBook_addressBookArn,
    getAddressBookResponse_addressBook,
    getAddressBookResponse_httpStatus,

    -- ** ListBusinessReportSchedules
    listBusinessReportSchedules_nextToken,
    listBusinessReportSchedules_maxResults,
    listBusinessReportSchedulesResponse_businessReportSchedules,
    listBusinessReportSchedulesResponse_nextToken,
    listBusinessReportSchedulesResponse_httpStatus,

    -- ** DeleteDeviceUsageData
    deleteDeviceUsageData_deviceArn,
    deleteDeviceUsageData_deviceUsageType,
    deleteDeviceUsageDataResponse_httpStatus,

    -- ** CreateContact
    createContact_lastName,
    createContact_phoneNumbers,
    createContact_phoneNumber,
    createContact_sipAddresses,
    createContact_displayName,
    createContact_clientRequestToken,
    createContact_tags,
    createContact_firstName,
    createContactResponse_contactArn,
    createContactResponse_httpStatus,

    -- ** CreateProfile
    createProfile_setupModeDisabled,
    createProfile_pSTNEnabled,
    createProfile_locale,
    createProfile_meetingRoomConfiguration,
    createProfile_dataRetentionOptIn,
    createProfile_clientRequestToken,
    createProfile_maxVolumeLimit,
    createProfile_tags,
    createProfile_profileName,
    createProfile_timezone,
    createProfile_address,
    createProfile_distanceUnit,
    createProfile_temperatureUnit,
    createProfile_wakeWord,
    createProfileResponse_profileArn,
    createProfileResponse_httpStatus,

    -- ** DeleteSkillGroup
    deleteSkillGroup_skillGroupArn,
    deleteSkillGroupResponse_httpStatus,

    -- ** UpdateSkillGroup
    updateSkillGroup_skillGroupArn,
    updateSkillGroup_description,
    updateSkillGroup_skillGroupName,
    updateSkillGroupResponse_httpStatus,

    -- ** StartDeviceSync
    startDeviceSync_deviceArn,
    startDeviceSync_roomArn,
    startDeviceSync_features,
    startDeviceSyncResponse_httpStatus,

    -- ** GetInvitationConfiguration
    getInvitationConfigurationResponse_contactEmail,
    getInvitationConfigurationResponse_organizationName,
    getInvitationConfigurationResponse_privateSkillIds,
    getInvitationConfigurationResponse_httpStatus,

    -- ** DisassociateSkillFromUsers
    disassociateSkillFromUsers_skillId,
    disassociateSkillFromUsersResponse_httpStatus,

    -- ** SearchAddressBooks
    searchAddressBooks_filters,
    searchAddressBooks_sortCriteria,
    searchAddressBooks_nextToken,
    searchAddressBooks_maxResults,
    searchAddressBooksResponse_nextToken,
    searchAddressBooksResponse_addressBooks,
    searchAddressBooksResponse_totalCount,
    searchAddressBooksResponse_httpStatus,

    -- ** CreateSkillGroup
    createSkillGroup_clientRequestToken,
    createSkillGroup_description,
    createSkillGroup_tags,
    createSkillGroup_skillGroupName,
    createSkillGroupResponse_skillGroupArn,
    createSkillGroupResponse_httpStatus,

    -- ** GetProfile
    getProfile_profileArn,
    getProfileResponse_profile,
    getProfileResponse_httpStatus,

    -- ** DisassociateSkillGroupFromRoom
    disassociateSkillGroupFromRoom_skillGroupArn,
    disassociateSkillGroupFromRoom_roomArn,
    disassociateSkillGroupFromRoomResponse_httpStatus,

    -- ** SendInvitation
    sendInvitation_userArn,
    sendInvitationResponse_httpStatus,

    -- ** ListDeviceEvents
    listDeviceEvents_nextToken,
    listDeviceEvents_eventType,
    listDeviceEvents_maxResults,
    listDeviceEvents_deviceArn,
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_deviceEvents,
    listDeviceEventsResponse_httpStatus,

    -- ** CreateUser
    createUser_email,
    createUser_lastName,
    createUser_firstName,
    createUser_clientRequestToken,
    createUser_tags,
    createUser_userId,
    createUserResponse_userArn,
    createUserResponse_httpStatus,

    -- ** SearchDevices
    searchDevices_filters,
    searchDevices_sortCriteria,
    searchDevices_nextToken,
    searchDevices_maxResults,
    searchDevicesResponse_nextToken,
    searchDevicesResponse_devices,
    searchDevicesResponse_totalCount,
    searchDevicesResponse_httpStatus,

    -- ** SearchContacts
    searchContacts_filters,
    searchContacts_sortCriteria,
    searchContacts_nextToken,
    searchContacts_maxResults,
    searchContactsResponse_nextToken,
    searchContactsResponse_contacts,
    searchContactsResponse_totalCount,
    searchContactsResponse_httpStatus,

    -- ** SendAnnouncement
    sendAnnouncement_timeToLiveInSeconds,
    sendAnnouncement_roomFilters,
    sendAnnouncement_content,
    sendAnnouncement_clientRequestToken,
    sendAnnouncementResponse_announcementArn,
    sendAnnouncementResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userArn,
    deleteUser_enrollmentId,
    deleteUserResponse_httpStatus,

    -- ** SearchNetworkProfiles
    searchNetworkProfiles_filters,
    searchNetworkProfiles_sortCriteria,
    searchNetworkProfiles_nextToken,
    searchNetworkProfiles_maxResults,
    searchNetworkProfilesResponse_networkProfiles,
    searchNetworkProfilesResponse_nextToken,
    searchNetworkProfilesResponse_totalCount,
    searchNetworkProfilesResponse_httpStatus,

    -- ** GetSkillGroup
    getSkillGroup_skillGroupArn,
    getSkillGroupResponse_skillGroup,
    getSkillGroupResponse_httpStatus,

    -- ** ListSkills
    listSkills_skillGroupArn,
    listSkills_skillType,
    listSkills_nextToken,
    listSkills_enablementType,
    listSkills_maxResults,
    listSkillsResponse_nextToken,
    listSkillsResponse_skillSummaries,
    listSkillsResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DisassociateDeviceFromRoom
    disassociateDeviceFromRoom_deviceArn,
    disassociateDeviceFromRoomResponse_httpStatus,

    -- ** SearchSkillGroups
    searchSkillGroups_filters,
    searchSkillGroups_sortCriteria,
    searchSkillGroups_nextToken,
    searchSkillGroups_maxResults,
    searchSkillGroupsResponse_nextToken,
    searchSkillGroupsResponse_skillGroups,
    searchSkillGroupsResponse_totalCount,
    searchSkillGroupsResponse_httpStatus,

    -- ** PutSkillAuthorization
    putSkillAuthorization_roomArn,
    putSkillAuthorization_authorizationResult,
    putSkillAuthorization_skillId,
    putSkillAuthorizationResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_arn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DeleteSkillAuthorization
    deleteSkillAuthorization_roomArn,
    deleteSkillAuthorization_skillId,
    deleteSkillAuthorizationResponse_httpStatus,

    -- ** AssociateDeviceWithNetworkProfile
    associateDeviceWithNetworkProfile_deviceArn,
    associateDeviceWithNetworkProfile_networkProfileArn,
    associateDeviceWithNetworkProfileResponse_httpStatus,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateConferenceProvider
    createConferenceProvider_pSTNDialIn,
    createConferenceProvider_clientRequestToken,
    createConferenceProvider_iPDialIn,
    createConferenceProvider_tags,
    createConferenceProvider_conferenceProviderName,
    createConferenceProvider_conferenceProviderType,
    createConferenceProvider_meetingSetting,
    createConferenceProviderResponse_conferenceProviderArn,
    createConferenceProviderResponse_httpStatus,

    -- ** ResolveRoom
    resolveRoom_userId,
    resolveRoom_skillId,
    resolveRoomResponse_roomSkillParameters,
    resolveRoomResponse_roomArn,
    resolveRoomResponse_roomName,
    resolveRoomResponse_httpStatus,

    -- ** CreateGatewayGroup
    createGatewayGroup_description,
    createGatewayGroup_tags,
    createGatewayGroup_name,
    createGatewayGroup_clientRequestToken,
    createGatewayGroupResponse_gatewayGroupArn,
    createGatewayGroupResponse_httpStatus,

    -- ** CreateRoom
    createRoom_profileArn,
    createRoom_providerCalendarId,
    createRoom_clientRequestToken,
    createRoom_description,
    createRoom_tags,
    createRoom_roomName,
    createRoomResponse_roomArn,
    createRoomResponse_httpStatus,

    -- ** DeleteRoomSkillParameter
    deleteRoomSkillParameter_roomArn,
    deleteRoomSkillParameter_skillId,
    deleteRoomSkillParameter_parameterKey,
    deleteRoomSkillParameterResponse_httpStatus,

    -- ** ListGatewayGroups
    listGatewayGroups_nextToken,
    listGatewayGroups_maxResults,
    listGatewayGroupsResponse_gatewayGroups,
    listGatewayGroupsResponse_nextToken,
    listGatewayGroupsResponse_httpStatus,

    -- ** PutRoomSkillParameter
    putRoomSkillParameter_roomArn,
    putRoomSkillParameter_skillId,
    putRoomSkillParameter_roomSkillParameter,
    putRoomSkillParameterResponse_httpStatus,

    -- ** SearchProfiles
    searchProfiles_filters,
    searchProfiles_sortCriteria,
    searchProfiles_nextToken,
    searchProfiles_maxResults,
    searchProfilesResponse_profiles,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_totalCount,
    searchProfilesResponse_httpStatus,

    -- ** RejectSkill
    rejectSkill_skillId,
    rejectSkillResponse_httpStatus,

    -- ** ListConferenceProviders
    listConferenceProviders_nextToken,
    listConferenceProviders_maxResults,
    listConferenceProvidersResponse_conferenceProviders,
    listConferenceProvidersResponse_nextToken,
    listConferenceProvidersResponse_httpStatus,

    -- ** RevokeInvitation
    revokeInvitation_enrollmentId,
    revokeInvitation_userArn,
    revokeInvitationResponse_httpStatus,

    -- ** ListGateways
    listGateways_nextToken,
    listGateways_gatewayGroupArn,
    listGateways_maxResults,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,

    -- ** DeleteDevice
    deleteDevice_deviceArn,
    deleteDeviceResponse_httpStatus,

    -- ** UpdateDevice
    updateDevice_deviceArn,
    updateDevice_deviceName,
    updateDeviceResponse_httpStatus,

    -- ** AssociateSkillWithSkillGroup
    associateSkillWithSkillGroup_skillGroupArn,
    associateSkillWithSkillGroup_skillId,
    associateSkillWithSkillGroupResponse_httpStatus,

    -- ** GetConferenceProvider
    getConferenceProvider_conferenceProviderArn,
    getConferenceProviderResponse_conferenceProvider,
    getConferenceProviderResponse_httpStatus,

    -- ** GetRoom
    getRoom_roomArn,
    getRoomResponse_room,
    getRoomResponse_httpStatus,

    -- ** GetGatewayGroup
    getGatewayGroup_gatewayGroupArn,
    getGatewayGroupResponse_gatewayGroup,
    getGatewayGroupResponse_httpStatus,

    -- ** ListSkillsStoreCategories
    listSkillsStoreCategories_nextToken,
    listSkillsStoreCategories_maxResults,
    listSkillsStoreCategoriesResponse_categoryList,
    listSkillsStoreCategoriesResponse_nextToken,
    listSkillsStoreCategoriesResponse_httpStatus,

    -- ** StartSmartHomeApplianceDiscovery
    startSmartHomeApplianceDiscovery_roomArn,
    startSmartHomeApplianceDiscoveryResponse_httpStatus,

    -- ** AssociateContactWithAddressBook
    associateContactWithAddressBook_contactArn,
    associateContactWithAddressBook_addressBookArn,
    associateContactWithAddressBookResponse_httpStatus,

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
    businessReport_status,
    businessReport_failureCode,
    businessReport_deliveryTime,
    businessReport_downloadUrl,
    businessReport_s3Location,

    -- ** BusinessReportContentRange
    businessReportContentRange_interval,

    -- ** BusinessReportRecurrence
    businessReportRecurrence_startDate,

    -- ** BusinessReportS3Location
    businessReportS3Location_path,
    businessReportS3Location_bucketName,

    -- ** BusinessReportSchedule
    businessReportSchedule_s3KeyPrefix,
    businessReportSchedule_lastBusinessReport,
    businessReportSchedule_format,
    businessReportSchedule_recurrence,
    businessReportSchedule_scheduleName,
    businessReportSchedule_scheduleArn,
    businessReportSchedule_contentRange,
    businessReportSchedule_s3BucketName,

    -- ** Category
    category_categoryName,
    category_categoryId,

    -- ** ConferencePreference
    conferencePreference_defaultConferenceProviderArn,

    -- ** ConferenceProvider
    conferenceProvider_meetingSetting,
    conferenceProvider_arn,
    conferenceProvider_pSTNDialIn,
    conferenceProvider_name,
    conferenceProvider_type,
    conferenceProvider_iPDialIn,

    -- ** Contact
    contact_lastName,
    contact_contactArn,
    contact_phoneNumbers,
    contact_phoneNumber,
    contact_sipAddresses,
    contact_firstName,
    contact_displayName,

    -- ** ContactData
    contactData_lastName,
    contactData_contactArn,
    contactData_phoneNumbers,
    contactData_phoneNumber,
    contactData_sipAddresses,
    contactData_firstName,
    contactData_displayName,

    -- ** Content
    content_audioList,
    content_textList,
    content_ssmlList,

    -- ** CreateEndOfMeetingReminder
    createEndOfMeetingReminder_reminderAtMinutes,
    createEndOfMeetingReminder_reminderType,
    createEndOfMeetingReminder_enabled,

    -- ** CreateInstantBooking
    createInstantBooking_durationInMinutes,
    createInstantBooking_enabled,

    -- ** CreateMeetingRoomConfiguration
    createMeetingRoomConfiguration_instantBooking,
    createMeetingRoomConfiguration_endOfMeetingReminder,
    createMeetingRoomConfiguration_requireCheckIn,
    createMeetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- ** CreateRequireCheckIn
    createRequireCheckIn_releaseAfterMinutes,
    createRequireCheckIn_enabled,

    -- ** DeveloperInfo
    developerInfo_email,
    developerInfo_url,
    developerInfo_privacyPolicy,
    developerInfo_developerName,

    -- ** Device
    device_deviceStatus,
    device_deviceStatusInfo,
    device_deviceArn,
    device_macAddress,
    device_deviceName,
    device_roomArn,
    device_softwareVersion,
    device_deviceType,
    device_networkProfileInfo,
    device_deviceSerialNumber,

    -- ** DeviceData
    deviceData_deviceStatus,
    deviceData_networkProfileName,
    deviceData_deviceStatusInfo,
    deviceData_createdTime,
    deviceData_deviceArn,
    deviceData_networkProfileArn,
    deviceData_macAddress,
    deviceData_deviceName,
    deviceData_roomArn,
    deviceData_softwareVersion,
    deviceData_deviceType,
    deviceData_roomName,
    deviceData_deviceSerialNumber,

    -- ** DeviceEvent
    deviceEvent_value,
    deviceEvent_type,
    deviceEvent_timestamp,

    -- ** DeviceNetworkProfileInfo
    deviceNetworkProfileInfo_certificateArn,
    deviceNetworkProfileInfo_networkProfileArn,
    deviceNetworkProfileInfo_certificateExpirationTime,

    -- ** DeviceStatusDetail
    deviceStatusDetail_feature,
    deviceStatusDetail_code,

    -- ** DeviceStatusInfo
    deviceStatusInfo_connectionStatusUpdatedTime,
    deviceStatusInfo_deviceStatusDetails,
    deviceStatusInfo_connectionStatus,

    -- ** EndOfMeetingReminder
    endOfMeetingReminder_enabled,
    endOfMeetingReminder_reminderAtMinutes,
    endOfMeetingReminder_reminderType,

    -- ** Filter
    filter_key,
    filter_values,

    -- ** Gateway
    gateway_arn,
    gateway_name,
    gateway_gatewayGroupArn,
    gateway_softwareVersion,
    gateway_description,

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
    gatewaySummary_name,
    gatewaySummary_gatewayGroupArn,
    gatewaySummary_softwareVersion,
    gatewaySummary_description,

    -- ** IPDialIn
    iPDialIn_endpoint,
    iPDialIn_commsProtocol,

    -- ** InstantBooking
    instantBooking_enabled,
    instantBooking_durationInMinutes,

    -- ** MeetingRoomConfiguration
    meetingRoomConfiguration_instantBooking,
    meetingRoomConfiguration_endOfMeetingReminder,
    meetingRoomConfiguration_requireCheckIn,
    meetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- ** MeetingSetting
    meetingSetting_requirePin,

    -- ** NetworkProfile
    networkProfile_networkProfileName,
    networkProfile_ssid,
    networkProfile_networkProfileArn,
    networkProfile_securityType,
    networkProfile_currentPassword,
    networkProfile_nextPassword,
    networkProfile_eapMethod,
    networkProfile_description,
    networkProfile_trustAnchors,
    networkProfile_certificateAuthorityArn,

    -- ** NetworkProfileData
    networkProfileData_networkProfileName,
    networkProfileData_ssid,
    networkProfileData_networkProfileArn,
    networkProfileData_securityType,
    networkProfileData_eapMethod,
    networkProfileData_description,
    networkProfileData_certificateAuthorityArn,

    -- ** PSTNDialIn
    pSTNDialIn_countryCode,
    pSTNDialIn_phoneNumber,
    pSTNDialIn_oneClickIdDelay,
    pSTNDialIn_oneClickPinDelay,

    -- ** PhoneNumber
    phoneNumber_number,
    phoneNumber_type,

    -- ** Profile
    profile_setupModeDisabled,
    profile_pSTNEnabled,
    profile_addressBookArn,
    profile_distanceUnit,
    profile_locale,
    profile_address,
    profile_profileArn,
    profile_wakeWord,
    profile_meetingRoomConfiguration,
    profile_profileName,
    profile_temperatureUnit,
    profile_dataRetentionOptIn,
    profile_timezone,
    profile_maxVolumeLimit,
    profile_isDefault,

    -- ** ProfileData
    profileData_distanceUnit,
    profileData_locale,
    profileData_address,
    profileData_profileArn,
    profileData_wakeWord,
    profileData_profileName,
    profileData_temperatureUnit,
    profileData_timezone,
    profileData_isDefault,

    -- ** RequireCheckIn
    requireCheckIn_enabled,
    requireCheckIn_releaseAfterMinutes,

    -- ** Room
    room_profileArn,
    room_providerCalendarId,
    room_roomArn,
    room_roomName,
    room_description,

    -- ** RoomData
    roomData_profileArn,
    roomData_providerCalendarId,
    roomData_profileName,
    roomData_roomArn,
    roomData_roomName,
    roomData_description,

    -- ** RoomSkillParameter
    roomSkillParameter_parameterKey,
    roomSkillParameter_parameterValue,

    -- ** SipAddress
    sipAddress_uri,
    sipAddress_type,

    -- ** SkillDetails
    skillDetails_skillTypes,
    skillDetails_productDescription,
    skillDetails_invocationPhrase,
    skillDetails_developerInfo,
    skillDetails_endUserLicenseAgreement,
    skillDetails_genericKeywords,
    skillDetails_reviews,
    skillDetails_releaseDate,
    skillDetails_newInThisVersionBulletPoints,
    skillDetails_bulletPoints,

    -- ** SkillGroup
    skillGroup_skillGroupArn,
    skillGroup_description,
    skillGroup_skillGroupName,

    -- ** SkillGroupData
    skillGroupData_skillGroupArn,
    skillGroupData_description,
    skillGroupData_skillGroupName,

    -- ** SkillSummary
    skillSummary_skillId,
    skillSummary_supportsLinking,
    skillSummary_skillType,
    skillSummary_enablementType,
    skillSummary_skillName,

    -- ** SkillsStoreSkill
    skillsStoreSkill_skillId,
    skillsStoreSkill_supportsLinking,
    skillsStoreSkill_sampleUtterances,
    skillsStoreSkill_shortDescription,
    skillsStoreSkill_iconUrl,
    skillsStoreSkill_skillDetails,
    skillsStoreSkill_skillName,

    -- ** SmartHomeAppliance
    smartHomeAppliance_friendlyName,
    smartHomeAppliance_manufacturerName,
    smartHomeAppliance_description,

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
    updateEndOfMeetingReminder_enabled,
    updateEndOfMeetingReminder_reminderAtMinutes,
    updateEndOfMeetingReminder_reminderType,

    -- ** UpdateInstantBooking
    updateInstantBooking_enabled,
    updateInstantBooking_durationInMinutes,

    -- ** UpdateMeetingRoomConfiguration
    updateMeetingRoomConfiguration_instantBooking,
    updateMeetingRoomConfiguration_endOfMeetingReminder,
    updateMeetingRoomConfiguration_requireCheckIn,
    updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- ** UpdateRequireCheckIn
    updateRequireCheckIn_enabled,
    updateRequireCheckIn_releaseAfterMinutes,

    -- ** UserData
    userData_email,
    userData_lastName,
    userData_enrollmentId,
    userData_userArn,
    userData_firstName,
    userData_enrollmentStatus,
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

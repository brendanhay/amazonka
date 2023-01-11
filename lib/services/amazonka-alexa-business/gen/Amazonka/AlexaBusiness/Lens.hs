{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Lens
  ( -- * Operations

    -- ** ApproveSkill
    approveSkill_skillId,
    approveSkillResponse_httpStatus,

    -- ** AssociateContactWithAddressBook
    associateContactWithAddressBook_contactArn,
    associateContactWithAddressBook_addressBookArn,
    associateContactWithAddressBookResponse_httpStatus,

    -- ** AssociateDeviceWithNetworkProfile
    associateDeviceWithNetworkProfile_deviceArn,
    associateDeviceWithNetworkProfile_networkProfileArn,
    associateDeviceWithNetworkProfileResponse_httpStatus,

    -- ** AssociateDeviceWithRoom
    associateDeviceWithRoom_deviceArn,
    associateDeviceWithRoom_roomArn,
    associateDeviceWithRoomResponse_httpStatus,

    -- ** AssociateSkillGroupWithRoom
    associateSkillGroupWithRoom_roomArn,
    associateSkillGroupWithRoom_skillGroupArn,
    associateSkillGroupWithRoomResponse_httpStatus,

    -- ** AssociateSkillWithSkillGroup
    associateSkillWithSkillGroup_skillGroupArn,
    associateSkillWithSkillGroup_skillId,
    associateSkillWithSkillGroupResponse_httpStatus,

    -- ** AssociateSkillWithUsers
    associateSkillWithUsers_skillId,
    associateSkillWithUsersResponse_httpStatus,

    -- ** CreateAddressBook
    createAddressBook_clientRequestToken,
    createAddressBook_description,
    createAddressBook_tags,
    createAddressBook_name,
    createAddressBookResponse_addressBookArn,
    createAddressBookResponse_httpStatus,

    -- ** CreateBusinessReportSchedule
    createBusinessReportSchedule_clientRequestToken,
    createBusinessReportSchedule_recurrence,
    createBusinessReportSchedule_s3BucketName,
    createBusinessReportSchedule_s3KeyPrefix,
    createBusinessReportSchedule_scheduleName,
    createBusinessReportSchedule_tags,
    createBusinessReportSchedule_format,
    createBusinessReportSchedule_contentRange,
    createBusinessReportScheduleResponse_scheduleArn,
    createBusinessReportScheduleResponse_httpStatus,

    -- ** CreateConferenceProvider
    createConferenceProvider_clientRequestToken,
    createConferenceProvider_iPDialIn,
    createConferenceProvider_pSTNDialIn,
    createConferenceProvider_tags,
    createConferenceProvider_conferenceProviderName,
    createConferenceProvider_conferenceProviderType,
    createConferenceProvider_meetingSetting,
    createConferenceProviderResponse_conferenceProviderArn,
    createConferenceProviderResponse_httpStatus,

    -- ** CreateContact
    createContact_clientRequestToken,
    createContact_displayName,
    createContact_lastName,
    createContact_phoneNumber,
    createContact_phoneNumbers,
    createContact_sipAddresses,
    createContact_tags,
    createContact_firstName,
    createContactResponse_contactArn,
    createContactResponse_httpStatus,

    -- ** CreateGatewayGroup
    createGatewayGroup_description,
    createGatewayGroup_tags,
    createGatewayGroup_name,
    createGatewayGroup_clientRequestToken,
    createGatewayGroupResponse_gatewayGroupArn,
    createGatewayGroupResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_certificateAuthorityArn,
    createNetworkProfile_currentPassword,
    createNetworkProfile_description,
    createNetworkProfile_eapMethod,
    createNetworkProfile_nextPassword,
    createNetworkProfile_tags,
    createNetworkProfile_trustAnchors,
    createNetworkProfile_networkProfileName,
    createNetworkProfile_ssid,
    createNetworkProfile_securityType,
    createNetworkProfile_clientRequestToken,
    createNetworkProfileResponse_networkProfileArn,
    createNetworkProfileResponse_httpStatus,

    -- ** CreateProfile
    createProfile_clientRequestToken,
    createProfile_dataRetentionOptIn,
    createProfile_locale,
    createProfile_maxVolumeLimit,
    createProfile_meetingRoomConfiguration,
    createProfile_pSTNEnabled,
    createProfile_setupModeDisabled,
    createProfile_tags,
    createProfile_profileName,
    createProfile_timezone,
    createProfile_address,
    createProfile_distanceUnit,
    createProfile_temperatureUnit,
    createProfile_wakeWord,
    createProfileResponse_profileArn,
    createProfileResponse_httpStatus,

    -- ** CreateRoom
    createRoom_clientRequestToken,
    createRoom_description,
    createRoom_profileArn,
    createRoom_providerCalendarId,
    createRoom_tags,
    createRoom_roomName,
    createRoomResponse_roomArn,
    createRoomResponse_httpStatus,

    -- ** CreateSkillGroup
    createSkillGroup_clientRequestToken,
    createSkillGroup_description,
    createSkillGroup_tags,
    createSkillGroup_skillGroupName,
    createSkillGroupResponse_skillGroupArn,
    createSkillGroupResponse_httpStatus,

    -- ** CreateUser
    createUser_clientRequestToken,
    createUser_email,
    createUser_firstName,
    createUser_lastName,
    createUser_tags,
    createUser_userId,
    createUserResponse_userArn,
    createUserResponse_httpStatus,

    -- ** DeleteAddressBook
    deleteAddressBook_addressBookArn,
    deleteAddressBookResponse_httpStatus,

    -- ** DeleteBusinessReportSchedule
    deleteBusinessReportSchedule_scheduleArn,
    deleteBusinessReportScheduleResponse_httpStatus,

    -- ** DeleteConferenceProvider
    deleteConferenceProvider_conferenceProviderArn,
    deleteConferenceProviderResponse_httpStatus,

    -- ** DeleteContact
    deleteContact_contactArn,
    deleteContactResponse_httpStatus,

    -- ** DeleteDevice
    deleteDevice_deviceArn,
    deleteDeviceResponse_httpStatus,

    -- ** DeleteDeviceUsageData
    deleteDeviceUsageData_deviceArn,
    deleteDeviceUsageData_deviceUsageType,
    deleteDeviceUsageDataResponse_httpStatus,

    -- ** DeleteGatewayGroup
    deleteGatewayGroup_gatewayGroupArn,
    deleteGatewayGroupResponse_httpStatus,

    -- ** DeleteNetworkProfile
    deleteNetworkProfile_networkProfileArn,
    deleteNetworkProfileResponse_httpStatus,

    -- ** DeleteProfile
    deleteProfile_profileArn,
    deleteProfileResponse_httpStatus,

    -- ** DeleteRoom
    deleteRoom_roomArn,
    deleteRoomResponse_httpStatus,

    -- ** DeleteRoomSkillParameter
    deleteRoomSkillParameter_roomArn,
    deleteRoomSkillParameter_skillId,
    deleteRoomSkillParameter_parameterKey,
    deleteRoomSkillParameterResponse_httpStatus,

    -- ** DeleteSkillAuthorization
    deleteSkillAuthorization_roomArn,
    deleteSkillAuthorization_skillId,
    deleteSkillAuthorizationResponse_httpStatus,

    -- ** DeleteSkillGroup
    deleteSkillGroup_skillGroupArn,
    deleteSkillGroupResponse_httpStatus,

    -- ** DeleteUser
    deleteUser_userArn,
    deleteUser_enrollmentId,
    deleteUserResponse_httpStatus,

    -- ** DisassociateContactFromAddressBook
    disassociateContactFromAddressBook_contactArn,
    disassociateContactFromAddressBook_addressBookArn,
    disassociateContactFromAddressBookResponse_httpStatus,

    -- ** DisassociateDeviceFromRoom
    disassociateDeviceFromRoom_deviceArn,
    disassociateDeviceFromRoomResponse_httpStatus,

    -- ** DisassociateSkillFromSkillGroup
    disassociateSkillFromSkillGroup_skillGroupArn,
    disassociateSkillFromSkillGroup_skillId,
    disassociateSkillFromSkillGroupResponse_httpStatus,

    -- ** DisassociateSkillFromUsers
    disassociateSkillFromUsers_skillId,
    disassociateSkillFromUsersResponse_httpStatus,

    -- ** DisassociateSkillGroupFromRoom
    disassociateSkillGroupFromRoom_roomArn,
    disassociateSkillGroupFromRoom_skillGroupArn,
    disassociateSkillGroupFromRoomResponse_httpStatus,

    -- ** ForgetSmartHomeAppliances
    forgetSmartHomeAppliances_roomArn,
    forgetSmartHomeAppliancesResponse_httpStatus,

    -- ** GetAddressBook
    getAddressBook_addressBookArn,
    getAddressBookResponse_addressBook,
    getAddressBookResponse_httpStatus,

    -- ** GetConferencePreference
    getConferencePreferenceResponse_preference,
    getConferencePreferenceResponse_httpStatus,

    -- ** GetConferenceProvider
    getConferenceProvider_conferenceProviderArn,
    getConferenceProviderResponse_conferenceProvider,
    getConferenceProviderResponse_httpStatus,

    -- ** GetContact
    getContact_contactArn,
    getContactResponse_contact,
    getContactResponse_httpStatus,

    -- ** GetDevice
    getDevice_deviceArn,
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,

    -- ** GetGateway
    getGateway_gatewayArn,
    getGatewayResponse_gateway,
    getGatewayResponse_httpStatus,

    -- ** GetGatewayGroup
    getGatewayGroup_gatewayGroupArn,
    getGatewayGroupResponse_gatewayGroup,
    getGatewayGroupResponse_httpStatus,

    -- ** GetInvitationConfiguration
    getInvitationConfigurationResponse_contactEmail,
    getInvitationConfigurationResponse_organizationName,
    getInvitationConfigurationResponse_privateSkillIds,
    getInvitationConfigurationResponse_httpStatus,

    -- ** GetNetworkProfile
    getNetworkProfile_networkProfileArn,
    getNetworkProfileResponse_networkProfile,
    getNetworkProfileResponse_httpStatus,

    -- ** GetProfile
    getProfile_profileArn,
    getProfileResponse_profile,
    getProfileResponse_httpStatus,

    -- ** GetRoom
    getRoom_roomArn,
    getRoomResponse_room,
    getRoomResponse_httpStatus,

    -- ** GetRoomSkillParameter
    getRoomSkillParameter_roomArn,
    getRoomSkillParameter_skillId,
    getRoomSkillParameter_parameterKey,
    getRoomSkillParameterResponse_roomSkillParameter,
    getRoomSkillParameterResponse_httpStatus,

    -- ** GetSkillGroup
    getSkillGroup_skillGroupArn,
    getSkillGroupResponse_skillGroup,
    getSkillGroupResponse_httpStatus,

    -- ** ListBusinessReportSchedules
    listBusinessReportSchedules_maxResults,
    listBusinessReportSchedules_nextToken,
    listBusinessReportSchedulesResponse_businessReportSchedules,
    listBusinessReportSchedulesResponse_nextToken,
    listBusinessReportSchedulesResponse_httpStatus,

    -- ** ListConferenceProviders
    listConferenceProviders_maxResults,
    listConferenceProviders_nextToken,
    listConferenceProvidersResponse_conferenceProviders,
    listConferenceProvidersResponse_nextToken,
    listConferenceProvidersResponse_httpStatus,

    -- ** ListDeviceEvents
    listDeviceEvents_eventType,
    listDeviceEvents_maxResults,
    listDeviceEvents_nextToken,
    listDeviceEvents_deviceArn,
    listDeviceEventsResponse_deviceEvents,
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_httpStatus,

    -- ** ListGatewayGroups
    listGatewayGroups_maxResults,
    listGatewayGroups_nextToken,
    listGatewayGroupsResponse_gatewayGroups,
    listGatewayGroupsResponse_nextToken,
    listGatewayGroupsResponse_httpStatus,

    -- ** ListGateways
    listGateways_gatewayGroupArn,
    listGateways_maxResults,
    listGateways_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_httpStatus,

    -- ** ListSkills
    listSkills_enablementType,
    listSkills_maxResults,
    listSkills_nextToken,
    listSkills_skillGroupArn,
    listSkills_skillType,
    listSkillsResponse_nextToken,
    listSkillsResponse_skillSummaries,
    listSkillsResponse_httpStatus,

    -- ** ListSkillsStoreCategories
    listSkillsStoreCategories_maxResults,
    listSkillsStoreCategories_nextToken,
    listSkillsStoreCategoriesResponse_categoryList,
    listSkillsStoreCategoriesResponse_nextToken,
    listSkillsStoreCategoriesResponse_httpStatus,

    -- ** ListSkillsStoreSkillsByCategory
    listSkillsStoreSkillsByCategory_maxResults,
    listSkillsStoreSkillsByCategory_nextToken,
    listSkillsStoreSkillsByCategory_categoryId,
    listSkillsStoreSkillsByCategoryResponse_nextToken,
    listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills,
    listSkillsStoreSkillsByCategoryResponse_httpStatus,

    -- ** ListSmartHomeAppliances
    listSmartHomeAppliances_maxResults,
    listSmartHomeAppliances_nextToken,
    listSmartHomeAppliances_roomArn,
    listSmartHomeAppliancesResponse_nextToken,
    listSmartHomeAppliancesResponse_smartHomeAppliances,
    listSmartHomeAppliancesResponse_httpStatus,

    -- ** ListTags
    listTags_maxResults,
    listTags_nextToken,
    listTags_arn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** PutConferencePreference
    putConferencePreference_conferencePreference,
    putConferencePreferenceResponse_httpStatus,

    -- ** PutInvitationConfiguration
    putInvitationConfiguration_contactEmail,
    putInvitationConfiguration_privateSkillIds,
    putInvitationConfiguration_organizationName,
    putInvitationConfigurationResponse_httpStatus,

    -- ** PutRoomSkillParameter
    putRoomSkillParameter_roomArn,
    putRoomSkillParameter_skillId,
    putRoomSkillParameter_roomSkillParameter,
    putRoomSkillParameterResponse_httpStatus,

    -- ** PutSkillAuthorization
    putSkillAuthorization_roomArn,
    putSkillAuthorization_authorizationResult,
    putSkillAuthorization_skillId,
    putSkillAuthorizationResponse_httpStatus,

    -- ** RegisterAVSDevice
    registerAVSDevice_deviceSerialNumber,
    registerAVSDevice_roomArn,
    registerAVSDevice_tags,
    registerAVSDevice_clientId,
    registerAVSDevice_userCode,
    registerAVSDevice_productId,
    registerAVSDevice_amazonId,
    registerAVSDeviceResponse_deviceArn,
    registerAVSDeviceResponse_httpStatus,

    -- ** RejectSkill
    rejectSkill_skillId,
    rejectSkillResponse_httpStatus,

    -- ** ResolveRoom
    resolveRoom_userId,
    resolveRoom_skillId,
    resolveRoomResponse_roomArn,
    resolveRoomResponse_roomName,
    resolveRoomResponse_roomSkillParameters,
    resolveRoomResponse_httpStatus,

    -- ** RevokeInvitation
    revokeInvitation_enrollmentId,
    revokeInvitation_userArn,
    revokeInvitationResponse_httpStatus,

    -- ** SearchAddressBooks
    searchAddressBooks_filters,
    searchAddressBooks_maxResults,
    searchAddressBooks_nextToken,
    searchAddressBooks_sortCriteria,
    searchAddressBooksResponse_addressBooks,
    searchAddressBooksResponse_nextToken,
    searchAddressBooksResponse_totalCount,
    searchAddressBooksResponse_httpStatus,

    -- ** SearchContacts
    searchContacts_filters,
    searchContacts_maxResults,
    searchContacts_nextToken,
    searchContacts_sortCriteria,
    searchContactsResponse_contacts,
    searchContactsResponse_nextToken,
    searchContactsResponse_totalCount,
    searchContactsResponse_httpStatus,

    -- ** SearchDevices
    searchDevices_filters,
    searchDevices_maxResults,
    searchDevices_nextToken,
    searchDevices_sortCriteria,
    searchDevicesResponse_devices,
    searchDevicesResponse_nextToken,
    searchDevicesResponse_totalCount,
    searchDevicesResponse_httpStatus,

    -- ** SearchNetworkProfiles
    searchNetworkProfiles_filters,
    searchNetworkProfiles_maxResults,
    searchNetworkProfiles_nextToken,
    searchNetworkProfiles_sortCriteria,
    searchNetworkProfilesResponse_networkProfiles,
    searchNetworkProfilesResponse_nextToken,
    searchNetworkProfilesResponse_totalCount,
    searchNetworkProfilesResponse_httpStatus,

    -- ** SearchProfiles
    searchProfiles_filters,
    searchProfiles_maxResults,
    searchProfiles_nextToken,
    searchProfiles_sortCriteria,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_profiles,
    searchProfilesResponse_totalCount,
    searchProfilesResponse_httpStatus,

    -- ** SearchRooms
    searchRooms_filters,
    searchRooms_maxResults,
    searchRooms_nextToken,
    searchRooms_sortCriteria,
    searchRoomsResponse_nextToken,
    searchRoomsResponse_rooms,
    searchRoomsResponse_totalCount,
    searchRoomsResponse_httpStatus,

    -- ** SearchSkillGroups
    searchSkillGroups_filters,
    searchSkillGroups_maxResults,
    searchSkillGroups_nextToken,
    searchSkillGroups_sortCriteria,
    searchSkillGroupsResponse_nextToken,
    searchSkillGroupsResponse_skillGroups,
    searchSkillGroupsResponse_totalCount,
    searchSkillGroupsResponse_httpStatus,

    -- ** SearchUsers
    searchUsers_filters,
    searchUsers_maxResults,
    searchUsers_nextToken,
    searchUsers_sortCriteria,
    searchUsersResponse_nextToken,
    searchUsersResponse_totalCount,
    searchUsersResponse_users,
    searchUsersResponse_httpStatus,

    -- ** SendAnnouncement
    sendAnnouncement_timeToLiveInSeconds,
    sendAnnouncement_roomFilters,
    sendAnnouncement_content,
    sendAnnouncement_clientRequestToken,
    sendAnnouncementResponse_announcementArn,
    sendAnnouncementResponse_httpStatus,

    -- ** SendInvitation
    sendInvitation_userArn,
    sendInvitationResponse_httpStatus,

    -- ** StartDeviceSync
    startDeviceSync_deviceArn,
    startDeviceSync_roomArn,
    startDeviceSync_features,
    startDeviceSyncResponse_httpStatus,

    -- ** StartSmartHomeApplianceDiscovery
    startSmartHomeApplianceDiscovery_roomArn,
    startSmartHomeApplianceDiscoveryResponse_httpStatus,

    -- ** TagResource
    tagResource_arn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_arn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAddressBook
    updateAddressBook_description,
    updateAddressBook_name,
    updateAddressBook_addressBookArn,
    updateAddressBookResponse_httpStatus,

    -- ** UpdateBusinessReportSchedule
    updateBusinessReportSchedule_format,
    updateBusinessReportSchedule_recurrence,
    updateBusinessReportSchedule_s3BucketName,
    updateBusinessReportSchedule_s3KeyPrefix,
    updateBusinessReportSchedule_scheduleName,
    updateBusinessReportSchedule_scheduleArn,
    updateBusinessReportScheduleResponse_httpStatus,

    -- ** UpdateConferenceProvider
    updateConferenceProvider_iPDialIn,
    updateConferenceProvider_pSTNDialIn,
    updateConferenceProvider_conferenceProviderArn,
    updateConferenceProvider_conferenceProviderType,
    updateConferenceProvider_meetingSetting,
    updateConferenceProviderResponse_httpStatus,

    -- ** UpdateContact
    updateContact_displayName,
    updateContact_firstName,
    updateContact_lastName,
    updateContact_phoneNumber,
    updateContact_phoneNumbers,
    updateContact_sipAddresses,
    updateContact_contactArn,
    updateContactResponse_httpStatus,

    -- ** UpdateDevice
    updateDevice_deviceArn,
    updateDevice_deviceName,
    updateDeviceResponse_httpStatus,

    -- ** UpdateGateway
    updateGateway_description,
    updateGateway_name,
    updateGateway_softwareVersion,
    updateGateway_gatewayArn,
    updateGatewayResponse_httpStatus,

    -- ** UpdateGatewayGroup
    updateGatewayGroup_description,
    updateGatewayGroup_name,
    updateGatewayGroup_gatewayGroupArn,
    updateGatewayGroupResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_certificateAuthorityArn,
    updateNetworkProfile_currentPassword,
    updateNetworkProfile_description,
    updateNetworkProfile_networkProfileName,
    updateNetworkProfile_nextPassword,
    updateNetworkProfile_trustAnchors,
    updateNetworkProfile_networkProfileArn,
    updateNetworkProfileResponse_httpStatus,

    -- ** UpdateProfile
    updateProfile_address,
    updateProfile_dataRetentionOptIn,
    updateProfile_distanceUnit,
    updateProfile_isDefault,
    updateProfile_locale,
    updateProfile_maxVolumeLimit,
    updateProfile_meetingRoomConfiguration,
    updateProfile_pSTNEnabled,
    updateProfile_profileArn,
    updateProfile_profileName,
    updateProfile_setupModeDisabled,
    updateProfile_temperatureUnit,
    updateProfile_timezone,
    updateProfile_wakeWord,
    updateProfileResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_description,
    updateRoom_profileArn,
    updateRoom_providerCalendarId,
    updateRoom_roomArn,
    updateRoom_roomName,
    updateRoomResponse_httpStatus,

    -- ** UpdateSkillGroup
    updateSkillGroup_description,
    updateSkillGroup_skillGroupArn,
    updateSkillGroup_skillGroupName,
    updateSkillGroupResponse_httpStatus,

    -- * Types

    -- ** AddressBook
    addressBook_addressBookArn,
    addressBook_description,
    addressBook_name,

    -- ** AddressBookData
    addressBookData_addressBookArn,
    addressBookData_description,
    addressBookData_name,

    -- ** Audio
    audio_locale,
    audio_location,

    -- ** BusinessReport
    businessReport_deliveryTime,
    businessReport_downloadUrl,
    businessReport_failureCode,
    businessReport_s3Location,
    businessReport_status,

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
    businessReportSchedule_lastBusinessReport,
    businessReportSchedule_recurrence,
    businessReportSchedule_s3BucketName,
    businessReportSchedule_s3KeyPrefix,
    businessReportSchedule_scheduleArn,
    businessReportSchedule_scheduleName,

    -- ** Category
    category_categoryId,
    category_categoryName,

    -- ** ConferencePreference
    conferencePreference_defaultConferenceProviderArn,

    -- ** ConferenceProvider
    conferenceProvider_arn,
    conferenceProvider_iPDialIn,
    conferenceProvider_meetingSetting,
    conferenceProvider_name,
    conferenceProvider_pSTNDialIn,
    conferenceProvider_type,

    -- ** Contact
    contact_contactArn,
    contact_displayName,
    contact_firstName,
    contact_lastName,
    contact_phoneNumber,
    contact_phoneNumbers,
    contact_sipAddresses,

    -- ** ContactData
    contactData_contactArn,
    contactData_displayName,
    contactData_firstName,
    contactData_lastName,
    contactData_phoneNumber,
    contactData_phoneNumbers,
    contactData_sipAddresses,

    -- ** Content
    content_audioList,
    content_ssmlList,
    content_textList,

    -- ** CreateEndOfMeetingReminder
    createEndOfMeetingReminder_reminderAtMinutes,
    createEndOfMeetingReminder_reminderType,
    createEndOfMeetingReminder_enabled,

    -- ** CreateInstantBooking
    createInstantBooking_durationInMinutes,
    createInstantBooking_enabled,

    -- ** CreateMeetingRoomConfiguration
    createMeetingRoomConfiguration_endOfMeetingReminder,
    createMeetingRoomConfiguration_instantBooking,
    createMeetingRoomConfiguration_requireCheckIn,
    createMeetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- ** CreateRequireCheckIn
    createRequireCheckIn_releaseAfterMinutes,
    createRequireCheckIn_enabled,

    -- ** DeveloperInfo
    developerInfo_developerName,
    developerInfo_email,
    developerInfo_privacyPolicy,
    developerInfo_url,

    -- ** Device
    device_deviceArn,
    device_deviceName,
    device_deviceSerialNumber,
    device_deviceStatus,
    device_deviceStatusInfo,
    device_deviceType,
    device_macAddress,
    device_networkProfileInfo,
    device_roomArn,
    device_softwareVersion,

    -- ** DeviceData
    deviceData_createdTime,
    deviceData_deviceArn,
    deviceData_deviceName,
    deviceData_deviceSerialNumber,
    deviceData_deviceStatus,
    deviceData_deviceStatusInfo,
    deviceData_deviceType,
    deviceData_macAddress,
    deviceData_networkProfileArn,
    deviceData_networkProfileName,
    deviceData_roomArn,
    deviceData_roomName,
    deviceData_softwareVersion,

    -- ** DeviceEvent
    deviceEvent_timestamp,
    deviceEvent_type,
    deviceEvent_value,

    -- ** DeviceNetworkProfileInfo
    deviceNetworkProfileInfo_certificateArn,
    deviceNetworkProfileInfo_certificateExpirationTime,
    deviceNetworkProfileInfo_networkProfileArn,

    -- ** DeviceStatusDetail
    deviceStatusDetail_code,
    deviceStatusDetail_feature,

    -- ** DeviceStatusInfo
    deviceStatusInfo_connectionStatus,
    deviceStatusInfo_connectionStatusUpdatedTime,
    deviceStatusInfo_deviceStatusDetails,

    -- ** EndOfMeetingReminder
    endOfMeetingReminder_enabled,
    endOfMeetingReminder_reminderAtMinutes,
    endOfMeetingReminder_reminderType,

    -- ** Filter
    filter_key,
    filter_values,

    -- ** Gateway
    gateway_arn,
    gateway_description,
    gateway_gatewayGroupArn,
    gateway_name,
    gateway_softwareVersion,

    -- ** GatewayGroup
    gatewayGroup_arn,
    gatewayGroup_description,
    gatewayGroup_name,

    -- ** GatewayGroupSummary
    gatewayGroupSummary_arn,
    gatewayGroupSummary_description,
    gatewayGroupSummary_name,

    -- ** GatewaySummary
    gatewaySummary_arn,
    gatewaySummary_description,
    gatewaySummary_gatewayGroupArn,
    gatewaySummary_name,
    gatewaySummary_softwareVersion,

    -- ** IPDialIn
    iPDialIn_endpoint,
    iPDialIn_commsProtocol,

    -- ** InstantBooking
    instantBooking_durationInMinutes,
    instantBooking_enabled,

    -- ** MeetingRoomConfiguration
    meetingRoomConfiguration_endOfMeetingReminder,
    meetingRoomConfiguration_instantBooking,
    meetingRoomConfiguration_requireCheckIn,
    meetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- ** MeetingSetting
    meetingSetting_requirePin,

    -- ** NetworkProfile
    networkProfile_certificateAuthorityArn,
    networkProfile_currentPassword,
    networkProfile_description,
    networkProfile_eapMethod,
    networkProfile_networkProfileArn,
    networkProfile_networkProfileName,
    networkProfile_nextPassword,
    networkProfile_securityType,
    networkProfile_ssid,
    networkProfile_trustAnchors,

    -- ** NetworkProfileData
    networkProfileData_certificateAuthorityArn,
    networkProfileData_description,
    networkProfileData_eapMethod,
    networkProfileData_networkProfileArn,
    networkProfileData_networkProfileName,
    networkProfileData_securityType,
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
    profile_address,
    profile_addressBookArn,
    profile_dataRetentionOptIn,
    profile_distanceUnit,
    profile_isDefault,
    profile_locale,
    profile_maxVolumeLimit,
    profile_meetingRoomConfiguration,
    profile_pSTNEnabled,
    profile_profileArn,
    profile_profileName,
    profile_setupModeDisabled,
    profile_temperatureUnit,
    profile_timezone,
    profile_wakeWord,

    -- ** ProfileData
    profileData_address,
    profileData_distanceUnit,
    profileData_isDefault,
    profileData_locale,
    profileData_profileArn,
    profileData_profileName,
    profileData_temperatureUnit,
    profileData_timezone,
    profileData_wakeWord,

    -- ** RequireCheckIn
    requireCheckIn_enabled,
    requireCheckIn_releaseAfterMinutes,

    -- ** Room
    room_description,
    room_profileArn,
    room_providerCalendarId,
    room_roomArn,
    room_roomName,

    -- ** RoomData
    roomData_description,
    roomData_profileArn,
    roomData_profileName,
    roomData_providerCalendarId,
    roomData_roomArn,
    roomData_roomName,

    -- ** RoomSkillParameter
    roomSkillParameter_parameterKey,
    roomSkillParameter_parameterValue,

    -- ** SipAddress
    sipAddress_uri,
    sipAddress_type,

    -- ** SkillDetails
    skillDetails_bulletPoints,
    skillDetails_developerInfo,
    skillDetails_endUserLicenseAgreement,
    skillDetails_genericKeywords,
    skillDetails_invocationPhrase,
    skillDetails_newInThisVersionBulletPoints,
    skillDetails_productDescription,
    skillDetails_releaseDate,
    skillDetails_reviews,
    skillDetails_skillTypes,

    -- ** SkillGroup
    skillGroup_description,
    skillGroup_skillGroupArn,
    skillGroup_skillGroupName,

    -- ** SkillGroupData
    skillGroupData_description,
    skillGroupData_skillGroupArn,
    skillGroupData_skillGroupName,

    -- ** SkillSummary
    skillSummary_enablementType,
    skillSummary_skillId,
    skillSummary_skillName,
    skillSummary_skillType,
    skillSummary_supportsLinking,

    -- ** SkillsStoreSkill
    skillsStoreSkill_iconUrl,
    skillsStoreSkill_sampleUtterances,
    skillsStoreSkill_shortDescription,
    skillsStoreSkill_skillDetails,
    skillsStoreSkill_skillId,
    skillsStoreSkill_skillName,
    skillsStoreSkill_supportsLinking,

    -- ** SmartHomeAppliance
    smartHomeAppliance_description,
    smartHomeAppliance_friendlyName,
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
    updateEndOfMeetingReminder_enabled,
    updateEndOfMeetingReminder_reminderAtMinutes,
    updateEndOfMeetingReminder_reminderType,

    -- ** UpdateInstantBooking
    updateInstantBooking_durationInMinutes,
    updateInstantBooking_enabled,

    -- ** UpdateMeetingRoomConfiguration
    updateMeetingRoomConfiguration_endOfMeetingReminder,
    updateMeetingRoomConfiguration_instantBooking,
    updateMeetingRoomConfiguration_requireCheckIn,
    updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled,

    -- ** UpdateRequireCheckIn
    updateRequireCheckIn_enabled,
    updateRequireCheckIn_releaseAfterMinutes,

    -- ** UserData
    userData_email,
    userData_enrollmentId,
    userData_enrollmentStatus,
    userData_firstName,
    userData_lastName,
    userData_userArn,
  )
where

import Amazonka.AlexaBusiness.ApproveSkill
import Amazonka.AlexaBusiness.AssociateContactWithAddressBook
import Amazonka.AlexaBusiness.AssociateDeviceWithNetworkProfile
import Amazonka.AlexaBusiness.AssociateDeviceWithRoom
import Amazonka.AlexaBusiness.AssociateSkillGroupWithRoom
import Amazonka.AlexaBusiness.AssociateSkillWithSkillGroup
import Amazonka.AlexaBusiness.AssociateSkillWithUsers
import Amazonka.AlexaBusiness.CreateAddressBook
import Amazonka.AlexaBusiness.CreateBusinessReportSchedule
import Amazonka.AlexaBusiness.CreateConferenceProvider
import Amazonka.AlexaBusiness.CreateContact
import Amazonka.AlexaBusiness.CreateGatewayGroup
import Amazonka.AlexaBusiness.CreateNetworkProfile
import Amazonka.AlexaBusiness.CreateProfile
import Amazonka.AlexaBusiness.CreateRoom
import Amazonka.AlexaBusiness.CreateSkillGroup
import Amazonka.AlexaBusiness.CreateUser
import Amazonka.AlexaBusiness.DeleteAddressBook
import Amazonka.AlexaBusiness.DeleteBusinessReportSchedule
import Amazonka.AlexaBusiness.DeleteConferenceProvider
import Amazonka.AlexaBusiness.DeleteContact
import Amazonka.AlexaBusiness.DeleteDevice
import Amazonka.AlexaBusiness.DeleteDeviceUsageData
import Amazonka.AlexaBusiness.DeleteGatewayGroup
import Amazonka.AlexaBusiness.DeleteNetworkProfile
import Amazonka.AlexaBusiness.DeleteProfile
import Amazonka.AlexaBusiness.DeleteRoom
import Amazonka.AlexaBusiness.DeleteRoomSkillParameter
import Amazonka.AlexaBusiness.DeleteSkillAuthorization
import Amazonka.AlexaBusiness.DeleteSkillGroup
import Amazonka.AlexaBusiness.DeleteUser
import Amazonka.AlexaBusiness.DisassociateContactFromAddressBook
import Amazonka.AlexaBusiness.DisassociateDeviceFromRoom
import Amazonka.AlexaBusiness.DisassociateSkillFromSkillGroup
import Amazonka.AlexaBusiness.DisassociateSkillFromUsers
import Amazonka.AlexaBusiness.DisassociateSkillGroupFromRoom
import Amazonka.AlexaBusiness.ForgetSmartHomeAppliances
import Amazonka.AlexaBusiness.GetAddressBook
import Amazonka.AlexaBusiness.GetConferencePreference
import Amazonka.AlexaBusiness.GetConferenceProvider
import Amazonka.AlexaBusiness.GetContact
import Amazonka.AlexaBusiness.GetDevice
import Amazonka.AlexaBusiness.GetGateway
import Amazonka.AlexaBusiness.GetGatewayGroup
import Amazonka.AlexaBusiness.GetInvitationConfiguration
import Amazonka.AlexaBusiness.GetNetworkProfile
import Amazonka.AlexaBusiness.GetProfile
import Amazonka.AlexaBusiness.GetRoom
import Amazonka.AlexaBusiness.GetRoomSkillParameter
import Amazonka.AlexaBusiness.GetSkillGroup
import Amazonka.AlexaBusiness.ListBusinessReportSchedules
import Amazonka.AlexaBusiness.ListConferenceProviders
import Amazonka.AlexaBusiness.ListDeviceEvents
import Amazonka.AlexaBusiness.ListGatewayGroups
import Amazonka.AlexaBusiness.ListGateways
import Amazonka.AlexaBusiness.ListSkills
import Amazonka.AlexaBusiness.ListSkillsStoreCategories
import Amazonka.AlexaBusiness.ListSkillsStoreSkillsByCategory
import Amazonka.AlexaBusiness.ListSmartHomeAppliances
import Amazonka.AlexaBusiness.ListTags
import Amazonka.AlexaBusiness.PutConferencePreference
import Amazonka.AlexaBusiness.PutInvitationConfiguration
import Amazonka.AlexaBusiness.PutRoomSkillParameter
import Amazonka.AlexaBusiness.PutSkillAuthorization
import Amazonka.AlexaBusiness.RegisterAVSDevice
import Amazonka.AlexaBusiness.RejectSkill
import Amazonka.AlexaBusiness.ResolveRoom
import Amazonka.AlexaBusiness.RevokeInvitation
import Amazonka.AlexaBusiness.SearchAddressBooks
import Amazonka.AlexaBusiness.SearchContacts
import Amazonka.AlexaBusiness.SearchDevices
import Amazonka.AlexaBusiness.SearchNetworkProfiles
import Amazonka.AlexaBusiness.SearchProfiles
import Amazonka.AlexaBusiness.SearchRooms
import Amazonka.AlexaBusiness.SearchSkillGroups
import Amazonka.AlexaBusiness.SearchUsers
import Amazonka.AlexaBusiness.SendAnnouncement
import Amazonka.AlexaBusiness.SendInvitation
import Amazonka.AlexaBusiness.StartDeviceSync
import Amazonka.AlexaBusiness.StartSmartHomeApplianceDiscovery
import Amazonka.AlexaBusiness.TagResource
import Amazonka.AlexaBusiness.Types.AddressBook
import Amazonka.AlexaBusiness.Types.AddressBookData
import Amazonka.AlexaBusiness.Types.Audio
import Amazonka.AlexaBusiness.Types.BusinessReport
import Amazonka.AlexaBusiness.Types.BusinessReportContentRange
import Amazonka.AlexaBusiness.Types.BusinessReportRecurrence
import Amazonka.AlexaBusiness.Types.BusinessReportS3Location
import Amazonka.AlexaBusiness.Types.BusinessReportSchedule
import Amazonka.AlexaBusiness.Types.Category
import Amazonka.AlexaBusiness.Types.ConferencePreference
import Amazonka.AlexaBusiness.Types.ConferenceProvider
import Amazonka.AlexaBusiness.Types.Contact
import Amazonka.AlexaBusiness.Types.ContactData
import Amazonka.AlexaBusiness.Types.Content
import Amazonka.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.CreateInstantBooking
import Amazonka.AlexaBusiness.Types.CreateMeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.CreateRequireCheckIn
import Amazonka.AlexaBusiness.Types.DeveloperInfo
import Amazonka.AlexaBusiness.Types.Device
import Amazonka.AlexaBusiness.Types.DeviceData
import Amazonka.AlexaBusiness.Types.DeviceEvent
import Amazonka.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Amazonka.AlexaBusiness.Types.DeviceStatusDetail
import Amazonka.AlexaBusiness.Types.DeviceStatusInfo
import Amazonka.AlexaBusiness.Types.EndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.Filter
import Amazonka.AlexaBusiness.Types.Gateway
import Amazonka.AlexaBusiness.Types.GatewayGroup
import Amazonka.AlexaBusiness.Types.GatewayGroupSummary
import Amazonka.AlexaBusiness.Types.GatewaySummary
import Amazonka.AlexaBusiness.Types.IPDialIn
import Amazonka.AlexaBusiness.Types.InstantBooking
import Amazonka.AlexaBusiness.Types.MeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.MeetingSetting
import Amazonka.AlexaBusiness.Types.NetworkProfile
import Amazonka.AlexaBusiness.Types.NetworkProfileData
import Amazonka.AlexaBusiness.Types.PSTNDialIn
import Amazonka.AlexaBusiness.Types.PhoneNumber
import Amazonka.AlexaBusiness.Types.Profile
import Amazonka.AlexaBusiness.Types.ProfileData
import Amazonka.AlexaBusiness.Types.RequireCheckIn
import Amazonka.AlexaBusiness.Types.Room
import Amazonka.AlexaBusiness.Types.RoomData
import Amazonka.AlexaBusiness.Types.RoomSkillParameter
import Amazonka.AlexaBusiness.Types.SipAddress
import Amazonka.AlexaBusiness.Types.SkillDetails
import Amazonka.AlexaBusiness.Types.SkillGroup
import Amazonka.AlexaBusiness.Types.SkillGroupData
import Amazonka.AlexaBusiness.Types.SkillSummary
import Amazonka.AlexaBusiness.Types.SkillsStoreSkill
import Amazonka.AlexaBusiness.Types.SmartHomeAppliance
import Amazonka.AlexaBusiness.Types.Sort
import Amazonka.AlexaBusiness.Types.Ssml
import Amazonka.AlexaBusiness.Types.Tag
import Amazonka.AlexaBusiness.Types.TextMessage
import Amazonka.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.UpdateInstantBooking
import Amazonka.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
import Amazonka.AlexaBusiness.Types.UpdateRequireCheckIn
import Amazonka.AlexaBusiness.Types.UserData
import Amazonka.AlexaBusiness.UntagResource
import Amazonka.AlexaBusiness.UpdateAddressBook
import Amazonka.AlexaBusiness.UpdateBusinessReportSchedule
import Amazonka.AlexaBusiness.UpdateConferenceProvider
import Amazonka.AlexaBusiness.UpdateContact
import Amazonka.AlexaBusiness.UpdateDevice
import Amazonka.AlexaBusiness.UpdateGateway
import Amazonka.AlexaBusiness.UpdateGatewayGroup
import Amazonka.AlexaBusiness.UpdateNetworkProfile
import Amazonka.AlexaBusiness.UpdateProfile
import Amazonka.AlexaBusiness.UpdateRoom
import Amazonka.AlexaBusiness.UpdateSkillGroup

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    associateDeviceWithRoom_roomArn,
    associateDeviceWithRoom_deviceArn,
    associateDeviceWithRoomResponse_httpStatus,

    -- ** AssociateSkillGroupWithRoom
    associateSkillGroupWithRoom_skillGroupArn,
    associateSkillGroupWithRoom_roomArn,
    associateSkillGroupWithRoomResponse_httpStatus,

    -- ** AssociateSkillWithSkillGroup
    associateSkillWithSkillGroup_skillGroupArn,
    associateSkillWithSkillGroup_skillId,
    associateSkillWithSkillGroupResponse_httpStatus,

    -- ** AssociateSkillWithUsers
    associateSkillWithUsers_skillId,
    associateSkillWithUsersResponse_httpStatus,

    -- ** CreateAddressBook
    createAddressBook_tags,
    createAddressBook_clientRequestToken,
    createAddressBook_description,
    createAddressBook_name,
    createAddressBookResponse_addressBookArn,
    createAddressBookResponse_httpStatus,

    -- ** CreateBusinessReportSchedule
    createBusinessReportSchedule_tags,
    createBusinessReportSchedule_s3KeyPrefix,
    createBusinessReportSchedule_clientRequestToken,
    createBusinessReportSchedule_s3BucketName,
    createBusinessReportSchedule_recurrence,
    createBusinessReportSchedule_scheduleName,
    createBusinessReportSchedule_format,
    createBusinessReportSchedule_contentRange,
    createBusinessReportScheduleResponse_scheduleArn,
    createBusinessReportScheduleResponse_httpStatus,

    -- ** CreateConferenceProvider
    createConferenceProvider_tags,
    createConferenceProvider_iPDialIn,
    createConferenceProvider_clientRequestToken,
    createConferenceProvider_pSTNDialIn,
    createConferenceProvider_conferenceProviderName,
    createConferenceProvider_conferenceProviderType,
    createConferenceProvider_meetingSetting,
    createConferenceProviderResponse_conferenceProviderArn,
    createConferenceProviderResponse_httpStatus,

    -- ** CreateContact
    createContact_tags,
    createContact_clientRequestToken,
    createContact_sipAddresses,
    createContact_displayName,
    createContact_lastName,
    createContact_phoneNumber,
    createContact_phoneNumbers,
    createContact_firstName,
    createContactResponse_contactArn,
    createContactResponse_httpStatus,

    -- ** CreateGatewayGroup
    createGatewayGroup_tags,
    createGatewayGroup_description,
    createGatewayGroup_name,
    createGatewayGroup_clientRequestToken,
    createGatewayGroupResponse_gatewayGroupArn,
    createGatewayGroupResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_tags,
    createNetworkProfile_certificateAuthorityArn,
    createNetworkProfile_description,
    createNetworkProfile_eapMethod,
    createNetworkProfile_trustAnchors,
    createNetworkProfile_currentPassword,
    createNetworkProfile_nextPassword,
    createNetworkProfile_networkProfileName,
    createNetworkProfile_ssid,
    createNetworkProfile_securityType,
    createNetworkProfile_clientRequestToken,
    createNetworkProfileResponse_networkProfileArn,
    createNetworkProfileResponse_httpStatus,

    -- ** CreateProfile
    createProfile_tags,
    createProfile_setupModeDisabled,
    createProfile_clientRequestToken,
    createProfile_dataRetentionOptIn,
    createProfile_locale,
    createProfile_meetingRoomConfiguration,
    createProfile_pSTNEnabled,
    createProfile_maxVolumeLimit,
    createProfile_profileName,
    createProfile_timezone,
    createProfile_address,
    createProfile_distanceUnit,
    createProfile_temperatureUnit,
    createProfile_wakeWord,
    createProfileResponse_profileArn,
    createProfileResponse_httpStatus,

    -- ** CreateRoom
    createRoom_tags,
    createRoom_clientRequestToken,
    createRoom_profileArn,
    createRoom_description,
    createRoom_providerCalendarId,
    createRoom_roomName,
    createRoomResponse_roomArn,
    createRoomResponse_httpStatus,

    -- ** CreateSkillGroup
    createSkillGroup_tags,
    createSkillGroup_clientRequestToken,
    createSkillGroup_description,
    createSkillGroup_skillGroupName,
    createSkillGroupResponse_skillGroupArn,
    createSkillGroupResponse_httpStatus,

    -- ** CreateUser
    createUser_tags,
    createUser_clientRequestToken,
    createUser_firstName,
    createUser_email,
    createUser_lastName,
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
    disassociateSkillGroupFromRoom_skillGroupArn,
    disassociateSkillGroupFromRoom_roomArn,
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
    getInvitationConfigurationResponse_privateSkillIds,
    getInvitationConfigurationResponse_organizationName,
    getInvitationConfigurationResponse_contactEmail,
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
    listBusinessReportSchedules_nextToken,
    listBusinessReportSchedules_maxResults,
    listBusinessReportSchedulesResponse_businessReportSchedules,
    listBusinessReportSchedulesResponse_nextToken,
    listBusinessReportSchedulesResponse_httpStatus,

    -- ** ListConferenceProviders
    listConferenceProviders_nextToken,
    listConferenceProviders_maxResults,
    listConferenceProvidersResponse_nextToken,
    listConferenceProvidersResponse_conferenceProviders,
    listConferenceProvidersResponse_httpStatus,

    -- ** ListDeviceEvents
    listDeviceEvents_eventType,
    listDeviceEvents_nextToken,
    listDeviceEvents_maxResults,
    listDeviceEvents_deviceArn,
    listDeviceEventsResponse_nextToken,
    listDeviceEventsResponse_deviceEvents,
    listDeviceEventsResponse_httpStatus,

    -- ** ListGatewayGroups
    listGatewayGroups_nextToken,
    listGatewayGroups_maxResults,
    listGatewayGroupsResponse_nextToken,
    listGatewayGroupsResponse_gatewayGroups,
    listGatewayGroupsResponse_httpStatus,

    -- ** ListGateways
    listGateways_nextToken,
    listGateways_gatewayGroupArn,
    listGateways_maxResults,
    listGatewaysResponse_nextToken,
    listGatewaysResponse_gateways,
    listGatewaysResponse_httpStatus,

    -- ** ListSkills
    listSkills_nextToken,
    listSkills_skillGroupArn,
    listSkills_maxResults,
    listSkills_enablementType,
    listSkills_skillType,
    listSkillsResponse_nextToken,
    listSkillsResponse_skillSummaries,
    listSkillsResponse_httpStatus,

    -- ** ListSkillsStoreCategories
    listSkillsStoreCategories_nextToken,
    listSkillsStoreCategories_maxResults,
    listSkillsStoreCategoriesResponse_categoryList,
    listSkillsStoreCategoriesResponse_nextToken,
    listSkillsStoreCategoriesResponse_httpStatus,

    -- ** ListSkillsStoreSkillsByCategory
    listSkillsStoreSkillsByCategory_nextToken,
    listSkillsStoreSkillsByCategory_maxResults,
    listSkillsStoreSkillsByCategory_categoryId,
    listSkillsStoreSkillsByCategoryResponse_nextToken,
    listSkillsStoreSkillsByCategoryResponse_skillsStoreSkills,
    listSkillsStoreSkillsByCategoryResponse_httpStatus,

    -- ** ListSmartHomeAppliances
    listSmartHomeAppliances_nextToken,
    listSmartHomeAppliances_maxResults,
    listSmartHomeAppliances_roomArn,
    listSmartHomeAppliancesResponse_nextToken,
    listSmartHomeAppliancesResponse_smartHomeAppliances,
    listSmartHomeAppliancesResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_arn,
    listTagsResponse_tags,
    listTagsResponse_nextToken,
    listTagsResponse_httpStatus,

    -- ** PutConferencePreference
    putConferencePreference_conferencePreference,
    putConferencePreferenceResponse_httpStatus,

    -- ** PutInvitationConfiguration
    putInvitationConfiguration_privateSkillIds,
    putInvitationConfiguration_contactEmail,
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
    registerAVSDevice_tags,
    registerAVSDevice_deviceSerialNumber,
    registerAVSDevice_roomArn,
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
    resolveRoomResponse_roomSkillParameters,
    resolveRoomResponse_roomName,
    resolveRoomResponse_httpStatus,

    -- ** RevokeInvitation
    revokeInvitation_enrollmentId,
    revokeInvitation_userArn,
    revokeInvitationResponse_httpStatus,

    -- ** SearchAddressBooks
    searchAddressBooks_sortCriteria,
    searchAddressBooks_nextToken,
    searchAddressBooks_filters,
    searchAddressBooks_maxResults,
    searchAddressBooksResponse_nextToken,
    searchAddressBooksResponse_addressBooks,
    searchAddressBooksResponse_totalCount,
    searchAddressBooksResponse_httpStatus,

    -- ** SearchContacts
    searchContacts_sortCriteria,
    searchContacts_nextToken,
    searchContacts_filters,
    searchContacts_maxResults,
    searchContactsResponse_nextToken,
    searchContactsResponse_contacts,
    searchContactsResponse_totalCount,
    searchContactsResponse_httpStatus,

    -- ** SearchDevices
    searchDevices_sortCriteria,
    searchDevices_nextToken,
    searchDevices_filters,
    searchDevices_maxResults,
    searchDevicesResponse_devices,
    searchDevicesResponse_nextToken,
    searchDevicesResponse_totalCount,
    searchDevicesResponse_httpStatus,

    -- ** SearchNetworkProfiles
    searchNetworkProfiles_sortCriteria,
    searchNetworkProfiles_nextToken,
    searchNetworkProfiles_filters,
    searchNetworkProfiles_maxResults,
    searchNetworkProfilesResponse_nextToken,
    searchNetworkProfilesResponse_networkProfiles,
    searchNetworkProfilesResponse_totalCount,
    searchNetworkProfilesResponse_httpStatus,

    -- ** SearchProfiles
    searchProfiles_sortCriteria,
    searchProfiles_nextToken,
    searchProfiles_filters,
    searchProfiles_maxResults,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_profiles,
    searchProfilesResponse_totalCount,
    searchProfilesResponse_httpStatus,

    -- ** SearchRooms
    searchRooms_sortCriteria,
    searchRooms_nextToken,
    searchRooms_filters,
    searchRooms_maxResults,
    searchRoomsResponse_nextToken,
    searchRoomsResponse_rooms,
    searchRoomsResponse_totalCount,
    searchRoomsResponse_httpStatus,

    -- ** SearchSkillGroups
    searchSkillGroups_sortCriteria,
    searchSkillGroups_nextToken,
    searchSkillGroups_filters,
    searchSkillGroups_maxResults,
    searchSkillGroupsResponse_nextToken,
    searchSkillGroupsResponse_skillGroups,
    searchSkillGroupsResponse_totalCount,
    searchSkillGroupsResponse_httpStatus,

    -- ** SearchUsers
    searchUsers_sortCriteria,
    searchUsers_nextToken,
    searchUsers_filters,
    searchUsers_maxResults,
    searchUsersResponse_nextToken,
    searchUsersResponse_users,
    searchUsersResponse_totalCount,
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
    startDeviceSync_roomArn,
    startDeviceSync_deviceArn,
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
    updateAddressBook_name,
    updateAddressBook_description,
    updateAddressBook_addressBookArn,
    updateAddressBookResponse_httpStatus,

    -- ** UpdateBusinessReportSchedule
    updateBusinessReportSchedule_s3KeyPrefix,
    updateBusinessReportSchedule_s3BucketName,
    updateBusinessReportSchedule_format,
    updateBusinessReportSchedule_recurrence,
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
    updateContact_firstName,
    updateContact_sipAddresses,
    updateContact_displayName,
    updateContact_lastName,
    updateContact_phoneNumber,
    updateContact_phoneNumbers,
    updateContact_contactArn,
    updateContactResponse_httpStatus,

    -- ** UpdateDevice
    updateDevice_deviceName,
    updateDevice_deviceArn,
    updateDeviceResponse_httpStatus,

    -- ** UpdateGateway
    updateGateway_name,
    updateGateway_description,
    updateGateway_softwareVersion,
    updateGateway_gatewayArn,
    updateGatewayResponse_httpStatus,

    -- ** UpdateGatewayGroup
    updateGatewayGroup_name,
    updateGatewayGroup_description,
    updateGatewayGroup_gatewayGroupArn,
    updateGatewayGroupResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_certificateAuthorityArn,
    updateNetworkProfile_description,
    updateNetworkProfile_trustAnchors,
    updateNetworkProfile_currentPassword,
    updateNetworkProfile_nextPassword,
    updateNetworkProfile_networkProfileName,
    updateNetworkProfile_networkProfileArn,
    updateNetworkProfileResponse_httpStatus,

    -- ** UpdateProfile
    updateProfile_setupModeDisabled,
    updateProfile_distanceUnit,
    updateProfile_dataRetentionOptIn,
    updateProfile_profileName,
    updateProfile_wakeWord,
    updateProfile_locale,
    updateProfile_meetingRoomConfiguration,
    updateProfile_timezone,
    updateProfile_profileArn,
    updateProfile_pSTNEnabled,
    updateProfile_isDefault,
    updateProfile_address,
    updateProfile_maxVolumeLimit,
    updateProfile_temperatureUnit,
    updateProfileResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_profileArn,
    updateRoom_roomArn,
    updateRoom_description,
    updateRoom_providerCalendarId,
    updateRoom_roomName,
    updateRoomResponse_httpStatus,

    -- ** UpdateSkillGroup
    updateSkillGroup_skillGroupArn,
    updateSkillGroup_description,
    updateSkillGroup_skillGroupName,
    updateSkillGroupResponse_httpStatus,

    -- * Types

    -- ** AddressBook
    addressBook_name,
    addressBook_description,
    addressBook_addressBookArn,

    -- ** AddressBookData
    addressBookData_name,
    addressBookData_description,
    addressBookData_addressBookArn,

    -- ** Audio
    audio_locale,
    audio_location,

    -- ** BusinessReport
    businessReport_deliveryTime,
    businessReport_failureCode,
    businessReport_downloadUrl,
    businessReport_status,
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
    businessReportSchedule_s3BucketName,
    businessReportSchedule_format,
    businessReportSchedule_scheduleArn,
    businessReportSchedule_contentRange,
    businessReportSchedule_recurrence,
    businessReportSchedule_scheduleName,
    businessReportSchedule_lastBusinessReport,

    -- ** Category
    category_categoryId,
    category_categoryName,

    -- ** ConferencePreference
    conferencePreference_defaultConferenceProviderArn,

    -- ** ConferenceProvider
    conferenceProvider_iPDialIn,
    conferenceProvider_name,
    conferenceProvider_type,
    conferenceProvider_meetingSetting,
    conferenceProvider_pSTNDialIn,
    conferenceProvider_arn,

    -- ** Contact
    contact_firstName,
    contact_sipAddresses,
    contact_displayName,
    contact_contactArn,
    contact_lastName,
    contact_phoneNumber,
    contact_phoneNumbers,

    -- ** ContactData
    contactData_firstName,
    contactData_sipAddresses,
    contactData_displayName,
    contactData_contactArn,
    contactData_lastName,
    contactData_phoneNumber,
    contactData_phoneNumbers,

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
    createMeetingRoomConfiguration_roomUtilizationMetricsEnabled,
    createMeetingRoomConfiguration_requireCheckIn,

    -- ** CreateRequireCheckIn
    createRequireCheckIn_releaseAfterMinutes,
    createRequireCheckIn_enabled,

    -- ** DeveloperInfo
    developerInfo_email,
    developerInfo_developerName,
    developerInfo_url,
    developerInfo_privacyPolicy,

    -- ** Device
    device_deviceSerialNumber,
    device_deviceName,
    device_deviceStatusInfo,
    device_roomArn,
    device_softwareVersion,
    device_macAddress,
    device_networkProfileInfo,
    device_deviceStatus,
    device_deviceArn,
    device_deviceType,

    -- ** DeviceData
    deviceData_createdTime,
    deviceData_deviceSerialNumber,
    deviceData_deviceName,
    deviceData_deviceStatusInfo,
    deviceData_roomArn,
    deviceData_softwareVersion,
    deviceData_macAddress,
    deviceData_deviceStatus,
    deviceData_deviceArn,
    deviceData_deviceType,
    deviceData_networkProfileArn,
    deviceData_roomName,
    deviceData_networkProfileName,

    -- ** DeviceEvent
    deviceEvent_type,
    deviceEvent_timestamp,
    deviceEvent_value,

    -- ** DeviceNetworkProfileInfo
    deviceNetworkProfileInfo_certificateArn,
    deviceNetworkProfileInfo_certificateExpirationTime,
    deviceNetworkProfileInfo_networkProfileArn,

    -- ** DeviceStatusDetail
    deviceStatusDetail_code,
    deviceStatusDetail_feature,

    -- ** DeviceStatusInfo
    deviceStatusInfo_connectionStatusUpdatedTime,
    deviceStatusInfo_deviceStatusDetails,
    deviceStatusInfo_connectionStatus,

    -- ** EndOfMeetingReminder
    endOfMeetingReminder_reminderType,
    endOfMeetingReminder_enabled,
    endOfMeetingReminder_reminderAtMinutes,

    -- ** Filter
    filter_key,
    filter_values,

    -- ** Gateway
    gateway_name,
    gateway_gatewayGroupArn,
    gateway_arn,
    gateway_description,
    gateway_softwareVersion,

    -- ** GatewayGroup
    gatewayGroup_name,
    gatewayGroup_arn,
    gatewayGroup_description,

    -- ** GatewayGroupSummary
    gatewayGroupSummary_name,
    gatewayGroupSummary_arn,
    gatewayGroupSummary_description,

    -- ** GatewaySummary
    gatewaySummary_name,
    gatewaySummary_gatewayGroupArn,
    gatewaySummary_arn,
    gatewaySummary_description,
    gatewaySummary_softwareVersion,

    -- ** IPDialIn
    iPDialIn_endpoint,
    iPDialIn_commsProtocol,

    -- ** InstantBooking
    instantBooking_enabled,
    instantBooking_durationInMinutes,

    -- ** MeetingRoomConfiguration
    meetingRoomConfiguration_instantBooking,
    meetingRoomConfiguration_endOfMeetingReminder,
    meetingRoomConfiguration_roomUtilizationMetricsEnabled,
    meetingRoomConfiguration_requireCheckIn,

    -- ** MeetingSetting
    meetingSetting_requirePin,

    -- ** NetworkProfile
    networkProfile_certificateAuthorityArn,
    networkProfile_description,
    networkProfile_eapMethod,
    networkProfile_trustAnchors,
    networkProfile_currentPassword,
    networkProfile_nextPassword,
    networkProfile_ssid,
    networkProfile_securityType,
    networkProfile_networkProfileArn,
    networkProfile_networkProfileName,

    -- ** NetworkProfileData
    networkProfileData_certificateAuthorityArn,
    networkProfileData_description,
    networkProfileData_eapMethod,
    networkProfileData_ssid,
    networkProfileData_securityType,
    networkProfileData_networkProfileArn,
    networkProfileData_networkProfileName,

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
    profile_distanceUnit,
    profile_dataRetentionOptIn,
    profile_profileName,
    profile_wakeWord,
    profile_locale,
    profile_meetingRoomConfiguration,
    profile_timezone,
    profile_profileArn,
    profile_pSTNEnabled,
    profile_isDefault,
    profile_address,
    profile_addressBookArn,
    profile_maxVolumeLimit,
    profile_temperatureUnit,

    -- ** ProfileData
    profileData_distanceUnit,
    profileData_profileName,
    profileData_wakeWord,
    profileData_locale,
    profileData_timezone,
    profileData_profileArn,
    profileData_isDefault,
    profileData_address,
    profileData_temperatureUnit,

    -- ** RequireCheckIn
    requireCheckIn_enabled,
    requireCheckIn_releaseAfterMinutes,

    -- ** Room
    room_profileArn,
    room_roomArn,
    room_description,
    room_providerCalendarId,
    room_roomName,

    -- ** RoomData
    roomData_profileName,
    roomData_profileArn,
    roomData_roomArn,
    roomData_description,
    roomData_providerCalendarId,
    roomData_roomName,

    -- ** RoomSkillParameter
    roomSkillParameter_parameterKey,
    roomSkillParameter_parameterValue,

    -- ** SipAddress
    sipAddress_uri,
    sipAddress_type,

    -- ** SkillDetails
    skillDetails_invocationPhrase,
    skillDetails_endUserLicenseAgreement,
    skillDetails_reviews,
    skillDetails_releaseDate,
    skillDetails_genericKeywords,
    skillDetails_developerInfo,
    skillDetails_newInThisVersionBulletPoints,
    skillDetails_productDescription,
    skillDetails_skillTypes,
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
    skillSummary_skillName,
    skillSummary_supportsLinking,
    skillSummary_enablementType,
    skillSummary_skillType,
    skillSummary_skillId,

    -- ** SkillsStoreSkill
    skillsStoreSkill_sampleUtterances,
    skillsStoreSkill_shortDescription,
    skillsStoreSkill_skillName,
    skillsStoreSkill_iconUrl,
    skillsStoreSkill_supportsLinking,
    skillsStoreSkill_skillDetails,
    skillsStoreSkill_skillId,

    -- ** SmartHomeAppliance
    smartHomeAppliance_manufacturerName,
    smartHomeAppliance_description,
    smartHomeAppliance_friendlyName,

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
    updateEndOfMeetingReminder_enabled,
    updateEndOfMeetingReminder_reminderAtMinutes,

    -- ** UpdateInstantBooking
    updateInstantBooking_enabled,
    updateInstantBooking_durationInMinutes,

    -- ** UpdateMeetingRoomConfiguration
    updateMeetingRoomConfiguration_instantBooking,
    updateMeetingRoomConfiguration_endOfMeetingReminder,
    updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled,
    updateMeetingRoomConfiguration_requireCheckIn,

    -- ** UpdateRequireCheckIn
    updateRequireCheckIn_enabled,
    updateRequireCheckIn_releaseAfterMinutes,

    -- ** UserData
    userData_firstName,
    userData_email,
    userData_enrollmentStatus,
    userData_lastName,
    userData_enrollmentId,
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

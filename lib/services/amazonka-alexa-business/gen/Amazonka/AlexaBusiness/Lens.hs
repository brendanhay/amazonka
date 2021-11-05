{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AlexaBusiness.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Lens
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

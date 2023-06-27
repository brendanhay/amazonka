{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkVoice.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Lens
  ( -- * Operations

    -- ** AssociatePhoneNumbersWithVoiceConnector
    associatePhoneNumbersWithVoiceConnector_forceAssociate,
    associatePhoneNumbersWithVoiceConnector_voiceConnectorId,
    associatePhoneNumbersWithVoiceConnector_e164PhoneNumbers,
    associatePhoneNumbersWithVoiceConnectorResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorResponse_httpStatus,

    -- ** AssociatePhoneNumbersWithVoiceConnectorGroup
    associatePhoneNumbersWithVoiceConnectorGroup_forceAssociate,
    associatePhoneNumbersWithVoiceConnectorGroup_voiceConnectorGroupId,
    associatePhoneNumbersWithVoiceConnectorGroup_e164PhoneNumbers,
    associatePhoneNumbersWithVoiceConnectorGroupResponse_phoneNumberErrors,
    associatePhoneNumbersWithVoiceConnectorGroupResponse_httpStatus,

    -- ** BatchDeletePhoneNumber
    batchDeletePhoneNumber_phoneNumberIds,
    batchDeletePhoneNumberResponse_phoneNumberErrors,
    batchDeletePhoneNumberResponse_httpStatus,

    -- ** BatchUpdatePhoneNumber
    batchUpdatePhoneNumber_updatePhoneNumberRequestItems,
    batchUpdatePhoneNumberResponse_phoneNumberErrors,
    batchUpdatePhoneNumberResponse_httpStatus,

    -- ** CreatePhoneNumberOrder
    createPhoneNumberOrder_productType,
    createPhoneNumberOrder_e164PhoneNumbers,
    createPhoneNumberOrderResponse_phoneNumberOrder,
    createPhoneNumberOrderResponse_httpStatus,

    -- ** CreateProxySession
    createProxySession_expiryMinutes,
    createProxySession_geoMatchLevel,
    createProxySession_geoMatchParams,
    createProxySession_name,
    createProxySession_numberSelectionBehavior,
    createProxySession_participantPhoneNumbers,
    createProxySession_capabilities,
    createProxySession_voiceConnectorId,
    createProxySessionResponse_proxySession,
    createProxySessionResponse_httpStatus,

    -- ** CreateSipMediaApplication
    createSipMediaApplication_tags,
    createSipMediaApplication_awsRegion,
    createSipMediaApplication_name,
    createSipMediaApplication_endpoints,
    createSipMediaApplicationResponse_sipMediaApplication,
    createSipMediaApplicationResponse_httpStatus,

    -- ** CreateSipMediaApplicationCall
    createSipMediaApplicationCall_argumentsMap,
    createSipMediaApplicationCall_sipHeaders,
    createSipMediaApplicationCall_fromPhoneNumber,
    createSipMediaApplicationCall_toPhoneNumber,
    createSipMediaApplicationCall_sipMediaApplicationId,
    createSipMediaApplicationCallResponse_sipMediaApplicationCall,
    createSipMediaApplicationCallResponse_httpStatus,

    -- ** CreateSipRule
    createSipRule_disabled,
    createSipRule_targetApplications,
    createSipRule_name,
    createSipRule_triggerType,
    createSipRule_triggerValue,
    createSipRuleResponse_sipRule,
    createSipRuleResponse_httpStatus,

    -- ** CreateVoiceConnector
    createVoiceConnector_awsRegion,
    createVoiceConnector_tags,
    createVoiceConnector_name,
    createVoiceConnector_requireEncryption,
    createVoiceConnectorResponse_voiceConnector,
    createVoiceConnectorResponse_httpStatus,

    -- ** CreateVoiceConnectorGroup
    createVoiceConnectorGroup_voiceConnectorItems,
    createVoiceConnectorGroup_name,
    createVoiceConnectorGroupResponse_voiceConnectorGroup,
    createVoiceConnectorGroupResponse_httpStatus,

    -- ** CreateVoiceProfile
    createVoiceProfile_speakerSearchTaskId,
    createVoiceProfileResponse_voiceProfile,
    createVoiceProfileResponse_httpStatus,

    -- ** CreateVoiceProfileDomain
    createVoiceProfileDomain_clientRequestToken,
    createVoiceProfileDomain_description,
    createVoiceProfileDomain_tags,
    createVoiceProfileDomain_name,
    createVoiceProfileDomain_serverSideEncryptionConfiguration,
    createVoiceProfileDomainResponse_voiceProfileDomain,
    createVoiceProfileDomainResponse_httpStatus,

    -- ** DeletePhoneNumber
    deletePhoneNumber_phoneNumberId,

    -- ** DeleteProxySession
    deleteProxySession_voiceConnectorId,
    deleteProxySession_proxySessionId,

    -- ** DeleteSipMediaApplication
    deleteSipMediaApplication_sipMediaApplicationId,

    -- ** DeleteSipRule
    deleteSipRule_sipRuleId,

    -- ** DeleteVoiceConnector
    deleteVoiceConnector_voiceConnectorId,

    -- ** DeleteVoiceConnectorEmergencyCallingConfiguration
    deleteVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,

    -- ** DeleteVoiceConnectorGroup
    deleteVoiceConnectorGroup_voiceConnectorGroupId,

    -- ** DeleteVoiceConnectorOrigination
    deleteVoiceConnectorOrigination_voiceConnectorId,

    -- ** DeleteVoiceConnectorProxy
    deleteVoiceConnectorProxy_voiceConnectorId,

    -- ** DeleteVoiceConnectorStreamingConfiguration
    deleteVoiceConnectorStreamingConfiguration_voiceConnectorId,

    -- ** DeleteVoiceConnectorTermination
    deleteVoiceConnectorTermination_voiceConnectorId,

    -- ** DeleteVoiceConnectorTerminationCredentials
    deleteVoiceConnectorTerminationCredentials_usernames,
    deleteVoiceConnectorTerminationCredentials_voiceConnectorId,

    -- ** DeleteVoiceProfile
    deleteVoiceProfile_voiceProfileId,

    -- ** DeleteVoiceProfileDomain
    deleteVoiceProfileDomain_voiceProfileDomainId,

    -- ** DisassociatePhoneNumbersFromVoiceConnector
    disassociatePhoneNumbersFromVoiceConnector_voiceConnectorId,
    disassociatePhoneNumbersFromVoiceConnector_e164PhoneNumbers,
    disassociatePhoneNumbersFromVoiceConnectorResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorResponse_httpStatus,

    -- ** DisassociatePhoneNumbersFromVoiceConnectorGroup
    disassociatePhoneNumbersFromVoiceConnectorGroup_voiceConnectorGroupId,
    disassociatePhoneNumbersFromVoiceConnectorGroup_e164PhoneNumbers,
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_phoneNumberErrors,
    disassociatePhoneNumbersFromVoiceConnectorGroupResponse_httpStatus,

    -- ** GetGlobalSettings
    getGlobalSettingsResponse_voiceConnector,
    getGlobalSettingsResponse_httpStatus,

    -- ** GetPhoneNumber
    getPhoneNumber_phoneNumberId,
    getPhoneNumberResponse_phoneNumber,
    getPhoneNumberResponse_httpStatus,

    -- ** GetPhoneNumberOrder
    getPhoneNumberOrder_phoneNumberOrderId,
    getPhoneNumberOrderResponse_phoneNumberOrder,
    getPhoneNumberOrderResponse_httpStatus,

    -- ** GetPhoneNumberSettings
    getPhoneNumberSettingsResponse_callingName,
    getPhoneNumberSettingsResponse_callingNameUpdatedTimestamp,
    getPhoneNumberSettingsResponse_httpStatus,

    -- ** GetProxySession
    getProxySession_voiceConnectorId,
    getProxySession_proxySessionId,
    getProxySessionResponse_proxySession,
    getProxySessionResponse_httpStatus,

    -- ** GetSipMediaApplication
    getSipMediaApplication_sipMediaApplicationId,
    getSipMediaApplicationResponse_sipMediaApplication,
    getSipMediaApplicationResponse_httpStatus,

    -- ** GetSipMediaApplicationAlexaSkillConfiguration
    getSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId,
    getSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration,
    getSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus,

    -- ** GetSipMediaApplicationLoggingConfiguration
    getSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,
    getSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    getSipMediaApplicationLoggingConfigurationResponse_httpStatus,

    -- ** GetSipRule
    getSipRule_sipRuleId,
    getSipRuleResponse_sipRule,
    getSipRuleResponse_httpStatus,

    -- ** GetSpeakerSearchTask
    getSpeakerSearchTask_voiceConnectorId,
    getSpeakerSearchTask_speakerSearchTaskId,
    getSpeakerSearchTaskResponse_speakerSearchTask,
    getSpeakerSearchTaskResponse_httpStatus,

    -- ** GetVoiceConnector
    getVoiceConnector_voiceConnectorId,
    getVoiceConnectorResponse_voiceConnector,
    getVoiceConnectorResponse_httpStatus,

    -- ** GetVoiceConnectorEmergencyCallingConfiguration
    getVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,
    getVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration,
    getVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus,

    -- ** GetVoiceConnectorGroup
    getVoiceConnectorGroup_voiceConnectorGroupId,
    getVoiceConnectorGroupResponse_voiceConnectorGroup,
    getVoiceConnectorGroupResponse_httpStatus,

    -- ** GetVoiceConnectorLoggingConfiguration
    getVoiceConnectorLoggingConfiguration_voiceConnectorId,
    getVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    getVoiceConnectorLoggingConfigurationResponse_httpStatus,

    -- ** GetVoiceConnectorOrigination
    getVoiceConnectorOrigination_voiceConnectorId,
    getVoiceConnectorOriginationResponse_origination,
    getVoiceConnectorOriginationResponse_httpStatus,

    -- ** GetVoiceConnectorProxy
    getVoiceConnectorProxy_voiceConnectorId,
    getVoiceConnectorProxyResponse_proxy,
    getVoiceConnectorProxyResponse_httpStatus,

    -- ** GetVoiceConnectorStreamingConfiguration
    getVoiceConnectorStreamingConfiguration_voiceConnectorId,
    getVoiceConnectorStreamingConfigurationResponse_streamingConfiguration,
    getVoiceConnectorStreamingConfigurationResponse_httpStatus,

    -- ** GetVoiceConnectorTermination
    getVoiceConnectorTermination_voiceConnectorId,
    getVoiceConnectorTerminationResponse_termination,
    getVoiceConnectorTerminationResponse_httpStatus,

    -- ** GetVoiceConnectorTerminationHealth
    getVoiceConnectorTerminationHealth_voiceConnectorId,
    getVoiceConnectorTerminationHealthResponse_terminationHealth,
    getVoiceConnectorTerminationHealthResponse_httpStatus,

    -- ** GetVoiceProfile
    getVoiceProfile_voiceProfileId,
    getVoiceProfileResponse_voiceProfile,
    getVoiceProfileResponse_httpStatus,

    -- ** GetVoiceProfileDomain
    getVoiceProfileDomain_voiceProfileDomainId,
    getVoiceProfileDomainResponse_voiceProfileDomain,
    getVoiceProfileDomainResponse_httpStatus,

    -- ** GetVoiceToneAnalysisTask
    getVoiceToneAnalysisTask_voiceConnectorId,
    getVoiceToneAnalysisTask_voiceToneAnalysisTaskId,
    getVoiceToneAnalysisTask_isCaller,
    getVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask,
    getVoiceToneAnalysisTaskResponse_httpStatus,

    -- ** ListAvailableVoiceConnectorRegions
    listAvailableVoiceConnectorRegionsResponse_voiceConnectorRegions,
    listAvailableVoiceConnectorRegionsResponse_httpStatus,

    -- ** ListPhoneNumberOrders
    listPhoneNumberOrders_maxResults,
    listPhoneNumberOrders_nextToken,
    listPhoneNumberOrdersResponse_nextToken,
    listPhoneNumberOrdersResponse_phoneNumberOrders,
    listPhoneNumberOrdersResponse_httpStatus,

    -- ** ListPhoneNumbers
    listPhoneNumbers_filterName,
    listPhoneNumbers_filterValue,
    listPhoneNumbers_maxResults,
    listPhoneNumbers_nextToken,
    listPhoneNumbers_productType,
    listPhoneNumbers_status,
    listPhoneNumbersResponse_nextToken,
    listPhoneNumbersResponse_phoneNumbers,
    listPhoneNumbersResponse_httpStatus,

    -- ** ListProxySessions
    listProxySessions_maxResults,
    listProxySessions_nextToken,
    listProxySessions_status,
    listProxySessions_voiceConnectorId,
    listProxySessionsResponse_nextToken,
    listProxySessionsResponse_proxySessions,
    listProxySessionsResponse_httpStatus,

    -- ** ListSipMediaApplications
    listSipMediaApplications_maxResults,
    listSipMediaApplications_nextToken,
    listSipMediaApplicationsResponse_nextToken,
    listSipMediaApplicationsResponse_sipMediaApplications,
    listSipMediaApplicationsResponse_httpStatus,

    -- ** ListSipRules
    listSipRules_maxResults,
    listSipRules_nextToken,
    listSipRules_sipMediaApplicationId,
    listSipRulesResponse_nextToken,
    listSipRulesResponse_sipRules,
    listSipRulesResponse_httpStatus,

    -- ** ListSupportedPhoneNumberCountries
    listSupportedPhoneNumberCountries_productType,
    listSupportedPhoneNumberCountriesResponse_phoneNumberCountries,
    listSupportedPhoneNumberCountriesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListVoiceConnectorGroups
    listVoiceConnectorGroups_maxResults,
    listVoiceConnectorGroups_nextToken,
    listVoiceConnectorGroupsResponse_nextToken,
    listVoiceConnectorGroupsResponse_voiceConnectorGroups,
    listVoiceConnectorGroupsResponse_httpStatus,

    -- ** ListVoiceConnectorTerminationCredentials
    listVoiceConnectorTerminationCredentials_voiceConnectorId,
    listVoiceConnectorTerminationCredentialsResponse_usernames,
    listVoiceConnectorTerminationCredentialsResponse_httpStatus,

    -- ** ListVoiceConnectors
    listVoiceConnectors_maxResults,
    listVoiceConnectors_nextToken,
    listVoiceConnectorsResponse_nextToken,
    listVoiceConnectorsResponse_voiceConnectors,
    listVoiceConnectorsResponse_httpStatus,

    -- ** ListVoiceProfileDomains
    listVoiceProfileDomains_maxResults,
    listVoiceProfileDomains_nextToken,
    listVoiceProfileDomainsResponse_nextToken,
    listVoiceProfileDomainsResponse_voiceProfileDomains,
    listVoiceProfileDomainsResponse_httpStatus,

    -- ** ListVoiceProfiles
    listVoiceProfiles_maxResults,
    listVoiceProfiles_nextToken,
    listVoiceProfiles_voiceProfileDomainId,
    listVoiceProfilesResponse_nextToken,
    listVoiceProfilesResponse_voiceProfiles,
    listVoiceProfilesResponse_httpStatus,

    -- ** PutSipMediaApplicationAlexaSkillConfiguration
    putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationAlexaSkillConfiguration,
    putSipMediaApplicationAlexaSkillConfiguration_sipMediaApplicationId,
    putSipMediaApplicationAlexaSkillConfigurationResponse_sipMediaApplicationAlexaSkillConfiguration,
    putSipMediaApplicationAlexaSkillConfigurationResponse_httpStatus,

    -- ** PutSipMediaApplicationLoggingConfiguration
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfiguration_sipMediaApplicationId,
    putSipMediaApplicationLoggingConfigurationResponse_sipMediaApplicationLoggingConfiguration,
    putSipMediaApplicationLoggingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorEmergencyCallingConfiguration
    putVoiceConnectorEmergencyCallingConfiguration_voiceConnectorId,
    putVoiceConnectorEmergencyCallingConfiguration_emergencyCallingConfiguration,
    putVoiceConnectorEmergencyCallingConfigurationResponse_emergencyCallingConfiguration,
    putVoiceConnectorEmergencyCallingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorLoggingConfiguration
    putVoiceConnectorLoggingConfiguration_voiceConnectorId,
    putVoiceConnectorLoggingConfiguration_loggingConfiguration,
    putVoiceConnectorLoggingConfigurationResponse_loggingConfiguration,
    putVoiceConnectorLoggingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorOrigination
    putVoiceConnectorOrigination_voiceConnectorId,
    putVoiceConnectorOrigination_origination,
    putVoiceConnectorOriginationResponse_origination,
    putVoiceConnectorOriginationResponse_httpStatus,

    -- ** PutVoiceConnectorProxy
    putVoiceConnectorProxy_disabled,
    putVoiceConnectorProxy_fallBackPhoneNumber,
    putVoiceConnectorProxy_defaultSessionExpiryMinutes,
    putVoiceConnectorProxy_phoneNumberPoolCountries,
    putVoiceConnectorProxy_voiceConnectorId,
    putVoiceConnectorProxyResponse_proxy,
    putVoiceConnectorProxyResponse_httpStatus,

    -- ** PutVoiceConnectorStreamingConfiguration
    putVoiceConnectorStreamingConfiguration_voiceConnectorId,
    putVoiceConnectorStreamingConfiguration_streamingConfiguration,
    putVoiceConnectorStreamingConfigurationResponse_streamingConfiguration,
    putVoiceConnectorStreamingConfigurationResponse_httpStatus,

    -- ** PutVoiceConnectorTermination
    putVoiceConnectorTermination_voiceConnectorId,
    putVoiceConnectorTermination_termination,
    putVoiceConnectorTerminationResponse_termination,
    putVoiceConnectorTerminationResponse_httpStatus,

    -- ** PutVoiceConnectorTerminationCredentials
    putVoiceConnectorTerminationCredentials_credentials,
    putVoiceConnectorTerminationCredentials_voiceConnectorId,

    -- ** RestorePhoneNumber
    restorePhoneNumber_phoneNumberId,
    restorePhoneNumberResponse_phoneNumber,
    restorePhoneNumberResponse_httpStatus,

    -- ** SearchAvailablePhoneNumbers
    searchAvailablePhoneNumbers_areaCode,
    searchAvailablePhoneNumbers_city,
    searchAvailablePhoneNumbers_country,
    searchAvailablePhoneNumbers_maxResults,
    searchAvailablePhoneNumbers_nextToken,
    searchAvailablePhoneNumbers_phoneNumberType,
    searchAvailablePhoneNumbers_state,
    searchAvailablePhoneNumbers_tollFreePrefix,
    searchAvailablePhoneNumbersResponse_e164PhoneNumbers,
    searchAvailablePhoneNumbersResponse_nextToken,
    searchAvailablePhoneNumbersResponse_httpStatus,

    -- ** StartSpeakerSearchTask
    startSpeakerSearchTask_callLeg,
    startSpeakerSearchTask_clientRequestToken,
    startSpeakerSearchTask_voiceConnectorId,
    startSpeakerSearchTask_transactionId,
    startSpeakerSearchTask_voiceProfileDomainId,
    startSpeakerSearchTaskResponse_speakerSearchTask,
    startSpeakerSearchTaskResponse_httpStatus,

    -- ** StartVoiceToneAnalysisTask
    startVoiceToneAnalysisTask_clientRequestToken,
    startVoiceToneAnalysisTask_voiceConnectorId,
    startVoiceToneAnalysisTask_transactionId,
    startVoiceToneAnalysisTask_languageCode,
    startVoiceToneAnalysisTaskResponse_voiceToneAnalysisTask,
    startVoiceToneAnalysisTaskResponse_httpStatus,

    -- ** StopSpeakerSearchTask
    stopSpeakerSearchTask_voiceConnectorId,
    stopSpeakerSearchTask_speakerSearchTaskId,

    -- ** StopVoiceToneAnalysisTask
    stopVoiceToneAnalysisTask_voiceConnectorId,
    stopVoiceToneAnalysisTask_voiceToneAnalysisTaskId,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_voiceConnector,

    -- ** UpdatePhoneNumber
    updatePhoneNumber_callingName,
    updatePhoneNumber_productType,
    updatePhoneNumber_phoneNumberId,
    updatePhoneNumberResponse_phoneNumber,
    updatePhoneNumberResponse_httpStatus,

    -- ** UpdatePhoneNumberSettings
    updatePhoneNumberSettings_callingName,

    -- ** UpdateProxySession
    updateProxySession_expiryMinutes,
    updateProxySession_capabilities,
    updateProxySession_voiceConnectorId,
    updateProxySession_proxySessionId,
    updateProxySessionResponse_proxySession,
    updateProxySessionResponse_httpStatus,

    -- ** UpdateSipMediaApplication
    updateSipMediaApplication_endpoints,
    updateSipMediaApplication_name,
    updateSipMediaApplication_sipMediaApplicationId,
    updateSipMediaApplicationResponse_sipMediaApplication,
    updateSipMediaApplicationResponse_httpStatus,

    -- ** UpdateSipMediaApplicationCall
    updateSipMediaApplicationCall_sipMediaApplicationId,
    updateSipMediaApplicationCall_transactionId,
    updateSipMediaApplicationCall_arguments,
    updateSipMediaApplicationCallResponse_sipMediaApplicationCall,
    updateSipMediaApplicationCallResponse_httpStatus,

    -- ** UpdateSipRule
    updateSipRule_disabled,
    updateSipRule_targetApplications,
    updateSipRule_sipRuleId,
    updateSipRule_name,
    updateSipRuleResponse_sipRule,
    updateSipRuleResponse_httpStatus,

    -- ** UpdateVoiceConnector
    updateVoiceConnector_voiceConnectorId,
    updateVoiceConnector_name,
    updateVoiceConnector_requireEncryption,
    updateVoiceConnectorResponse_voiceConnector,
    updateVoiceConnectorResponse_httpStatus,

    -- ** UpdateVoiceConnectorGroup
    updateVoiceConnectorGroup_voiceConnectorGroupId,
    updateVoiceConnectorGroup_name,
    updateVoiceConnectorGroup_voiceConnectorItems,
    updateVoiceConnectorGroupResponse_voiceConnectorGroup,
    updateVoiceConnectorGroupResponse_httpStatus,

    -- ** UpdateVoiceProfile
    updateVoiceProfile_voiceProfileId,
    updateVoiceProfile_speakerSearchTaskId,
    updateVoiceProfileResponse_voiceProfile,
    updateVoiceProfileResponse_httpStatus,

    -- ** UpdateVoiceProfileDomain
    updateVoiceProfileDomain_description,
    updateVoiceProfileDomain_name,
    updateVoiceProfileDomain_voiceProfileDomainId,
    updateVoiceProfileDomainResponse_voiceProfileDomain,
    updateVoiceProfileDomainResponse_httpStatus,

    -- ** ValidateE911Address
    validateE911Address_awsAccountId,
    validateE911Address_streetNumber,
    validateE911Address_streetInfo,
    validateE911Address_city,
    validateE911Address_state,
    validateE911Address_country,
    validateE911Address_postalCode,
    validateE911AddressResponse_address,
    validateE911AddressResponse_addressExternalId,
    validateE911AddressResponse_candidateAddressList,
    validateE911AddressResponse_validationResult,
    validateE911AddressResponse_httpStatus,

    -- * Types

    -- ** Address
    address_city,
    address_country,
    address_postDirectional,
    address_postalCode,
    address_postalCodePlus4,
    address_preDirectional,
    address_state,
    address_streetName,
    address_streetNumber,
    address_streetSuffix,

    -- ** CallDetails
    callDetails_isCaller,
    callDetails_transactionId,
    callDetails_voiceConnectorId,

    -- ** CandidateAddress
    candidateAddress_city,
    candidateAddress_country,
    candidateAddress_postalCode,
    candidateAddress_postalCodePlus4,
    candidateAddress_state,
    candidateAddress_streetInfo,
    candidateAddress_streetNumber,

    -- ** Credential
    credential_password,
    credential_username,

    -- ** DNISEmergencyCallingConfiguration
    dNISEmergencyCallingConfiguration_testPhoneNumber,
    dNISEmergencyCallingConfiguration_emergencyPhoneNumber,
    dNISEmergencyCallingConfiguration_callingCountry,

    -- ** EmergencyCallingConfiguration
    emergencyCallingConfiguration_dnis,

    -- ** GeoMatchParams
    geoMatchParams_country,
    geoMatchParams_areaCode,

    -- ** LoggingConfiguration
    loggingConfiguration_enableMediaMetricLogs,
    loggingConfiguration_enableSIPLogs,

    -- ** MediaInsightsConfiguration
    mediaInsightsConfiguration_configurationArn,
    mediaInsightsConfiguration_disabled,

    -- ** OrderedPhoneNumber
    orderedPhoneNumber_e164PhoneNumber,
    orderedPhoneNumber_status,

    -- ** Origination
    origination_disabled,
    origination_routes,

    -- ** OriginationRoute
    originationRoute_host,
    originationRoute_port,
    originationRoute_priority,
    originationRoute_protocol,
    originationRoute_weight,

    -- ** Participant
    participant_phoneNumber,
    participant_proxyPhoneNumber,

    -- ** PhoneNumber
    phoneNumber_associations,
    phoneNumber_callingName,
    phoneNumber_callingNameStatus,
    phoneNumber_capabilities,
    phoneNumber_country,
    phoneNumber_createdTimestamp,
    phoneNumber_deletionTimestamp,
    phoneNumber_e164PhoneNumber,
    phoneNumber_orderId,
    phoneNumber_phoneNumberId,
    phoneNumber_productType,
    phoneNumber_status,
    phoneNumber_type,
    phoneNumber_updatedTimestamp,

    -- ** PhoneNumberAssociation
    phoneNumberAssociation_associatedTimestamp,
    phoneNumberAssociation_name,
    phoneNumberAssociation_value,

    -- ** PhoneNumberCapabilities
    phoneNumberCapabilities_inboundCall,
    phoneNumberCapabilities_inboundMMS,
    phoneNumberCapabilities_inboundSMS,
    phoneNumberCapabilities_outboundCall,
    phoneNumberCapabilities_outboundMMS,
    phoneNumberCapabilities_outboundSMS,

    -- ** PhoneNumberCountry
    phoneNumberCountry_countryCode,
    phoneNumberCountry_supportedPhoneNumberTypes,

    -- ** PhoneNumberError
    phoneNumberError_errorCode,
    phoneNumberError_errorMessage,
    phoneNumberError_phoneNumberId,

    -- ** PhoneNumberOrder
    phoneNumberOrder_createdTimestamp,
    phoneNumberOrder_orderType,
    phoneNumberOrder_orderedPhoneNumbers,
    phoneNumberOrder_phoneNumberOrderId,
    phoneNumberOrder_productType,
    phoneNumberOrder_status,
    phoneNumberOrder_updatedTimestamp,

    -- ** Proxy
    proxy_defaultSessionExpiryMinutes,
    proxy_disabled,
    proxy_fallBackPhoneNumber,
    proxy_phoneNumberCountries,

    -- ** ProxySession
    proxySession_capabilities,
    proxySession_createdTimestamp,
    proxySession_endedTimestamp,
    proxySession_expiryMinutes,
    proxySession_geoMatchLevel,
    proxySession_geoMatchParams,
    proxySession_name,
    proxySession_numberSelectionBehavior,
    proxySession_participants,
    proxySession_proxySessionId,
    proxySession_status,
    proxySession_updatedTimestamp,
    proxySession_voiceConnectorId,

    -- ** ServerSideEncryptionConfiguration
    serverSideEncryptionConfiguration_kmsKeyArn,

    -- ** SipMediaApplication
    sipMediaApplication_awsRegion,
    sipMediaApplication_createdTimestamp,
    sipMediaApplication_endpoints,
    sipMediaApplication_name,
    sipMediaApplication_sipMediaApplicationArn,
    sipMediaApplication_sipMediaApplicationId,
    sipMediaApplication_updatedTimestamp,

    -- ** SipMediaApplicationAlexaSkillConfiguration
    sipMediaApplicationAlexaSkillConfiguration_alexaSkillStatus,
    sipMediaApplicationAlexaSkillConfiguration_alexaSkillIds,

    -- ** SipMediaApplicationCall
    sipMediaApplicationCall_transactionId,

    -- ** SipMediaApplicationEndpoint
    sipMediaApplicationEndpoint_lambdaArn,

    -- ** SipMediaApplicationLoggingConfiguration
    sipMediaApplicationLoggingConfiguration_enableSipMediaApplicationMessageLogs,

    -- ** SipRule
    sipRule_createdTimestamp,
    sipRule_disabled,
    sipRule_name,
    sipRule_sipRuleId,
    sipRule_targetApplications,
    sipRule_triggerType,
    sipRule_triggerValue,
    sipRule_updatedTimestamp,

    -- ** SipRuleTargetApplication
    sipRuleTargetApplication_awsRegion,
    sipRuleTargetApplication_priority,
    sipRuleTargetApplication_sipMediaApplicationId,

    -- ** SpeakerSearchDetails
    speakerSearchDetails_results,
    speakerSearchDetails_voiceprintGenerationStatus,

    -- ** SpeakerSearchResult
    speakerSearchResult_confidenceScore,
    speakerSearchResult_voiceProfileId,

    -- ** SpeakerSearchTask
    speakerSearchTask_callDetails,
    speakerSearchTask_createdTimestamp,
    speakerSearchTask_speakerSearchDetails,
    speakerSearchTask_speakerSearchTaskId,
    speakerSearchTask_speakerSearchTaskStatus,
    speakerSearchTask_startedTimestamp,
    speakerSearchTask_statusMessage,
    speakerSearchTask_updatedTimestamp,

    -- ** StreamingConfiguration
    streamingConfiguration_mediaInsightsConfiguration,
    streamingConfiguration_streamingNotificationTargets,
    streamingConfiguration_dataRetentionInHours,
    streamingConfiguration_disabled,

    -- ** StreamingNotificationTarget
    streamingNotificationTarget_notificationTarget,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Termination
    termination_callingRegions,
    termination_cidrAllowedList,
    termination_cpsLimit,
    termination_defaultPhoneNumber,
    termination_disabled,

    -- ** TerminationHealth
    terminationHealth_source,
    terminationHealth_timestamp,

    -- ** UpdatePhoneNumberRequestItem
    updatePhoneNumberRequestItem_callingName,
    updatePhoneNumberRequestItem_productType,
    updatePhoneNumberRequestItem_phoneNumberId,

    -- ** VoiceConnector
    voiceConnector_awsRegion,
    voiceConnector_createdTimestamp,
    voiceConnector_name,
    voiceConnector_outboundHostName,
    voiceConnector_requireEncryption,
    voiceConnector_updatedTimestamp,
    voiceConnector_voiceConnectorArn,
    voiceConnector_voiceConnectorId,

    -- ** VoiceConnectorGroup
    voiceConnectorGroup_createdTimestamp,
    voiceConnectorGroup_name,
    voiceConnectorGroup_updatedTimestamp,
    voiceConnectorGroup_voiceConnectorGroupArn,
    voiceConnectorGroup_voiceConnectorGroupId,
    voiceConnectorGroup_voiceConnectorItems,

    -- ** VoiceConnectorItem
    voiceConnectorItem_voiceConnectorId,
    voiceConnectorItem_priority,

    -- ** VoiceConnectorSettings
    voiceConnectorSettings_cdrBucket,

    -- ** VoiceProfile
    voiceProfile_createdTimestamp,
    voiceProfile_expirationTimestamp,
    voiceProfile_updatedTimestamp,
    voiceProfile_voiceProfileArn,
    voiceProfile_voiceProfileDomainId,
    voiceProfile_voiceProfileId,

    -- ** VoiceProfileDomain
    voiceProfileDomain_createdTimestamp,
    voiceProfileDomain_description,
    voiceProfileDomain_name,
    voiceProfileDomain_serverSideEncryptionConfiguration,
    voiceProfileDomain_updatedTimestamp,
    voiceProfileDomain_voiceProfileDomainArn,
    voiceProfileDomain_voiceProfileDomainId,

    -- ** VoiceProfileDomainSummary
    voiceProfileDomainSummary_createdTimestamp,
    voiceProfileDomainSummary_description,
    voiceProfileDomainSummary_name,
    voiceProfileDomainSummary_updatedTimestamp,
    voiceProfileDomainSummary_voiceProfileDomainArn,
    voiceProfileDomainSummary_voiceProfileDomainId,

    -- ** VoiceProfileSummary
    voiceProfileSummary_createdTimestamp,
    voiceProfileSummary_expirationTimestamp,
    voiceProfileSummary_updatedTimestamp,
    voiceProfileSummary_voiceProfileArn,
    voiceProfileSummary_voiceProfileDomainId,
    voiceProfileSummary_voiceProfileId,

    -- ** VoiceToneAnalysisTask
    voiceToneAnalysisTask_callDetails,
    voiceToneAnalysisTask_createdTimestamp,
    voiceToneAnalysisTask_startedTimestamp,
    voiceToneAnalysisTask_statusMessage,
    voiceToneAnalysisTask_updatedTimestamp,
    voiceToneAnalysisTask_voiceToneAnalysisTaskId,
    voiceToneAnalysisTask_voiceToneAnalysisTaskStatus,
  )
where

import Amazonka.ChimeSdkVoice.AssociatePhoneNumbersWithVoiceConnector
import Amazonka.ChimeSdkVoice.AssociatePhoneNumbersWithVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.BatchDeletePhoneNumber
import Amazonka.ChimeSdkVoice.BatchUpdatePhoneNumber
import Amazonka.ChimeSdkVoice.CreatePhoneNumberOrder
import Amazonka.ChimeSdkVoice.CreateProxySession
import Amazonka.ChimeSdkVoice.CreateSipMediaApplication
import Amazonka.ChimeSdkVoice.CreateSipMediaApplicationCall
import Amazonka.ChimeSdkVoice.CreateSipRule
import Amazonka.ChimeSdkVoice.CreateVoiceConnector
import Amazonka.ChimeSdkVoice.CreateVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.CreateVoiceProfile
import Amazonka.ChimeSdkVoice.CreateVoiceProfileDomain
import Amazonka.ChimeSdkVoice.DeletePhoneNumber
import Amazonka.ChimeSdkVoice.DeleteProxySession
import Amazonka.ChimeSdkVoice.DeleteSipMediaApplication
import Amazonka.ChimeSdkVoice.DeleteSipRule
import Amazonka.ChimeSdkVoice.DeleteVoiceConnector
import Amazonka.ChimeSdkVoice.DeleteVoiceConnectorEmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.DeleteVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.DeleteVoiceConnectorOrigination
import Amazonka.ChimeSdkVoice.DeleteVoiceConnectorProxy
import Amazonka.ChimeSdkVoice.DeleteVoiceConnectorStreamingConfiguration
import Amazonka.ChimeSdkVoice.DeleteVoiceConnectorTermination
import Amazonka.ChimeSdkVoice.DeleteVoiceConnectorTerminationCredentials
import Amazonka.ChimeSdkVoice.DeleteVoiceProfile
import Amazonka.ChimeSdkVoice.DeleteVoiceProfileDomain
import Amazonka.ChimeSdkVoice.DisassociatePhoneNumbersFromVoiceConnector
import Amazonka.ChimeSdkVoice.DisassociatePhoneNumbersFromVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.GetGlobalSettings
import Amazonka.ChimeSdkVoice.GetPhoneNumber
import Amazonka.ChimeSdkVoice.GetPhoneNumberOrder
import Amazonka.ChimeSdkVoice.GetPhoneNumberSettings
import Amazonka.ChimeSdkVoice.GetProxySession
import Amazonka.ChimeSdkVoice.GetSipMediaApplication
import Amazonka.ChimeSdkVoice.GetSipMediaApplicationAlexaSkillConfiguration
import Amazonka.ChimeSdkVoice.GetSipMediaApplicationLoggingConfiguration
import Amazonka.ChimeSdkVoice.GetSipRule
import Amazonka.ChimeSdkVoice.GetSpeakerSearchTask
import Amazonka.ChimeSdkVoice.GetVoiceConnector
import Amazonka.ChimeSdkVoice.GetVoiceConnectorEmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.GetVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.GetVoiceConnectorLoggingConfiguration
import Amazonka.ChimeSdkVoice.GetVoiceConnectorOrigination
import Amazonka.ChimeSdkVoice.GetVoiceConnectorProxy
import Amazonka.ChimeSdkVoice.GetVoiceConnectorStreamingConfiguration
import Amazonka.ChimeSdkVoice.GetVoiceConnectorTermination
import Amazonka.ChimeSdkVoice.GetVoiceConnectorTerminationHealth
import Amazonka.ChimeSdkVoice.GetVoiceProfile
import Amazonka.ChimeSdkVoice.GetVoiceProfileDomain
import Amazonka.ChimeSdkVoice.GetVoiceToneAnalysisTask
import Amazonka.ChimeSdkVoice.ListAvailableVoiceConnectorRegions
import Amazonka.ChimeSdkVoice.ListPhoneNumberOrders
import Amazonka.ChimeSdkVoice.ListPhoneNumbers
import Amazonka.ChimeSdkVoice.ListProxySessions
import Amazonka.ChimeSdkVoice.ListSipMediaApplications
import Amazonka.ChimeSdkVoice.ListSipRules
import Amazonka.ChimeSdkVoice.ListSupportedPhoneNumberCountries
import Amazonka.ChimeSdkVoice.ListTagsForResource
import Amazonka.ChimeSdkVoice.ListVoiceConnectorGroups
import Amazonka.ChimeSdkVoice.ListVoiceConnectorTerminationCredentials
import Amazonka.ChimeSdkVoice.ListVoiceConnectors
import Amazonka.ChimeSdkVoice.ListVoiceProfileDomains
import Amazonka.ChimeSdkVoice.ListVoiceProfiles
import Amazonka.ChimeSdkVoice.PutSipMediaApplicationAlexaSkillConfiguration
import Amazonka.ChimeSdkVoice.PutSipMediaApplicationLoggingConfiguration
import Amazonka.ChimeSdkVoice.PutVoiceConnectorEmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.PutVoiceConnectorLoggingConfiguration
import Amazonka.ChimeSdkVoice.PutVoiceConnectorOrigination
import Amazonka.ChimeSdkVoice.PutVoiceConnectorProxy
import Amazonka.ChimeSdkVoice.PutVoiceConnectorStreamingConfiguration
import Amazonka.ChimeSdkVoice.PutVoiceConnectorTermination
import Amazonka.ChimeSdkVoice.PutVoiceConnectorTerminationCredentials
import Amazonka.ChimeSdkVoice.RestorePhoneNumber
import Amazonka.ChimeSdkVoice.SearchAvailablePhoneNumbers
import Amazonka.ChimeSdkVoice.StartSpeakerSearchTask
import Amazonka.ChimeSdkVoice.StartVoiceToneAnalysisTask
import Amazonka.ChimeSdkVoice.StopSpeakerSearchTask
import Amazonka.ChimeSdkVoice.StopVoiceToneAnalysisTask
import Amazonka.ChimeSdkVoice.TagResource
import Amazonka.ChimeSdkVoice.Types.Address
import Amazonka.ChimeSdkVoice.Types.CallDetails
import Amazonka.ChimeSdkVoice.Types.CandidateAddress
import Amazonka.ChimeSdkVoice.Types.Credential
import Amazonka.ChimeSdkVoice.Types.DNISEmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.Types.EmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.Types.GeoMatchParams
import Amazonka.ChimeSdkVoice.Types.LoggingConfiguration
import Amazonka.ChimeSdkVoice.Types.MediaInsightsConfiguration
import Amazonka.ChimeSdkVoice.Types.OrderedPhoneNumber
import Amazonka.ChimeSdkVoice.Types.Origination
import Amazonka.ChimeSdkVoice.Types.OriginationRoute
import Amazonka.ChimeSdkVoice.Types.Participant
import Amazonka.ChimeSdkVoice.Types.PhoneNumber
import Amazonka.ChimeSdkVoice.Types.PhoneNumberAssociation
import Amazonka.ChimeSdkVoice.Types.PhoneNumberCapabilities
import Amazonka.ChimeSdkVoice.Types.PhoneNumberCountry
import Amazonka.ChimeSdkVoice.Types.PhoneNumberError
import Amazonka.ChimeSdkVoice.Types.PhoneNumberOrder
import Amazonka.ChimeSdkVoice.Types.Proxy
import Amazonka.ChimeSdkVoice.Types.ProxySession
import Amazonka.ChimeSdkVoice.Types.ServerSideEncryptionConfiguration
import Amazonka.ChimeSdkVoice.Types.SipMediaApplication
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationAlexaSkillConfiguration
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationCall
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationEndpoint
import Amazonka.ChimeSdkVoice.Types.SipMediaApplicationLoggingConfiguration
import Amazonka.ChimeSdkVoice.Types.SipRule
import Amazonka.ChimeSdkVoice.Types.SipRuleTargetApplication
import Amazonka.ChimeSdkVoice.Types.SpeakerSearchDetails
import Amazonka.ChimeSdkVoice.Types.SpeakerSearchResult
import Amazonka.ChimeSdkVoice.Types.SpeakerSearchTask
import Amazonka.ChimeSdkVoice.Types.StreamingConfiguration
import Amazonka.ChimeSdkVoice.Types.StreamingNotificationTarget
import Amazonka.ChimeSdkVoice.Types.Tag
import Amazonka.ChimeSdkVoice.Types.Termination
import Amazonka.ChimeSdkVoice.Types.TerminationHealth
import Amazonka.ChimeSdkVoice.Types.UpdatePhoneNumberRequestItem
import Amazonka.ChimeSdkVoice.Types.VoiceConnector
import Amazonka.ChimeSdkVoice.Types.VoiceConnectorGroup
import Amazonka.ChimeSdkVoice.Types.VoiceConnectorItem
import Amazonka.ChimeSdkVoice.Types.VoiceConnectorSettings
import Amazonka.ChimeSdkVoice.Types.VoiceProfile
import Amazonka.ChimeSdkVoice.Types.VoiceProfileDomain
import Amazonka.ChimeSdkVoice.Types.VoiceProfileDomainSummary
import Amazonka.ChimeSdkVoice.Types.VoiceProfileSummary
import Amazonka.ChimeSdkVoice.Types.VoiceToneAnalysisTask
import Amazonka.ChimeSdkVoice.UntagResource
import Amazonka.ChimeSdkVoice.UpdateGlobalSettings
import Amazonka.ChimeSdkVoice.UpdatePhoneNumber
import Amazonka.ChimeSdkVoice.UpdatePhoneNumberSettings
import Amazonka.ChimeSdkVoice.UpdateProxySession
import Amazonka.ChimeSdkVoice.UpdateSipMediaApplication
import Amazonka.ChimeSdkVoice.UpdateSipMediaApplicationCall
import Amazonka.ChimeSdkVoice.UpdateSipRule
import Amazonka.ChimeSdkVoice.UpdateVoiceConnector
import Amazonka.ChimeSdkVoice.UpdateVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.UpdateVoiceProfile
import Amazonka.ChimeSdkVoice.UpdateVoiceProfileDomain
import Amazonka.ChimeSdkVoice.ValidateE911Address

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ChimeSdkVoice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-08-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon Chime SDK Voice APIs enable software developers to add
-- telephony capabilties to their custom communication solutions. You use
-- these APIs with SIP infrastructure and Amazon Chime SDK Voice
-- Connectors. For more information, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/APIReference/API_Operations_Amazon_Chime_SDK_Voice.html Amazon Chime SDK Voice>.
module Amazonka.ChimeSdkVoice
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ServiceFailureException
    _ServiceFailureException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottledClientException
    _ThrottledClientException,

    -- ** UnauthorizedClientException
    _UnauthorizedClientException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociatePhoneNumbersWithVoiceConnector
    AssociatePhoneNumbersWithVoiceConnector (AssociatePhoneNumbersWithVoiceConnector'),
    newAssociatePhoneNumbersWithVoiceConnector,
    AssociatePhoneNumbersWithVoiceConnectorResponse (AssociatePhoneNumbersWithVoiceConnectorResponse'),
    newAssociatePhoneNumbersWithVoiceConnectorResponse,

    -- ** AssociatePhoneNumbersWithVoiceConnectorGroup
    AssociatePhoneNumbersWithVoiceConnectorGroup (AssociatePhoneNumbersWithVoiceConnectorGroup'),
    newAssociatePhoneNumbersWithVoiceConnectorGroup,
    AssociatePhoneNumbersWithVoiceConnectorGroupResponse (AssociatePhoneNumbersWithVoiceConnectorGroupResponse'),
    newAssociatePhoneNumbersWithVoiceConnectorGroupResponse,

    -- ** BatchDeletePhoneNumber
    BatchDeletePhoneNumber (BatchDeletePhoneNumber'),
    newBatchDeletePhoneNumber,
    BatchDeletePhoneNumberResponse (BatchDeletePhoneNumberResponse'),
    newBatchDeletePhoneNumberResponse,

    -- ** BatchUpdatePhoneNumber
    BatchUpdatePhoneNumber (BatchUpdatePhoneNumber'),
    newBatchUpdatePhoneNumber,
    BatchUpdatePhoneNumberResponse (BatchUpdatePhoneNumberResponse'),
    newBatchUpdatePhoneNumberResponse,

    -- ** CreatePhoneNumberOrder
    CreatePhoneNumberOrder (CreatePhoneNumberOrder'),
    newCreatePhoneNumberOrder,
    CreatePhoneNumberOrderResponse (CreatePhoneNumberOrderResponse'),
    newCreatePhoneNumberOrderResponse,

    -- ** CreateProxySession
    CreateProxySession (CreateProxySession'),
    newCreateProxySession,
    CreateProxySessionResponse (CreateProxySessionResponse'),
    newCreateProxySessionResponse,

    -- ** CreateSipMediaApplication
    CreateSipMediaApplication (CreateSipMediaApplication'),
    newCreateSipMediaApplication,
    CreateSipMediaApplicationResponse (CreateSipMediaApplicationResponse'),
    newCreateSipMediaApplicationResponse,

    -- ** CreateSipMediaApplicationCall
    CreateSipMediaApplicationCall (CreateSipMediaApplicationCall'),
    newCreateSipMediaApplicationCall,
    CreateSipMediaApplicationCallResponse (CreateSipMediaApplicationCallResponse'),
    newCreateSipMediaApplicationCallResponse,

    -- ** CreateSipRule
    CreateSipRule (CreateSipRule'),
    newCreateSipRule,
    CreateSipRuleResponse (CreateSipRuleResponse'),
    newCreateSipRuleResponse,

    -- ** CreateVoiceConnector
    CreateVoiceConnector (CreateVoiceConnector'),
    newCreateVoiceConnector,
    CreateVoiceConnectorResponse (CreateVoiceConnectorResponse'),
    newCreateVoiceConnectorResponse,

    -- ** CreateVoiceConnectorGroup
    CreateVoiceConnectorGroup (CreateVoiceConnectorGroup'),
    newCreateVoiceConnectorGroup,
    CreateVoiceConnectorGroupResponse (CreateVoiceConnectorGroupResponse'),
    newCreateVoiceConnectorGroupResponse,

    -- ** DeletePhoneNumber
    DeletePhoneNumber (DeletePhoneNumber'),
    newDeletePhoneNumber,
    DeletePhoneNumberResponse (DeletePhoneNumberResponse'),
    newDeletePhoneNumberResponse,

    -- ** DeleteProxySession
    DeleteProxySession (DeleteProxySession'),
    newDeleteProxySession,
    DeleteProxySessionResponse (DeleteProxySessionResponse'),
    newDeleteProxySessionResponse,

    -- ** DeleteSipMediaApplication
    DeleteSipMediaApplication (DeleteSipMediaApplication'),
    newDeleteSipMediaApplication,
    DeleteSipMediaApplicationResponse (DeleteSipMediaApplicationResponse'),
    newDeleteSipMediaApplicationResponse,

    -- ** DeleteSipRule
    DeleteSipRule (DeleteSipRule'),
    newDeleteSipRule,
    DeleteSipRuleResponse (DeleteSipRuleResponse'),
    newDeleteSipRuleResponse,

    -- ** DeleteVoiceConnector
    DeleteVoiceConnector (DeleteVoiceConnector'),
    newDeleteVoiceConnector,
    DeleteVoiceConnectorResponse (DeleteVoiceConnectorResponse'),
    newDeleteVoiceConnectorResponse,

    -- ** DeleteVoiceConnectorEmergencyCallingConfiguration
    DeleteVoiceConnectorEmergencyCallingConfiguration (DeleteVoiceConnectorEmergencyCallingConfiguration'),
    newDeleteVoiceConnectorEmergencyCallingConfiguration,
    DeleteVoiceConnectorEmergencyCallingConfigurationResponse (DeleteVoiceConnectorEmergencyCallingConfigurationResponse'),
    newDeleteVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** DeleteVoiceConnectorGroup
    DeleteVoiceConnectorGroup (DeleteVoiceConnectorGroup'),
    newDeleteVoiceConnectorGroup,
    DeleteVoiceConnectorGroupResponse (DeleteVoiceConnectorGroupResponse'),
    newDeleteVoiceConnectorGroupResponse,

    -- ** DeleteVoiceConnectorOrigination
    DeleteVoiceConnectorOrigination (DeleteVoiceConnectorOrigination'),
    newDeleteVoiceConnectorOrigination,
    DeleteVoiceConnectorOriginationResponse (DeleteVoiceConnectorOriginationResponse'),
    newDeleteVoiceConnectorOriginationResponse,

    -- ** DeleteVoiceConnectorProxy
    DeleteVoiceConnectorProxy (DeleteVoiceConnectorProxy'),
    newDeleteVoiceConnectorProxy,
    DeleteVoiceConnectorProxyResponse (DeleteVoiceConnectorProxyResponse'),
    newDeleteVoiceConnectorProxyResponse,

    -- ** DeleteVoiceConnectorStreamingConfiguration
    DeleteVoiceConnectorStreamingConfiguration (DeleteVoiceConnectorStreamingConfiguration'),
    newDeleteVoiceConnectorStreamingConfiguration,
    DeleteVoiceConnectorStreamingConfigurationResponse (DeleteVoiceConnectorStreamingConfigurationResponse'),
    newDeleteVoiceConnectorStreamingConfigurationResponse,

    -- ** DeleteVoiceConnectorTermination
    DeleteVoiceConnectorTermination (DeleteVoiceConnectorTermination'),
    newDeleteVoiceConnectorTermination,
    DeleteVoiceConnectorTerminationResponse (DeleteVoiceConnectorTerminationResponse'),
    newDeleteVoiceConnectorTerminationResponse,

    -- ** DeleteVoiceConnectorTerminationCredentials
    DeleteVoiceConnectorTerminationCredentials (DeleteVoiceConnectorTerminationCredentials'),
    newDeleteVoiceConnectorTerminationCredentials,
    DeleteVoiceConnectorTerminationCredentialsResponse (DeleteVoiceConnectorTerminationCredentialsResponse'),
    newDeleteVoiceConnectorTerminationCredentialsResponse,

    -- ** DisassociatePhoneNumbersFromVoiceConnector
    DisassociatePhoneNumbersFromVoiceConnector (DisassociatePhoneNumbersFromVoiceConnector'),
    newDisassociatePhoneNumbersFromVoiceConnector,
    DisassociatePhoneNumbersFromVoiceConnectorResponse (DisassociatePhoneNumbersFromVoiceConnectorResponse'),
    newDisassociatePhoneNumbersFromVoiceConnectorResponse,

    -- ** DisassociatePhoneNumbersFromVoiceConnectorGroup
    DisassociatePhoneNumbersFromVoiceConnectorGroup (DisassociatePhoneNumbersFromVoiceConnectorGroup'),
    newDisassociatePhoneNumbersFromVoiceConnectorGroup,
    DisassociatePhoneNumbersFromVoiceConnectorGroupResponse (DisassociatePhoneNumbersFromVoiceConnectorGroupResponse'),
    newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse,

    -- ** GetGlobalSettings
    GetGlobalSettings (GetGlobalSettings'),
    newGetGlobalSettings,
    GetGlobalSettingsResponse (GetGlobalSettingsResponse'),
    newGetGlobalSettingsResponse,

    -- ** GetPhoneNumber
    GetPhoneNumber (GetPhoneNumber'),
    newGetPhoneNumber,
    GetPhoneNumberResponse (GetPhoneNumberResponse'),
    newGetPhoneNumberResponse,

    -- ** GetPhoneNumberOrder
    GetPhoneNumberOrder (GetPhoneNumberOrder'),
    newGetPhoneNumberOrder,
    GetPhoneNumberOrderResponse (GetPhoneNumberOrderResponse'),
    newGetPhoneNumberOrderResponse,

    -- ** GetPhoneNumberSettings
    GetPhoneNumberSettings (GetPhoneNumberSettings'),
    newGetPhoneNumberSettings,
    GetPhoneNumberSettingsResponse (GetPhoneNumberSettingsResponse'),
    newGetPhoneNumberSettingsResponse,

    -- ** GetProxySession
    GetProxySession (GetProxySession'),
    newGetProxySession,
    GetProxySessionResponse (GetProxySessionResponse'),
    newGetProxySessionResponse,

    -- ** GetSipMediaApplication
    GetSipMediaApplication (GetSipMediaApplication'),
    newGetSipMediaApplication,
    GetSipMediaApplicationResponse (GetSipMediaApplicationResponse'),
    newGetSipMediaApplicationResponse,

    -- ** GetSipMediaApplicationAlexaSkillConfiguration
    GetSipMediaApplicationAlexaSkillConfiguration (GetSipMediaApplicationAlexaSkillConfiguration'),
    newGetSipMediaApplicationAlexaSkillConfiguration,
    GetSipMediaApplicationAlexaSkillConfigurationResponse (GetSipMediaApplicationAlexaSkillConfigurationResponse'),
    newGetSipMediaApplicationAlexaSkillConfigurationResponse,

    -- ** GetSipMediaApplicationLoggingConfiguration
    GetSipMediaApplicationLoggingConfiguration (GetSipMediaApplicationLoggingConfiguration'),
    newGetSipMediaApplicationLoggingConfiguration,
    GetSipMediaApplicationLoggingConfigurationResponse (GetSipMediaApplicationLoggingConfigurationResponse'),
    newGetSipMediaApplicationLoggingConfigurationResponse,

    -- ** GetSipRule
    GetSipRule (GetSipRule'),
    newGetSipRule,
    GetSipRuleResponse (GetSipRuleResponse'),
    newGetSipRuleResponse,

    -- ** GetVoiceConnector
    GetVoiceConnector (GetVoiceConnector'),
    newGetVoiceConnector,
    GetVoiceConnectorResponse (GetVoiceConnectorResponse'),
    newGetVoiceConnectorResponse,

    -- ** GetVoiceConnectorEmergencyCallingConfiguration
    GetVoiceConnectorEmergencyCallingConfiguration (GetVoiceConnectorEmergencyCallingConfiguration'),
    newGetVoiceConnectorEmergencyCallingConfiguration,
    GetVoiceConnectorEmergencyCallingConfigurationResponse (GetVoiceConnectorEmergencyCallingConfigurationResponse'),
    newGetVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** GetVoiceConnectorGroup
    GetVoiceConnectorGroup (GetVoiceConnectorGroup'),
    newGetVoiceConnectorGroup,
    GetVoiceConnectorGroupResponse (GetVoiceConnectorGroupResponse'),
    newGetVoiceConnectorGroupResponse,

    -- ** GetVoiceConnectorLoggingConfiguration
    GetVoiceConnectorLoggingConfiguration (GetVoiceConnectorLoggingConfiguration'),
    newGetVoiceConnectorLoggingConfiguration,
    GetVoiceConnectorLoggingConfigurationResponse (GetVoiceConnectorLoggingConfigurationResponse'),
    newGetVoiceConnectorLoggingConfigurationResponse,

    -- ** GetVoiceConnectorOrigination
    GetVoiceConnectorOrigination (GetVoiceConnectorOrigination'),
    newGetVoiceConnectorOrigination,
    GetVoiceConnectorOriginationResponse (GetVoiceConnectorOriginationResponse'),
    newGetVoiceConnectorOriginationResponse,

    -- ** GetVoiceConnectorProxy
    GetVoiceConnectorProxy (GetVoiceConnectorProxy'),
    newGetVoiceConnectorProxy,
    GetVoiceConnectorProxyResponse (GetVoiceConnectorProxyResponse'),
    newGetVoiceConnectorProxyResponse,

    -- ** GetVoiceConnectorStreamingConfiguration
    GetVoiceConnectorStreamingConfiguration (GetVoiceConnectorStreamingConfiguration'),
    newGetVoiceConnectorStreamingConfiguration,
    GetVoiceConnectorStreamingConfigurationResponse (GetVoiceConnectorStreamingConfigurationResponse'),
    newGetVoiceConnectorStreamingConfigurationResponse,

    -- ** GetVoiceConnectorTermination
    GetVoiceConnectorTermination (GetVoiceConnectorTermination'),
    newGetVoiceConnectorTermination,
    GetVoiceConnectorTerminationResponse (GetVoiceConnectorTerminationResponse'),
    newGetVoiceConnectorTerminationResponse,

    -- ** GetVoiceConnectorTerminationHealth
    GetVoiceConnectorTerminationHealth (GetVoiceConnectorTerminationHealth'),
    newGetVoiceConnectorTerminationHealth,
    GetVoiceConnectorTerminationHealthResponse (GetVoiceConnectorTerminationHealthResponse'),
    newGetVoiceConnectorTerminationHealthResponse,

    -- ** ListAvailableVoiceConnectorRegions
    ListAvailableVoiceConnectorRegions (ListAvailableVoiceConnectorRegions'),
    newListAvailableVoiceConnectorRegions,
    ListAvailableVoiceConnectorRegionsResponse (ListAvailableVoiceConnectorRegionsResponse'),
    newListAvailableVoiceConnectorRegionsResponse,

    -- ** ListPhoneNumberOrders
    ListPhoneNumberOrders (ListPhoneNumberOrders'),
    newListPhoneNumberOrders,
    ListPhoneNumberOrdersResponse (ListPhoneNumberOrdersResponse'),
    newListPhoneNumberOrdersResponse,

    -- ** ListPhoneNumbers
    ListPhoneNumbers (ListPhoneNumbers'),
    newListPhoneNumbers,
    ListPhoneNumbersResponse (ListPhoneNumbersResponse'),
    newListPhoneNumbersResponse,

    -- ** ListProxySessions
    ListProxySessions (ListProxySessions'),
    newListProxySessions,
    ListProxySessionsResponse (ListProxySessionsResponse'),
    newListProxySessionsResponse,

    -- ** ListSipMediaApplications (Paginated)
    ListSipMediaApplications (ListSipMediaApplications'),
    newListSipMediaApplications,
    ListSipMediaApplicationsResponse (ListSipMediaApplicationsResponse'),
    newListSipMediaApplicationsResponse,

    -- ** ListSipRules (Paginated)
    ListSipRules (ListSipRules'),
    newListSipRules,
    ListSipRulesResponse (ListSipRulesResponse'),
    newListSipRulesResponse,

    -- ** ListSupportedPhoneNumberCountries
    ListSupportedPhoneNumberCountries (ListSupportedPhoneNumberCountries'),
    newListSupportedPhoneNumberCountries,
    ListSupportedPhoneNumberCountriesResponse (ListSupportedPhoneNumberCountriesResponse'),
    newListSupportedPhoneNumberCountriesResponse,

    -- ** ListVoiceConnectorGroups
    ListVoiceConnectorGroups (ListVoiceConnectorGroups'),
    newListVoiceConnectorGroups,
    ListVoiceConnectorGroupsResponse (ListVoiceConnectorGroupsResponse'),
    newListVoiceConnectorGroupsResponse,

    -- ** ListVoiceConnectorTerminationCredentials
    ListVoiceConnectorTerminationCredentials (ListVoiceConnectorTerminationCredentials'),
    newListVoiceConnectorTerminationCredentials,
    ListVoiceConnectorTerminationCredentialsResponse (ListVoiceConnectorTerminationCredentialsResponse'),
    newListVoiceConnectorTerminationCredentialsResponse,

    -- ** ListVoiceConnectors
    ListVoiceConnectors (ListVoiceConnectors'),
    newListVoiceConnectors,
    ListVoiceConnectorsResponse (ListVoiceConnectorsResponse'),
    newListVoiceConnectorsResponse,

    -- ** PutSipMediaApplicationAlexaSkillConfiguration
    PutSipMediaApplicationAlexaSkillConfiguration (PutSipMediaApplicationAlexaSkillConfiguration'),
    newPutSipMediaApplicationAlexaSkillConfiguration,
    PutSipMediaApplicationAlexaSkillConfigurationResponse (PutSipMediaApplicationAlexaSkillConfigurationResponse'),
    newPutSipMediaApplicationAlexaSkillConfigurationResponse,

    -- ** PutSipMediaApplicationLoggingConfiguration
    PutSipMediaApplicationLoggingConfiguration (PutSipMediaApplicationLoggingConfiguration'),
    newPutSipMediaApplicationLoggingConfiguration,
    PutSipMediaApplicationLoggingConfigurationResponse (PutSipMediaApplicationLoggingConfigurationResponse'),
    newPutSipMediaApplicationLoggingConfigurationResponse,

    -- ** PutVoiceConnectorEmergencyCallingConfiguration
    PutVoiceConnectorEmergencyCallingConfiguration (PutVoiceConnectorEmergencyCallingConfiguration'),
    newPutVoiceConnectorEmergencyCallingConfiguration,
    PutVoiceConnectorEmergencyCallingConfigurationResponse (PutVoiceConnectorEmergencyCallingConfigurationResponse'),
    newPutVoiceConnectorEmergencyCallingConfigurationResponse,

    -- ** PutVoiceConnectorLoggingConfiguration
    PutVoiceConnectorLoggingConfiguration (PutVoiceConnectorLoggingConfiguration'),
    newPutVoiceConnectorLoggingConfiguration,
    PutVoiceConnectorLoggingConfigurationResponse (PutVoiceConnectorLoggingConfigurationResponse'),
    newPutVoiceConnectorLoggingConfigurationResponse,

    -- ** PutVoiceConnectorOrigination
    PutVoiceConnectorOrigination (PutVoiceConnectorOrigination'),
    newPutVoiceConnectorOrigination,
    PutVoiceConnectorOriginationResponse (PutVoiceConnectorOriginationResponse'),
    newPutVoiceConnectorOriginationResponse,

    -- ** PutVoiceConnectorProxy
    PutVoiceConnectorProxy (PutVoiceConnectorProxy'),
    newPutVoiceConnectorProxy,
    PutVoiceConnectorProxyResponse (PutVoiceConnectorProxyResponse'),
    newPutVoiceConnectorProxyResponse,

    -- ** PutVoiceConnectorStreamingConfiguration
    PutVoiceConnectorStreamingConfiguration (PutVoiceConnectorStreamingConfiguration'),
    newPutVoiceConnectorStreamingConfiguration,
    PutVoiceConnectorStreamingConfigurationResponse (PutVoiceConnectorStreamingConfigurationResponse'),
    newPutVoiceConnectorStreamingConfigurationResponse,

    -- ** PutVoiceConnectorTermination
    PutVoiceConnectorTermination (PutVoiceConnectorTermination'),
    newPutVoiceConnectorTermination,
    PutVoiceConnectorTerminationResponse (PutVoiceConnectorTerminationResponse'),
    newPutVoiceConnectorTerminationResponse,

    -- ** PutVoiceConnectorTerminationCredentials
    PutVoiceConnectorTerminationCredentials (PutVoiceConnectorTerminationCredentials'),
    newPutVoiceConnectorTerminationCredentials,
    PutVoiceConnectorTerminationCredentialsResponse (PutVoiceConnectorTerminationCredentialsResponse'),
    newPutVoiceConnectorTerminationCredentialsResponse,

    -- ** RestorePhoneNumber
    RestorePhoneNumber (RestorePhoneNumber'),
    newRestorePhoneNumber,
    RestorePhoneNumberResponse (RestorePhoneNumberResponse'),
    newRestorePhoneNumberResponse,

    -- ** SearchAvailablePhoneNumbers
    SearchAvailablePhoneNumbers (SearchAvailablePhoneNumbers'),
    newSearchAvailablePhoneNumbers,
    SearchAvailablePhoneNumbersResponse (SearchAvailablePhoneNumbersResponse'),
    newSearchAvailablePhoneNumbersResponse,

    -- ** UpdateGlobalSettings
    UpdateGlobalSettings (UpdateGlobalSettings'),
    newUpdateGlobalSettings,
    UpdateGlobalSettingsResponse (UpdateGlobalSettingsResponse'),
    newUpdateGlobalSettingsResponse,

    -- ** UpdatePhoneNumber
    UpdatePhoneNumber (UpdatePhoneNumber'),
    newUpdatePhoneNumber,
    UpdatePhoneNumberResponse (UpdatePhoneNumberResponse'),
    newUpdatePhoneNumberResponse,

    -- ** UpdatePhoneNumberSettings
    UpdatePhoneNumberSettings (UpdatePhoneNumberSettings'),
    newUpdatePhoneNumberSettings,
    UpdatePhoneNumberSettingsResponse (UpdatePhoneNumberSettingsResponse'),
    newUpdatePhoneNumberSettingsResponse,

    -- ** UpdateProxySession
    UpdateProxySession (UpdateProxySession'),
    newUpdateProxySession,
    UpdateProxySessionResponse (UpdateProxySessionResponse'),
    newUpdateProxySessionResponse,

    -- ** UpdateSipMediaApplication
    UpdateSipMediaApplication (UpdateSipMediaApplication'),
    newUpdateSipMediaApplication,
    UpdateSipMediaApplicationResponse (UpdateSipMediaApplicationResponse'),
    newUpdateSipMediaApplicationResponse,

    -- ** UpdateSipMediaApplicationCall
    UpdateSipMediaApplicationCall (UpdateSipMediaApplicationCall'),
    newUpdateSipMediaApplicationCall,
    UpdateSipMediaApplicationCallResponse (UpdateSipMediaApplicationCallResponse'),
    newUpdateSipMediaApplicationCallResponse,

    -- ** UpdateSipRule
    UpdateSipRule (UpdateSipRule'),
    newUpdateSipRule,
    UpdateSipRuleResponse (UpdateSipRuleResponse'),
    newUpdateSipRuleResponse,

    -- ** UpdateVoiceConnector
    UpdateVoiceConnector (UpdateVoiceConnector'),
    newUpdateVoiceConnector,
    UpdateVoiceConnectorResponse (UpdateVoiceConnectorResponse'),
    newUpdateVoiceConnectorResponse,

    -- ** UpdateVoiceConnectorGroup
    UpdateVoiceConnectorGroup (UpdateVoiceConnectorGroup'),
    newUpdateVoiceConnectorGroup,
    UpdateVoiceConnectorGroupResponse (UpdateVoiceConnectorGroupResponse'),
    newUpdateVoiceConnectorGroupResponse,

    -- ** ValidateE911Address
    ValidateE911Address (ValidateE911Address'),
    newValidateE911Address,
    ValidateE911AddressResponse (ValidateE911AddressResponse'),
    newValidateE911AddressResponse,

    -- * Types

    -- ** AlexaSkillStatus
    AlexaSkillStatus (..),

    -- ** CallingNameStatus
    CallingNameStatus (..),

    -- ** Capability
    Capability (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** GeoMatchLevel
    GeoMatchLevel (..),

    -- ** NotificationTarget
    NotificationTarget (..),

    -- ** NumberSelectionBehavior
    NumberSelectionBehavior (..),

    -- ** OrderedPhoneNumberStatus
    OrderedPhoneNumberStatus (..),

    -- ** OriginationRouteProtocol
    OriginationRouteProtocol (..),

    -- ** PhoneNumberAssociationName
    PhoneNumberAssociationName (..),

    -- ** PhoneNumberOrderStatus
    PhoneNumberOrderStatus (..),

    -- ** PhoneNumberOrderType
    PhoneNumberOrderType (..),

    -- ** PhoneNumberProductType
    PhoneNumberProductType (..),

    -- ** PhoneNumberStatus
    PhoneNumberStatus (..),

    -- ** PhoneNumberType
    PhoneNumberType (..),

    -- ** ProxySessionStatus
    ProxySessionStatus (..),

    -- ** SipRuleTriggerType
    SipRuleTriggerType (..),

    -- ** VoiceConnectorAwsRegion
    VoiceConnectorAwsRegion (..),

    -- ** Address
    Address (Address'),
    newAddress,

    -- ** CandidateAddress
    CandidateAddress (CandidateAddress'),
    newCandidateAddress,

    -- ** Credential
    Credential (Credential'),
    newCredential,

    -- ** DNISEmergencyCallingConfiguration
    DNISEmergencyCallingConfiguration (DNISEmergencyCallingConfiguration'),
    newDNISEmergencyCallingConfiguration,

    -- ** EmergencyCallingConfiguration
    EmergencyCallingConfiguration (EmergencyCallingConfiguration'),
    newEmergencyCallingConfiguration,

    -- ** GeoMatchParams
    GeoMatchParams (GeoMatchParams'),
    newGeoMatchParams,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** OrderedPhoneNumber
    OrderedPhoneNumber (OrderedPhoneNumber'),
    newOrderedPhoneNumber,

    -- ** Origination
    Origination (Origination'),
    newOrigination,

    -- ** OriginationRoute
    OriginationRoute (OriginationRoute'),
    newOriginationRoute,

    -- ** Participant
    Participant (Participant'),
    newParticipant,

    -- ** PhoneNumber
    PhoneNumber (PhoneNumber'),
    newPhoneNumber,

    -- ** PhoneNumberAssociation
    PhoneNumberAssociation (PhoneNumberAssociation'),
    newPhoneNumberAssociation,

    -- ** PhoneNumberCapabilities
    PhoneNumberCapabilities (PhoneNumberCapabilities'),
    newPhoneNumberCapabilities,

    -- ** PhoneNumberCountry
    PhoneNumberCountry (PhoneNumberCountry'),
    newPhoneNumberCountry,

    -- ** PhoneNumberError
    PhoneNumberError (PhoneNumberError'),
    newPhoneNumberError,

    -- ** PhoneNumberOrder
    PhoneNumberOrder (PhoneNumberOrder'),
    newPhoneNumberOrder,

    -- ** Proxy
    Proxy (Proxy'),
    newProxy,

    -- ** ProxySession
    ProxySession (ProxySession'),
    newProxySession,

    -- ** SipMediaApplication
    SipMediaApplication (SipMediaApplication'),
    newSipMediaApplication,

    -- ** SipMediaApplicationAlexaSkillConfiguration
    SipMediaApplicationAlexaSkillConfiguration (SipMediaApplicationAlexaSkillConfiguration'),
    newSipMediaApplicationAlexaSkillConfiguration,

    -- ** SipMediaApplicationCall
    SipMediaApplicationCall (SipMediaApplicationCall'),
    newSipMediaApplicationCall,

    -- ** SipMediaApplicationEndpoint
    SipMediaApplicationEndpoint (SipMediaApplicationEndpoint'),
    newSipMediaApplicationEndpoint,

    -- ** SipMediaApplicationLoggingConfiguration
    SipMediaApplicationLoggingConfiguration (SipMediaApplicationLoggingConfiguration'),
    newSipMediaApplicationLoggingConfiguration,

    -- ** SipRule
    SipRule (SipRule'),
    newSipRule,

    -- ** SipRuleTargetApplication
    SipRuleTargetApplication (SipRuleTargetApplication'),
    newSipRuleTargetApplication,

    -- ** StreamingConfiguration
    StreamingConfiguration (StreamingConfiguration'),
    newStreamingConfiguration,

    -- ** StreamingNotificationTarget
    StreamingNotificationTarget (StreamingNotificationTarget'),
    newStreamingNotificationTarget,

    -- ** Termination
    Termination (Termination'),
    newTermination,

    -- ** TerminationHealth
    TerminationHealth (TerminationHealth'),
    newTerminationHealth,

    -- ** UpdatePhoneNumberRequestItem
    UpdatePhoneNumberRequestItem (UpdatePhoneNumberRequestItem'),
    newUpdatePhoneNumberRequestItem,

    -- ** VoiceConnector
    VoiceConnector (VoiceConnector'),
    newVoiceConnector,

    -- ** VoiceConnectorGroup
    VoiceConnectorGroup (VoiceConnectorGroup'),
    newVoiceConnectorGroup,

    -- ** VoiceConnectorItem
    VoiceConnectorItem (VoiceConnectorItem'),
    newVoiceConnectorItem,

    -- ** VoiceConnectorSettings
    VoiceConnectorSettings (VoiceConnectorSettings'),
    newVoiceConnectorSettings,
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
import Amazonka.ChimeSdkVoice.GetVoiceConnector
import Amazonka.ChimeSdkVoice.GetVoiceConnectorEmergencyCallingConfiguration
import Amazonka.ChimeSdkVoice.GetVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.GetVoiceConnectorLoggingConfiguration
import Amazonka.ChimeSdkVoice.GetVoiceConnectorOrigination
import Amazonka.ChimeSdkVoice.GetVoiceConnectorProxy
import Amazonka.ChimeSdkVoice.GetVoiceConnectorStreamingConfiguration
import Amazonka.ChimeSdkVoice.GetVoiceConnectorTermination
import Amazonka.ChimeSdkVoice.GetVoiceConnectorTerminationHealth
import Amazonka.ChimeSdkVoice.Lens
import Amazonka.ChimeSdkVoice.ListAvailableVoiceConnectorRegions
import Amazonka.ChimeSdkVoice.ListPhoneNumberOrders
import Amazonka.ChimeSdkVoice.ListPhoneNumbers
import Amazonka.ChimeSdkVoice.ListProxySessions
import Amazonka.ChimeSdkVoice.ListSipMediaApplications
import Amazonka.ChimeSdkVoice.ListSipRules
import Amazonka.ChimeSdkVoice.ListSupportedPhoneNumberCountries
import Amazonka.ChimeSdkVoice.ListVoiceConnectorGroups
import Amazonka.ChimeSdkVoice.ListVoiceConnectorTerminationCredentials
import Amazonka.ChimeSdkVoice.ListVoiceConnectors
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
import Amazonka.ChimeSdkVoice.Types
import Amazonka.ChimeSdkVoice.UpdateGlobalSettings
import Amazonka.ChimeSdkVoice.UpdatePhoneNumber
import Amazonka.ChimeSdkVoice.UpdatePhoneNumberSettings
import Amazonka.ChimeSdkVoice.UpdateProxySession
import Amazonka.ChimeSdkVoice.UpdateSipMediaApplication
import Amazonka.ChimeSdkVoice.UpdateSipMediaApplicationCall
import Amazonka.ChimeSdkVoice.UpdateSipRule
import Amazonka.ChimeSdkVoice.UpdateVoiceConnector
import Amazonka.ChimeSdkVoice.UpdateVoiceConnectorGroup
import Amazonka.ChimeSdkVoice.ValidateE911Address
import Amazonka.ChimeSdkVoice.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ChimeSdkVoice'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

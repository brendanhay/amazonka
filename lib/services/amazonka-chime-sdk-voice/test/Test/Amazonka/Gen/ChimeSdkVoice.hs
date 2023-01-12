{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ChimeSdkVoice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ChimeSdkVoice where

import Amazonka.ChimeSdkVoice
import qualified Data.Proxy as Proxy
import Test.Amazonka.ChimeSdkVoice.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociatePhoneNumbersWithVoiceConnector $
--             newAssociatePhoneNumbersWithVoiceConnector
--
--         , requestAssociatePhoneNumbersWithVoiceConnectorGroup $
--             newAssociatePhoneNumbersWithVoiceConnectorGroup
--
--         , requestBatchDeletePhoneNumber $
--             newBatchDeletePhoneNumber
--
--         , requestBatchUpdatePhoneNumber $
--             newBatchUpdatePhoneNumber
--
--         , requestCreatePhoneNumberOrder $
--             newCreatePhoneNumberOrder
--
--         , requestCreateProxySession $
--             newCreateProxySession
--
--         , requestCreateSipMediaApplication $
--             newCreateSipMediaApplication
--
--         , requestCreateSipMediaApplicationCall $
--             newCreateSipMediaApplicationCall
--
--         , requestCreateSipRule $
--             newCreateSipRule
--
--         , requestCreateVoiceConnector $
--             newCreateVoiceConnector
--
--         , requestCreateVoiceConnectorGroup $
--             newCreateVoiceConnectorGroup
--
--         , requestDeletePhoneNumber $
--             newDeletePhoneNumber
--
--         , requestDeleteProxySession $
--             newDeleteProxySession
--
--         , requestDeleteSipMediaApplication $
--             newDeleteSipMediaApplication
--
--         , requestDeleteSipRule $
--             newDeleteSipRule
--
--         , requestDeleteVoiceConnector $
--             newDeleteVoiceConnector
--
--         , requestDeleteVoiceConnectorEmergencyCallingConfiguration $
--             newDeleteVoiceConnectorEmergencyCallingConfiguration
--
--         , requestDeleteVoiceConnectorGroup $
--             newDeleteVoiceConnectorGroup
--
--         , requestDeleteVoiceConnectorOrigination $
--             newDeleteVoiceConnectorOrigination
--
--         , requestDeleteVoiceConnectorProxy $
--             newDeleteVoiceConnectorProxy
--
--         , requestDeleteVoiceConnectorStreamingConfiguration $
--             newDeleteVoiceConnectorStreamingConfiguration
--
--         , requestDeleteVoiceConnectorTermination $
--             newDeleteVoiceConnectorTermination
--
--         , requestDeleteVoiceConnectorTerminationCredentials $
--             newDeleteVoiceConnectorTerminationCredentials
--
--         , requestDisassociatePhoneNumbersFromVoiceConnector $
--             newDisassociatePhoneNumbersFromVoiceConnector
--
--         , requestDisassociatePhoneNumbersFromVoiceConnectorGroup $
--             newDisassociatePhoneNumbersFromVoiceConnectorGroup
--
--         , requestGetGlobalSettings $
--             newGetGlobalSettings
--
--         , requestGetPhoneNumber $
--             newGetPhoneNumber
--
--         , requestGetPhoneNumberOrder $
--             newGetPhoneNumberOrder
--
--         , requestGetPhoneNumberSettings $
--             newGetPhoneNumberSettings
--
--         , requestGetProxySession $
--             newGetProxySession
--
--         , requestGetSipMediaApplication $
--             newGetSipMediaApplication
--
--         , requestGetSipMediaApplicationAlexaSkillConfiguration $
--             newGetSipMediaApplicationAlexaSkillConfiguration
--
--         , requestGetSipMediaApplicationLoggingConfiguration $
--             newGetSipMediaApplicationLoggingConfiguration
--
--         , requestGetSipRule $
--             newGetSipRule
--
--         , requestGetVoiceConnector $
--             newGetVoiceConnector
--
--         , requestGetVoiceConnectorEmergencyCallingConfiguration $
--             newGetVoiceConnectorEmergencyCallingConfiguration
--
--         , requestGetVoiceConnectorGroup $
--             newGetVoiceConnectorGroup
--
--         , requestGetVoiceConnectorLoggingConfiguration $
--             newGetVoiceConnectorLoggingConfiguration
--
--         , requestGetVoiceConnectorOrigination $
--             newGetVoiceConnectorOrigination
--
--         , requestGetVoiceConnectorProxy $
--             newGetVoiceConnectorProxy
--
--         , requestGetVoiceConnectorStreamingConfiguration $
--             newGetVoiceConnectorStreamingConfiguration
--
--         , requestGetVoiceConnectorTermination $
--             newGetVoiceConnectorTermination
--
--         , requestGetVoiceConnectorTerminationHealth $
--             newGetVoiceConnectorTerminationHealth
--
--         , requestListAvailableVoiceConnectorRegions $
--             newListAvailableVoiceConnectorRegions
--
--         , requestListPhoneNumberOrders $
--             newListPhoneNumberOrders
--
--         , requestListPhoneNumbers $
--             newListPhoneNumbers
--
--         , requestListProxySessions $
--             newListProxySessions
--
--         , requestListSipMediaApplications $
--             newListSipMediaApplications
--
--         , requestListSipRules $
--             newListSipRules
--
--         , requestListSupportedPhoneNumberCountries $
--             newListSupportedPhoneNumberCountries
--
--         , requestListVoiceConnectorGroups $
--             newListVoiceConnectorGroups
--
--         , requestListVoiceConnectorTerminationCredentials $
--             newListVoiceConnectorTerminationCredentials
--
--         , requestListVoiceConnectors $
--             newListVoiceConnectors
--
--         , requestPutSipMediaApplicationAlexaSkillConfiguration $
--             newPutSipMediaApplicationAlexaSkillConfiguration
--
--         , requestPutSipMediaApplicationLoggingConfiguration $
--             newPutSipMediaApplicationLoggingConfiguration
--
--         , requestPutVoiceConnectorEmergencyCallingConfiguration $
--             newPutVoiceConnectorEmergencyCallingConfiguration
--
--         , requestPutVoiceConnectorLoggingConfiguration $
--             newPutVoiceConnectorLoggingConfiguration
--
--         , requestPutVoiceConnectorOrigination $
--             newPutVoiceConnectorOrigination
--
--         , requestPutVoiceConnectorProxy $
--             newPutVoiceConnectorProxy
--
--         , requestPutVoiceConnectorStreamingConfiguration $
--             newPutVoiceConnectorStreamingConfiguration
--
--         , requestPutVoiceConnectorTermination $
--             newPutVoiceConnectorTermination
--
--         , requestPutVoiceConnectorTerminationCredentials $
--             newPutVoiceConnectorTerminationCredentials
--
--         , requestRestorePhoneNumber $
--             newRestorePhoneNumber
--
--         , requestSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbers
--
--         , requestUpdateGlobalSettings $
--             newUpdateGlobalSettings
--
--         , requestUpdatePhoneNumber $
--             newUpdatePhoneNumber
--
--         , requestUpdatePhoneNumberSettings $
--             newUpdatePhoneNumberSettings
--
--         , requestUpdateProxySession $
--             newUpdateProxySession
--
--         , requestUpdateSipMediaApplication $
--             newUpdateSipMediaApplication
--
--         , requestUpdateSipMediaApplicationCall $
--             newUpdateSipMediaApplicationCall
--
--         , requestUpdateSipRule $
--             newUpdateSipRule
--
--         , requestUpdateVoiceConnector $
--             newUpdateVoiceConnector
--
--         , requestUpdateVoiceConnectorGroup $
--             newUpdateVoiceConnectorGroup
--
--         , requestValidateE911Address $
--             newValidateE911Address
--
--           ]

--     , testGroup "response"
--         [ responseAssociatePhoneNumbersWithVoiceConnector $
--             newAssociatePhoneNumbersWithVoiceConnectorResponse
--
--         , responseAssociatePhoneNumbersWithVoiceConnectorGroup $
--             newAssociatePhoneNumbersWithVoiceConnectorGroupResponse
--
--         , responseBatchDeletePhoneNumber $
--             newBatchDeletePhoneNumberResponse
--
--         , responseBatchUpdatePhoneNumber $
--             newBatchUpdatePhoneNumberResponse
--
--         , responseCreatePhoneNumberOrder $
--             newCreatePhoneNumberOrderResponse
--
--         , responseCreateProxySession $
--             newCreateProxySessionResponse
--
--         , responseCreateSipMediaApplication $
--             newCreateSipMediaApplicationResponse
--
--         , responseCreateSipMediaApplicationCall $
--             newCreateSipMediaApplicationCallResponse
--
--         , responseCreateSipRule $
--             newCreateSipRuleResponse
--
--         , responseCreateVoiceConnector $
--             newCreateVoiceConnectorResponse
--
--         , responseCreateVoiceConnectorGroup $
--             newCreateVoiceConnectorGroupResponse
--
--         , responseDeletePhoneNumber $
--             newDeletePhoneNumberResponse
--
--         , responseDeleteProxySession $
--             newDeleteProxySessionResponse
--
--         , responseDeleteSipMediaApplication $
--             newDeleteSipMediaApplicationResponse
--
--         , responseDeleteSipRule $
--             newDeleteSipRuleResponse
--
--         , responseDeleteVoiceConnector $
--             newDeleteVoiceConnectorResponse
--
--         , responseDeleteVoiceConnectorEmergencyCallingConfiguration $
--             newDeleteVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responseDeleteVoiceConnectorGroup $
--             newDeleteVoiceConnectorGroupResponse
--
--         , responseDeleteVoiceConnectorOrigination $
--             newDeleteVoiceConnectorOriginationResponse
--
--         , responseDeleteVoiceConnectorProxy $
--             newDeleteVoiceConnectorProxyResponse
--
--         , responseDeleteVoiceConnectorStreamingConfiguration $
--             newDeleteVoiceConnectorStreamingConfigurationResponse
--
--         , responseDeleteVoiceConnectorTermination $
--             newDeleteVoiceConnectorTerminationResponse
--
--         , responseDeleteVoiceConnectorTerminationCredentials $
--             newDeleteVoiceConnectorTerminationCredentialsResponse
--
--         , responseDisassociatePhoneNumbersFromVoiceConnector $
--             newDisassociatePhoneNumbersFromVoiceConnectorResponse
--
--         , responseDisassociatePhoneNumbersFromVoiceConnectorGroup $
--             newDisassociatePhoneNumbersFromVoiceConnectorGroupResponse
--
--         , responseGetGlobalSettings $
--             newGetGlobalSettingsResponse
--
--         , responseGetPhoneNumber $
--             newGetPhoneNumberResponse
--
--         , responseGetPhoneNumberOrder $
--             newGetPhoneNumberOrderResponse
--
--         , responseGetPhoneNumberSettings $
--             newGetPhoneNumberSettingsResponse
--
--         , responseGetProxySession $
--             newGetProxySessionResponse
--
--         , responseGetSipMediaApplication $
--             newGetSipMediaApplicationResponse
--
--         , responseGetSipMediaApplicationAlexaSkillConfiguration $
--             newGetSipMediaApplicationAlexaSkillConfigurationResponse
--
--         , responseGetSipMediaApplicationLoggingConfiguration $
--             newGetSipMediaApplicationLoggingConfigurationResponse
--
--         , responseGetSipRule $
--             newGetSipRuleResponse
--
--         , responseGetVoiceConnector $
--             newGetVoiceConnectorResponse
--
--         , responseGetVoiceConnectorEmergencyCallingConfiguration $
--             newGetVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responseGetVoiceConnectorGroup $
--             newGetVoiceConnectorGroupResponse
--
--         , responseGetVoiceConnectorLoggingConfiguration $
--             newGetVoiceConnectorLoggingConfigurationResponse
--
--         , responseGetVoiceConnectorOrigination $
--             newGetVoiceConnectorOriginationResponse
--
--         , responseGetVoiceConnectorProxy $
--             newGetVoiceConnectorProxyResponse
--
--         , responseGetVoiceConnectorStreamingConfiguration $
--             newGetVoiceConnectorStreamingConfigurationResponse
--
--         , responseGetVoiceConnectorTermination $
--             newGetVoiceConnectorTerminationResponse
--
--         , responseGetVoiceConnectorTerminationHealth $
--             newGetVoiceConnectorTerminationHealthResponse
--
--         , responseListAvailableVoiceConnectorRegions $
--             newListAvailableVoiceConnectorRegionsResponse
--
--         , responseListPhoneNumberOrders $
--             newListPhoneNumberOrdersResponse
--
--         , responseListPhoneNumbers $
--             newListPhoneNumbersResponse
--
--         , responseListProxySessions $
--             newListProxySessionsResponse
--
--         , responseListSipMediaApplications $
--             newListSipMediaApplicationsResponse
--
--         , responseListSipRules $
--             newListSipRulesResponse
--
--         , responseListSupportedPhoneNumberCountries $
--             newListSupportedPhoneNumberCountriesResponse
--
--         , responseListVoiceConnectorGroups $
--             newListVoiceConnectorGroupsResponse
--
--         , responseListVoiceConnectorTerminationCredentials $
--             newListVoiceConnectorTerminationCredentialsResponse
--
--         , responseListVoiceConnectors $
--             newListVoiceConnectorsResponse
--
--         , responsePutSipMediaApplicationAlexaSkillConfiguration $
--             newPutSipMediaApplicationAlexaSkillConfigurationResponse
--
--         , responsePutSipMediaApplicationLoggingConfiguration $
--             newPutSipMediaApplicationLoggingConfigurationResponse
--
--         , responsePutVoiceConnectorEmergencyCallingConfiguration $
--             newPutVoiceConnectorEmergencyCallingConfigurationResponse
--
--         , responsePutVoiceConnectorLoggingConfiguration $
--             newPutVoiceConnectorLoggingConfigurationResponse
--
--         , responsePutVoiceConnectorOrigination $
--             newPutVoiceConnectorOriginationResponse
--
--         , responsePutVoiceConnectorProxy $
--             newPutVoiceConnectorProxyResponse
--
--         , responsePutVoiceConnectorStreamingConfiguration $
--             newPutVoiceConnectorStreamingConfigurationResponse
--
--         , responsePutVoiceConnectorTermination $
--             newPutVoiceConnectorTerminationResponse
--
--         , responsePutVoiceConnectorTerminationCredentials $
--             newPutVoiceConnectorTerminationCredentialsResponse
--
--         , responseRestorePhoneNumber $
--             newRestorePhoneNumberResponse
--
--         , responseSearchAvailablePhoneNumbers $
--             newSearchAvailablePhoneNumbersResponse
--
--         , responseUpdateGlobalSettings $
--             newUpdateGlobalSettingsResponse
--
--         , responseUpdatePhoneNumber $
--             newUpdatePhoneNumberResponse
--
--         , responseUpdatePhoneNumberSettings $
--             newUpdatePhoneNumberSettingsResponse
--
--         , responseUpdateProxySession $
--             newUpdateProxySessionResponse
--
--         , responseUpdateSipMediaApplication $
--             newUpdateSipMediaApplicationResponse
--
--         , responseUpdateSipMediaApplicationCall $
--             newUpdateSipMediaApplicationCallResponse
--
--         , responseUpdateSipRule $
--             newUpdateSipRuleResponse
--
--         , responseUpdateVoiceConnector $
--             newUpdateVoiceConnectorResponse
--
--         , responseUpdateVoiceConnectorGroup $
--             newUpdateVoiceConnectorGroupResponse
--
--         , responseValidateE911Address $
--             newValidateE911AddressResponse
--
--           ]
--     ]

-- Requests

requestAssociatePhoneNumbersWithVoiceConnector :: AssociatePhoneNumbersWithVoiceConnector -> TestTree
requestAssociatePhoneNumbersWithVoiceConnector =
  req
    "AssociatePhoneNumbersWithVoiceConnector"
    "fixture/AssociatePhoneNumbersWithVoiceConnector.yaml"

requestAssociatePhoneNumbersWithVoiceConnectorGroup :: AssociatePhoneNumbersWithVoiceConnectorGroup -> TestTree
requestAssociatePhoneNumbersWithVoiceConnectorGroup =
  req
    "AssociatePhoneNumbersWithVoiceConnectorGroup"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorGroup.yaml"

requestBatchDeletePhoneNumber :: BatchDeletePhoneNumber -> TestTree
requestBatchDeletePhoneNumber =
  req
    "BatchDeletePhoneNumber"
    "fixture/BatchDeletePhoneNumber.yaml"

requestBatchUpdatePhoneNumber :: BatchUpdatePhoneNumber -> TestTree
requestBatchUpdatePhoneNumber =
  req
    "BatchUpdatePhoneNumber"
    "fixture/BatchUpdatePhoneNumber.yaml"

requestCreatePhoneNumberOrder :: CreatePhoneNumberOrder -> TestTree
requestCreatePhoneNumberOrder =
  req
    "CreatePhoneNumberOrder"
    "fixture/CreatePhoneNumberOrder.yaml"

requestCreateProxySession :: CreateProxySession -> TestTree
requestCreateProxySession =
  req
    "CreateProxySession"
    "fixture/CreateProxySession.yaml"

requestCreateSipMediaApplication :: CreateSipMediaApplication -> TestTree
requestCreateSipMediaApplication =
  req
    "CreateSipMediaApplication"
    "fixture/CreateSipMediaApplication.yaml"

requestCreateSipMediaApplicationCall :: CreateSipMediaApplicationCall -> TestTree
requestCreateSipMediaApplicationCall =
  req
    "CreateSipMediaApplicationCall"
    "fixture/CreateSipMediaApplicationCall.yaml"

requestCreateSipRule :: CreateSipRule -> TestTree
requestCreateSipRule =
  req
    "CreateSipRule"
    "fixture/CreateSipRule.yaml"

requestCreateVoiceConnector :: CreateVoiceConnector -> TestTree
requestCreateVoiceConnector =
  req
    "CreateVoiceConnector"
    "fixture/CreateVoiceConnector.yaml"

requestCreateVoiceConnectorGroup :: CreateVoiceConnectorGroup -> TestTree
requestCreateVoiceConnectorGroup =
  req
    "CreateVoiceConnectorGroup"
    "fixture/CreateVoiceConnectorGroup.yaml"

requestDeletePhoneNumber :: DeletePhoneNumber -> TestTree
requestDeletePhoneNumber =
  req
    "DeletePhoneNumber"
    "fixture/DeletePhoneNumber.yaml"

requestDeleteProxySession :: DeleteProxySession -> TestTree
requestDeleteProxySession =
  req
    "DeleteProxySession"
    "fixture/DeleteProxySession.yaml"

requestDeleteSipMediaApplication :: DeleteSipMediaApplication -> TestTree
requestDeleteSipMediaApplication =
  req
    "DeleteSipMediaApplication"
    "fixture/DeleteSipMediaApplication.yaml"

requestDeleteSipRule :: DeleteSipRule -> TestTree
requestDeleteSipRule =
  req
    "DeleteSipRule"
    "fixture/DeleteSipRule.yaml"

requestDeleteVoiceConnector :: DeleteVoiceConnector -> TestTree
requestDeleteVoiceConnector =
  req
    "DeleteVoiceConnector"
    "fixture/DeleteVoiceConnector.yaml"

requestDeleteVoiceConnectorEmergencyCallingConfiguration :: DeleteVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestDeleteVoiceConnectorEmergencyCallingConfiguration =
  req
    "DeleteVoiceConnectorEmergencyCallingConfiguration"
    "fixture/DeleteVoiceConnectorEmergencyCallingConfiguration.yaml"

requestDeleteVoiceConnectorGroup :: DeleteVoiceConnectorGroup -> TestTree
requestDeleteVoiceConnectorGroup =
  req
    "DeleteVoiceConnectorGroup"
    "fixture/DeleteVoiceConnectorGroup.yaml"

requestDeleteVoiceConnectorOrigination :: DeleteVoiceConnectorOrigination -> TestTree
requestDeleteVoiceConnectorOrigination =
  req
    "DeleteVoiceConnectorOrigination"
    "fixture/DeleteVoiceConnectorOrigination.yaml"

requestDeleteVoiceConnectorProxy :: DeleteVoiceConnectorProxy -> TestTree
requestDeleteVoiceConnectorProxy =
  req
    "DeleteVoiceConnectorProxy"
    "fixture/DeleteVoiceConnectorProxy.yaml"

requestDeleteVoiceConnectorStreamingConfiguration :: DeleteVoiceConnectorStreamingConfiguration -> TestTree
requestDeleteVoiceConnectorStreamingConfiguration =
  req
    "DeleteVoiceConnectorStreamingConfiguration"
    "fixture/DeleteVoiceConnectorStreamingConfiguration.yaml"

requestDeleteVoiceConnectorTermination :: DeleteVoiceConnectorTermination -> TestTree
requestDeleteVoiceConnectorTermination =
  req
    "DeleteVoiceConnectorTermination"
    "fixture/DeleteVoiceConnectorTermination.yaml"

requestDeleteVoiceConnectorTerminationCredentials :: DeleteVoiceConnectorTerminationCredentials -> TestTree
requestDeleteVoiceConnectorTerminationCredentials =
  req
    "DeleteVoiceConnectorTerminationCredentials"
    "fixture/DeleteVoiceConnectorTerminationCredentials.yaml"

requestDisassociatePhoneNumbersFromVoiceConnector :: DisassociatePhoneNumbersFromVoiceConnector -> TestTree
requestDisassociatePhoneNumbersFromVoiceConnector =
  req
    "DisassociatePhoneNumbersFromVoiceConnector"
    "fixture/DisassociatePhoneNumbersFromVoiceConnector.yaml"

requestDisassociatePhoneNumbersFromVoiceConnectorGroup :: DisassociatePhoneNumbersFromVoiceConnectorGroup -> TestTree
requestDisassociatePhoneNumbersFromVoiceConnectorGroup =
  req
    "DisassociatePhoneNumbersFromVoiceConnectorGroup"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorGroup.yaml"

requestGetGlobalSettings :: GetGlobalSettings -> TestTree
requestGetGlobalSettings =
  req
    "GetGlobalSettings"
    "fixture/GetGlobalSettings.yaml"

requestGetPhoneNumber :: GetPhoneNumber -> TestTree
requestGetPhoneNumber =
  req
    "GetPhoneNumber"
    "fixture/GetPhoneNumber.yaml"

requestGetPhoneNumberOrder :: GetPhoneNumberOrder -> TestTree
requestGetPhoneNumberOrder =
  req
    "GetPhoneNumberOrder"
    "fixture/GetPhoneNumberOrder.yaml"

requestGetPhoneNumberSettings :: GetPhoneNumberSettings -> TestTree
requestGetPhoneNumberSettings =
  req
    "GetPhoneNumberSettings"
    "fixture/GetPhoneNumberSettings.yaml"

requestGetProxySession :: GetProxySession -> TestTree
requestGetProxySession =
  req
    "GetProxySession"
    "fixture/GetProxySession.yaml"

requestGetSipMediaApplication :: GetSipMediaApplication -> TestTree
requestGetSipMediaApplication =
  req
    "GetSipMediaApplication"
    "fixture/GetSipMediaApplication.yaml"

requestGetSipMediaApplicationAlexaSkillConfiguration :: GetSipMediaApplicationAlexaSkillConfiguration -> TestTree
requestGetSipMediaApplicationAlexaSkillConfiguration =
  req
    "GetSipMediaApplicationAlexaSkillConfiguration"
    "fixture/GetSipMediaApplicationAlexaSkillConfiguration.yaml"

requestGetSipMediaApplicationLoggingConfiguration :: GetSipMediaApplicationLoggingConfiguration -> TestTree
requestGetSipMediaApplicationLoggingConfiguration =
  req
    "GetSipMediaApplicationLoggingConfiguration"
    "fixture/GetSipMediaApplicationLoggingConfiguration.yaml"

requestGetSipRule :: GetSipRule -> TestTree
requestGetSipRule =
  req
    "GetSipRule"
    "fixture/GetSipRule.yaml"

requestGetVoiceConnector :: GetVoiceConnector -> TestTree
requestGetVoiceConnector =
  req
    "GetVoiceConnector"
    "fixture/GetVoiceConnector.yaml"

requestGetVoiceConnectorEmergencyCallingConfiguration :: GetVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestGetVoiceConnectorEmergencyCallingConfiguration =
  req
    "GetVoiceConnectorEmergencyCallingConfiguration"
    "fixture/GetVoiceConnectorEmergencyCallingConfiguration.yaml"

requestGetVoiceConnectorGroup :: GetVoiceConnectorGroup -> TestTree
requestGetVoiceConnectorGroup =
  req
    "GetVoiceConnectorGroup"
    "fixture/GetVoiceConnectorGroup.yaml"

requestGetVoiceConnectorLoggingConfiguration :: GetVoiceConnectorLoggingConfiguration -> TestTree
requestGetVoiceConnectorLoggingConfiguration =
  req
    "GetVoiceConnectorLoggingConfiguration"
    "fixture/GetVoiceConnectorLoggingConfiguration.yaml"

requestGetVoiceConnectorOrigination :: GetVoiceConnectorOrigination -> TestTree
requestGetVoiceConnectorOrigination =
  req
    "GetVoiceConnectorOrigination"
    "fixture/GetVoiceConnectorOrigination.yaml"

requestGetVoiceConnectorProxy :: GetVoiceConnectorProxy -> TestTree
requestGetVoiceConnectorProxy =
  req
    "GetVoiceConnectorProxy"
    "fixture/GetVoiceConnectorProxy.yaml"

requestGetVoiceConnectorStreamingConfiguration :: GetVoiceConnectorStreamingConfiguration -> TestTree
requestGetVoiceConnectorStreamingConfiguration =
  req
    "GetVoiceConnectorStreamingConfiguration"
    "fixture/GetVoiceConnectorStreamingConfiguration.yaml"

requestGetVoiceConnectorTermination :: GetVoiceConnectorTermination -> TestTree
requestGetVoiceConnectorTermination =
  req
    "GetVoiceConnectorTermination"
    "fixture/GetVoiceConnectorTermination.yaml"

requestGetVoiceConnectorTerminationHealth :: GetVoiceConnectorTerminationHealth -> TestTree
requestGetVoiceConnectorTerminationHealth =
  req
    "GetVoiceConnectorTerminationHealth"
    "fixture/GetVoiceConnectorTerminationHealth.yaml"

requestListAvailableVoiceConnectorRegions :: ListAvailableVoiceConnectorRegions -> TestTree
requestListAvailableVoiceConnectorRegions =
  req
    "ListAvailableVoiceConnectorRegions"
    "fixture/ListAvailableVoiceConnectorRegions.yaml"

requestListPhoneNumberOrders :: ListPhoneNumberOrders -> TestTree
requestListPhoneNumberOrders =
  req
    "ListPhoneNumberOrders"
    "fixture/ListPhoneNumberOrders.yaml"

requestListPhoneNumbers :: ListPhoneNumbers -> TestTree
requestListPhoneNumbers =
  req
    "ListPhoneNumbers"
    "fixture/ListPhoneNumbers.yaml"

requestListProxySessions :: ListProxySessions -> TestTree
requestListProxySessions =
  req
    "ListProxySessions"
    "fixture/ListProxySessions.yaml"

requestListSipMediaApplications :: ListSipMediaApplications -> TestTree
requestListSipMediaApplications =
  req
    "ListSipMediaApplications"
    "fixture/ListSipMediaApplications.yaml"

requestListSipRules :: ListSipRules -> TestTree
requestListSipRules =
  req
    "ListSipRules"
    "fixture/ListSipRules.yaml"

requestListSupportedPhoneNumberCountries :: ListSupportedPhoneNumberCountries -> TestTree
requestListSupportedPhoneNumberCountries =
  req
    "ListSupportedPhoneNumberCountries"
    "fixture/ListSupportedPhoneNumberCountries.yaml"

requestListVoiceConnectorGroups :: ListVoiceConnectorGroups -> TestTree
requestListVoiceConnectorGroups =
  req
    "ListVoiceConnectorGroups"
    "fixture/ListVoiceConnectorGroups.yaml"

requestListVoiceConnectorTerminationCredentials :: ListVoiceConnectorTerminationCredentials -> TestTree
requestListVoiceConnectorTerminationCredentials =
  req
    "ListVoiceConnectorTerminationCredentials"
    "fixture/ListVoiceConnectorTerminationCredentials.yaml"

requestListVoiceConnectors :: ListVoiceConnectors -> TestTree
requestListVoiceConnectors =
  req
    "ListVoiceConnectors"
    "fixture/ListVoiceConnectors.yaml"

requestPutSipMediaApplicationAlexaSkillConfiguration :: PutSipMediaApplicationAlexaSkillConfiguration -> TestTree
requestPutSipMediaApplicationAlexaSkillConfiguration =
  req
    "PutSipMediaApplicationAlexaSkillConfiguration"
    "fixture/PutSipMediaApplicationAlexaSkillConfiguration.yaml"

requestPutSipMediaApplicationLoggingConfiguration :: PutSipMediaApplicationLoggingConfiguration -> TestTree
requestPutSipMediaApplicationLoggingConfiguration =
  req
    "PutSipMediaApplicationLoggingConfiguration"
    "fixture/PutSipMediaApplicationLoggingConfiguration.yaml"

requestPutVoiceConnectorEmergencyCallingConfiguration :: PutVoiceConnectorEmergencyCallingConfiguration -> TestTree
requestPutVoiceConnectorEmergencyCallingConfiguration =
  req
    "PutVoiceConnectorEmergencyCallingConfiguration"
    "fixture/PutVoiceConnectorEmergencyCallingConfiguration.yaml"

requestPutVoiceConnectorLoggingConfiguration :: PutVoiceConnectorLoggingConfiguration -> TestTree
requestPutVoiceConnectorLoggingConfiguration =
  req
    "PutVoiceConnectorLoggingConfiguration"
    "fixture/PutVoiceConnectorLoggingConfiguration.yaml"

requestPutVoiceConnectorOrigination :: PutVoiceConnectorOrigination -> TestTree
requestPutVoiceConnectorOrigination =
  req
    "PutVoiceConnectorOrigination"
    "fixture/PutVoiceConnectorOrigination.yaml"

requestPutVoiceConnectorProxy :: PutVoiceConnectorProxy -> TestTree
requestPutVoiceConnectorProxy =
  req
    "PutVoiceConnectorProxy"
    "fixture/PutVoiceConnectorProxy.yaml"

requestPutVoiceConnectorStreamingConfiguration :: PutVoiceConnectorStreamingConfiguration -> TestTree
requestPutVoiceConnectorStreamingConfiguration =
  req
    "PutVoiceConnectorStreamingConfiguration"
    "fixture/PutVoiceConnectorStreamingConfiguration.yaml"

requestPutVoiceConnectorTermination :: PutVoiceConnectorTermination -> TestTree
requestPutVoiceConnectorTermination =
  req
    "PutVoiceConnectorTermination"
    "fixture/PutVoiceConnectorTermination.yaml"

requestPutVoiceConnectorTerminationCredentials :: PutVoiceConnectorTerminationCredentials -> TestTree
requestPutVoiceConnectorTerminationCredentials =
  req
    "PutVoiceConnectorTerminationCredentials"
    "fixture/PutVoiceConnectorTerminationCredentials.yaml"

requestRestorePhoneNumber :: RestorePhoneNumber -> TestTree
requestRestorePhoneNumber =
  req
    "RestorePhoneNumber"
    "fixture/RestorePhoneNumber.yaml"

requestSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbers -> TestTree
requestSearchAvailablePhoneNumbers =
  req
    "SearchAvailablePhoneNumbers"
    "fixture/SearchAvailablePhoneNumbers.yaml"

requestUpdateGlobalSettings :: UpdateGlobalSettings -> TestTree
requestUpdateGlobalSettings =
  req
    "UpdateGlobalSettings"
    "fixture/UpdateGlobalSettings.yaml"

requestUpdatePhoneNumber :: UpdatePhoneNumber -> TestTree
requestUpdatePhoneNumber =
  req
    "UpdatePhoneNumber"
    "fixture/UpdatePhoneNumber.yaml"

requestUpdatePhoneNumberSettings :: UpdatePhoneNumberSettings -> TestTree
requestUpdatePhoneNumberSettings =
  req
    "UpdatePhoneNumberSettings"
    "fixture/UpdatePhoneNumberSettings.yaml"

requestUpdateProxySession :: UpdateProxySession -> TestTree
requestUpdateProxySession =
  req
    "UpdateProxySession"
    "fixture/UpdateProxySession.yaml"

requestUpdateSipMediaApplication :: UpdateSipMediaApplication -> TestTree
requestUpdateSipMediaApplication =
  req
    "UpdateSipMediaApplication"
    "fixture/UpdateSipMediaApplication.yaml"

requestUpdateSipMediaApplicationCall :: UpdateSipMediaApplicationCall -> TestTree
requestUpdateSipMediaApplicationCall =
  req
    "UpdateSipMediaApplicationCall"
    "fixture/UpdateSipMediaApplicationCall.yaml"

requestUpdateSipRule :: UpdateSipRule -> TestTree
requestUpdateSipRule =
  req
    "UpdateSipRule"
    "fixture/UpdateSipRule.yaml"

requestUpdateVoiceConnector :: UpdateVoiceConnector -> TestTree
requestUpdateVoiceConnector =
  req
    "UpdateVoiceConnector"
    "fixture/UpdateVoiceConnector.yaml"

requestUpdateVoiceConnectorGroup :: UpdateVoiceConnectorGroup -> TestTree
requestUpdateVoiceConnectorGroup =
  req
    "UpdateVoiceConnectorGroup"
    "fixture/UpdateVoiceConnectorGroup.yaml"

requestValidateE911Address :: ValidateE911Address -> TestTree
requestValidateE911Address =
  req
    "ValidateE911Address"
    "fixture/ValidateE911Address.yaml"

-- Responses

responseAssociatePhoneNumbersWithVoiceConnector :: AssociatePhoneNumbersWithVoiceConnectorResponse -> TestTree
responseAssociatePhoneNumbersWithVoiceConnector =
  res
    "AssociatePhoneNumbersWithVoiceConnectorResponse"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePhoneNumbersWithVoiceConnector)

responseAssociatePhoneNumbersWithVoiceConnectorGroup :: AssociatePhoneNumbersWithVoiceConnectorGroupResponse -> TestTree
responseAssociatePhoneNumbersWithVoiceConnectorGroup =
  res
    "AssociatePhoneNumbersWithVoiceConnectorGroupResponse"
    "fixture/AssociatePhoneNumbersWithVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociatePhoneNumbersWithVoiceConnectorGroup)

responseBatchDeletePhoneNumber :: BatchDeletePhoneNumberResponse -> TestTree
responseBatchDeletePhoneNumber =
  res
    "BatchDeletePhoneNumberResponse"
    "fixture/BatchDeletePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeletePhoneNumber)

responseBatchUpdatePhoneNumber :: BatchUpdatePhoneNumberResponse -> TestTree
responseBatchUpdatePhoneNumber =
  res
    "BatchUpdatePhoneNumberResponse"
    "fixture/BatchUpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdatePhoneNumber)

responseCreatePhoneNumberOrder :: CreatePhoneNumberOrderResponse -> TestTree
responseCreatePhoneNumberOrder =
  res
    "CreatePhoneNumberOrderResponse"
    "fixture/CreatePhoneNumberOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePhoneNumberOrder)

responseCreateProxySession :: CreateProxySessionResponse -> TestTree
responseCreateProxySession =
  res
    "CreateProxySessionResponse"
    "fixture/CreateProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProxySession)

responseCreateSipMediaApplication :: CreateSipMediaApplicationResponse -> TestTree
responseCreateSipMediaApplication =
  res
    "CreateSipMediaApplicationResponse"
    "fixture/CreateSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSipMediaApplication)

responseCreateSipMediaApplicationCall :: CreateSipMediaApplicationCallResponse -> TestTree
responseCreateSipMediaApplicationCall =
  res
    "CreateSipMediaApplicationCallResponse"
    "fixture/CreateSipMediaApplicationCallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSipMediaApplicationCall)

responseCreateSipRule :: CreateSipRuleResponse -> TestTree
responseCreateSipRule =
  res
    "CreateSipRuleResponse"
    "fixture/CreateSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSipRule)

responseCreateVoiceConnector :: CreateVoiceConnectorResponse -> TestTree
responseCreateVoiceConnector =
  res
    "CreateVoiceConnectorResponse"
    "fixture/CreateVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVoiceConnector)

responseCreateVoiceConnectorGroup :: CreateVoiceConnectorGroupResponse -> TestTree
responseCreateVoiceConnectorGroup =
  res
    "CreateVoiceConnectorGroupResponse"
    "fixture/CreateVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVoiceConnectorGroup)

responseDeletePhoneNumber :: DeletePhoneNumberResponse -> TestTree
responseDeletePhoneNumber =
  res
    "DeletePhoneNumberResponse"
    "fixture/DeletePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePhoneNumber)

responseDeleteProxySession :: DeleteProxySessionResponse -> TestTree
responseDeleteProxySession =
  res
    "DeleteProxySessionResponse"
    "fixture/DeleteProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProxySession)

responseDeleteSipMediaApplication :: DeleteSipMediaApplicationResponse -> TestTree
responseDeleteSipMediaApplication =
  res
    "DeleteSipMediaApplicationResponse"
    "fixture/DeleteSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSipMediaApplication)

responseDeleteSipRule :: DeleteSipRuleResponse -> TestTree
responseDeleteSipRule =
  res
    "DeleteSipRuleResponse"
    "fixture/DeleteSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSipRule)

responseDeleteVoiceConnector :: DeleteVoiceConnectorResponse -> TestTree
responseDeleteVoiceConnector =
  res
    "DeleteVoiceConnectorResponse"
    "fixture/DeleteVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnector)

responseDeleteVoiceConnectorEmergencyCallingConfiguration :: DeleteVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responseDeleteVoiceConnectorEmergencyCallingConfiguration =
  res
    "DeleteVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/DeleteVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorEmergencyCallingConfiguration)

responseDeleteVoiceConnectorGroup :: DeleteVoiceConnectorGroupResponse -> TestTree
responseDeleteVoiceConnectorGroup =
  res
    "DeleteVoiceConnectorGroupResponse"
    "fixture/DeleteVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorGroup)

responseDeleteVoiceConnectorOrigination :: DeleteVoiceConnectorOriginationResponse -> TestTree
responseDeleteVoiceConnectorOrigination =
  res
    "DeleteVoiceConnectorOriginationResponse"
    "fixture/DeleteVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorOrigination)

responseDeleteVoiceConnectorProxy :: DeleteVoiceConnectorProxyResponse -> TestTree
responseDeleteVoiceConnectorProxy =
  res
    "DeleteVoiceConnectorProxyResponse"
    "fixture/DeleteVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorProxy)

responseDeleteVoiceConnectorStreamingConfiguration :: DeleteVoiceConnectorStreamingConfigurationResponse -> TestTree
responseDeleteVoiceConnectorStreamingConfiguration =
  res
    "DeleteVoiceConnectorStreamingConfigurationResponse"
    "fixture/DeleteVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorStreamingConfiguration)

responseDeleteVoiceConnectorTermination :: DeleteVoiceConnectorTerminationResponse -> TestTree
responseDeleteVoiceConnectorTermination =
  res
    "DeleteVoiceConnectorTerminationResponse"
    "fixture/DeleteVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorTermination)

responseDeleteVoiceConnectorTerminationCredentials :: DeleteVoiceConnectorTerminationCredentialsResponse -> TestTree
responseDeleteVoiceConnectorTerminationCredentials =
  res
    "DeleteVoiceConnectorTerminationCredentialsResponse"
    "fixture/DeleteVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceConnectorTerminationCredentials)

responseDisassociatePhoneNumbersFromVoiceConnector :: DisassociatePhoneNumbersFromVoiceConnectorResponse -> TestTree
responseDisassociatePhoneNumbersFromVoiceConnector =
  res
    "DisassociatePhoneNumbersFromVoiceConnectorResponse"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePhoneNumbersFromVoiceConnector)

responseDisassociatePhoneNumbersFromVoiceConnectorGroup :: DisassociatePhoneNumbersFromVoiceConnectorGroupResponse -> TestTree
responseDisassociatePhoneNumbersFromVoiceConnectorGroup =
  res
    "DisassociatePhoneNumbersFromVoiceConnectorGroupResponse"
    "fixture/DisassociatePhoneNumbersFromVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociatePhoneNumbersFromVoiceConnectorGroup)

responseGetGlobalSettings :: GetGlobalSettingsResponse -> TestTree
responseGetGlobalSettings =
  res
    "GetGlobalSettingsResponse"
    "fixture/GetGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGlobalSettings)

responseGetPhoneNumber :: GetPhoneNumberResponse -> TestTree
responseGetPhoneNumber =
  res
    "GetPhoneNumberResponse"
    "fixture/GetPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPhoneNumber)

responseGetPhoneNumberOrder :: GetPhoneNumberOrderResponse -> TestTree
responseGetPhoneNumberOrder =
  res
    "GetPhoneNumberOrderResponse"
    "fixture/GetPhoneNumberOrderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPhoneNumberOrder)

responseGetPhoneNumberSettings :: GetPhoneNumberSettingsResponse -> TestTree
responseGetPhoneNumberSettings =
  res
    "GetPhoneNumberSettingsResponse"
    "fixture/GetPhoneNumberSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPhoneNumberSettings)

responseGetProxySession :: GetProxySessionResponse -> TestTree
responseGetProxySession =
  res
    "GetProxySessionResponse"
    "fixture/GetProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProxySession)

responseGetSipMediaApplication :: GetSipMediaApplicationResponse -> TestTree
responseGetSipMediaApplication =
  res
    "GetSipMediaApplicationResponse"
    "fixture/GetSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSipMediaApplication)

responseGetSipMediaApplicationAlexaSkillConfiguration :: GetSipMediaApplicationAlexaSkillConfigurationResponse -> TestTree
responseGetSipMediaApplicationAlexaSkillConfiguration =
  res
    "GetSipMediaApplicationAlexaSkillConfigurationResponse"
    "fixture/GetSipMediaApplicationAlexaSkillConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSipMediaApplicationAlexaSkillConfiguration)

responseGetSipMediaApplicationLoggingConfiguration :: GetSipMediaApplicationLoggingConfigurationResponse -> TestTree
responseGetSipMediaApplicationLoggingConfiguration =
  res
    "GetSipMediaApplicationLoggingConfigurationResponse"
    "fixture/GetSipMediaApplicationLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSipMediaApplicationLoggingConfiguration)

responseGetSipRule :: GetSipRuleResponse -> TestTree
responseGetSipRule =
  res
    "GetSipRuleResponse"
    "fixture/GetSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSipRule)

responseGetVoiceConnector :: GetVoiceConnectorResponse -> TestTree
responseGetVoiceConnector =
  res
    "GetVoiceConnectorResponse"
    "fixture/GetVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnector)

responseGetVoiceConnectorEmergencyCallingConfiguration :: GetVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responseGetVoiceConnectorEmergencyCallingConfiguration =
  res
    "GetVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/GetVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorEmergencyCallingConfiguration)

responseGetVoiceConnectorGroup :: GetVoiceConnectorGroupResponse -> TestTree
responseGetVoiceConnectorGroup =
  res
    "GetVoiceConnectorGroupResponse"
    "fixture/GetVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorGroup)

responseGetVoiceConnectorLoggingConfiguration :: GetVoiceConnectorLoggingConfigurationResponse -> TestTree
responseGetVoiceConnectorLoggingConfiguration =
  res
    "GetVoiceConnectorLoggingConfigurationResponse"
    "fixture/GetVoiceConnectorLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorLoggingConfiguration)

responseGetVoiceConnectorOrigination :: GetVoiceConnectorOriginationResponse -> TestTree
responseGetVoiceConnectorOrigination =
  res
    "GetVoiceConnectorOriginationResponse"
    "fixture/GetVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorOrigination)

responseGetVoiceConnectorProxy :: GetVoiceConnectorProxyResponse -> TestTree
responseGetVoiceConnectorProxy =
  res
    "GetVoiceConnectorProxyResponse"
    "fixture/GetVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorProxy)

responseGetVoiceConnectorStreamingConfiguration :: GetVoiceConnectorStreamingConfigurationResponse -> TestTree
responseGetVoiceConnectorStreamingConfiguration =
  res
    "GetVoiceConnectorStreamingConfigurationResponse"
    "fixture/GetVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorStreamingConfiguration)

responseGetVoiceConnectorTermination :: GetVoiceConnectorTerminationResponse -> TestTree
responseGetVoiceConnectorTermination =
  res
    "GetVoiceConnectorTerminationResponse"
    "fixture/GetVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorTermination)

responseGetVoiceConnectorTerminationHealth :: GetVoiceConnectorTerminationHealthResponse -> TestTree
responseGetVoiceConnectorTerminationHealth =
  res
    "GetVoiceConnectorTerminationHealthResponse"
    "fixture/GetVoiceConnectorTerminationHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetVoiceConnectorTerminationHealth)

responseListAvailableVoiceConnectorRegions :: ListAvailableVoiceConnectorRegionsResponse -> TestTree
responseListAvailableVoiceConnectorRegions =
  res
    "ListAvailableVoiceConnectorRegionsResponse"
    "fixture/ListAvailableVoiceConnectorRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableVoiceConnectorRegions)

responseListPhoneNumberOrders :: ListPhoneNumberOrdersResponse -> TestTree
responseListPhoneNumberOrders =
  res
    "ListPhoneNumberOrdersResponse"
    "fixture/ListPhoneNumberOrdersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumberOrders)

responseListPhoneNumbers :: ListPhoneNumbersResponse -> TestTree
responseListPhoneNumbers =
  res
    "ListPhoneNumbersResponse"
    "fixture/ListPhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumbers)

responseListProxySessions :: ListProxySessionsResponse -> TestTree
responseListProxySessions =
  res
    "ListProxySessionsResponse"
    "fixture/ListProxySessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProxySessions)

responseListSipMediaApplications :: ListSipMediaApplicationsResponse -> TestTree
responseListSipMediaApplications =
  res
    "ListSipMediaApplicationsResponse"
    "fixture/ListSipMediaApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSipMediaApplications)

responseListSipRules :: ListSipRulesResponse -> TestTree
responseListSipRules =
  res
    "ListSipRulesResponse"
    "fixture/ListSipRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSipRules)

responseListSupportedPhoneNumberCountries :: ListSupportedPhoneNumberCountriesResponse -> TestTree
responseListSupportedPhoneNumberCountries =
  res
    "ListSupportedPhoneNumberCountriesResponse"
    "fixture/ListSupportedPhoneNumberCountriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSupportedPhoneNumberCountries)

responseListVoiceConnectorGroups :: ListVoiceConnectorGroupsResponse -> TestTree
responseListVoiceConnectorGroups =
  res
    "ListVoiceConnectorGroupsResponse"
    "fixture/ListVoiceConnectorGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVoiceConnectorGroups)

responseListVoiceConnectorTerminationCredentials :: ListVoiceConnectorTerminationCredentialsResponse -> TestTree
responseListVoiceConnectorTerminationCredentials =
  res
    "ListVoiceConnectorTerminationCredentialsResponse"
    "fixture/ListVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVoiceConnectorTerminationCredentials)

responseListVoiceConnectors :: ListVoiceConnectorsResponse -> TestTree
responseListVoiceConnectors =
  res
    "ListVoiceConnectorsResponse"
    "fixture/ListVoiceConnectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVoiceConnectors)

responsePutSipMediaApplicationAlexaSkillConfiguration :: PutSipMediaApplicationAlexaSkillConfigurationResponse -> TestTree
responsePutSipMediaApplicationAlexaSkillConfiguration =
  res
    "PutSipMediaApplicationAlexaSkillConfigurationResponse"
    "fixture/PutSipMediaApplicationAlexaSkillConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSipMediaApplicationAlexaSkillConfiguration)

responsePutSipMediaApplicationLoggingConfiguration :: PutSipMediaApplicationLoggingConfigurationResponse -> TestTree
responsePutSipMediaApplicationLoggingConfiguration =
  res
    "PutSipMediaApplicationLoggingConfigurationResponse"
    "fixture/PutSipMediaApplicationLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSipMediaApplicationLoggingConfiguration)

responsePutVoiceConnectorEmergencyCallingConfiguration :: PutVoiceConnectorEmergencyCallingConfigurationResponse -> TestTree
responsePutVoiceConnectorEmergencyCallingConfiguration =
  res
    "PutVoiceConnectorEmergencyCallingConfigurationResponse"
    "fixture/PutVoiceConnectorEmergencyCallingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorEmergencyCallingConfiguration)

responsePutVoiceConnectorLoggingConfiguration :: PutVoiceConnectorLoggingConfigurationResponse -> TestTree
responsePutVoiceConnectorLoggingConfiguration =
  res
    "PutVoiceConnectorLoggingConfigurationResponse"
    "fixture/PutVoiceConnectorLoggingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorLoggingConfiguration)

responsePutVoiceConnectorOrigination :: PutVoiceConnectorOriginationResponse -> TestTree
responsePutVoiceConnectorOrigination =
  res
    "PutVoiceConnectorOriginationResponse"
    "fixture/PutVoiceConnectorOriginationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorOrigination)

responsePutVoiceConnectorProxy :: PutVoiceConnectorProxyResponse -> TestTree
responsePutVoiceConnectorProxy =
  res
    "PutVoiceConnectorProxyResponse"
    "fixture/PutVoiceConnectorProxyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorProxy)

responsePutVoiceConnectorStreamingConfiguration :: PutVoiceConnectorStreamingConfigurationResponse -> TestTree
responsePutVoiceConnectorStreamingConfiguration =
  res
    "PutVoiceConnectorStreamingConfigurationResponse"
    "fixture/PutVoiceConnectorStreamingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorStreamingConfiguration)

responsePutVoiceConnectorTermination :: PutVoiceConnectorTerminationResponse -> TestTree
responsePutVoiceConnectorTermination =
  res
    "PutVoiceConnectorTerminationResponse"
    "fixture/PutVoiceConnectorTerminationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorTermination)

responsePutVoiceConnectorTerminationCredentials :: PutVoiceConnectorTerminationCredentialsResponse -> TestTree
responsePutVoiceConnectorTerminationCredentials =
  res
    "PutVoiceConnectorTerminationCredentialsResponse"
    "fixture/PutVoiceConnectorTerminationCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutVoiceConnectorTerminationCredentials)

responseRestorePhoneNumber :: RestorePhoneNumberResponse -> TestTree
responseRestorePhoneNumber =
  res
    "RestorePhoneNumberResponse"
    "fixture/RestorePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestorePhoneNumber)

responseSearchAvailablePhoneNumbers :: SearchAvailablePhoneNumbersResponse -> TestTree
responseSearchAvailablePhoneNumbers =
  res
    "SearchAvailablePhoneNumbersResponse"
    "fixture/SearchAvailablePhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchAvailablePhoneNumbers)

responseUpdateGlobalSettings :: UpdateGlobalSettingsResponse -> TestTree
responseUpdateGlobalSettings =
  res
    "UpdateGlobalSettingsResponse"
    "fixture/UpdateGlobalSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGlobalSettings)

responseUpdatePhoneNumber :: UpdatePhoneNumberResponse -> TestTree
responseUpdatePhoneNumber =
  res
    "UpdatePhoneNumberResponse"
    "fixture/UpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePhoneNumber)

responseUpdatePhoneNumberSettings :: UpdatePhoneNumberSettingsResponse -> TestTree
responseUpdatePhoneNumberSettings =
  res
    "UpdatePhoneNumberSettingsResponse"
    "fixture/UpdatePhoneNumberSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePhoneNumberSettings)

responseUpdateProxySession :: UpdateProxySessionResponse -> TestTree
responseUpdateProxySession =
  res
    "UpdateProxySessionResponse"
    "fixture/UpdateProxySessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProxySession)

responseUpdateSipMediaApplication :: UpdateSipMediaApplicationResponse -> TestTree
responseUpdateSipMediaApplication =
  res
    "UpdateSipMediaApplicationResponse"
    "fixture/UpdateSipMediaApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSipMediaApplication)

responseUpdateSipMediaApplicationCall :: UpdateSipMediaApplicationCallResponse -> TestTree
responseUpdateSipMediaApplicationCall =
  res
    "UpdateSipMediaApplicationCallResponse"
    "fixture/UpdateSipMediaApplicationCallResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSipMediaApplicationCall)

responseUpdateSipRule :: UpdateSipRuleResponse -> TestTree
responseUpdateSipRule =
  res
    "UpdateSipRuleResponse"
    "fixture/UpdateSipRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSipRule)

responseUpdateVoiceConnector :: UpdateVoiceConnectorResponse -> TestTree
responseUpdateVoiceConnector =
  res
    "UpdateVoiceConnectorResponse"
    "fixture/UpdateVoiceConnectorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceConnector)

responseUpdateVoiceConnectorGroup :: UpdateVoiceConnectorGroupResponse -> TestTree
responseUpdateVoiceConnectorGroup =
  res
    "UpdateVoiceConnectorGroupResponse"
    "fixture/UpdateVoiceConnectorGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateVoiceConnectorGroup)

responseValidateE911Address :: ValidateE911AddressResponse -> TestTree
responseValidateE911Address =
  res
    "ValidateE911AddressResponse"
    "fixture/ValidateE911AddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateE911Address)

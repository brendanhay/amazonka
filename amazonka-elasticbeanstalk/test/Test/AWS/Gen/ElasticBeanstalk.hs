-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.ElasticBeanstalk where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.ElasticBeanstalk

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ abortEnvironmentUpdateTest $
--             abortEnvironmentUpdate
--
--         , checkDNSAvailabilityTest $
--             checkDNSAvailability
--
--         , createApplicationTest $
--             createApplication
--
--         , createApplicationVersionTest $
--             createApplicationVersion
--
--         , createConfigurationTemplateTest $
--             createConfigurationTemplate
--
--         , createEnvironmentTest $
--             createEnvironment
--
--         , createStorageLocationTest $
--             createStorageLocation
--
--         , deleteApplicationTest $
--             deleteApplication
--
--         , deleteApplicationVersionTest $
--             deleteApplicationVersion
--
--         , deleteConfigurationTemplateTest $
--             deleteConfigurationTemplate
--
--         , deleteEnvironmentConfigurationTest $
--             deleteEnvironmentConfiguration
--
--         , describeApplicationVersionsTest $
--             describeApplicationVersions
--
--         , describeApplicationsTest $
--             describeApplications
--
--         , describeConfigurationOptionsTest $
--             describeConfigurationOptions
--
--         , describeConfigurationSettingsTest $
--             describeConfigurationSettings
--
--         , describeEnvironmentResourcesTest $
--             describeEnvironmentResources
--
--         , describeEnvironmentsTest $
--             describeEnvironments
--
--         , describeEventsTest $
--             describeEvents
--
--         , listAvailableSolutionStacksTest $
--             listAvailableSolutionStacks
--
--         , rebuildEnvironmentTest $
--             rebuildEnvironment
--
--         , requestEnvironmentInfoTest $
--             requestEnvironmentInfo
--
--         , restartAppServerTest $
--             restartAppServer
--
--         , retrieveEnvironmentInfoTest $
--             retrieveEnvironmentInfo
--
--         , swapEnvironmentCNAMEsTest $
--             swapEnvironmentCNAMEs
--
--         , terminateEnvironmentTest $
--             terminateEnvironment
--
--         , updateApplicationTest $
--             updateApplication
--
--         , updateApplicationVersionTest $
--             updateApplicationVersion
--
--         , updateConfigurationTemplateTest $
--             updateConfigurationTemplate
--
--         , updateEnvironmentTest $
--             updateEnvironment
--
--         , validateConfigurationSettingsTest $
--             validateConfigurationSettings
--
--           ]

--     , testGroup "response"
--         [ abortEnvironmentUpdateResponseTest $
--             abortEnvironmentUpdateResponse
--
--         , checkDNSAvailabilityResponseTest $
--             checkDNSAvailabilityResponse
--
--         , createApplicationResponseTest $
--             applicationDescriptionMessage
--
--         , createApplicationVersionResponseTest $
--             applicationVersionDescriptionMessage
--
--         , createConfigurationTemplateResponseTest $
--             configurationSettingsDescription
--
--         , createEnvironmentResponseTest $
--             environmentDescription
--
--         , createStorageLocationResponseTest $
--             createStorageLocationResponse
--
--         , deleteApplicationResponseTest $
--             deleteApplicationResponse
--
--         , deleteApplicationVersionResponseTest $
--             deleteApplicationVersionResponse
--
--         , deleteConfigurationTemplateResponseTest $
--             deleteConfigurationTemplateResponse
--
--         , deleteEnvironmentConfigurationResponseTest $
--             deleteEnvironmentConfigurationResponse
--
--         , describeApplicationVersionsResponseTest $
--             describeApplicationVersionsResponse
--
--         , describeApplicationsResponseTest $
--             describeApplicationsResponse
--
--         , describeConfigurationOptionsResponseTest $
--             describeConfigurationOptionsResponse
--
--         , describeConfigurationSettingsResponseTest $
--             describeConfigurationSettingsResponse
--
--         , describeEnvironmentResourcesResponseTest $
--             describeEnvironmentResourcesResponse
--
--         , describeEnvironmentsResponseTest $
--             describeEnvironmentsResponse
--
--         , describeEventsResponseTest $
--             describeEventsResponse
--
--         , listAvailableSolutionStacksResponseTest $
--             listAvailableSolutionStacksResponse
--
--         , rebuildEnvironmentResponseTest $
--             rebuildEnvironmentResponse
--
--         , requestEnvironmentInfoResponseTest $
--             requestEnvironmentInfoResponse
--
--         , restartAppServerResponseTest $
--             restartAppServerResponse
--
--         , retrieveEnvironmentInfoResponseTest $
--             retrieveEnvironmentInfoResponse
--
--         , swapEnvironmentCNAMEsResponseTest $
--             swapEnvironmentCNAMEsResponse
--
--         , terminateEnvironmentResponseTest $
--             environmentDescription
--
--         , updateApplicationResponseTest $
--             applicationDescriptionMessage
--
--         , updateApplicationVersionResponseTest $
--             applicationVersionDescriptionMessage
--
--         , updateConfigurationTemplateResponseTest $
--             configurationSettingsDescription
--
--         , updateEnvironmentResponseTest $
--             environmentDescription
--
--         , validateConfigurationSettingsResponseTest $
--             validateConfigurationSettingsResponse
--
--           ]
--     ]

-- Requests

abortEnvironmentUpdateTest :: AbortEnvironmentUpdate -> TestTree
abortEnvironmentUpdateTest = undefined

checkDNSAvailabilityTest :: CheckDNSAvailability -> TestTree
checkDNSAvailabilityTest = undefined

createApplicationTest :: CreateApplication -> TestTree
createApplicationTest = undefined

createApplicationVersionTest :: CreateApplicationVersion -> TestTree
createApplicationVersionTest = undefined

createConfigurationTemplateTest :: CreateConfigurationTemplate -> TestTree
createConfigurationTemplateTest = undefined

createEnvironmentTest :: CreateEnvironment -> TestTree
createEnvironmentTest = undefined

createStorageLocationTest :: CreateStorageLocation -> TestTree
createStorageLocationTest = undefined

deleteApplicationTest :: DeleteApplication -> TestTree
deleteApplicationTest = undefined

deleteApplicationVersionTest :: DeleteApplicationVersion -> TestTree
deleteApplicationVersionTest = undefined

deleteConfigurationTemplateTest :: DeleteConfigurationTemplate -> TestTree
deleteConfigurationTemplateTest = undefined

deleteEnvironmentConfigurationTest :: DeleteEnvironmentConfiguration -> TestTree
deleteEnvironmentConfigurationTest = undefined

describeApplicationVersionsTest :: DescribeApplicationVersions -> TestTree
describeApplicationVersionsTest = undefined

describeApplicationsTest :: DescribeApplications -> TestTree
describeApplicationsTest = undefined

describeConfigurationOptionsTest :: DescribeConfigurationOptions -> TestTree
describeConfigurationOptionsTest = undefined

describeConfigurationSettingsTest :: DescribeConfigurationSettings -> TestTree
describeConfigurationSettingsTest = undefined

describeEnvironmentResourcesTest :: DescribeEnvironmentResources -> TestTree
describeEnvironmentResourcesTest = undefined

describeEnvironmentsTest :: DescribeEnvironments -> TestTree
describeEnvironmentsTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

listAvailableSolutionStacksTest :: ListAvailableSolutionStacks -> TestTree
listAvailableSolutionStacksTest = undefined

rebuildEnvironmentTest :: RebuildEnvironment -> TestTree
rebuildEnvironmentTest = undefined

requestEnvironmentInfoTest :: RequestEnvironmentInfo -> TestTree
requestEnvironmentInfoTest = undefined

restartAppServerTest :: RestartAppServer -> TestTree
restartAppServerTest = undefined

retrieveEnvironmentInfoTest :: RetrieveEnvironmentInfo -> TestTree
retrieveEnvironmentInfoTest = undefined

swapEnvironmentCNAMEsTest :: SwapEnvironmentCNAMEs -> TestTree
swapEnvironmentCNAMEsTest = undefined

terminateEnvironmentTest :: TerminateEnvironment -> TestTree
terminateEnvironmentTest = undefined

updateApplicationTest :: UpdateApplication -> TestTree
updateApplicationTest = undefined

updateApplicationVersionTest :: UpdateApplicationVersion -> TestTree
updateApplicationVersionTest = undefined

updateConfigurationTemplateTest :: UpdateConfigurationTemplate -> TestTree
updateConfigurationTemplateTest = undefined

updateEnvironmentTest :: UpdateEnvironment -> TestTree
updateEnvironmentTest = undefined

validateConfigurationSettingsTest :: ValidateConfigurationSettings -> TestTree
validateConfigurationSettingsTest = undefined

-- Responses

abortEnvironmentUpdateResponseTest :: AbortEnvironmentUpdateResponse -> TestTree
abortEnvironmentUpdateResponseTest = resp
    "abortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse"
    (Proxy :: Proxy AbortEnvironmentUpdate)

checkDNSAvailabilityResponseTest :: CheckDNSAvailabilityResponse -> TestTree
checkDNSAvailabilityResponseTest = resp
    "checkDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse"
    (Proxy :: Proxy CheckDNSAvailability)

createApplicationResponseTest :: ApplicationDescriptionMessage -> TestTree
createApplicationResponseTest = resp
    "createApplicationResponse"
    "fixture/ApplicationDescriptionMessage"
    (Proxy :: Proxy CreateApplication)

createApplicationVersionResponseTest :: ApplicationVersionDescriptionMessage -> TestTree
createApplicationVersionResponseTest = resp
    "createApplicationVersionResponse"
    "fixture/ApplicationVersionDescriptionMessage"
    (Proxy :: Proxy CreateApplicationVersion)

createConfigurationTemplateResponseTest :: ConfigurationSettingsDescription -> TestTree
createConfigurationTemplateResponseTest = resp
    "createConfigurationTemplateResponse"
    "fixture/ConfigurationSettingsDescription"
    (Proxy :: Proxy CreateConfigurationTemplate)

createEnvironmentResponseTest :: EnvironmentDescription -> TestTree
createEnvironmentResponseTest = resp
    "createEnvironmentResponse"
    "fixture/EnvironmentDescription"
    (Proxy :: Proxy CreateEnvironment)

createStorageLocationResponseTest :: CreateStorageLocationResponse -> TestTree
createStorageLocationResponseTest = resp
    "createStorageLocationResponse"
    "fixture/CreateStorageLocationResponse"
    (Proxy :: Proxy CreateStorageLocation)

deleteApplicationResponseTest :: DeleteApplicationResponse -> TestTree
deleteApplicationResponseTest = resp
    "deleteApplicationResponse"
    "fixture/DeleteApplicationResponse"
    (Proxy :: Proxy DeleteApplication)

deleteApplicationVersionResponseTest :: DeleteApplicationVersionResponse -> TestTree
deleteApplicationVersionResponseTest = resp
    "deleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse"
    (Proxy :: Proxy DeleteApplicationVersion)

deleteConfigurationTemplateResponseTest :: DeleteConfigurationTemplateResponse -> TestTree
deleteConfigurationTemplateResponseTest = resp
    "deleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse"
    (Proxy :: Proxy DeleteConfigurationTemplate)

deleteEnvironmentConfigurationResponseTest :: DeleteEnvironmentConfigurationResponse -> TestTree
deleteEnvironmentConfigurationResponseTest = resp
    "deleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse"
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

describeApplicationVersionsResponseTest :: DescribeApplicationVersionsResponse -> TestTree
describeApplicationVersionsResponseTest = resp
    "describeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse"
    (Proxy :: Proxy DescribeApplicationVersions)

describeApplicationsResponseTest :: DescribeApplicationsResponse -> TestTree
describeApplicationsResponseTest = resp
    "describeApplicationsResponse"
    "fixture/DescribeApplicationsResponse"
    (Proxy :: Proxy DescribeApplications)

describeConfigurationOptionsResponseTest :: DescribeConfigurationOptionsResponse -> TestTree
describeConfigurationOptionsResponseTest = resp
    "describeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse"
    (Proxy :: Proxy DescribeConfigurationOptions)

describeConfigurationSettingsResponseTest :: DescribeConfigurationSettingsResponse -> TestTree
describeConfigurationSettingsResponseTest = resp
    "describeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse"
    (Proxy :: Proxy DescribeConfigurationSettings)

describeEnvironmentResourcesResponseTest :: DescribeEnvironmentResourcesResponse -> TestTree
describeEnvironmentResourcesResponseTest = resp
    "describeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse"
    (Proxy :: Proxy DescribeEnvironmentResources)

describeEnvironmentsResponseTest :: DescribeEnvironmentsResponse -> TestTree
describeEnvironmentsResponseTest = resp
    "describeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse"
    (Proxy :: Proxy DescribeEnvironments)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "describeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

listAvailableSolutionStacksResponseTest :: ListAvailableSolutionStacksResponse -> TestTree
listAvailableSolutionStacksResponseTest = resp
    "listAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse"
    (Proxy :: Proxy ListAvailableSolutionStacks)

rebuildEnvironmentResponseTest :: RebuildEnvironmentResponse -> TestTree
rebuildEnvironmentResponseTest = resp
    "rebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse"
    (Proxy :: Proxy RebuildEnvironment)

requestEnvironmentInfoResponseTest :: RequestEnvironmentInfoResponse -> TestTree
requestEnvironmentInfoResponseTest = resp
    "requestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse"
    (Proxy :: Proxy RequestEnvironmentInfo)

restartAppServerResponseTest :: RestartAppServerResponse -> TestTree
restartAppServerResponseTest = resp
    "restartAppServerResponse"
    "fixture/RestartAppServerResponse"
    (Proxy :: Proxy RestartAppServer)

retrieveEnvironmentInfoResponseTest :: RetrieveEnvironmentInfoResponse -> TestTree
retrieveEnvironmentInfoResponseTest = resp
    "retrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse"
    (Proxy :: Proxy RetrieveEnvironmentInfo)

swapEnvironmentCNAMEsResponseTest :: SwapEnvironmentCNAMEsResponse -> TestTree
swapEnvironmentCNAMEsResponseTest = resp
    "swapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse"
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

terminateEnvironmentResponseTest :: EnvironmentDescription -> TestTree
terminateEnvironmentResponseTest = resp
    "terminateEnvironmentResponse"
    "fixture/EnvironmentDescription"
    (Proxy :: Proxy TerminateEnvironment)

updateApplicationResponseTest :: ApplicationDescriptionMessage -> TestTree
updateApplicationResponseTest = resp
    "updateApplicationResponse"
    "fixture/ApplicationDescriptionMessage"
    (Proxy :: Proxy UpdateApplication)

updateApplicationVersionResponseTest :: ApplicationVersionDescriptionMessage -> TestTree
updateApplicationVersionResponseTest = resp
    "updateApplicationVersionResponse"
    "fixture/ApplicationVersionDescriptionMessage"
    (Proxy :: Proxy UpdateApplicationVersion)

updateConfigurationTemplateResponseTest :: ConfigurationSettingsDescription -> TestTree
updateConfigurationTemplateResponseTest = resp
    "updateConfigurationTemplateResponse"
    "fixture/ConfigurationSettingsDescription"
    (Proxy :: Proxy UpdateConfigurationTemplate)

updateEnvironmentResponseTest :: EnvironmentDescription -> TestTree
updateEnvironmentResponseTest = resp
    "updateEnvironmentResponse"
    "fixture/EnvironmentDescription"
    (Proxy :: Proxy UpdateEnvironment)

validateConfigurationSettingsResponseTest :: ValidateConfigurationSettingsResponse -> TestTree
validateConfigurationSettingsResponseTest = resp
    "validateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse"
    (Proxy :: Proxy ValidateConfigurationSettings)

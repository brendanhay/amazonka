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

import           Data.Proxy
import           Network.AWS.ElasticBeanstalk
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeApplicationsTest $
--             describeApplications
--
--         , createApplicationVersionTest $
--             createApplicationVersion
--
--         , updateEnvironmentTest $
--             updateEnvironment
--
--         , terminateEnvironmentTest $
--             terminateEnvironment
--
--         , describeEventsTest $
--             describeEvents
--
--         , requestEnvironmentInfoTest $
--             requestEnvironmentInfo
--
--         , retrieveEnvironmentInfoTest $
--             retrieveEnvironmentInfo
--
--         , deleteApplicationTest $
--             deleteApplication
--
--         , updateApplicationTest $
--             updateApplication
--
--         , createApplicationTest $
--             createApplication
--
--         , abortEnvironmentUpdateTest $
--             abortEnvironmentUpdate
--
--         , deleteApplicationVersionTest $
--             deleteApplicationVersion
--
--         , updateApplicationVersionTest $
--             updateApplicationVersion
--
--         , describeEnvironmentResourcesTest $
--             describeEnvironmentResources
--
--         , deleteConfigurationTemplateTest $
--             deleteConfigurationTemplate
--
--         , updateConfigurationTemplateTest $
--             updateConfigurationTemplate
--
--         , rebuildEnvironmentTest $
--             rebuildEnvironment
--
--         , deleteEnvironmentConfigurationTest $
--             deleteEnvironmentConfiguration
--
--         , createConfigurationTemplateTest $
--             createConfigurationTemplate
--
--         , listAvailableSolutionStacksTest $
--             listAvailableSolutionStacks
--
--         , swapEnvironmentCNAMEsTest $
--             swapEnvironmentCNAMEs
--
--         , describeConfigurationOptionsTest $
--             describeConfigurationOptions
--
--         , describeConfigurationSettingsTest $
--             describeConfigurationSettings
--
--         , createStorageLocationTest $
--             createStorageLocation
--
--         , describeEnvironmentsTest $
--             describeEnvironments
--
--         , restartAppServerTest $
--             restartAppServer
--
--         , validateConfigurationSettingsTest $
--             validateConfigurationSettings
--
--         , describeApplicationVersionsTest $
--             describeApplicationVersions
--
--         , checkDNSAvailabilityTest $
--             checkDNSAvailability
--
--         , createEnvironmentTest $
--             createEnvironment
--
--           ]

--     , testGroup "response"
--         [ describeApplicationsResponseTest $
--             describeApplicationsResponse
--
--         , applicationVersionDescriptionMessageTest $
--             applicationVersionDescriptionMessage
--
--         , environmentDescriptionTest $
--             environmentDescription
--
--         , environmentDescriptionTest $
--             environmentDescription
--
--         , describeEventsResponseTest $
--             describeEventsResponse
--
--         , requestEnvironmentInfoResponseTest $
--             requestEnvironmentInfoResponse
--
--         , retrieveEnvironmentInfoResponseTest $
--             retrieveEnvironmentInfoResponse
--
--         , deleteApplicationResponseTest $
--             deleteApplicationResponse
--
--         , applicationDescriptionMessageTest $
--             applicationDescriptionMessage
--
--         , applicationDescriptionMessageTest $
--             applicationDescriptionMessage
--
--         , abortEnvironmentUpdateResponseTest $
--             abortEnvironmentUpdateResponse
--
--         , deleteApplicationVersionResponseTest $
--             deleteApplicationVersionResponse
--
--         , applicationVersionDescriptionMessageTest $
--             applicationVersionDescriptionMessage
--
--         , describeEnvironmentResourcesResponseTest $
--             describeEnvironmentResourcesResponse
--
--         , deleteConfigurationTemplateResponseTest $
--             deleteConfigurationTemplateResponse
--
--         , configurationSettingsDescriptionTest $
--             configurationSettingsDescription
--
--         , rebuildEnvironmentResponseTest $
--             rebuildEnvironmentResponse
--
--         , deleteEnvironmentConfigurationResponseTest $
--             deleteEnvironmentConfigurationResponse
--
--         , configurationSettingsDescriptionTest $
--             configurationSettingsDescription
--
--         , listAvailableSolutionStacksResponseTest $
--             listAvailableSolutionStacksResponse
--
--         , swapEnvironmentCNAMEsResponseTest $
--             swapEnvironmentCNAMEsResponse
--
--         , describeConfigurationOptionsResponseTest $
--             describeConfigurationOptionsResponse
--
--         , describeConfigurationSettingsResponseTest $
--             describeConfigurationSettingsResponse
--
--         , createStorageLocationResponseTest $
--             createStorageLocationResponse
--
--         , describeEnvironmentsResponseTest $
--             describeEnvironmentsResponse
--
--         , restartAppServerResponseTest $
--             restartAppServerResponse
--
--         , validateConfigurationSettingsResponseTest $
--             validateConfigurationSettingsResponse
--
--         , describeApplicationVersionsResponseTest $
--             describeApplicationVersionsResponse
--
--         , checkDNSAvailabilityResponseTest $
--             checkDNSAvailabilityResponse
--
--         , environmentDescriptionTest $
--             environmentDescription
--
--           ]
--     ]

-- Requests

describeApplicationsTest :: DescribeApplications -> TestTree
describeApplicationsTest = undefined

createApplicationVersionTest :: CreateApplicationVersion -> TestTree
createApplicationVersionTest = undefined

updateEnvironmentTest :: UpdateEnvironment -> TestTree
updateEnvironmentTest = undefined

terminateEnvironmentTest :: TerminateEnvironment -> TestTree
terminateEnvironmentTest = undefined

describeEventsTest :: DescribeEvents -> TestTree
describeEventsTest = undefined

requestEnvironmentInfoTest :: RequestEnvironmentInfo -> TestTree
requestEnvironmentInfoTest = undefined

retrieveEnvironmentInfoTest :: RetrieveEnvironmentInfo -> TestTree
retrieveEnvironmentInfoTest = undefined

deleteApplicationTest :: DeleteApplication -> TestTree
deleteApplicationTest = undefined

updateApplicationTest :: UpdateApplication -> TestTree
updateApplicationTest = undefined

createApplicationTest :: CreateApplication -> TestTree
createApplicationTest = undefined

abortEnvironmentUpdateTest :: AbortEnvironmentUpdate -> TestTree
abortEnvironmentUpdateTest = undefined

deleteApplicationVersionTest :: DeleteApplicationVersion -> TestTree
deleteApplicationVersionTest = undefined

updateApplicationVersionTest :: UpdateApplicationVersion -> TestTree
updateApplicationVersionTest = undefined

describeEnvironmentResourcesTest :: DescribeEnvironmentResources -> TestTree
describeEnvironmentResourcesTest = undefined

deleteConfigurationTemplateTest :: DeleteConfigurationTemplate -> TestTree
deleteConfigurationTemplateTest = undefined

updateConfigurationTemplateTest :: UpdateConfigurationTemplate -> TestTree
updateConfigurationTemplateTest = undefined

rebuildEnvironmentTest :: RebuildEnvironment -> TestTree
rebuildEnvironmentTest = undefined

deleteEnvironmentConfigurationTest :: DeleteEnvironmentConfiguration -> TestTree
deleteEnvironmentConfigurationTest = undefined

createConfigurationTemplateTest :: CreateConfigurationTemplate -> TestTree
createConfigurationTemplateTest = undefined

listAvailableSolutionStacksTest :: ListAvailableSolutionStacks -> TestTree
listAvailableSolutionStacksTest = undefined

swapEnvironmentCNAMEsTest :: SwapEnvironmentCNAMEs -> TestTree
swapEnvironmentCNAMEsTest = undefined

describeConfigurationOptionsTest :: DescribeConfigurationOptions -> TestTree
describeConfigurationOptionsTest = undefined

describeConfigurationSettingsTest :: DescribeConfigurationSettings -> TestTree
describeConfigurationSettingsTest = undefined

createStorageLocationTest :: CreateStorageLocation -> TestTree
createStorageLocationTest = undefined

describeEnvironmentsTest :: DescribeEnvironments -> TestTree
describeEnvironmentsTest = undefined

restartAppServerTest :: RestartAppServer -> TestTree
restartAppServerTest = undefined

validateConfigurationSettingsTest :: ValidateConfigurationSettings -> TestTree
validateConfigurationSettingsTest = undefined

describeApplicationVersionsTest :: DescribeApplicationVersions -> TestTree
describeApplicationVersionsTest = undefined

checkDNSAvailabilityTest :: CheckDNSAvailability -> TestTree
checkDNSAvailabilityTest = undefined

createEnvironmentTest :: CreateEnvironment -> TestTree
createEnvironmentTest = undefined

-- Responses

describeApplicationsResponseTest :: DescribeApplicationsResponse -> TestTree
describeApplicationsResponseTest = resp
    "DescribeApplicationsResponse"
    "fixture/ElasticBeanstalk/DescribeApplicationsResponse"
    (Proxy :: Proxy DescribeApplications)

applicationVersionDescriptionMessageTest :: ApplicationVersionDescriptionMessage -> TestTree
applicationVersionDescriptionMessageTest = resp
    "ApplicationVersionDescriptionMessage"
    "fixture/ElasticBeanstalk/ApplicationVersionDescriptionMessage"
    (Proxy :: Proxy CreateApplicationVersion)

environmentDescriptionTest :: EnvironmentDescription -> TestTree
environmentDescriptionTest = resp
    "EnvironmentDescription"
    "fixture/ElasticBeanstalk/EnvironmentDescription"
    (Proxy :: Proxy UpdateEnvironment)

environmentDescriptionTest :: EnvironmentDescription -> TestTree
environmentDescriptionTest = resp
    "EnvironmentDescription"
    "fixture/ElasticBeanstalk/EnvironmentDescription"
    (Proxy :: Proxy TerminateEnvironment)

describeEventsResponseTest :: DescribeEventsResponse -> TestTree
describeEventsResponseTest = resp
    "DescribeEventsResponse"
    "fixture/ElasticBeanstalk/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

requestEnvironmentInfoResponseTest :: RequestEnvironmentInfoResponse -> TestTree
requestEnvironmentInfoResponseTest = resp
    "RequestEnvironmentInfoResponse"
    "fixture/ElasticBeanstalk/RequestEnvironmentInfoResponse"
    (Proxy :: Proxy RequestEnvironmentInfo)

retrieveEnvironmentInfoResponseTest :: RetrieveEnvironmentInfoResponse -> TestTree
retrieveEnvironmentInfoResponseTest = resp
    "RetrieveEnvironmentInfoResponse"
    "fixture/ElasticBeanstalk/RetrieveEnvironmentInfoResponse"
    (Proxy :: Proxy RetrieveEnvironmentInfo)

deleteApplicationResponseTest :: DeleteApplicationResponse -> TestTree
deleteApplicationResponseTest = resp
    "DeleteApplicationResponse"
    "fixture/ElasticBeanstalk/DeleteApplicationResponse"
    (Proxy :: Proxy DeleteApplication)

applicationDescriptionMessageTest :: ApplicationDescriptionMessage -> TestTree
applicationDescriptionMessageTest = resp
    "ApplicationDescriptionMessage"
    "fixture/ElasticBeanstalk/ApplicationDescriptionMessage"
    (Proxy :: Proxy UpdateApplication)

applicationDescriptionMessageTest :: ApplicationDescriptionMessage -> TestTree
applicationDescriptionMessageTest = resp
    "ApplicationDescriptionMessage"
    "fixture/ElasticBeanstalk/ApplicationDescriptionMessage"
    (Proxy :: Proxy CreateApplication)

abortEnvironmentUpdateResponseTest :: AbortEnvironmentUpdateResponse -> TestTree
abortEnvironmentUpdateResponseTest = resp
    "AbortEnvironmentUpdateResponse"
    "fixture/ElasticBeanstalk/AbortEnvironmentUpdateResponse"
    (Proxy :: Proxy AbortEnvironmentUpdate)

deleteApplicationVersionResponseTest :: DeleteApplicationVersionResponse -> TestTree
deleteApplicationVersionResponseTest = resp
    "DeleteApplicationVersionResponse"
    "fixture/ElasticBeanstalk/DeleteApplicationVersionResponse"
    (Proxy :: Proxy DeleteApplicationVersion)

applicationVersionDescriptionMessageTest :: ApplicationVersionDescriptionMessage -> TestTree
applicationVersionDescriptionMessageTest = resp
    "ApplicationVersionDescriptionMessage"
    "fixture/ElasticBeanstalk/ApplicationVersionDescriptionMessage"
    (Proxy :: Proxy UpdateApplicationVersion)

describeEnvironmentResourcesResponseTest :: DescribeEnvironmentResourcesResponse -> TestTree
describeEnvironmentResourcesResponseTest = resp
    "DescribeEnvironmentResourcesResponse"
    "fixture/ElasticBeanstalk/DescribeEnvironmentResourcesResponse"
    (Proxy :: Proxy DescribeEnvironmentResources)

deleteConfigurationTemplateResponseTest :: DeleteConfigurationTemplateResponse -> TestTree
deleteConfigurationTemplateResponseTest = resp
    "DeleteConfigurationTemplateResponse"
    "fixture/ElasticBeanstalk/DeleteConfigurationTemplateResponse"
    (Proxy :: Proxy DeleteConfigurationTemplate)

configurationSettingsDescriptionTest :: ConfigurationSettingsDescription -> TestTree
configurationSettingsDescriptionTest = resp
    "ConfigurationSettingsDescription"
    "fixture/ElasticBeanstalk/ConfigurationSettingsDescription"
    (Proxy :: Proxy UpdateConfigurationTemplate)

rebuildEnvironmentResponseTest :: RebuildEnvironmentResponse -> TestTree
rebuildEnvironmentResponseTest = resp
    "RebuildEnvironmentResponse"
    "fixture/ElasticBeanstalk/RebuildEnvironmentResponse"
    (Proxy :: Proxy RebuildEnvironment)

deleteEnvironmentConfigurationResponseTest :: DeleteEnvironmentConfigurationResponse -> TestTree
deleteEnvironmentConfigurationResponseTest = resp
    "DeleteEnvironmentConfigurationResponse"
    "fixture/ElasticBeanstalk/DeleteEnvironmentConfigurationResponse"
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

configurationSettingsDescriptionTest :: ConfigurationSettingsDescription -> TestTree
configurationSettingsDescriptionTest = resp
    "ConfigurationSettingsDescription"
    "fixture/ElasticBeanstalk/ConfigurationSettingsDescription"
    (Proxy :: Proxy CreateConfigurationTemplate)

listAvailableSolutionStacksResponseTest :: ListAvailableSolutionStacksResponse -> TestTree
listAvailableSolutionStacksResponseTest = resp
    "ListAvailableSolutionStacksResponse"
    "fixture/ElasticBeanstalk/ListAvailableSolutionStacksResponse"
    (Proxy :: Proxy ListAvailableSolutionStacks)

swapEnvironmentCNAMEsResponseTest :: SwapEnvironmentCNAMEsResponse -> TestTree
swapEnvironmentCNAMEsResponseTest = resp
    "SwapEnvironmentCNAMEsResponse"
    "fixture/ElasticBeanstalk/SwapEnvironmentCNAMEsResponse"
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

describeConfigurationOptionsResponseTest :: DescribeConfigurationOptionsResponse -> TestTree
describeConfigurationOptionsResponseTest = resp
    "DescribeConfigurationOptionsResponse"
    "fixture/ElasticBeanstalk/DescribeConfigurationOptionsResponse"
    (Proxy :: Proxy DescribeConfigurationOptions)

describeConfigurationSettingsResponseTest :: DescribeConfigurationSettingsResponse -> TestTree
describeConfigurationSettingsResponseTest = resp
    "DescribeConfigurationSettingsResponse"
    "fixture/ElasticBeanstalk/DescribeConfigurationSettingsResponse"
    (Proxy :: Proxy DescribeConfigurationSettings)

createStorageLocationResponseTest :: CreateStorageLocationResponse -> TestTree
createStorageLocationResponseTest = resp
    "CreateStorageLocationResponse"
    "fixture/ElasticBeanstalk/CreateStorageLocationResponse"
    (Proxy :: Proxy CreateStorageLocation)

describeEnvironmentsResponseTest :: DescribeEnvironmentsResponse -> TestTree
describeEnvironmentsResponseTest = resp
    "DescribeEnvironmentsResponse"
    "fixture/ElasticBeanstalk/DescribeEnvironmentsResponse"
    (Proxy :: Proxy DescribeEnvironments)

restartAppServerResponseTest :: RestartAppServerResponse -> TestTree
restartAppServerResponseTest = resp
    "RestartAppServerResponse"
    "fixture/ElasticBeanstalk/RestartAppServerResponse"
    (Proxy :: Proxy RestartAppServer)

validateConfigurationSettingsResponseTest :: ValidateConfigurationSettingsResponse -> TestTree
validateConfigurationSettingsResponseTest = resp
    "ValidateConfigurationSettingsResponse"
    "fixture/ElasticBeanstalk/ValidateConfigurationSettingsResponse"
    (Proxy :: Proxy ValidateConfigurationSettings)

describeApplicationVersionsResponseTest :: DescribeApplicationVersionsResponse -> TestTree
describeApplicationVersionsResponseTest = resp
    "DescribeApplicationVersionsResponse"
    "fixture/ElasticBeanstalk/DescribeApplicationVersionsResponse"
    (Proxy :: Proxy DescribeApplicationVersions)

checkDNSAvailabilityResponseTest :: CheckDNSAvailabilityResponse -> TestTree
checkDNSAvailabilityResponseTest = resp
    "CheckDNSAvailabilityResponse"
    "fixture/ElasticBeanstalk/CheckDNSAvailabilityResponse"
    (Proxy :: Proxy CheckDNSAvailability)

environmentDescriptionTest :: EnvironmentDescription -> TestTree
environmentDescriptionTest = resp
    "EnvironmentDescription"
    "fixture/ElasticBeanstalk/EnvironmentDescription"
    (Proxy :: Proxy CreateEnvironment)

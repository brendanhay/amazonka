{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AppConfig where

import Data.Proxy
import Network.AWS.AppConfig
import Test.AWS.AppConfig.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListEnvironments $
--             newListEnvironments
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestGetDeploymentStrategy $
--             newGetDeploymentStrategy
--
--         , requestCreateConfigurationProfile $
--             newCreateConfigurationProfile
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestUpdateConfigurationProfile $
--             newUpdateConfigurationProfile
--
--         , requestDeleteConfigurationProfile $
--             newDeleteConfigurationProfile
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListHostedConfigurationVersions $
--             newListHostedConfigurationVersions
--
--         , requestGetConfigurationProfile $
--             newGetConfigurationProfile
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateDeploymentStrategy $
--             newUpdateDeploymentStrategy
--
--         , requestDeleteDeploymentStrategy $
--             newDeleteDeploymentStrategy
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestValidateConfiguration $
--             newValidateConfiguration
--
--         , requestStopDeployment $
--             newStopDeployment
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestCreateHostedConfigurationVersion $
--             newCreateHostedConfigurationVersion
--
--         , requestListConfigurationProfiles $
--             newListConfigurationProfiles
--
--         , requestDeleteHostedConfigurationVersion $
--             newDeleteHostedConfigurationVersion
--
--         , requestGetHostedConfigurationVersion $
--             newGetHostedConfigurationVersion
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListDeploymentStrategies $
--             newListDeploymentStrategies
--
--         , requestGetConfiguration $
--             newGetConfiguration
--
--         , requestCreateDeploymentStrategy $
--             newCreateDeploymentStrategy
--
--         , requestStartDeployment $
--             newStartDeployment
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--           ]

--     , testGroup "response"
--         [ responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseUpdateEnvironment $
--             newEnvironment
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseGetDeploymentStrategy $
--             newDeploymentStrategy
--
--         , responseCreateConfigurationProfile $
--             newConfigurationProfile
--
--         , responseGetDeployment $
--             newDeployment
--
--         , responseUpdateConfigurationProfile $
--             newConfigurationProfile
--
--         , responseDeleteConfigurationProfile $
--             newDeleteConfigurationProfileResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListHostedConfigurationVersions $
--             newListHostedConfigurationVersionsResponse
--
--         , responseGetConfigurationProfile $
--             newConfigurationProfile
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             newApplication
--
--         , responseUpdateDeploymentStrategy $
--             newDeploymentStrategy
--
--         , responseDeleteDeploymentStrategy $
--             newDeleteDeploymentStrategyResponse
--
--         , responseCreateApplication $
--             newApplication
--
--         , responseValidateConfiguration $
--             newValidateConfigurationResponse
--
--         , responseStopDeployment $
--             newDeployment
--
--         , responseGetApplication $
--             newApplication
--
--         , responseCreateHostedConfigurationVersion $
--             newHostedConfigurationVersion
--
--         , responseListConfigurationProfiles $
--             newListConfigurationProfilesResponse
--
--         , responseDeleteHostedConfigurationVersion $
--             newDeleteHostedConfigurationVersionResponse
--
--         , responseGetHostedConfigurationVersion $
--             newHostedConfigurationVersion
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetEnvironment $
--             newEnvironment
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListDeploymentStrategies $
--             newListDeploymentStrategiesResponse
--
--         , responseGetConfiguration $
--             newGetConfigurationResponse
--
--         , responseCreateDeploymentStrategy $
--             newDeploymentStrategy
--
--         , responseStartDeployment $
--             newDeployment
--
--         , responseCreateEnvironment $
--             newEnvironment
--
--           ]
--     ]

-- Requests

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestGetDeploymentStrategy :: GetDeploymentStrategy -> TestTree
requestGetDeploymentStrategy =
  req
    "GetDeploymentStrategy"
    "fixture/GetDeploymentStrategy.yaml"

requestCreateConfigurationProfile :: CreateConfigurationProfile -> TestTree
requestCreateConfigurationProfile =
  req
    "CreateConfigurationProfile"
    "fixture/CreateConfigurationProfile.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestUpdateConfigurationProfile :: UpdateConfigurationProfile -> TestTree
requestUpdateConfigurationProfile =
  req
    "UpdateConfigurationProfile"
    "fixture/UpdateConfigurationProfile.yaml"

requestDeleteConfigurationProfile :: DeleteConfigurationProfile -> TestTree
requestDeleteConfigurationProfile =
  req
    "DeleteConfigurationProfile"
    "fixture/DeleteConfigurationProfile.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListHostedConfigurationVersions :: ListHostedConfigurationVersions -> TestTree
requestListHostedConfigurationVersions =
  req
    "ListHostedConfigurationVersions"
    "fixture/ListHostedConfigurationVersions.yaml"

requestGetConfigurationProfile :: GetConfigurationProfile -> TestTree
requestGetConfigurationProfile =
  req
    "GetConfigurationProfile"
    "fixture/GetConfigurationProfile.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateDeploymentStrategy :: UpdateDeploymentStrategy -> TestTree
requestUpdateDeploymentStrategy =
  req
    "UpdateDeploymentStrategy"
    "fixture/UpdateDeploymentStrategy.yaml"

requestDeleteDeploymentStrategy :: DeleteDeploymentStrategy -> TestTree
requestDeleteDeploymentStrategy =
  req
    "DeleteDeploymentStrategy"
    "fixture/DeleteDeploymentStrategy.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestValidateConfiguration :: ValidateConfiguration -> TestTree
requestValidateConfiguration =
  req
    "ValidateConfiguration"
    "fixture/ValidateConfiguration.yaml"

requestStopDeployment :: StopDeployment -> TestTree
requestStopDeployment =
  req
    "StopDeployment"
    "fixture/StopDeployment.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestCreateHostedConfigurationVersion :: CreateHostedConfigurationVersion -> TestTree
requestCreateHostedConfigurationVersion =
  req
    "CreateHostedConfigurationVersion"
    "fixture/CreateHostedConfigurationVersion.yaml"

requestListConfigurationProfiles :: ListConfigurationProfiles -> TestTree
requestListConfigurationProfiles =
  req
    "ListConfigurationProfiles"
    "fixture/ListConfigurationProfiles.yaml"

requestDeleteHostedConfigurationVersion :: DeleteHostedConfigurationVersion -> TestTree
requestDeleteHostedConfigurationVersion =
  req
    "DeleteHostedConfigurationVersion"
    "fixture/DeleteHostedConfigurationVersion.yaml"

requestGetHostedConfigurationVersion :: GetHostedConfigurationVersion -> TestTree
requestGetHostedConfigurationVersion =
  req
    "GetHostedConfigurationVersion"
    "fixture/GetHostedConfigurationVersion.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListDeploymentStrategies :: ListDeploymentStrategies -> TestTree
requestListDeploymentStrategies =
  req
    "ListDeploymentStrategies"
    "fixture/ListDeploymentStrategies.yaml"

requestGetConfiguration :: GetConfiguration -> TestTree
requestGetConfiguration =
  req
    "GetConfiguration"
    "fixture/GetConfiguration.yaml"

requestCreateDeploymentStrategy :: CreateDeploymentStrategy -> TestTree
requestCreateDeploymentStrategy =
  req
    "CreateDeploymentStrategy"
    "fixture/CreateDeploymentStrategy.yaml"

requestStartDeployment :: StartDeployment -> TestTree
requestStartDeployment =
  req
    "StartDeployment"
    "fixture/StartDeployment.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

-- Responses

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEnvironments)

responseUpdateEnvironment :: Environment -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnvironment)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEnvironment)

responseGetDeploymentStrategy :: DeploymentStrategy -> TestTree
responseGetDeploymentStrategy =
  res
    "GetDeploymentStrategyResponse"
    "fixture/GetDeploymentStrategyResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeploymentStrategy)

responseCreateConfigurationProfile :: ConfigurationProfile -> TestTree
responseCreateConfigurationProfile =
  res
    "CreateConfigurationProfileResponse"
    "fixture/CreateConfigurationProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationProfile)

responseGetDeployment :: Deployment -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeployment)

responseUpdateConfigurationProfile :: ConfigurationProfile -> TestTree
responseUpdateConfigurationProfile =
  res
    "UpdateConfigurationProfileResponse"
    "fixture/UpdateConfigurationProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationProfile)

responseDeleteConfigurationProfile :: DeleteConfigurationProfileResponse -> TestTree
responseDeleteConfigurationProfile =
  res
    "DeleteConfigurationProfileResponse"
    "fixture/DeleteConfigurationProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationProfile)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListHostedConfigurationVersions :: ListHostedConfigurationVersionsResponse -> TestTree
responseListHostedConfigurationVersions =
  res
    "ListHostedConfigurationVersionsResponse"
    "fixture/ListHostedConfigurationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListHostedConfigurationVersions)

responseGetConfigurationProfile :: ConfigurationProfile -> TestTree
responseGetConfigurationProfile =
  res
    "GetConfigurationProfileResponse"
    "fixture/GetConfigurationProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfigurationProfile)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: Application -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)

responseUpdateDeploymentStrategy :: DeploymentStrategy -> TestTree
responseUpdateDeploymentStrategy =
  res
    "UpdateDeploymentStrategyResponse"
    "fixture/UpdateDeploymentStrategyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeploymentStrategy)

responseDeleteDeploymentStrategy :: DeleteDeploymentStrategyResponse -> TestTree
responseDeleteDeploymentStrategy =
  res
    "DeleteDeploymentStrategyResponse"
    "fixture/DeleteDeploymentStrategyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeploymentStrategy)

responseCreateApplication :: Application -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplication)

responseValidateConfiguration :: ValidateConfigurationResponse -> TestTree
responseValidateConfiguration =
  res
    "ValidateConfigurationResponse"
    "fixture/ValidateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateConfiguration)

responseStopDeployment :: Deployment -> TestTree
responseStopDeployment =
  res
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StopDeployment)

responseGetApplication :: Application -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy GetApplication)

responseCreateHostedConfigurationVersion :: HostedConfigurationVersion -> TestTree
responseCreateHostedConfigurationVersion =
  res
    "CreateHostedConfigurationVersionResponse"
    "fixture/CreateHostedConfigurationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHostedConfigurationVersion)

responseListConfigurationProfiles :: ListConfigurationProfilesResponse -> TestTree
responseListConfigurationProfiles =
  res
    "ListConfigurationProfilesResponse"
    "fixture/ListConfigurationProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigurationProfiles)

responseDeleteHostedConfigurationVersion :: DeleteHostedConfigurationVersionResponse -> TestTree
responseDeleteHostedConfigurationVersion =
  res
    "DeleteHostedConfigurationVersionResponse"
    "fixture/DeleteHostedConfigurationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHostedConfigurationVersion)

responseGetHostedConfigurationVersion :: HostedConfigurationVersion -> TestTree
responseGetHostedConfigurationVersion =
  res
    "GetHostedConfigurationVersionResponse"
    "fixture/GetHostedConfigurationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetHostedConfigurationVersion)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeployments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetEnvironment :: Environment -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy GetEnvironment)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplications)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListDeploymentStrategies :: ListDeploymentStrategiesResponse -> TestTree
responseListDeploymentStrategies =
  res
    "ListDeploymentStrategiesResponse"
    "fixture/ListDeploymentStrategiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeploymentStrategies)

responseGetConfiguration :: GetConfigurationResponse -> TestTree
responseGetConfiguration =
  res
    "GetConfigurationResponse"
    "fixture/GetConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfiguration)

responseCreateDeploymentStrategy :: DeploymentStrategy -> TestTree
responseCreateDeploymentStrategy =
  res
    "CreateDeploymentStrategyResponse"
    "fixture/CreateDeploymentStrategyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeploymentStrategy)

responseStartDeployment :: Deployment -> TestTree
responseStartDeployment =
  res
    "StartDeploymentResponse"
    "fixture/StartDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StartDeployment)

responseCreateEnvironment :: Environment -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEnvironment)

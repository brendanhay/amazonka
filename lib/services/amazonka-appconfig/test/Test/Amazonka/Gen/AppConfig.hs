{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppConfig where

import Amazonka.AppConfig
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppConfig.Internal
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
--         [ requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateConfigurationProfile $
--             newCreateConfigurationProfile
--
--         , requestCreateDeploymentStrategy $
--             newCreateDeploymentStrategy
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateExtension $
--             newCreateExtension
--
--         , requestCreateExtensionAssociation $
--             newCreateExtensionAssociation
--
--         , requestCreateHostedConfigurationVersion $
--             newCreateHostedConfigurationVersion
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteConfigurationProfile $
--             newDeleteConfigurationProfile
--
--         , requestDeleteDeploymentStrategy $
--             newDeleteDeploymentStrategy
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestDeleteExtension $
--             newDeleteExtension
--
--         , requestDeleteExtensionAssociation $
--             newDeleteExtensionAssociation
--
--         , requestDeleteHostedConfigurationVersion $
--             newDeleteHostedConfigurationVersion
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetConfigurationProfile $
--             newGetConfigurationProfile
--
--         , requestGetDeployment $
--             newGetDeployment
--
--         , requestGetDeploymentStrategy $
--             newGetDeploymentStrategy
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestGetExtension $
--             newGetExtension
--
--         , requestGetExtensionAssociation $
--             newGetExtensionAssociation
--
--         , requestGetHostedConfigurationVersion $
--             newGetHostedConfigurationVersion
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListConfigurationProfiles $
--             newListConfigurationProfiles
--
--         , requestListDeploymentStrategies $
--             newListDeploymentStrategies
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestListEnvironments $
--             newListEnvironments
--
--         , requestListExtensionAssociations $
--             newListExtensionAssociations
--
--         , requestListExtensions $
--             newListExtensions
--
--         , requestListHostedConfigurationVersions $
--             newListHostedConfigurationVersions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartDeployment $
--             newStartDeployment
--
--         , requestStopDeployment $
--             newStopDeployment
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateConfigurationProfile $
--             newUpdateConfigurationProfile
--
--         , requestUpdateDeploymentStrategy $
--             newUpdateDeploymentStrategy
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestUpdateExtension $
--             newUpdateExtension
--
--         , requestUpdateExtensionAssociation $
--             newUpdateExtensionAssociation
--
--         , requestValidateConfiguration $
--             newValidateConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseCreateApplication $
--             newApplication
--
--         , responseCreateConfigurationProfile $
--             newConfigurationProfile
--
--         , responseCreateDeploymentStrategy $
--             newDeploymentStrategy
--
--         , responseCreateEnvironment $
--             newEnvironment
--
--         , responseCreateExtension $
--             newExtension
--
--         , responseCreateExtensionAssociation $
--             newExtensionAssociation
--
--         , responseCreateHostedConfigurationVersion $
--             newHostedConfigurationVersion
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteConfigurationProfile $
--             newDeleteConfigurationProfileResponse
--
--         , responseDeleteDeploymentStrategy $
--             newDeleteDeploymentStrategyResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseDeleteExtension $
--             newDeleteExtensionResponse
--
--         , responseDeleteExtensionAssociation $
--             newDeleteExtensionAssociationResponse
--
--         , responseDeleteHostedConfigurationVersion $
--             newDeleteHostedConfigurationVersionResponse
--
--         , responseGetApplication $
--             newApplication
--
--         , responseGetConfigurationProfile $
--             newConfigurationProfile
--
--         , responseGetDeployment $
--             newDeployment
--
--         , responseGetDeploymentStrategy $
--             newDeploymentStrategy
--
--         , responseGetEnvironment $
--             newEnvironment
--
--         , responseGetExtension $
--             newExtension
--
--         , responseGetExtensionAssociation $
--             newExtensionAssociation
--
--         , responseGetHostedConfigurationVersion $
--             newHostedConfigurationVersion
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListConfigurationProfiles $
--             newListConfigurationProfilesResponse
--
--         , responseListDeploymentStrategies $
--             newListDeploymentStrategiesResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseListExtensionAssociations $
--             newListExtensionAssociationsResponse
--
--         , responseListExtensions $
--             newListExtensionsResponse
--
--         , responseListHostedConfigurationVersions $
--             newListHostedConfigurationVersionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartDeployment $
--             newDeployment
--
--         , responseStopDeployment $
--             newDeployment
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newApplication
--
--         , responseUpdateConfigurationProfile $
--             newConfigurationProfile
--
--         , responseUpdateDeploymentStrategy $
--             newDeploymentStrategy
--
--         , responseUpdateEnvironment $
--             newEnvironment
--
--         , responseUpdateExtension $
--             newExtension
--
--         , responseUpdateExtensionAssociation $
--             newExtensionAssociation
--
--         , responseValidateConfiguration $
--             newValidateConfigurationResponse
--
--           ]
--     ]

-- Requests

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateConfigurationProfile :: CreateConfigurationProfile -> TestTree
requestCreateConfigurationProfile =
  req
    "CreateConfigurationProfile"
    "fixture/CreateConfigurationProfile.yaml"

requestCreateDeploymentStrategy :: CreateDeploymentStrategy -> TestTree
requestCreateDeploymentStrategy =
  req
    "CreateDeploymentStrategy"
    "fixture/CreateDeploymentStrategy.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestCreateExtension :: CreateExtension -> TestTree
requestCreateExtension =
  req
    "CreateExtension"
    "fixture/CreateExtension.yaml"

requestCreateExtensionAssociation :: CreateExtensionAssociation -> TestTree
requestCreateExtensionAssociation =
  req
    "CreateExtensionAssociation"
    "fixture/CreateExtensionAssociation.yaml"

requestCreateHostedConfigurationVersion :: CreateHostedConfigurationVersion -> TestTree
requestCreateHostedConfigurationVersion =
  req
    "CreateHostedConfigurationVersion"
    "fixture/CreateHostedConfigurationVersion.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteConfigurationProfile :: DeleteConfigurationProfile -> TestTree
requestDeleteConfigurationProfile =
  req
    "DeleteConfigurationProfile"
    "fixture/DeleteConfigurationProfile.yaml"

requestDeleteDeploymentStrategy :: DeleteDeploymentStrategy -> TestTree
requestDeleteDeploymentStrategy =
  req
    "DeleteDeploymentStrategy"
    "fixture/DeleteDeploymentStrategy.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestDeleteExtension :: DeleteExtension -> TestTree
requestDeleteExtension =
  req
    "DeleteExtension"
    "fixture/DeleteExtension.yaml"

requestDeleteExtensionAssociation :: DeleteExtensionAssociation -> TestTree
requestDeleteExtensionAssociation =
  req
    "DeleteExtensionAssociation"
    "fixture/DeleteExtensionAssociation.yaml"

requestDeleteHostedConfigurationVersion :: DeleteHostedConfigurationVersion -> TestTree
requestDeleteHostedConfigurationVersion =
  req
    "DeleteHostedConfigurationVersion"
    "fixture/DeleteHostedConfigurationVersion.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetConfigurationProfile :: GetConfigurationProfile -> TestTree
requestGetConfigurationProfile =
  req
    "GetConfigurationProfile"
    "fixture/GetConfigurationProfile.yaml"

requestGetDeployment :: GetDeployment -> TestTree
requestGetDeployment =
  req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

requestGetDeploymentStrategy :: GetDeploymentStrategy -> TestTree
requestGetDeploymentStrategy =
  req
    "GetDeploymentStrategy"
    "fixture/GetDeploymentStrategy.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestGetExtension :: GetExtension -> TestTree
requestGetExtension =
  req
    "GetExtension"
    "fixture/GetExtension.yaml"

requestGetExtensionAssociation :: GetExtensionAssociation -> TestTree
requestGetExtensionAssociation =
  req
    "GetExtensionAssociation"
    "fixture/GetExtensionAssociation.yaml"

requestGetHostedConfigurationVersion :: GetHostedConfigurationVersion -> TestTree
requestGetHostedConfigurationVersion =
  req
    "GetHostedConfigurationVersion"
    "fixture/GetHostedConfigurationVersion.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListConfigurationProfiles :: ListConfigurationProfiles -> TestTree
requestListConfigurationProfiles =
  req
    "ListConfigurationProfiles"
    "fixture/ListConfigurationProfiles.yaml"

requestListDeploymentStrategies :: ListDeploymentStrategies -> TestTree
requestListDeploymentStrategies =
  req
    "ListDeploymentStrategies"
    "fixture/ListDeploymentStrategies.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestListExtensionAssociations :: ListExtensionAssociations -> TestTree
requestListExtensionAssociations =
  req
    "ListExtensionAssociations"
    "fixture/ListExtensionAssociations.yaml"

requestListExtensions :: ListExtensions -> TestTree
requestListExtensions =
  req
    "ListExtensions"
    "fixture/ListExtensions.yaml"

requestListHostedConfigurationVersions :: ListHostedConfigurationVersions -> TestTree
requestListHostedConfigurationVersions =
  req
    "ListHostedConfigurationVersions"
    "fixture/ListHostedConfigurationVersions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartDeployment :: StartDeployment -> TestTree
requestStartDeployment =
  req
    "StartDeployment"
    "fixture/StartDeployment.yaml"

requestStopDeployment :: StopDeployment -> TestTree
requestStopDeployment =
  req
    "StopDeployment"
    "fixture/StopDeployment.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateConfigurationProfile :: UpdateConfigurationProfile -> TestTree
requestUpdateConfigurationProfile =
  req
    "UpdateConfigurationProfile"
    "fixture/UpdateConfigurationProfile.yaml"

requestUpdateDeploymentStrategy :: UpdateDeploymentStrategy -> TestTree
requestUpdateDeploymentStrategy =
  req
    "UpdateDeploymentStrategy"
    "fixture/UpdateDeploymentStrategy.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestUpdateExtension :: UpdateExtension -> TestTree
requestUpdateExtension =
  req
    "UpdateExtension"
    "fixture/UpdateExtension.yaml"

requestUpdateExtensionAssociation :: UpdateExtensionAssociation -> TestTree
requestUpdateExtensionAssociation =
  req
    "UpdateExtensionAssociation"
    "fixture/UpdateExtensionAssociation.yaml"

requestValidateConfiguration :: ValidateConfiguration -> TestTree
requestValidateConfiguration =
  req
    "ValidateConfiguration"
    "fixture/ValidateConfiguration.yaml"

-- Responses

responseCreateApplication :: Application -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateConfigurationProfile :: ConfigurationProfile -> TestTree
responseCreateConfigurationProfile =
  res
    "CreateConfigurationProfileResponse"
    "fixture/CreateConfigurationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationProfile)

responseCreateDeploymentStrategy :: DeploymentStrategy -> TestTree
responseCreateDeploymentStrategy =
  res
    "CreateDeploymentStrategyResponse"
    "fixture/CreateDeploymentStrategyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeploymentStrategy)

responseCreateEnvironment :: Environment -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreateExtension :: Extension -> TestTree
responseCreateExtension =
  res
    "CreateExtensionResponse"
    "fixture/CreateExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExtension)

responseCreateExtensionAssociation :: ExtensionAssociation -> TestTree
responseCreateExtensionAssociation =
  res
    "CreateExtensionAssociationResponse"
    "fixture/CreateExtensionAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExtensionAssociation)

responseCreateHostedConfigurationVersion :: HostedConfigurationVersion -> TestTree
responseCreateHostedConfigurationVersion =
  res
    "CreateHostedConfigurationVersionResponse"
    "fixture/CreateHostedConfigurationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHostedConfigurationVersion)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteConfigurationProfile :: DeleteConfigurationProfileResponse -> TestTree
responseDeleteConfigurationProfile =
  res
    "DeleteConfigurationProfileResponse"
    "fixture/DeleteConfigurationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationProfile)

responseDeleteDeploymentStrategy :: DeleteDeploymentStrategyResponse -> TestTree
responseDeleteDeploymentStrategy =
  res
    "DeleteDeploymentStrategyResponse"
    "fixture/DeleteDeploymentStrategyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeploymentStrategy)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

responseDeleteExtension :: DeleteExtensionResponse -> TestTree
responseDeleteExtension =
  res
    "DeleteExtensionResponse"
    "fixture/DeleteExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExtension)

responseDeleteExtensionAssociation :: DeleteExtensionAssociationResponse -> TestTree
responseDeleteExtensionAssociation =
  res
    "DeleteExtensionAssociationResponse"
    "fixture/DeleteExtensionAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExtensionAssociation)

responseDeleteHostedConfigurationVersion :: DeleteHostedConfigurationVersionResponse -> TestTree
responseDeleteHostedConfigurationVersion =
  res
    "DeleteHostedConfigurationVersionResponse"
    "fixture/DeleteHostedConfigurationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHostedConfigurationVersion)

responseGetApplication :: Application -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseGetConfigurationProfile :: ConfigurationProfile -> TestTree
responseGetConfigurationProfile =
  res
    "GetConfigurationProfileResponse"
    "fixture/GetConfigurationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfigurationProfile)

responseGetDeployment :: Deployment -> TestTree
responseGetDeployment =
  res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeployment)

responseGetDeploymentStrategy :: DeploymentStrategy -> TestTree
responseGetDeploymentStrategy =
  res
    "GetDeploymentStrategyResponse"
    "fixture/GetDeploymentStrategyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentStrategy)

responseGetEnvironment :: Environment -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseGetExtension :: Extension -> TestTree
responseGetExtension =
  res
    "GetExtensionResponse"
    "fixture/GetExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExtension)

responseGetExtensionAssociation :: ExtensionAssociation -> TestTree
responseGetExtensionAssociation =
  res
    "GetExtensionAssociationResponse"
    "fixture/GetExtensionAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExtensionAssociation)

responseGetHostedConfigurationVersion :: HostedConfigurationVersion -> TestTree
responseGetHostedConfigurationVersion =
  res
    "GetHostedConfigurationVersionResponse"
    "fixture/GetHostedConfigurationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostedConfigurationVersion)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListConfigurationProfiles :: ListConfigurationProfilesResponse -> TestTree
responseListConfigurationProfiles =
  res
    "ListConfigurationProfilesResponse"
    "fixture/ListConfigurationProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfigurationProfiles)

responseListDeploymentStrategies :: ListDeploymentStrategiesResponse -> TestTree
responseListDeploymentStrategies =
  res
    "ListDeploymentStrategiesResponse"
    "fixture/ListDeploymentStrategiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeploymentStrategies)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeployments)

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseListExtensionAssociations :: ListExtensionAssociationsResponse -> TestTree
responseListExtensionAssociations =
  res
    "ListExtensionAssociationsResponse"
    "fixture/ListExtensionAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExtensionAssociations)

responseListExtensions :: ListExtensionsResponse -> TestTree
responseListExtensions =
  res
    "ListExtensionsResponse"
    "fixture/ListExtensionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExtensions)

responseListHostedConfigurationVersions :: ListHostedConfigurationVersionsResponse -> TestTree
responseListHostedConfigurationVersions =
  res
    "ListHostedConfigurationVersionsResponse"
    "fixture/ListHostedConfigurationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostedConfigurationVersions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartDeployment :: Deployment -> TestTree
responseStartDeployment =
  res
    "StartDeploymentResponse"
    "fixture/StartDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDeployment)

responseStopDeployment :: Deployment -> TestTree
responseStopDeployment =
  res
    "StopDeploymentResponse"
    "fixture/StopDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopDeployment)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApplication :: Application -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateConfigurationProfile :: ConfigurationProfile -> TestTree
responseUpdateConfigurationProfile =
  res
    "UpdateConfigurationProfileResponse"
    "fixture/UpdateConfigurationProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationProfile)

responseUpdateDeploymentStrategy :: DeploymentStrategy -> TestTree
responseUpdateDeploymentStrategy =
  res
    "UpdateDeploymentStrategyResponse"
    "fixture/UpdateDeploymentStrategyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeploymentStrategy)

responseUpdateEnvironment :: Environment -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironment)

responseUpdateExtension :: Extension -> TestTree
responseUpdateExtension =
  res
    "UpdateExtensionResponse"
    "fixture/UpdateExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExtension)

responseUpdateExtensionAssociation :: ExtensionAssociation -> TestTree
responseUpdateExtensionAssociation =
  res
    "UpdateExtensionAssociationResponse"
    "fixture/UpdateExtensionAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExtensionAssociation)

responseValidateConfiguration :: ValidateConfigurationResponse -> TestTree
responseValidateConfiguration =
  res
    "ValidateConfigurationResponse"
    "fixture/ValidateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateConfiguration)

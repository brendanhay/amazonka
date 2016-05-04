{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.APIGateway
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.APIGateway where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.APIGateway
import Test.AWS.APIGateway.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetResource $
--             getResource
--
--         , testGetDeployments $
--             getDeployments
--
--         , testGetDeployment $
--             getDeployment
--
--         , testGetDomainNames $
--             getDomainNames
--
--         , testGetClientCertificate $
--             getClientCertificate
--
--         , testGetMethodResponse $
--             getMethodResponse
--
--         , testGetModels $
--             getModels
--
--         , testGetBasePathMapping $
--             getBasePathMapping
--
--         , testPutMethodResponse $
--             putMethodResponse
--
--         , testImportRestAPI $
--             importRestAPI
--
--         , testDeleteMethodResponse $
--             deleteMethodResponse
--
--         , testUpdateMethodResponse $
--             updateMethodResponse
--
--         , testDeleteStage $
--             deleteStage
--
--         , testUpdateStage $
--             updateStage
--
--         , testGetRestAPIs $
--             getRestAPIs
--
--         , testCreateDeployment $
--             createDeployment
--
--         , testCreateBasePathMapping $
--             createBasePathMapping
--
--         , testGetIntegration $
--             getIntegration
--
--         , testUpdateAccount $
--             updateAccount
--
--         , testDeleteDeployment $
--             deleteDeployment
--
--         , testUpdateDeployment $
--             updateDeployment
--
--         , testDeleteResource $
--             deleteResource
--
--         , testUpdateResource $
--             updateResource
--
--         , testCreateModel $
--             createModel
--
--         , testGetIntegrationResponse $
--             getIntegrationResponse
--
--         , testCreateDomainName $
--             createDomainName
--
--         , testFlushStageAuthorizersCache $
--             flushStageAuthorizersCache
--
--         , testDeleteModel $
--             deleteModel
--
--         , testUpdateModel $
--             updateModel
--
--         , testDeleteAPIKey $
--             deleteAPIKey
--
--         , testUpdateAPIKey $
--             updateAPIKey
--
--         , testGetRestAPI $
--             getRestAPI
--
--         , testGetStages $
--             getStages
--
--         , testPutRestAPI $
--             putRestAPI
--
--         , testGetMethod $
--             getMethod
--
--         , testGetModel $
--             getModel
--
--         , testUpdateRestAPI $
--             updateRestAPI
--
--         , testDeleteRestAPI $
--             deleteRestAPI
--
--         , testTestInvokeMethod $
--             testInvokeMethod
--
--         , testGetDomainName $
--             getDomainName
--
--         , testGetAuthorizers $
--             getAuthorizers
--
--         , testPutIntegrationResponse $
--             putIntegrationResponse
--
--         , testFlushStageCache $
--             flushStageCache
--
--         , testCreateRestAPI $
--             createRestAPI
--
--         , testDeleteIntegrationResponse $
--             deleteIntegrationResponse
--
--         , testUpdateIntegrationResponse $
--             updateIntegrationResponse
--
--         , testDeleteIntegration $
--             deleteIntegration
--
--         , testUpdateIntegration $
--             updateIntegration
--
--         , testTestInvokeAuthorizer $
--             testInvokeAuthorizer
--
--         , testGenerateClientCertificate $
--             generateClientCertificate
--
--         , testGetResources $
--             getResources
--
--         , testGetAccount $
--             getAccount
--
--         , testPutIntegration $
--             putIntegration
--
--         , testGetAuthorizer $
--             getAuthorizer
--
--         , testGetStage $
--             getStage
--
--         , testGetExport $
--             getExport
--
--         , testGetSDK $
--             getSDK
--
--         , testGetAPIKeys $
--             getAPIKeys
--
--         , testDeleteBasePathMapping $
--             deleteBasePathMapping
--
--         , testUpdateBasePathMapping $
--             updateBasePathMapping
--
--         , testDeleteClientCertificate $
--             deleteClientCertificate
--
--         , testUpdateClientCertificate $
--             updateClientCertificate
--
--         , testCreateAuthorizer $
--             createAuthorizer
--
--         , testUpdateAuthorizer $
--             updateAuthorizer
--
--         , testDeleteAuthorizer $
--             deleteAuthorizer
--
--         , testCreateStage $
--             createStage
--
--         , testCreateAPIKey $
--             createAPIKey
--
--         , testPutMethod $
--             putMethod
--
--         , testUpdateDomainName $
--             updateDomainName
--
--         , testDeleteDomainName $
--             deleteDomainName
--
--         , testCreateResource $
--             createResource
--
--         , testDeleteMethod $
--             deleteMethod
--
--         , testUpdateMethod $
--             updateMethod
--
--         , testGetClientCertificates $
--             getClientCertificates
--
--         , testGetModelTemplate $
--             getModelTemplate
--
--         , testGetBasePathMappings $
--             getBasePathMappings
--
--         , testGetAPIKey $
--             getAPIKey
--
--           ]

--     , testGroup "response"
--         [ testGetResourceResponse $
--             resource
--
--         , testGetDeploymentsResponse $
--             getDeploymentsResponse
--
--         , testGetDeploymentResponse $
--             deployment
--
--         , testGetDomainNamesResponse $
--             getDomainNamesResponse
--
--         , testGetClientCertificateResponse $
--             clientCertificate
--
--         , testGetMethodResponseResponse $
--             methodResponse
--
--         , testGetModelsResponse $
--             getModelsResponse
--
--         , testGetBasePathMappingResponse $
--             basePathMapping
--
--         , testPutMethodResponseResponse $
--             methodResponse
--
--         , testImportRestAPIResponse $
--             restAPI
--
--         , testDeleteMethodResponseResponse $
--             deleteMethodResponseResponse
--
--         , testUpdateMethodResponseResponse $
--             methodResponse
--
--         , testDeleteStageResponse $
--             deleteStageResponse
--
--         , testUpdateStageResponse $
--             stage
--
--         , testGetRestAPIsResponse $
--             getRestAPIsResponse
--
--         , testCreateDeploymentResponse $
--             deployment
--
--         , testCreateBasePathMappingResponse $
--             basePathMapping
--
--         , testGetIntegrationResponse $
--             integration
--
--         , testUpdateAccountResponse $
--             account
--
--         , testDeleteDeploymentResponse $
--             deleteDeploymentResponse
--
--         , testUpdateDeploymentResponse $
--             deployment
--
--         , testDeleteResourceResponse $
--             deleteResourceResponse
--
--         , testUpdateResourceResponse $
--             resource
--
--         , testCreateModelResponse $
--             model
--
--         , testGetIntegrationResponseResponse $
--             integrationResponse
--
--         , testCreateDomainNameResponse $
--             domainName
--
--         , testFlushStageAuthorizersCacheResponse $
--             flushStageAuthorizersCacheResponse
--
--         , testDeleteModelResponse $
--             deleteModelResponse
--
--         , testUpdateModelResponse $
--             model
--
--         , testDeleteAPIKeyResponse $
--             deleteAPIKeyResponse
--
--         , testUpdateAPIKeyResponse $
--             apiKey
--
--         , testGetRestAPIResponse $
--             restAPI
--
--         , testGetStagesResponse $
--             getStagesResponse
--
--         , testPutRestAPIResponse $
--             restAPI
--
--         , testGetMethodResponse $
--             method
--
--         , testGetModelResponse $
--             model
--
--         , testUpdateRestAPIResponse $
--             restAPI
--
--         , testDeleteRestAPIResponse $
--             deleteRestAPIResponse
--
--         , testTestInvokeMethodResponse $
--             testInvokeMethodResponse
--
--         , testGetDomainNameResponse $
--             domainName
--
--         , testGetAuthorizersResponse $
--             getAuthorizersResponse
--
--         , testPutIntegrationResponseResponse $
--             integrationResponse
--
--         , testFlushStageCacheResponse $
--             flushStageCacheResponse
--
--         , testCreateRestAPIResponse $
--             restAPI
--
--         , testDeleteIntegrationResponseResponse $
--             deleteIntegrationResponseResponse
--
--         , testUpdateIntegrationResponseResponse $
--             integrationResponse
--
--         , testDeleteIntegrationResponse $
--             deleteIntegrationResponse'
--
--         , testUpdateIntegrationResponse $
--             integration
--
--         , testTestInvokeAuthorizerResponse $
--             testInvokeAuthorizerResponse
--
--         , testGenerateClientCertificateResponse $
--             clientCertificate
--
--         , testGetResourcesResponse $
--             getResourcesResponse
--
--         , testGetAccountResponse $
--             account
--
--         , testPutIntegrationResponse $
--             integration
--
--         , testGetAuthorizerResponse $
--             authorizer
--
--         , testGetStageResponse $
--             stage
--
--         , testGetExportResponse $
--             getExportResponse
--
--         , testGetSDKResponse $
--             getSDKResponse
--
--         , testGetAPIKeysResponse $
--             getAPIKeysResponse
--
--         , testDeleteBasePathMappingResponse $
--             deleteBasePathMappingResponse
--
--         , testUpdateBasePathMappingResponse $
--             basePathMapping
--
--         , testDeleteClientCertificateResponse $
--             deleteClientCertificateResponse
--
--         , testUpdateClientCertificateResponse $
--             clientCertificate
--
--         , testCreateAuthorizerResponse $
--             authorizer
--
--         , testUpdateAuthorizerResponse $
--             authorizer
--
--         , testDeleteAuthorizerResponse $
--             deleteAuthorizerResponse
--
--         , testCreateStageResponse $
--             stage
--
--         , testCreateAPIKeyResponse $
--             apiKey
--
--         , testPutMethodResponse $
--             method
--
--         , testUpdateDomainNameResponse $
--             domainName
--
--         , testDeleteDomainNameResponse $
--             deleteDomainNameResponse
--
--         , testCreateResourceResponse $
--             resource
--
--         , testDeleteMethodResponse $
--             deleteMethodResponse'
--
--         , testUpdateMethodResponse $
--             method
--
--         , testGetClientCertificatesResponse $
--             getClientCertificatesResponse
--
--         , testGetModelTemplateResponse $
--             getModelTemplateResponse
--
--         , testGetBasePathMappingsResponse $
--             getBasePathMappingsResponse
--
--         , testGetAPIKeyResponse $
--             apiKey
--
--           ]
--     ]

-- Requests

testGetResource :: GetResource -> TestTree
testGetResource = req
    "GetResource"
    "fixture/GetResource.yaml"

testGetDeployments :: GetDeployments -> TestTree
testGetDeployments = req
    "GetDeployments"
    "fixture/GetDeployments.yaml"

testGetDeployment :: GetDeployment -> TestTree
testGetDeployment = req
    "GetDeployment"
    "fixture/GetDeployment.yaml"

testGetDomainNames :: GetDomainNames -> TestTree
testGetDomainNames = req
    "GetDomainNames"
    "fixture/GetDomainNames.yaml"

testGetClientCertificate :: GetClientCertificate -> TestTree
testGetClientCertificate = req
    "GetClientCertificate"
    "fixture/GetClientCertificate.yaml"

testGetMethodResponse :: GetMethodResponse -> TestTree
testGetMethodResponse = req
    "GetMethodResponse"
    "fixture/GetMethodResponse.yaml"

testGetModels :: GetModels -> TestTree
testGetModels = req
    "GetModels"
    "fixture/GetModels.yaml"

testGetBasePathMapping :: GetBasePathMapping -> TestTree
testGetBasePathMapping = req
    "GetBasePathMapping"
    "fixture/GetBasePathMapping.yaml"

testPutMethodResponse :: PutMethodResponse -> TestTree
testPutMethodResponse = req
    "PutMethodResponse"
    "fixture/PutMethodResponse.yaml"

testImportRestAPI :: ImportRestAPI -> TestTree
testImportRestAPI = req
    "ImportRestAPI"
    "fixture/ImportRestAPI.yaml"

testDeleteMethodResponse :: DeleteMethodResponse -> TestTree
testDeleteMethodResponse = req
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.yaml"

testUpdateMethodResponse :: UpdateMethodResponse -> TestTree
testUpdateMethodResponse = req
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.yaml"

testDeleteStage :: DeleteStage -> TestTree
testDeleteStage = req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

testUpdateStage :: UpdateStage -> TestTree
testUpdateStage = req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

testGetRestAPIs :: GetRestAPIs -> TestTree
testGetRestAPIs = req
    "GetRestAPIs"
    "fixture/GetRestAPIs.yaml"

testCreateDeployment :: CreateDeployment -> TestTree
testCreateDeployment = req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

testCreateBasePathMapping :: CreateBasePathMapping -> TestTree
testCreateBasePathMapping = req
    "CreateBasePathMapping"
    "fixture/CreateBasePathMapping.yaml"

testGetIntegration :: GetIntegration -> TestTree
testGetIntegration = req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

testUpdateAccount :: UpdateAccount -> TestTree
testUpdateAccount = req
    "UpdateAccount"
    "fixture/UpdateAccount.yaml"

testDeleteDeployment :: DeleteDeployment -> TestTree
testDeleteDeployment = req
    "DeleteDeployment"
    "fixture/DeleteDeployment.yaml"

testUpdateDeployment :: UpdateDeployment -> TestTree
testUpdateDeployment = req
    "UpdateDeployment"
    "fixture/UpdateDeployment.yaml"

testDeleteResource :: DeleteResource -> TestTree
testDeleteResource = req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

testUpdateResource :: UpdateResource -> TestTree
testUpdateResource = req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

testCreateModel :: CreateModel -> TestTree
testCreateModel = req
    "CreateModel"
    "fixture/CreateModel.yaml"

testGetIntegrationResponse :: GetIntegrationResponse -> TestTree
testGetIntegrationResponse = req
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.yaml"

testCreateDomainName :: CreateDomainName -> TestTree
testCreateDomainName = req
    "CreateDomainName"
    "fixture/CreateDomainName.yaml"

testFlushStageAuthorizersCache :: FlushStageAuthorizersCache -> TestTree
testFlushStageAuthorizersCache = req
    "FlushStageAuthorizersCache"
    "fixture/FlushStageAuthorizersCache.yaml"

testDeleteModel :: DeleteModel -> TestTree
testDeleteModel = req
    "DeleteModel"
    "fixture/DeleteModel.yaml"

testUpdateModel :: UpdateModel -> TestTree
testUpdateModel = req
    "UpdateModel"
    "fixture/UpdateModel.yaml"

testDeleteAPIKey :: DeleteAPIKey -> TestTree
testDeleteAPIKey = req
    "DeleteAPIKey"
    "fixture/DeleteAPIKey.yaml"

testUpdateAPIKey :: UpdateAPIKey -> TestTree
testUpdateAPIKey = req
    "UpdateAPIKey"
    "fixture/UpdateAPIKey.yaml"

testGetRestAPI :: GetRestAPI -> TestTree
testGetRestAPI = req
    "GetRestAPI"
    "fixture/GetRestAPI.yaml"

testGetStages :: GetStages -> TestTree
testGetStages = req
    "GetStages"
    "fixture/GetStages.yaml"

testPutRestAPI :: PutRestAPI -> TestTree
testPutRestAPI = req
    "PutRestAPI"
    "fixture/PutRestAPI.yaml"

testGetMethod :: GetMethod -> TestTree
testGetMethod = req
    "GetMethod"
    "fixture/GetMethod.yaml"

testGetModel :: GetModel -> TestTree
testGetModel = req
    "GetModel"
    "fixture/GetModel.yaml"

testUpdateRestAPI :: UpdateRestAPI -> TestTree
testUpdateRestAPI = req
    "UpdateRestAPI"
    "fixture/UpdateRestAPI.yaml"

testDeleteRestAPI :: DeleteRestAPI -> TestTree
testDeleteRestAPI = req
    "DeleteRestAPI"
    "fixture/DeleteRestAPI.yaml"

testTestInvokeMethod :: TestInvokeMethod -> TestTree
testTestInvokeMethod = req
    "TestInvokeMethod"
    "fixture/TestInvokeMethod.yaml"

testGetDomainName :: GetDomainName -> TestTree
testGetDomainName = req
    "GetDomainName"
    "fixture/GetDomainName.yaml"

testGetAuthorizers :: GetAuthorizers -> TestTree
testGetAuthorizers = req
    "GetAuthorizers"
    "fixture/GetAuthorizers.yaml"

testPutIntegrationResponse :: PutIntegrationResponse -> TestTree
testPutIntegrationResponse = req
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.yaml"

testFlushStageCache :: FlushStageCache -> TestTree
testFlushStageCache = req
    "FlushStageCache"
    "fixture/FlushStageCache.yaml"

testCreateRestAPI :: CreateRestAPI -> TestTree
testCreateRestAPI = req
    "CreateRestAPI"
    "fixture/CreateRestAPI.yaml"

testDeleteIntegrationResponse :: DeleteIntegrationResponse -> TestTree
testDeleteIntegrationResponse = req
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.yaml"

testUpdateIntegrationResponse :: UpdateIntegrationResponse -> TestTree
testUpdateIntegrationResponse = req
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.yaml"

testDeleteIntegration :: DeleteIntegration -> TestTree
testDeleteIntegration = req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

testUpdateIntegration :: UpdateIntegration -> TestTree
testUpdateIntegration = req
    "UpdateIntegration"
    "fixture/UpdateIntegration.yaml"

testTestInvokeAuthorizer :: TestInvokeAuthorizer -> TestTree
testTestInvokeAuthorizer = req
    "TestInvokeAuthorizer"
    "fixture/TestInvokeAuthorizer.yaml"

testGenerateClientCertificate :: GenerateClientCertificate -> TestTree
testGenerateClientCertificate = req
    "GenerateClientCertificate"
    "fixture/GenerateClientCertificate.yaml"

testGetResources :: GetResources -> TestTree
testGetResources = req
    "GetResources"
    "fixture/GetResources.yaml"

testGetAccount :: GetAccount -> TestTree
testGetAccount = req
    "GetAccount"
    "fixture/GetAccount.yaml"

testPutIntegration :: PutIntegration -> TestTree
testPutIntegration = req
    "PutIntegration"
    "fixture/PutIntegration.yaml"

testGetAuthorizer :: GetAuthorizer -> TestTree
testGetAuthorizer = req
    "GetAuthorizer"
    "fixture/GetAuthorizer.yaml"

testGetStage :: GetStage -> TestTree
testGetStage = req
    "GetStage"
    "fixture/GetStage.yaml"

testGetExport :: GetExport -> TestTree
testGetExport = req
    "GetExport"
    "fixture/GetExport.yaml"

testGetSDK :: GetSDK -> TestTree
testGetSDK = req
    "GetSDK"
    "fixture/GetSDK.yaml"

testGetAPIKeys :: GetAPIKeys -> TestTree
testGetAPIKeys = req
    "GetAPIKeys"
    "fixture/GetAPIKeys.yaml"

testDeleteBasePathMapping :: DeleteBasePathMapping -> TestTree
testDeleteBasePathMapping = req
    "DeleteBasePathMapping"
    "fixture/DeleteBasePathMapping.yaml"

testUpdateBasePathMapping :: UpdateBasePathMapping -> TestTree
testUpdateBasePathMapping = req
    "UpdateBasePathMapping"
    "fixture/UpdateBasePathMapping.yaml"

testDeleteClientCertificate :: DeleteClientCertificate -> TestTree
testDeleteClientCertificate = req
    "DeleteClientCertificate"
    "fixture/DeleteClientCertificate.yaml"

testUpdateClientCertificate :: UpdateClientCertificate -> TestTree
testUpdateClientCertificate = req
    "UpdateClientCertificate"
    "fixture/UpdateClientCertificate.yaml"

testCreateAuthorizer :: CreateAuthorizer -> TestTree
testCreateAuthorizer = req
    "CreateAuthorizer"
    "fixture/CreateAuthorizer.yaml"

testUpdateAuthorizer :: UpdateAuthorizer -> TestTree
testUpdateAuthorizer = req
    "UpdateAuthorizer"
    "fixture/UpdateAuthorizer.yaml"

testDeleteAuthorizer :: DeleteAuthorizer -> TestTree
testDeleteAuthorizer = req
    "DeleteAuthorizer"
    "fixture/DeleteAuthorizer.yaml"

testCreateStage :: CreateStage -> TestTree
testCreateStage = req
    "CreateStage"
    "fixture/CreateStage.yaml"

testCreateAPIKey :: CreateAPIKey -> TestTree
testCreateAPIKey = req
    "CreateAPIKey"
    "fixture/CreateAPIKey.yaml"

testPutMethod :: PutMethod -> TestTree
testPutMethod = req
    "PutMethod"
    "fixture/PutMethod.yaml"

testUpdateDomainName :: UpdateDomainName -> TestTree
testUpdateDomainName = req
    "UpdateDomainName"
    "fixture/UpdateDomainName.yaml"

testDeleteDomainName :: DeleteDomainName -> TestTree
testDeleteDomainName = req
    "DeleteDomainName"
    "fixture/DeleteDomainName.yaml"

testCreateResource :: CreateResource -> TestTree
testCreateResource = req
    "CreateResource"
    "fixture/CreateResource.yaml"

testDeleteMethod :: DeleteMethod -> TestTree
testDeleteMethod = req
    "DeleteMethod"
    "fixture/DeleteMethod.yaml"

testUpdateMethod :: UpdateMethod -> TestTree
testUpdateMethod = req
    "UpdateMethod"
    "fixture/UpdateMethod.yaml"

testGetClientCertificates :: GetClientCertificates -> TestTree
testGetClientCertificates = req
    "GetClientCertificates"
    "fixture/GetClientCertificates.yaml"

testGetModelTemplate :: GetModelTemplate -> TestTree
testGetModelTemplate = req
    "GetModelTemplate"
    "fixture/GetModelTemplate.yaml"

testGetBasePathMappings :: GetBasePathMappings -> TestTree
testGetBasePathMappings = req
    "GetBasePathMappings"
    "fixture/GetBasePathMappings.yaml"

testGetAPIKey :: GetAPIKey -> TestTree
testGetAPIKey = req
    "GetAPIKey"
    "fixture/GetAPIKey.yaml"

-- Responses

testGetResourceResponse :: Resource -> TestTree
testGetResourceResponse = res
    "GetResourceResponse"
    "fixture/GetResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy GetResource)

testGetDeploymentsResponse :: GetDeploymentsResponse -> TestTree
testGetDeploymentsResponse = res
    "GetDeploymentsResponse"
    "fixture/GetDeploymentsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDeployments)

testGetDeploymentResponse :: Deployment -> TestTree
testGetDeploymentResponse = res
    "GetDeploymentResponse"
    "fixture/GetDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDeployment)

testGetDomainNamesResponse :: GetDomainNamesResponse -> TestTree
testGetDomainNamesResponse = res
    "GetDomainNamesResponse"
    "fixture/GetDomainNamesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDomainNames)

testGetClientCertificateResponse :: ClientCertificate -> TestTree
testGetClientCertificateResponse = res
    "GetClientCertificateResponse"
    "fixture/GetClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy GetClientCertificate)

testGetMethodResponseResponse :: MethodResponse -> TestTree
testGetMethodResponseResponse = res
    "GetMethodResponseResponse"
    "fixture/GetMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy GetMethodResponse)

testGetModelsResponse :: GetModelsResponse -> TestTree
testGetModelsResponse = res
    "GetModelsResponse"
    "fixture/GetModelsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetModels)

testGetBasePathMappingResponse :: BasePathMapping -> TestTree
testGetBasePathMappingResponse = res
    "GetBasePathMappingResponse"
    "fixture/GetBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy GetBasePathMapping)

testPutMethodResponseResponse :: MethodResponse -> TestTree
testPutMethodResponseResponse = res
    "PutMethodResponseResponse"
    "fixture/PutMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy PutMethodResponse)

testImportRestAPIResponse :: RestAPI -> TestTree
testImportRestAPIResponse = res
    "ImportRestAPIResponse"
    "fixture/ImportRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy ImportRestAPI)

testDeleteMethodResponseResponse :: DeleteMethodResponseResponse -> TestTree
testDeleteMethodResponseResponse = res
    "DeleteMethodResponseResponse"
    "fixture/DeleteMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteMethodResponse)

testUpdateMethodResponseResponse :: MethodResponse -> TestTree
testUpdateMethodResponseResponse = res
    "UpdateMethodResponseResponse"
    "fixture/UpdateMethodResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateMethodResponse)

testDeleteStageResponse :: DeleteStageResponse -> TestTree
testDeleteStageResponse = res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteStage)

testUpdateStageResponse :: Stage -> TestTree
testUpdateStageResponse = res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateStage)

testGetRestAPIsResponse :: GetRestAPIsResponse -> TestTree
testGetRestAPIsResponse = res
    "GetRestAPIsResponse"
    "fixture/GetRestAPIsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetRestAPIs)

testCreateDeploymentResponse :: Deployment -> TestTree
testCreateDeploymentResponse = res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateDeployment)

testCreateBasePathMappingResponse :: BasePathMapping -> TestTree
testCreateBasePathMappingResponse = res
    "CreateBasePathMappingResponse"
    "fixture/CreateBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateBasePathMapping)

testGetIntegrationResponse :: Integration -> TestTree
testGetIntegrationResponse = res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy GetIntegration)

testUpdateAccountResponse :: Account -> TestTree
testUpdateAccountResponse = res
    "UpdateAccountResponse"
    "fixture/UpdateAccountResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateAccount)

testDeleteDeploymentResponse :: DeleteDeploymentResponse -> TestTree
testDeleteDeploymentResponse = res
    "DeleteDeploymentResponse"
    "fixture/DeleteDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteDeployment)

testUpdateDeploymentResponse :: Deployment -> TestTree
testUpdateDeploymentResponse = res
    "UpdateDeploymentResponse"
    "fixture/UpdateDeploymentResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateDeployment)

testDeleteResourceResponse :: DeleteResourceResponse -> TestTree
testDeleteResourceResponse = res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteResource)

testUpdateResourceResponse :: Resource -> TestTree
testUpdateResourceResponse = res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateResource)

testCreateModelResponse :: Model -> TestTree
testCreateModelResponse = res
    "CreateModelResponse"
    "fixture/CreateModelResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateModel)

testGetIntegrationResponseResponse :: IntegrationResponse -> TestTree
testGetIntegrationResponseResponse = res
    "GetIntegrationResponseResponse"
    "fixture/GetIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy GetIntegrationResponse)

testCreateDomainNameResponse :: DomainName -> TestTree
testCreateDomainNameResponse = res
    "CreateDomainNameResponse"
    "fixture/CreateDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateDomainName)

testFlushStageAuthorizersCacheResponse :: FlushStageAuthorizersCacheResponse -> TestTree
testFlushStageAuthorizersCacheResponse = res
    "FlushStageAuthorizersCacheResponse"
    "fixture/FlushStageAuthorizersCacheResponse.proto"
    apiGateway
    (Proxy :: Proxy FlushStageAuthorizersCache)

testDeleteModelResponse :: DeleteModelResponse -> TestTree
testDeleteModelResponse = res
    "DeleteModelResponse"
    "fixture/DeleteModelResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteModel)

testUpdateModelResponse :: Model -> TestTree
testUpdateModelResponse = res
    "UpdateModelResponse"
    "fixture/UpdateModelResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateModel)

testDeleteAPIKeyResponse :: DeleteAPIKeyResponse -> TestTree
testDeleteAPIKeyResponse = res
    "DeleteAPIKeyResponse"
    "fixture/DeleteAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteAPIKey)

testUpdateAPIKeyResponse :: APIKey -> TestTree
testUpdateAPIKeyResponse = res
    "UpdateAPIKeyResponse"
    "fixture/UpdateAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateAPIKey)

testGetRestAPIResponse :: RestAPI -> TestTree
testGetRestAPIResponse = res
    "GetRestAPIResponse"
    "fixture/GetRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy GetRestAPI)

testGetStagesResponse :: GetStagesResponse -> TestTree
testGetStagesResponse = res
    "GetStagesResponse"
    "fixture/GetStagesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetStages)

testPutRestAPIResponse :: RestAPI -> TestTree
testPutRestAPIResponse = res
    "PutRestAPIResponse"
    "fixture/PutRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy PutRestAPI)

testGetMethodResponse :: Method -> TestTree
testGetMethodResponse = res
    "GetMethodResponse"
    "fixture/GetMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy GetMethod)

testGetModelResponse :: Model -> TestTree
testGetModelResponse = res
    "GetModelResponse"
    "fixture/GetModelResponse.proto"
    apiGateway
    (Proxy :: Proxy GetModel)

testUpdateRestAPIResponse :: RestAPI -> TestTree
testUpdateRestAPIResponse = res
    "UpdateRestAPIResponse"
    "fixture/UpdateRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateRestAPI)

testDeleteRestAPIResponse :: DeleteRestAPIResponse -> TestTree
testDeleteRestAPIResponse = res
    "DeleteRestAPIResponse"
    "fixture/DeleteRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteRestAPI)

testTestInvokeMethodResponse :: TestInvokeMethodResponse -> TestTree
testTestInvokeMethodResponse = res
    "TestInvokeMethodResponse"
    "fixture/TestInvokeMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy TestInvokeMethod)

testGetDomainNameResponse :: DomainName -> TestTree
testGetDomainNameResponse = res
    "GetDomainNameResponse"
    "fixture/GetDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy GetDomainName)

testGetAuthorizersResponse :: GetAuthorizersResponse -> TestTree
testGetAuthorizersResponse = res
    "GetAuthorizersResponse"
    "fixture/GetAuthorizersResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAuthorizers)

testPutIntegrationResponseResponse :: IntegrationResponse -> TestTree
testPutIntegrationResponseResponse = res
    "PutIntegrationResponseResponse"
    "fixture/PutIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy PutIntegrationResponse)

testFlushStageCacheResponse :: FlushStageCacheResponse -> TestTree
testFlushStageCacheResponse = res
    "FlushStageCacheResponse"
    "fixture/FlushStageCacheResponse.proto"
    apiGateway
    (Proxy :: Proxy FlushStageCache)

testCreateRestAPIResponse :: RestAPI -> TestTree
testCreateRestAPIResponse = res
    "CreateRestAPIResponse"
    "fixture/CreateRestAPIResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateRestAPI)

testDeleteIntegrationResponseResponse :: DeleteIntegrationResponseResponse -> TestTree
testDeleteIntegrationResponseResponse = res
    "DeleteIntegrationResponseResponse"
    "fixture/DeleteIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteIntegrationResponse)

testUpdateIntegrationResponseResponse :: IntegrationResponse -> TestTree
testUpdateIntegrationResponseResponse = res
    "UpdateIntegrationResponseResponse"
    "fixture/UpdateIntegrationResponseResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateIntegrationResponse)

testDeleteIntegrationResponse :: DeleteIntegrationResponse' -> TestTree
testDeleteIntegrationResponse = res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteIntegration)

testUpdateIntegrationResponse :: Integration -> TestTree
testUpdateIntegrationResponse = res
    "UpdateIntegrationResponse"
    "fixture/UpdateIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateIntegration)

testTestInvokeAuthorizerResponse :: TestInvokeAuthorizerResponse -> TestTree
testTestInvokeAuthorizerResponse = res
    "TestInvokeAuthorizerResponse"
    "fixture/TestInvokeAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy TestInvokeAuthorizer)

testGenerateClientCertificateResponse :: ClientCertificate -> TestTree
testGenerateClientCertificateResponse = res
    "GenerateClientCertificateResponse"
    "fixture/GenerateClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy GenerateClientCertificate)

testGetResourcesResponse :: GetResourcesResponse -> TestTree
testGetResourcesResponse = res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetResources)

testGetAccountResponse :: Account -> TestTree
testGetAccountResponse = res
    "GetAccountResponse"
    "fixture/GetAccountResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAccount)

testPutIntegrationResponse :: Integration -> TestTree
testPutIntegrationResponse = res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    apiGateway
    (Proxy :: Proxy PutIntegration)

testGetAuthorizerResponse :: Authorizer -> TestTree
testGetAuthorizerResponse = res
    "GetAuthorizerResponse"
    "fixture/GetAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAuthorizer)

testGetStageResponse :: Stage -> TestTree
testGetStageResponse = res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    apiGateway
    (Proxy :: Proxy GetStage)

testGetExportResponse :: GetExportResponse -> TestTree
testGetExportResponse = res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    apiGateway
    (Proxy :: Proxy GetExport)

testGetSDKResponse :: GetSDKResponse -> TestTree
testGetSDKResponse = res
    "GetSDKResponse"
    "fixture/GetSDKResponse.proto"
    apiGateway
    (Proxy :: Proxy GetSDK)

testGetAPIKeysResponse :: GetAPIKeysResponse -> TestTree
testGetAPIKeysResponse = res
    "GetAPIKeysResponse"
    "fixture/GetAPIKeysResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAPIKeys)

testDeleteBasePathMappingResponse :: DeleteBasePathMappingResponse -> TestTree
testDeleteBasePathMappingResponse = res
    "DeleteBasePathMappingResponse"
    "fixture/DeleteBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteBasePathMapping)

testUpdateBasePathMappingResponse :: BasePathMapping -> TestTree
testUpdateBasePathMappingResponse = res
    "UpdateBasePathMappingResponse"
    "fixture/UpdateBasePathMappingResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateBasePathMapping)

testDeleteClientCertificateResponse :: DeleteClientCertificateResponse -> TestTree
testDeleteClientCertificateResponse = res
    "DeleteClientCertificateResponse"
    "fixture/DeleteClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteClientCertificate)

testUpdateClientCertificateResponse :: ClientCertificate -> TestTree
testUpdateClientCertificateResponse = res
    "UpdateClientCertificateResponse"
    "fixture/UpdateClientCertificateResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateClientCertificate)

testCreateAuthorizerResponse :: Authorizer -> TestTree
testCreateAuthorizerResponse = res
    "CreateAuthorizerResponse"
    "fixture/CreateAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateAuthorizer)

testUpdateAuthorizerResponse :: Authorizer -> TestTree
testUpdateAuthorizerResponse = res
    "UpdateAuthorizerResponse"
    "fixture/UpdateAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateAuthorizer)

testDeleteAuthorizerResponse :: DeleteAuthorizerResponse -> TestTree
testDeleteAuthorizerResponse = res
    "DeleteAuthorizerResponse"
    "fixture/DeleteAuthorizerResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteAuthorizer)

testCreateStageResponse :: Stage -> TestTree
testCreateStageResponse = res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateStage)

testCreateAPIKeyResponse :: APIKey -> TestTree
testCreateAPIKeyResponse = res
    "CreateAPIKeyResponse"
    "fixture/CreateAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateAPIKey)

testPutMethodResponse :: Method -> TestTree
testPutMethodResponse = res
    "PutMethodResponse"
    "fixture/PutMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy PutMethod)

testUpdateDomainNameResponse :: DomainName -> TestTree
testUpdateDomainNameResponse = res
    "UpdateDomainNameResponse"
    "fixture/UpdateDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateDomainName)

testDeleteDomainNameResponse :: DeleteDomainNameResponse -> TestTree
testDeleteDomainNameResponse = res
    "DeleteDomainNameResponse"
    "fixture/DeleteDomainNameResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteDomainName)

testCreateResourceResponse :: Resource -> TestTree
testCreateResourceResponse = res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    apiGateway
    (Proxy :: Proxy CreateResource)

testDeleteMethodResponse :: DeleteMethodResponse' -> TestTree
testDeleteMethodResponse = res
    "DeleteMethodResponse"
    "fixture/DeleteMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy DeleteMethod)

testUpdateMethodResponse :: Method -> TestTree
testUpdateMethodResponse = res
    "UpdateMethodResponse"
    "fixture/UpdateMethodResponse.proto"
    apiGateway
    (Proxy :: Proxy UpdateMethod)

testGetClientCertificatesResponse :: GetClientCertificatesResponse -> TestTree
testGetClientCertificatesResponse = res
    "GetClientCertificatesResponse"
    "fixture/GetClientCertificatesResponse.proto"
    apiGateway
    (Proxy :: Proxy GetClientCertificates)

testGetModelTemplateResponse :: GetModelTemplateResponse -> TestTree
testGetModelTemplateResponse = res
    "GetModelTemplateResponse"
    "fixture/GetModelTemplateResponse.proto"
    apiGateway
    (Proxy :: Proxy GetModelTemplate)

testGetBasePathMappingsResponse :: GetBasePathMappingsResponse -> TestTree
testGetBasePathMappingsResponse = res
    "GetBasePathMappingsResponse"
    "fixture/GetBasePathMappingsResponse.proto"
    apiGateway
    (Proxy :: Proxy GetBasePathMappings)

testGetAPIKeyResponse :: APIKey -> TestTree
testGetAPIKeyResponse = res
    "GetAPIKeyResponse"
    "fixture/GetAPIKeyResponse.proto"
    apiGateway
    (Proxy :: Proxy GetAPIKey)

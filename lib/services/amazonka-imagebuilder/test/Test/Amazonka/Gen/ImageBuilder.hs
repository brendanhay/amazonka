{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ImageBuilder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ImageBuilder where

import Amazonka.ImageBuilder
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.ImageBuilder.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCancelImageCreation $
--             newCancelImageCreation
--
--         , requestCreateComponent $
--             newCreateComponent
--
--         , requestCreateContainerRecipe $
--             newCreateContainerRecipe
--
--         , requestCreateDistributionConfiguration $
--             newCreateDistributionConfiguration
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestCreateImagePipeline $
--             newCreateImagePipeline
--
--         , requestCreateImageRecipe $
--             newCreateImageRecipe
--
--         , requestCreateInfrastructureConfiguration $
--             newCreateInfrastructureConfiguration
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestDeleteContainerRecipe $
--             newDeleteContainerRecipe
--
--         , requestDeleteDistributionConfiguration $
--             newDeleteDistributionConfiguration
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestDeleteImagePipeline $
--             newDeleteImagePipeline
--
--         , requestDeleteImageRecipe $
--             newDeleteImageRecipe
--
--         , requestDeleteInfrastructureConfiguration $
--             newDeleteInfrastructureConfiguration
--
--         , requestGetComponent $
--             newGetComponent
--
--         , requestGetComponentPolicy $
--             newGetComponentPolicy
--
--         , requestGetContainerRecipe $
--             newGetContainerRecipe
--
--         , requestGetContainerRecipePolicy $
--             newGetContainerRecipePolicy
--
--         , requestGetDistributionConfiguration $
--             newGetDistributionConfiguration
--
--         , requestGetImage $
--             newGetImage
--
--         , requestGetImagePipeline $
--             newGetImagePipeline
--
--         , requestGetImagePolicy $
--             newGetImagePolicy
--
--         , requestGetImageRecipe $
--             newGetImageRecipe
--
--         , requestGetImageRecipePolicy $
--             newGetImageRecipePolicy
--
--         , requestGetInfrastructureConfiguration $
--             newGetInfrastructureConfiguration
--
--         , requestGetWorkflowExecution $
--             newGetWorkflowExecution
--
--         , requestGetWorkflowStepExecution $
--             newGetWorkflowStepExecution
--
--         , requestImportComponent $
--             newImportComponent
--
--         , requestImportVmImage $
--             newImportVmImage
--
--         , requestListComponentBuildVersions $
--             newListComponentBuildVersions
--
--         , requestListComponents $
--             newListComponents
--
--         , requestListContainerRecipes $
--             newListContainerRecipes
--
--         , requestListDistributionConfigurations $
--             newListDistributionConfigurations
--
--         , requestListImageBuildVersions $
--             newListImageBuildVersions
--
--         , requestListImagePackages $
--             newListImagePackages
--
--         , requestListImagePipelineImages $
--             newListImagePipelineImages
--
--         , requestListImagePipelines $
--             newListImagePipelines
--
--         , requestListImageRecipes $
--             newListImageRecipes
--
--         , requestListImageScanFindingAggregations $
--             newListImageScanFindingAggregations
--
--         , requestListImageScanFindings $
--             newListImageScanFindings
--
--         , requestListImages $
--             newListImages
--
--         , requestListInfrastructureConfigurations $
--             newListInfrastructureConfigurations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkflowExecutions $
--             newListWorkflowExecutions
--
--         , requestListWorkflowStepExecutions $
--             newListWorkflowStepExecutions
--
--         , requestPutComponentPolicy $
--             newPutComponentPolicy
--
--         , requestPutContainerRecipePolicy $
--             newPutContainerRecipePolicy
--
--         , requestPutImagePolicy $
--             newPutImagePolicy
--
--         , requestPutImageRecipePolicy $
--             newPutImageRecipePolicy
--
--         , requestStartImagePipelineExecution $
--             newStartImagePipelineExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDistributionConfiguration $
--             newUpdateDistributionConfiguration
--
--         , requestUpdateImagePipeline $
--             newUpdateImagePipeline
--
--         , requestUpdateInfrastructureConfiguration $
--             newUpdateInfrastructureConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseCancelImageCreation $
--             newCancelImageCreationResponse
--
--         , responseCreateComponent $
--             newCreateComponentResponse
--
--         , responseCreateContainerRecipe $
--             newCreateContainerRecipeResponse
--
--         , responseCreateDistributionConfiguration $
--             newCreateDistributionConfigurationResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseCreateImagePipeline $
--             newCreateImagePipelineResponse
--
--         , responseCreateImageRecipe $
--             newCreateImageRecipeResponse
--
--         , responseCreateInfrastructureConfiguration $
--             newCreateInfrastructureConfigurationResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseDeleteContainerRecipe $
--             newDeleteContainerRecipeResponse
--
--         , responseDeleteDistributionConfiguration $
--             newDeleteDistributionConfigurationResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseDeleteImagePipeline $
--             newDeleteImagePipelineResponse
--
--         , responseDeleteImageRecipe $
--             newDeleteImageRecipeResponse
--
--         , responseDeleteInfrastructureConfiguration $
--             newDeleteInfrastructureConfigurationResponse
--
--         , responseGetComponent $
--             newGetComponentResponse
--
--         , responseGetComponentPolicy $
--             newGetComponentPolicyResponse
--
--         , responseGetContainerRecipe $
--             newGetContainerRecipeResponse
--
--         , responseGetContainerRecipePolicy $
--             newGetContainerRecipePolicyResponse
--
--         , responseGetDistributionConfiguration $
--             newGetDistributionConfigurationResponse
--
--         , responseGetImage $
--             newGetImageResponse
--
--         , responseGetImagePipeline $
--             newGetImagePipelineResponse
--
--         , responseGetImagePolicy $
--             newGetImagePolicyResponse
--
--         , responseGetImageRecipe $
--             newGetImageRecipeResponse
--
--         , responseGetImageRecipePolicy $
--             newGetImageRecipePolicyResponse
--
--         , responseGetInfrastructureConfiguration $
--             newGetInfrastructureConfigurationResponse
--
--         , responseGetWorkflowExecution $
--             newGetWorkflowExecutionResponse
--
--         , responseGetWorkflowStepExecution $
--             newGetWorkflowStepExecutionResponse
--
--         , responseImportComponent $
--             newImportComponentResponse
--
--         , responseImportVmImage $
--             newImportVmImageResponse
--
--         , responseListComponentBuildVersions $
--             newListComponentBuildVersionsResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responseListContainerRecipes $
--             newListContainerRecipesResponse
--
--         , responseListDistributionConfigurations $
--             newListDistributionConfigurationsResponse
--
--         , responseListImageBuildVersions $
--             newListImageBuildVersionsResponse
--
--         , responseListImagePackages $
--             newListImagePackagesResponse
--
--         , responseListImagePipelineImages $
--             newListImagePipelineImagesResponse
--
--         , responseListImagePipelines $
--             newListImagePipelinesResponse
--
--         , responseListImageRecipes $
--             newListImageRecipesResponse
--
--         , responseListImageScanFindingAggregations $
--             newListImageScanFindingAggregationsResponse
--
--         , responseListImageScanFindings $
--             newListImageScanFindingsResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseListInfrastructureConfigurations $
--             newListInfrastructureConfigurationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkflowExecutions $
--             newListWorkflowExecutionsResponse
--
--         , responseListWorkflowStepExecutions $
--             newListWorkflowStepExecutionsResponse
--
--         , responsePutComponentPolicy $
--             newPutComponentPolicyResponse
--
--         , responsePutContainerRecipePolicy $
--             newPutContainerRecipePolicyResponse
--
--         , responsePutImagePolicy $
--             newPutImagePolicyResponse
--
--         , responsePutImageRecipePolicy $
--             newPutImageRecipePolicyResponse
--
--         , responseStartImagePipelineExecution $
--             newStartImagePipelineExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDistributionConfiguration $
--             newUpdateDistributionConfigurationResponse
--
--         , responseUpdateImagePipeline $
--             newUpdateImagePipelineResponse
--
--         , responseUpdateInfrastructureConfiguration $
--             newUpdateInfrastructureConfigurationResponse
--
--           ]
--     ]

-- Requests

requestCancelImageCreation :: CancelImageCreation -> TestTree
requestCancelImageCreation =
  req
    "CancelImageCreation"
    "fixture/CancelImageCreation.yaml"

requestCreateComponent :: CreateComponent -> TestTree
requestCreateComponent =
  req
    "CreateComponent"
    "fixture/CreateComponent.yaml"

requestCreateContainerRecipe :: CreateContainerRecipe -> TestTree
requestCreateContainerRecipe =
  req
    "CreateContainerRecipe"
    "fixture/CreateContainerRecipe.yaml"

requestCreateDistributionConfiguration :: CreateDistributionConfiguration -> TestTree
requestCreateDistributionConfiguration =
  req
    "CreateDistributionConfiguration"
    "fixture/CreateDistributionConfiguration.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestCreateImagePipeline :: CreateImagePipeline -> TestTree
requestCreateImagePipeline =
  req
    "CreateImagePipeline"
    "fixture/CreateImagePipeline.yaml"

requestCreateImageRecipe :: CreateImageRecipe -> TestTree
requestCreateImageRecipe =
  req
    "CreateImageRecipe"
    "fixture/CreateImageRecipe.yaml"

requestCreateInfrastructureConfiguration :: CreateInfrastructureConfiguration -> TestTree
requestCreateInfrastructureConfiguration =
  req
    "CreateInfrastructureConfiguration"
    "fixture/CreateInfrastructureConfiguration.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestDeleteContainerRecipe :: DeleteContainerRecipe -> TestTree
requestDeleteContainerRecipe =
  req
    "DeleteContainerRecipe"
    "fixture/DeleteContainerRecipe.yaml"

requestDeleteDistributionConfiguration :: DeleteDistributionConfiguration -> TestTree
requestDeleteDistributionConfiguration =
  req
    "DeleteDistributionConfiguration"
    "fixture/DeleteDistributionConfiguration.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestDeleteImagePipeline :: DeleteImagePipeline -> TestTree
requestDeleteImagePipeline =
  req
    "DeleteImagePipeline"
    "fixture/DeleteImagePipeline.yaml"

requestDeleteImageRecipe :: DeleteImageRecipe -> TestTree
requestDeleteImageRecipe =
  req
    "DeleteImageRecipe"
    "fixture/DeleteImageRecipe.yaml"

requestDeleteInfrastructureConfiguration :: DeleteInfrastructureConfiguration -> TestTree
requestDeleteInfrastructureConfiguration =
  req
    "DeleteInfrastructureConfiguration"
    "fixture/DeleteInfrastructureConfiguration.yaml"

requestGetComponent :: GetComponent -> TestTree
requestGetComponent =
  req
    "GetComponent"
    "fixture/GetComponent.yaml"

requestGetComponentPolicy :: GetComponentPolicy -> TestTree
requestGetComponentPolicy =
  req
    "GetComponentPolicy"
    "fixture/GetComponentPolicy.yaml"

requestGetContainerRecipe :: GetContainerRecipe -> TestTree
requestGetContainerRecipe =
  req
    "GetContainerRecipe"
    "fixture/GetContainerRecipe.yaml"

requestGetContainerRecipePolicy :: GetContainerRecipePolicy -> TestTree
requestGetContainerRecipePolicy =
  req
    "GetContainerRecipePolicy"
    "fixture/GetContainerRecipePolicy.yaml"

requestGetDistributionConfiguration :: GetDistributionConfiguration -> TestTree
requestGetDistributionConfiguration =
  req
    "GetDistributionConfiguration"
    "fixture/GetDistributionConfiguration.yaml"

requestGetImage :: GetImage -> TestTree
requestGetImage =
  req
    "GetImage"
    "fixture/GetImage.yaml"

requestGetImagePipeline :: GetImagePipeline -> TestTree
requestGetImagePipeline =
  req
    "GetImagePipeline"
    "fixture/GetImagePipeline.yaml"

requestGetImagePolicy :: GetImagePolicy -> TestTree
requestGetImagePolicy =
  req
    "GetImagePolicy"
    "fixture/GetImagePolicy.yaml"

requestGetImageRecipe :: GetImageRecipe -> TestTree
requestGetImageRecipe =
  req
    "GetImageRecipe"
    "fixture/GetImageRecipe.yaml"

requestGetImageRecipePolicy :: GetImageRecipePolicy -> TestTree
requestGetImageRecipePolicy =
  req
    "GetImageRecipePolicy"
    "fixture/GetImageRecipePolicy.yaml"

requestGetInfrastructureConfiguration :: GetInfrastructureConfiguration -> TestTree
requestGetInfrastructureConfiguration =
  req
    "GetInfrastructureConfiguration"
    "fixture/GetInfrastructureConfiguration.yaml"

requestGetWorkflowExecution :: GetWorkflowExecution -> TestTree
requestGetWorkflowExecution =
  req
    "GetWorkflowExecution"
    "fixture/GetWorkflowExecution.yaml"

requestGetWorkflowStepExecution :: GetWorkflowStepExecution -> TestTree
requestGetWorkflowStepExecution =
  req
    "GetWorkflowStepExecution"
    "fixture/GetWorkflowStepExecution.yaml"

requestImportComponent :: ImportComponent -> TestTree
requestImportComponent =
  req
    "ImportComponent"
    "fixture/ImportComponent.yaml"

requestImportVmImage :: ImportVmImage -> TestTree
requestImportVmImage =
  req
    "ImportVmImage"
    "fixture/ImportVmImage.yaml"

requestListComponentBuildVersions :: ListComponentBuildVersions -> TestTree
requestListComponentBuildVersions =
  req
    "ListComponentBuildVersions"
    "fixture/ListComponentBuildVersions.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestListContainerRecipes :: ListContainerRecipes -> TestTree
requestListContainerRecipes =
  req
    "ListContainerRecipes"
    "fixture/ListContainerRecipes.yaml"

requestListDistributionConfigurations :: ListDistributionConfigurations -> TestTree
requestListDistributionConfigurations =
  req
    "ListDistributionConfigurations"
    "fixture/ListDistributionConfigurations.yaml"

requestListImageBuildVersions :: ListImageBuildVersions -> TestTree
requestListImageBuildVersions =
  req
    "ListImageBuildVersions"
    "fixture/ListImageBuildVersions.yaml"

requestListImagePackages :: ListImagePackages -> TestTree
requestListImagePackages =
  req
    "ListImagePackages"
    "fixture/ListImagePackages.yaml"

requestListImagePipelineImages :: ListImagePipelineImages -> TestTree
requestListImagePipelineImages =
  req
    "ListImagePipelineImages"
    "fixture/ListImagePipelineImages.yaml"

requestListImagePipelines :: ListImagePipelines -> TestTree
requestListImagePipelines =
  req
    "ListImagePipelines"
    "fixture/ListImagePipelines.yaml"

requestListImageRecipes :: ListImageRecipes -> TestTree
requestListImageRecipes =
  req
    "ListImageRecipes"
    "fixture/ListImageRecipes.yaml"

requestListImageScanFindingAggregations :: ListImageScanFindingAggregations -> TestTree
requestListImageScanFindingAggregations =
  req
    "ListImageScanFindingAggregations"
    "fixture/ListImageScanFindingAggregations.yaml"

requestListImageScanFindings :: ListImageScanFindings -> TestTree
requestListImageScanFindings =
  req
    "ListImageScanFindings"
    "fixture/ListImageScanFindings.yaml"

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestListInfrastructureConfigurations :: ListInfrastructureConfigurations -> TestTree
requestListInfrastructureConfigurations =
  req
    "ListInfrastructureConfigurations"
    "fixture/ListInfrastructureConfigurations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWorkflowExecutions :: ListWorkflowExecutions -> TestTree
requestListWorkflowExecutions =
  req
    "ListWorkflowExecutions"
    "fixture/ListWorkflowExecutions.yaml"

requestListWorkflowStepExecutions :: ListWorkflowStepExecutions -> TestTree
requestListWorkflowStepExecutions =
  req
    "ListWorkflowStepExecutions"
    "fixture/ListWorkflowStepExecutions.yaml"

requestPutComponentPolicy :: PutComponentPolicy -> TestTree
requestPutComponentPolicy =
  req
    "PutComponentPolicy"
    "fixture/PutComponentPolicy.yaml"

requestPutContainerRecipePolicy :: PutContainerRecipePolicy -> TestTree
requestPutContainerRecipePolicy =
  req
    "PutContainerRecipePolicy"
    "fixture/PutContainerRecipePolicy.yaml"

requestPutImagePolicy :: PutImagePolicy -> TestTree
requestPutImagePolicy =
  req
    "PutImagePolicy"
    "fixture/PutImagePolicy.yaml"

requestPutImageRecipePolicy :: PutImageRecipePolicy -> TestTree
requestPutImageRecipePolicy =
  req
    "PutImageRecipePolicy"
    "fixture/PutImageRecipePolicy.yaml"

requestStartImagePipelineExecution :: StartImagePipelineExecution -> TestTree
requestStartImagePipelineExecution =
  req
    "StartImagePipelineExecution"
    "fixture/StartImagePipelineExecution.yaml"

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

requestUpdateDistributionConfiguration :: UpdateDistributionConfiguration -> TestTree
requestUpdateDistributionConfiguration =
  req
    "UpdateDistributionConfiguration"
    "fixture/UpdateDistributionConfiguration.yaml"

requestUpdateImagePipeline :: UpdateImagePipeline -> TestTree
requestUpdateImagePipeline =
  req
    "UpdateImagePipeline"
    "fixture/UpdateImagePipeline.yaml"

requestUpdateInfrastructureConfiguration :: UpdateInfrastructureConfiguration -> TestTree
requestUpdateInfrastructureConfiguration =
  req
    "UpdateInfrastructureConfiguration"
    "fixture/UpdateInfrastructureConfiguration.yaml"

-- Responses

responseCancelImageCreation :: CancelImageCreationResponse -> TestTree
responseCancelImageCreation =
  res
    "CancelImageCreationResponse"
    "fixture/CancelImageCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelImageCreation)

responseCreateComponent :: CreateComponentResponse -> TestTree
responseCreateComponent =
  res
    "CreateComponentResponse"
    "fixture/CreateComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateComponent)

responseCreateContainerRecipe :: CreateContainerRecipeResponse -> TestTree
responseCreateContainerRecipe =
  res
    "CreateContainerRecipeResponse"
    "fixture/CreateContainerRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainerRecipe)

responseCreateDistributionConfiguration :: CreateDistributionConfigurationResponse -> TestTree
responseCreateDistributionConfiguration =
  res
    "CreateDistributionConfigurationResponse"
    "fixture/CreateDistributionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDistributionConfiguration)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImage)

responseCreateImagePipeline :: CreateImagePipelineResponse -> TestTree
responseCreateImagePipeline =
  res
    "CreateImagePipelineResponse"
    "fixture/CreateImagePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImagePipeline)

responseCreateImageRecipe :: CreateImageRecipeResponse -> TestTree
responseCreateImageRecipe =
  res
    "CreateImageRecipeResponse"
    "fixture/CreateImageRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImageRecipe)

responseCreateInfrastructureConfiguration :: CreateInfrastructureConfigurationResponse -> TestTree
responseCreateInfrastructureConfiguration =
  res
    "CreateInfrastructureConfigurationResponse"
    "fixture/CreateInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInfrastructureConfiguration)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteComponent)

responseDeleteContainerRecipe :: DeleteContainerRecipeResponse -> TestTree
responseDeleteContainerRecipe =
  res
    "DeleteContainerRecipeResponse"
    "fixture/DeleteContainerRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContainerRecipe)

responseDeleteDistributionConfiguration :: DeleteDistributionConfigurationResponse -> TestTree
responseDeleteDistributionConfiguration =
  res
    "DeleteDistributionConfigurationResponse"
    "fixture/DeleteDistributionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDistributionConfiguration)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImage)

responseDeleteImagePipeline :: DeleteImagePipelineResponse -> TestTree
responseDeleteImagePipeline =
  res
    "DeleteImagePipelineResponse"
    "fixture/DeleteImagePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImagePipeline)

responseDeleteImageRecipe :: DeleteImageRecipeResponse -> TestTree
responseDeleteImageRecipe =
  res
    "DeleteImageRecipeResponse"
    "fixture/DeleteImageRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImageRecipe)

responseDeleteInfrastructureConfiguration :: DeleteInfrastructureConfigurationResponse -> TestTree
responseDeleteInfrastructureConfiguration =
  res
    "DeleteInfrastructureConfigurationResponse"
    "fixture/DeleteInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInfrastructureConfiguration)

responseGetComponent :: GetComponentResponse -> TestTree
responseGetComponent =
  res
    "GetComponentResponse"
    "fixture/GetComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComponent)

responseGetComponentPolicy :: GetComponentPolicyResponse -> TestTree
responseGetComponentPolicy =
  res
    "GetComponentPolicyResponse"
    "fixture/GetComponentPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComponentPolicy)

responseGetContainerRecipe :: GetContainerRecipeResponse -> TestTree
responseGetContainerRecipe =
  res
    "GetContainerRecipeResponse"
    "fixture/GetContainerRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerRecipe)

responseGetContainerRecipePolicy :: GetContainerRecipePolicyResponse -> TestTree
responseGetContainerRecipePolicy =
  res
    "GetContainerRecipePolicyResponse"
    "fixture/GetContainerRecipePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerRecipePolicy)

responseGetDistributionConfiguration :: GetDistributionConfigurationResponse -> TestTree
responseGetDistributionConfiguration =
  res
    "GetDistributionConfigurationResponse"
    "fixture/GetDistributionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionConfiguration)

responseGetImage :: GetImageResponse -> TestTree
responseGetImage =
  res
    "GetImageResponse"
    "fixture/GetImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImage)

responseGetImagePipeline :: GetImagePipelineResponse -> TestTree
responseGetImagePipeline =
  res
    "GetImagePipelineResponse"
    "fixture/GetImagePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImagePipeline)

responseGetImagePolicy :: GetImagePolicyResponse -> TestTree
responseGetImagePolicy =
  res
    "GetImagePolicyResponse"
    "fixture/GetImagePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImagePolicy)

responseGetImageRecipe :: GetImageRecipeResponse -> TestTree
responseGetImageRecipe =
  res
    "GetImageRecipeResponse"
    "fixture/GetImageRecipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImageRecipe)

responseGetImageRecipePolicy :: GetImageRecipePolicyResponse -> TestTree
responseGetImageRecipePolicy =
  res
    "GetImageRecipePolicyResponse"
    "fixture/GetImageRecipePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImageRecipePolicy)

responseGetInfrastructureConfiguration :: GetInfrastructureConfigurationResponse -> TestTree
responseGetInfrastructureConfiguration =
  res
    "GetInfrastructureConfigurationResponse"
    "fixture/GetInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInfrastructureConfiguration)

responseGetWorkflowExecution :: GetWorkflowExecutionResponse -> TestTree
responseGetWorkflowExecution =
  res
    "GetWorkflowExecutionResponse"
    "fixture/GetWorkflowExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowExecution)

responseGetWorkflowStepExecution :: GetWorkflowStepExecutionResponse -> TestTree
responseGetWorkflowStepExecution =
  res
    "GetWorkflowStepExecutionResponse"
    "fixture/GetWorkflowStepExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowStepExecution)

responseImportComponent :: ImportComponentResponse -> TestTree
responseImportComponent =
  res
    "ImportComponentResponse"
    "fixture/ImportComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportComponent)

responseImportVmImage :: ImportVmImageResponse -> TestTree
responseImportVmImage =
  res
    "ImportVmImageResponse"
    "fixture/ImportVmImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportVmImage)

responseListComponentBuildVersions :: ListComponentBuildVersionsResponse -> TestTree
responseListComponentBuildVersions =
  res
    "ListComponentBuildVersionsResponse"
    "fixture/ListComponentBuildVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponentBuildVersions)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListComponents)

responseListContainerRecipes :: ListContainerRecipesResponse -> TestTree
responseListContainerRecipes =
  res
    "ListContainerRecipesResponse"
    "fixture/ListContainerRecipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContainerRecipes)

responseListDistributionConfigurations :: ListDistributionConfigurationsResponse -> TestTree
responseListDistributionConfigurations =
  res
    "ListDistributionConfigurationsResponse"
    "fixture/ListDistributionConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDistributionConfigurations)

responseListImageBuildVersions :: ListImageBuildVersionsResponse -> TestTree
responseListImageBuildVersions =
  res
    "ListImageBuildVersionsResponse"
    "fixture/ListImageBuildVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImageBuildVersions)

responseListImagePackages :: ListImagePackagesResponse -> TestTree
responseListImagePackages =
  res
    "ListImagePackagesResponse"
    "fixture/ListImagePackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImagePackages)

responseListImagePipelineImages :: ListImagePipelineImagesResponse -> TestTree
responseListImagePipelineImages =
  res
    "ListImagePipelineImagesResponse"
    "fixture/ListImagePipelineImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImagePipelineImages)

responseListImagePipelines :: ListImagePipelinesResponse -> TestTree
responseListImagePipelines =
  res
    "ListImagePipelinesResponse"
    "fixture/ListImagePipelinesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImagePipelines)

responseListImageRecipes :: ListImageRecipesResponse -> TestTree
responseListImageRecipes =
  res
    "ListImageRecipesResponse"
    "fixture/ListImageRecipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImageRecipes)

responseListImageScanFindingAggregations :: ListImageScanFindingAggregationsResponse -> TestTree
responseListImageScanFindingAggregations =
  res
    "ListImageScanFindingAggregationsResponse"
    "fixture/ListImageScanFindingAggregationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImageScanFindingAggregations)

responseListImageScanFindings :: ListImageScanFindingsResponse -> TestTree
responseListImageScanFindings =
  res
    "ListImageScanFindingsResponse"
    "fixture/ListImageScanFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImageScanFindings)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImages)

responseListInfrastructureConfigurations :: ListInfrastructureConfigurationsResponse -> TestTree
responseListInfrastructureConfigurations =
  res
    "ListInfrastructureConfigurationsResponse"
    "fixture/ListInfrastructureConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListInfrastructureConfigurations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWorkflowExecutions :: ListWorkflowExecutionsResponse -> TestTree
responseListWorkflowExecutions =
  res
    "ListWorkflowExecutionsResponse"
    "fixture/ListWorkflowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflowExecutions)

responseListWorkflowStepExecutions :: ListWorkflowStepExecutionsResponse -> TestTree
responseListWorkflowStepExecutions =
  res
    "ListWorkflowStepExecutionsResponse"
    "fixture/ListWorkflowStepExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflowStepExecutions)

responsePutComponentPolicy :: PutComponentPolicyResponse -> TestTree
responsePutComponentPolicy =
  res
    "PutComponentPolicyResponse"
    "fixture/PutComponentPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutComponentPolicy)

responsePutContainerRecipePolicy :: PutContainerRecipePolicyResponse -> TestTree
responsePutContainerRecipePolicy =
  res
    "PutContainerRecipePolicyResponse"
    "fixture/PutContainerRecipePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutContainerRecipePolicy)

responsePutImagePolicy :: PutImagePolicyResponse -> TestTree
responsePutImagePolicy =
  res
    "PutImagePolicyResponse"
    "fixture/PutImagePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImagePolicy)

responsePutImageRecipePolicy :: PutImageRecipePolicyResponse -> TestTree
responsePutImageRecipePolicy =
  res
    "PutImageRecipePolicyResponse"
    "fixture/PutImageRecipePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutImageRecipePolicy)

responseStartImagePipelineExecution :: StartImagePipelineExecutionResponse -> TestTree
responseStartImagePipelineExecution =
  res
    "StartImagePipelineExecutionResponse"
    "fixture/StartImagePipelineExecutionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImagePipelineExecution)

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

responseUpdateDistributionConfiguration :: UpdateDistributionConfigurationResponse -> TestTree
responseUpdateDistributionConfiguration =
  res
    "UpdateDistributionConfigurationResponse"
    "fixture/UpdateDistributionConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDistributionConfiguration)

responseUpdateImagePipeline :: UpdateImagePipelineResponse -> TestTree
responseUpdateImagePipeline =
  res
    "UpdateImagePipelineResponse"
    "fixture/UpdateImagePipelineResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateImagePipeline)

responseUpdateInfrastructureConfiguration :: UpdateInfrastructureConfigurationResponse -> TestTree
responseUpdateInfrastructureConfiguration =
  res
    "UpdateInfrastructureConfigurationResponse"
    "fixture/UpdateInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInfrastructureConfiguration)

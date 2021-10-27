{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ImageBuilder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ImageBuilder where

import Data.Proxy
import Network.AWS.ImageBuilder
import Test.AWS.Fixture
import Test.AWS.ImageBuilder.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListImagePackages $
--             newListImagePackages
--
--         , requestGetDistributionConfiguration $
--             newGetDistributionConfiguration
--
--         , requestImportComponent $
--             newImportComponent
--
--         , requestListComponentBuildVersions $
--             newListComponentBuildVersions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListImageBuildVersions $
--             newListImageBuildVersions
--
--         , requestCancelImageCreation $
--             newCancelImageCreation
--
--         , requestGetImagePipeline $
--             newGetImagePipeline
--
--         , requestCreateInfrastructureConfiguration $
--             newCreateInfrastructureConfiguration
--
--         , requestListDistributionConfigurations $
--             newListDistributionConfigurations
--
--         , requestCreateContainerRecipe $
--             newCreateContainerRecipe
--
--         , requestListInfrastructureConfigurations $
--             newListInfrastructureConfigurations
--
--         , requestPutImageRecipePolicy $
--             newPutImageRecipePolicy
--
--         , requestListContainerRecipes $
--             newListContainerRecipes
--
--         , requestPutImagePolicy $
--             newPutImagePolicy
--
--         , requestDeleteContainerRecipe $
--             newDeleteContainerRecipe
--
--         , requestPutComponentPolicy $
--             newPutComponentPolicy
--
--         , requestDeleteInfrastructureConfiguration $
--             newDeleteInfrastructureConfiguration
--
--         , requestUpdateInfrastructureConfiguration $
--             newUpdateInfrastructureConfiguration
--
--         , requestCreateImagePipeline $
--             newCreateImagePipeline
--
--         , requestGetContainerRecipe $
--             newGetContainerRecipe
--
--         , requestGetInfrastructureConfiguration $
--             newGetInfrastructureConfiguration
--
--         , requestGetImagePolicy $
--             newGetImagePolicy
--
--         , requestGetImageRecipePolicy $
--             newGetImageRecipePolicy
--
--         , requestGetComponentPolicy $
--             newGetComponentPolicy
--
--         , requestDeleteImagePipeline $
--             newDeleteImagePipeline
--
--         , requestUpdateImagePipeline $
--             newUpdateImagePipeline
--
--         , requestListImagePipelines $
--             newListImagePipelines
--
--         , requestStartImagePipelineExecution $
--             newStartImagePipelineExecution
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateImageRecipe $
--             newCreateImageRecipe
--
--         , requestCreateImage $
--             newCreateImage
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateComponent $
--             newCreateComponent
--
--         , requestDeleteDistributionConfiguration $
--             newDeleteDistributionConfiguration
--
--         , requestUpdateDistributionConfiguration $
--             newUpdateDistributionConfiguration
--
--         , requestListImagePipelineImages $
--             newListImagePipelineImages
--
--         , requestDeleteImageRecipe $
--             newDeleteImageRecipe
--
--         , requestListComponents $
--             newListComponents
--
--         , requestPutContainerRecipePolicy $
--             newPutContainerRecipePolicy
--
--         , requestListImages $
--             newListImages
--
--         , requestCreateDistributionConfiguration $
--             newCreateDistributionConfiguration
--
--         , requestListImageRecipes $
--             newListImageRecipes
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestDeleteComponent $
--             newDeleteComponent
--
--         , requestGetImage $
--             newGetImage
--
--         , requestGetContainerRecipePolicy $
--             newGetContainerRecipePolicy
--
--         , requestGetImageRecipe $
--             newGetImageRecipe
--
--         , requestGetComponent $
--             newGetComponent
--
--           ]

--     , testGroup "response"
--         [ responseListImagePackages $
--             newListImagePackagesResponse
--
--         , responseGetDistributionConfiguration $
--             newGetDistributionConfigurationResponse
--
--         , responseImportComponent $
--             newImportComponentResponse
--
--         , responseListComponentBuildVersions $
--             newListComponentBuildVersionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListImageBuildVersions $
--             newListImageBuildVersionsResponse
--
--         , responseCancelImageCreation $
--             newCancelImageCreationResponse
--
--         , responseGetImagePipeline $
--             newGetImagePipelineResponse
--
--         , responseCreateInfrastructureConfiguration $
--             newCreateInfrastructureConfigurationResponse
--
--         , responseListDistributionConfigurations $
--             newListDistributionConfigurationsResponse
--
--         , responseCreateContainerRecipe $
--             newCreateContainerRecipeResponse
--
--         , responseListInfrastructureConfigurations $
--             newListInfrastructureConfigurationsResponse
--
--         , responsePutImageRecipePolicy $
--             newPutImageRecipePolicyResponse
--
--         , responseListContainerRecipes $
--             newListContainerRecipesResponse
--
--         , responsePutImagePolicy $
--             newPutImagePolicyResponse
--
--         , responseDeleteContainerRecipe $
--             newDeleteContainerRecipeResponse
--
--         , responsePutComponentPolicy $
--             newPutComponentPolicyResponse
--
--         , responseDeleteInfrastructureConfiguration $
--             newDeleteInfrastructureConfigurationResponse
--
--         , responseUpdateInfrastructureConfiguration $
--             newUpdateInfrastructureConfigurationResponse
--
--         , responseCreateImagePipeline $
--             newCreateImagePipelineResponse
--
--         , responseGetContainerRecipe $
--             newGetContainerRecipeResponse
--
--         , responseGetInfrastructureConfiguration $
--             newGetInfrastructureConfigurationResponse
--
--         , responseGetImagePolicy $
--             newGetImagePolicyResponse
--
--         , responseGetImageRecipePolicy $
--             newGetImageRecipePolicyResponse
--
--         , responseGetComponentPolicy $
--             newGetComponentPolicyResponse
--
--         , responseDeleteImagePipeline $
--             newDeleteImagePipelineResponse
--
--         , responseUpdateImagePipeline $
--             newUpdateImagePipelineResponse
--
--         , responseListImagePipelines $
--             newListImagePipelinesResponse
--
--         , responseStartImagePipelineExecution $
--             newStartImagePipelineExecutionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateImageRecipe $
--             newCreateImageRecipeResponse
--
--         , responseCreateImage $
--             newCreateImageResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateComponent $
--             newCreateComponentResponse
--
--         , responseDeleteDistributionConfiguration $
--             newDeleteDistributionConfigurationResponse
--
--         , responseUpdateDistributionConfiguration $
--             newUpdateDistributionConfigurationResponse
--
--         , responseListImagePipelineImages $
--             newListImagePipelineImagesResponse
--
--         , responseDeleteImageRecipe $
--             newDeleteImageRecipeResponse
--
--         , responseListComponents $
--             newListComponentsResponse
--
--         , responsePutContainerRecipePolicy $
--             newPutContainerRecipePolicyResponse
--
--         , responseListImages $
--             newListImagesResponse
--
--         , responseCreateDistributionConfiguration $
--             newCreateDistributionConfigurationResponse
--
--         , responseListImageRecipes $
--             newListImageRecipesResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseDeleteComponent $
--             newDeleteComponentResponse
--
--         , responseGetImage $
--             newGetImageResponse
--
--         , responseGetContainerRecipePolicy $
--             newGetContainerRecipePolicyResponse
--
--         , responseGetImageRecipe $
--             newGetImageRecipeResponse
--
--         , responseGetComponent $
--             newGetComponentResponse
--
--           ]
--     ]

-- Requests

requestListImagePackages :: ListImagePackages -> TestTree
requestListImagePackages =
  req
    "ListImagePackages"
    "fixture/ListImagePackages.yaml"

requestGetDistributionConfiguration :: GetDistributionConfiguration -> TestTree
requestGetDistributionConfiguration =
  req
    "GetDistributionConfiguration"
    "fixture/GetDistributionConfiguration.yaml"

requestImportComponent :: ImportComponent -> TestTree
requestImportComponent =
  req
    "ImportComponent"
    "fixture/ImportComponent.yaml"

requestListComponentBuildVersions :: ListComponentBuildVersions -> TestTree
requestListComponentBuildVersions =
  req
    "ListComponentBuildVersions"
    "fixture/ListComponentBuildVersions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListImageBuildVersions :: ListImageBuildVersions -> TestTree
requestListImageBuildVersions =
  req
    "ListImageBuildVersions"
    "fixture/ListImageBuildVersions.yaml"

requestCancelImageCreation :: CancelImageCreation -> TestTree
requestCancelImageCreation =
  req
    "CancelImageCreation"
    "fixture/CancelImageCreation.yaml"

requestGetImagePipeline :: GetImagePipeline -> TestTree
requestGetImagePipeline =
  req
    "GetImagePipeline"
    "fixture/GetImagePipeline.yaml"

requestCreateInfrastructureConfiguration :: CreateInfrastructureConfiguration -> TestTree
requestCreateInfrastructureConfiguration =
  req
    "CreateInfrastructureConfiguration"
    "fixture/CreateInfrastructureConfiguration.yaml"

requestListDistributionConfigurations :: ListDistributionConfigurations -> TestTree
requestListDistributionConfigurations =
  req
    "ListDistributionConfigurations"
    "fixture/ListDistributionConfigurations.yaml"

requestCreateContainerRecipe :: CreateContainerRecipe -> TestTree
requestCreateContainerRecipe =
  req
    "CreateContainerRecipe"
    "fixture/CreateContainerRecipe.yaml"

requestListInfrastructureConfigurations :: ListInfrastructureConfigurations -> TestTree
requestListInfrastructureConfigurations =
  req
    "ListInfrastructureConfigurations"
    "fixture/ListInfrastructureConfigurations.yaml"

requestPutImageRecipePolicy :: PutImageRecipePolicy -> TestTree
requestPutImageRecipePolicy =
  req
    "PutImageRecipePolicy"
    "fixture/PutImageRecipePolicy.yaml"

requestListContainerRecipes :: ListContainerRecipes -> TestTree
requestListContainerRecipes =
  req
    "ListContainerRecipes"
    "fixture/ListContainerRecipes.yaml"

requestPutImagePolicy :: PutImagePolicy -> TestTree
requestPutImagePolicy =
  req
    "PutImagePolicy"
    "fixture/PutImagePolicy.yaml"

requestDeleteContainerRecipe :: DeleteContainerRecipe -> TestTree
requestDeleteContainerRecipe =
  req
    "DeleteContainerRecipe"
    "fixture/DeleteContainerRecipe.yaml"

requestPutComponentPolicy :: PutComponentPolicy -> TestTree
requestPutComponentPolicy =
  req
    "PutComponentPolicy"
    "fixture/PutComponentPolicy.yaml"

requestDeleteInfrastructureConfiguration :: DeleteInfrastructureConfiguration -> TestTree
requestDeleteInfrastructureConfiguration =
  req
    "DeleteInfrastructureConfiguration"
    "fixture/DeleteInfrastructureConfiguration.yaml"

requestUpdateInfrastructureConfiguration :: UpdateInfrastructureConfiguration -> TestTree
requestUpdateInfrastructureConfiguration =
  req
    "UpdateInfrastructureConfiguration"
    "fixture/UpdateInfrastructureConfiguration.yaml"

requestCreateImagePipeline :: CreateImagePipeline -> TestTree
requestCreateImagePipeline =
  req
    "CreateImagePipeline"
    "fixture/CreateImagePipeline.yaml"

requestGetContainerRecipe :: GetContainerRecipe -> TestTree
requestGetContainerRecipe =
  req
    "GetContainerRecipe"
    "fixture/GetContainerRecipe.yaml"

requestGetInfrastructureConfiguration :: GetInfrastructureConfiguration -> TestTree
requestGetInfrastructureConfiguration =
  req
    "GetInfrastructureConfiguration"
    "fixture/GetInfrastructureConfiguration.yaml"

requestGetImagePolicy :: GetImagePolicy -> TestTree
requestGetImagePolicy =
  req
    "GetImagePolicy"
    "fixture/GetImagePolicy.yaml"

requestGetImageRecipePolicy :: GetImageRecipePolicy -> TestTree
requestGetImageRecipePolicy =
  req
    "GetImageRecipePolicy"
    "fixture/GetImageRecipePolicy.yaml"

requestGetComponentPolicy :: GetComponentPolicy -> TestTree
requestGetComponentPolicy =
  req
    "GetComponentPolicy"
    "fixture/GetComponentPolicy.yaml"

requestDeleteImagePipeline :: DeleteImagePipeline -> TestTree
requestDeleteImagePipeline =
  req
    "DeleteImagePipeline"
    "fixture/DeleteImagePipeline.yaml"

requestUpdateImagePipeline :: UpdateImagePipeline -> TestTree
requestUpdateImagePipeline =
  req
    "UpdateImagePipeline"
    "fixture/UpdateImagePipeline.yaml"

requestListImagePipelines :: ListImagePipelines -> TestTree
requestListImagePipelines =
  req
    "ListImagePipelines"
    "fixture/ListImagePipelines.yaml"

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

requestCreateImageRecipe :: CreateImageRecipe -> TestTree
requestCreateImageRecipe =
  req
    "CreateImageRecipe"
    "fixture/CreateImageRecipe.yaml"

requestCreateImage :: CreateImage -> TestTree
requestCreateImage =
  req
    "CreateImage"
    "fixture/CreateImage.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateComponent :: CreateComponent -> TestTree
requestCreateComponent =
  req
    "CreateComponent"
    "fixture/CreateComponent.yaml"

requestDeleteDistributionConfiguration :: DeleteDistributionConfiguration -> TestTree
requestDeleteDistributionConfiguration =
  req
    "DeleteDistributionConfiguration"
    "fixture/DeleteDistributionConfiguration.yaml"

requestUpdateDistributionConfiguration :: UpdateDistributionConfiguration -> TestTree
requestUpdateDistributionConfiguration =
  req
    "UpdateDistributionConfiguration"
    "fixture/UpdateDistributionConfiguration.yaml"

requestListImagePipelineImages :: ListImagePipelineImages -> TestTree
requestListImagePipelineImages =
  req
    "ListImagePipelineImages"
    "fixture/ListImagePipelineImages.yaml"

requestDeleteImageRecipe :: DeleteImageRecipe -> TestTree
requestDeleteImageRecipe =
  req
    "DeleteImageRecipe"
    "fixture/DeleteImageRecipe.yaml"

requestListComponents :: ListComponents -> TestTree
requestListComponents =
  req
    "ListComponents"
    "fixture/ListComponents.yaml"

requestPutContainerRecipePolicy :: PutContainerRecipePolicy -> TestTree
requestPutContainerRecipePolicy =
  req
    "PutContainerRecipePolicy"
    "fixture/PutContainerRecipePolicy.yaml"

requestListImages :: ListImages -> TestTree
requestListImages =
  req
    "ListImages"
    "fixture/ListImages.yaml"

requestCreateDistributionConfiguration :: CreateDistributionConfiguration -> TestTree
requestCreateDistributionConfiguration =
  req
    "CreateDistributionConfiguration"
    "fixture/CreateDistributionConfiguration.yaml"

requestListImageRecipes :: ListImageRecipes -> TestTree
requestListImageRecipes =
  req
    "ListImageRecipes"
    "fixture/ListImageRecipes.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestDeleteComponent :: DeleteComponent -> TestTree
requestDeleteComponent =
  req
    "DeleteComponent"
    "fixture/DeleteComponent.yaml"

requestGetImage :: GetImage -> TestTree
requestGetImage =
  req
    "GetImage"
    "fixture/GetImage.yaml"

requestGetContainerRecipePolicy :: GetContainerRecipePolicy -> TestTree
requestGetContainerRecipePolicy =
  req
    "GetContainerRecipePolicy"
    "fixture/GetContainerRecipePolicy.yaml"

requestGetImageRecipe :: GetImageRecipe -> TestTree
requestGetImageRecipe =
  req
    "GetImageRecipe"
    "fixture/GetImageRecipe.yaml"

requestGetComponent :: GetComponent -> TestTree
requestGetComponent =
  req
    "GetComponent"
    "fixture/GetComponent.yaml"

-- Responses

responseListImagePackages :: ListImagePackagesResponse -> TestTree
responseListImagePackages =
  res
    "ListImagePackagesResponse"
    "fixture/ListImagePackagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImagePackages)

responseGetDistributionConfiguration :: GetDistributionConfigurationResponse -> TestTree
responseGetDistributionConfiguration =
  res
    "GetDistributionConfigurationResponse"
    "fixture/GetDistributionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionConfiguration)

responseImportComponent :: ImportComponentResponse -> TestTree
responseImportComponent =
  res
    "ImportComponentResponse"
    "fixture/ImportComponentResponse.proto"
    defaultService
    (Proxy :: Proxy ImportComponent)

responseListComponentBuildVersions :: ListComponentBuildVersionsResponse -> TestTree
responseListComponentBuildVersions =
  res
    "ListComponentBuildVersionsResponse"
    "fixture/ListComponentBuildVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListComponentBuildVersions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListImageBuildVersions :: ListImageBuildVersionsResponse -> TestTree
responseListImageBuildVersions =
  res
    "ListImageBuildVersionsResponse"
    "fixture/ListImageBuildVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListImageBuildVersions)

responseCancelImageCreation :: CancelImageCreationResponse -> TestTree
responseCancelImageCreation =
  res
    "CancelImageCreationResponse"
    "fixture/CancelImageCreationResponse.proto"
    defaultService
    (Proxy :: Proxy CancelImageCreation)

responseGetImagePipeline :: GetImagePipelineResponse -> TestTree
responseGetImagePipeline =
  res
    "GetImagePipelineResponse"
    "fixture/GetImagePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy GetImagePipeline)

responseCreateInfrastructureConfiguration :: CreateInfrastructureConfigurationResponse -> TestTree
responseCreateInfrastructureConfiguration =
  res
    "CreateInfrastructureConfigurationResponse"
    "fixture/CreateInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInfrastructureConfiguration)

responseListDistributionConfigurations :: ListDistributionConfigurationsResponse -> TestTree
responseListDistributionConfigurations =
  res
    "ListDistributionConfigurationsResponse"
    "fixture/ListDistributionConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDistributionConfigurations)

responseCreateContainerRecipe :: CreateContainerRecipeResponse -> TestTree
responseCreateContainerRecipe =
  res
    "CreateContainerRecipeResponse"
    "fixture/CreateContainerRecipeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainerRecipe)

responseListInfrastructureConfigurations :: ListInfrastructureConfigurationsResponse -> TestTree
responseListInfrastructureConfigurations =
  res
    "ListInfrastructureConfigurationsResponse"
    "fixture/ListInfrastructureConfigurationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListInfrastructureConfigurations)

responsePutImageRecipePolicy :: PutImageRecipePolicyResponse -> TestTree
responsePutImageRecipePolicy =
  res
    "PutImageRecipePolicyResponse"
    "fixture/PutImageRecipePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutImageRecipePolicy)

responseListContainerRecipes :: ListContainerRecipesResponse -> TestTree
responseListContainerRecipes =
  res
    "ListContainerRecipesResponse"
    "fixture/ListContainerRecipesResponse.proto"
    defaultService
    (Proxy :: Proxy ListContainerRecipes)

responsePutImagePolicy :: PutImagePolicyResponse -> TestTree
responsePutImagePolicy =
  res
    "PutImagePolicyResponse"
    "fixture/PutImagePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutImagePolicy)

responseDeleteContainerRecipe :: DeleteContainerRecipeResponse -> TestTree
responseDeleteContainerRecipe =
  res
    "DeleteContainerRecipeResponse"
    "fixture/DeleteContainerRecipeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainerRecipe)

responsePutComponentPolicy :: PutComponentPolicyResponse -> TestTree
responsePutComponentPolicy =
  res
    "PutComponentPolicyResponse"
    "fixture/PutComponentPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutComponentPolicy)

responseDeleteInfrastructureConfiguration :: DeleteInfrastructureConfigurationResponse -> TestTree
responseDeleteInfrastructureConfiguration =
  res
    "DeleteInfrastructureConfigurationResponse"
    "fixture/DeleteInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInfrastructureConfiguration)

responseUpdateInfrastructureConfiguration :: UpdateInfrastructureConfigurationResponse -> TestTree
responseUpdateInfrastructureConfiguration =
  res
    "UpdateInfrastructureConfigurationResponse"
    "fixture/UpdateInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateInfrastructureConfiguration)

responseCreateImagePipeline :: CreateImagePipelineResponse -> TestTree
responseCreateImagePipeline =
  res
    "CreateImagePipelineResponse"
    "fixture/CreateImagePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImagePipeline)

responseGetContainerRecipe :: GetContainerRecipeResponse -> TestTree
responseGetContainerRecipe =
  res
    "GetContainerRecipeResponse"
    "fixture/GetContainerRecipeResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerRecipe)

responseGetInfrastructureConfiguration :: GetInfrastructureConfigurationResponse -> TestTree
responseGetInfrastructureConfiguration =
  res
    "GetInfrastructureConfigurationResponse"
    "fixture/GetInfrastructureConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetInfrastructureConfiguration)

responseGetImagePolicy :: GetImagePolicyResponse -> TestTree
responseGetImagePolicy =
  res
    "GetImagePolicyResponse"
    "fixture/GetImagePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetImagePolicy)

responseGetImageRecipePolicy :: GetImageRecipePolicyResponse -> TestTree
responseGetImageRecipePolicy =
  res
    "GetImageRecipePolicyResponse"
    "fixture/GetImageRecipePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetImageRecipePolicy)

responseGetComponentPolicy :: GetComponentPolicyResponse -> TestTree
responseGetComponentPolicy =
  res
    "GetComponentPolicyResponse"
    "fixture/GetComponentPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetComponentPolicy)

responseDeleteImagePipeline :: DeleteImagePipelineResponse -> TestTree
responseDeleteImagePipeline =
  res
    "DeleteImagePipelineResponse"
    "fixture/DeleteImagePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImagePipeline)

responseUpdateImagePipeline :: UpdateImagePipelineResponse -> TestTree
responseUpdateImagePipeline =
  res
    "UpdateImagePipelineResponse"
    "fixture/UpdateImagePipelineResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateImagePipeline)

responseListImagePipelines :: ListImagePipelinesResponse -> TestTree
responseListImagePipelines =
  res
    "ListImagePipelinesResponse"
    "fixture/ListImagePipelinesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImagePipelines)

responseStartImagePipelineExecution :: StartImagePipelineExecutionResponse -> TestTree
responseStartImagePipelineExecution =
  res
    "StartImagePipelineExecutionResponse"
    "fixture/StartImagePipelineExecutionResponse.proto"
    defaultService
    (Proxy :: Proxy StartImagePipelineExecution)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateImageRecipe :: CreateImageRecipeResponse -> TestTree
responseCreateImageRecipe =
  res
    "CreateImageRecipeResponse"
    "fixture/CreateImageRecipeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageRecipe)

responseCreateImage :: CreateImageResponse -> TestTree
responseCreateImage =
  res
    "CreateImageResponse"
    "fixture/CreateImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImage)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateComponent :: CreateComponentResponse -> TestTree
responseCreateComponent =
  res
    "CreateComponentResponse"
    "fixture/CreateComponentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateComponent)

responseDeleteDistributionConfiguration :: DeleteDistributionConfigurationResponse -> TestTree
responseDeleteDistributionConfiguration =
  res
    "DeleteDistributionConfigurationResponse"
    "fixture/DeleteDistributionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDistributionConfiguration)

responseUpdateDistributionConfiguration :: UpdateDistributionConfigurationResponse -> TestTree
responseUpdateDistributionConfiguration =
  res
    "UpdateDistributionConfigurationResponse"
    "fixture/UpdateDistributionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDistributionConfiguration)

responseListImagePipelineImages :: ListImagePipelineImagesResponse -> TestTree
responseListImagePipelineImages =
  res
    "ListImagePipelineImagesResponse"
    "fixture/ListImagePipelineImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImagePipelineImages)

responseDeleteImageRecipe :: DeleteImageRecipeResponse -> TestTree
responseDeleteImageRecipe =
  res
    "DeleteImageRecipeResponse"
    "fixture/DeleteImageRecipeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImageRecipe)

responseListComponents :: ListComponentsResponse -> TestTree
responseListComponents =
  res
    "ListComponentsResponse"
    "fixture/ListComponentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListComponents)

responsePutContainerRecipePolicy :: PutContainerRecipePolicyResponse -> TestTree
responsePutContainerRecipePolicy =
  res
    "PutContainerRecipePolicyResponse"
    "fixture/PutContainerRecipePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutContainerRecipePolicy)

responseListImages :: ListImagesResponse -> TestTree
responseListImages =
  res
    "ListImagesResponse"
    "fixture/ListImagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImages)

responseCreateDistributionConfiguration :: CreateDistributionConfigurationResponse -> TestTree
responseCreateDistributionConfiguration =
  res
    "CreateDistributionConfigurationResponse"
    "fixture/CreateDistributionConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDistributionConfiguration)

responseListImageRecipes :: ListImageRecipesResponse -> TestTree
responseListImageRecipes =
  res
    "ListImageRecipesResponse"
    "fixture/ListImageRecipesResponse.proto"
    defaultService
    (Proxy :: Proxy ListImageRecipes)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImage)

responseDeleteComponent :: DeleteComponentResponse -> TestTree
responseDeleteComponent =
  res
    "DeleteComponentResponse"
    "fixture/DeleteComponentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteComponent)

responseGetImage :: GetImageResponse -> TestTree
responseGetImage =
  res
    "GetImageResponse"
    "fixture/GetImageResponse.proto"
    defaultService
    (Proxy :: Proxy GetImage)

responseGetContainerRecipePolicy :: GetContainerRecipePolicyResponse -> TestTree
responseGetContainerRecipePolicy =
  res
    "GetContainerRecipePolicyResponse"
    "fixture/GetContainerRecipePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerRecipePolicy)

responseGetImageRecipe :: GetImageRecipeResponse -> TestTree
responseGetImageRecipe =
  res
    "GetImageRecipeResponse"
    "fixture/GetImageRecipeResponse.proto"
    defaultService
    (Proxy :: Proxy GetImageRecipe)

responseGetComponent :: GetComponentResponse -> TestTree
responseGetComponent =
  res
    "GetComponentResponse"
    "fixture/GetComponentResponse.proto"
    defaultService
    (Proxy :: Proxy GetComponent)

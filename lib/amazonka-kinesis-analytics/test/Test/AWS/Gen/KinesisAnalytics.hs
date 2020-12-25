{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisAnalytics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.KinesisAnalytics where

import Data.Proxy
import Network.AWS.KinesisAnalytics
import Test.AWS.Fixture
import Test.AWS.KinesisAnalytics.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddApplicationOutput $
--             mkAddApplicationOutput
--
--         , requestDiscoverInputSchema $
--             mkDiscoverInputSchema
--
--         , requestDescribeApplication $
--             mkDescribeApplication
--
--         , requestStartApplication $
--             mkStartApplication
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDeleteApplicationReferenceDataSource $
--             mkDeleteApplicationReferenceDataSource
--
--         , requestDeleteApplication $
--             mkDeleteApplication
--
--         , requestUpdateApplication $
--             mkUpdateApplication
--
--         , requestDeleteApplicationCloudWatchLoggingOption $
--             mkDeleteApplicationCloudWatchLoggingOption
--
--         , requestAddApplicationInputProcessingConfiguration $
--             mkAddApplicationInputProcessingConfiguration
--
--         , requestCreateApplication $
--             mkCreateApplication
--
--         , requestDeleteApplicationOutput $
--             mkDeleteApplicationOutput
--
--         , requestStopApplication $
--             mkStopApplication
--
--         , requestAddApplicationReferenceDataSource $
--             mkAddApplicationReferenceDataSource
--
--         , requestAddApplicationInput $
--             mkAddApplicationInput
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestAddApplicationCloudWatchLoggingOption $
--             mkAddApplicationCloudWatchLoggingOption
--
--         , requestListApplications $
--             mkListApplications
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteApplicationInputProcessingConfiguration $
--             mkDeleteApplicationInputProcessingConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAddApplicationOutput $
--             mkAddApplicationOutputResponse
--
--         , responseDiscoverInputSchema $
--             mkDiscoverInputSchemaResponse
--
--         , responseDescribeApplication $
--             mkDescribeApplicationResponse
--
--         , responseStartApplication $
--             mkStartApplicationResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDeleteApplicationReferenceDataSource $
--             mkDeleteApplicationReferenceDataSourceResponse
--
--         , responseDeleteApplication $
--             mkDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             mkUpdateApplicationResponse
--
--         , responseDeleteApplicationCloudWatchLoggingOption $
--             mkDeleteApplicationCloudWatchLoggingOptionResponse
--
--         , responseAddApplicationInputProcessingConfiguration $
--             mkAddApplicationInputProcessingConfigurationResponse
--
--         , responseCreateApplication $
--             mkCreateApplicationResponse
--
--         , responseDeleteApplicationOutput $
--             mkDeleteApplicationOutputResponse
--
--         , responseStopApplication $
--             mkStopApplicationResponse
--
--         , responseAddApplicationReferenceDataSource $
--             mkAddApplicationReferenceDataSourceResponse
--
--         , responseAddApplicationInput $
--             mkAddApplicationInputResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseAddApplicationCloudWatchLoggingOption $
--             mkAddApplicationCloudWatchLoggingOptionResponse
--
--         , responseListApplications $
--             mkListApplicationsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteApplicationInputProcessingConfiguration $
--             mkDeleteApplicationInputProcessingConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAddApplicationOutput :: AddApplicationOutput -> TestTree
requestAddApplicationOutput =
  req
    "AddApplicationOutput"
    "fixture/AddApplicationOutput.yaml"

requestDiscoverInputSchema :: DiscoverInputSchema -> TestTree
requestDiscoverInputSchema =
  req
    "DiscoverInputSchema"
    "fixture/DiscoverInputSchema.yaml"

requestDescribeApplication :: DescribeApplication -> TestTree
requestDescribeApplication =
  req
    "DescribeApplication"
    "fixture/DescribeApplication.yaml"

requestStartApplication :: StartApplication -> TestTree
requestStartApplication =
  req
    "StartApplication"
    "fixture/StartApplication.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSource -> TestTree
requestDeleteApplicationReferenceDataSource =
  req
    "DeleteApplicationReferenceDataSource"
    "fixture/DeleteApplicationReferenceDataSource.yaml"

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

requestDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOption -> TestTree
requestDeleteApplicationCloudWatchLoggingOption =
  req
    "DeleteApplicationCloudWatchLoggingOption"
    "fixture/DeleteApplicationCloudWatchLoggingOption.yaml"

requestAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfiguration -> TestTree
requestAddApplicationInputProcessingConfiguration =
  req
    "AddApplicationInputProcessingConfiguration"
    "fixture/AddApplicationInputProcessingConfiguration.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestDeleteApplicationOutput :: DeleteApplicationOutput -> TestTree
requestDeleteApplicationOutput =
  req
    "DeleteApplicationOutput"
    "fixture/DeleteApplicationOutput.yaml"

requestStopApplication :: StopApplication -> TestTree
requestStopApplication =
  req
    "StopApplication"
    "fixture/StopApplication.yaml"

requestAddApplicationReferenceDataSource :: AddApplicationReferenceDataSource -> TestTree
requestAddApplicationReferenceDataSource =
  req
    "AddApplicationReferenceDataSource"
    "fixture/AddApplicationReferenceDataSource.yaml"

requestAddApplicationInput :: AddApplicationInput -> TestTree
requestAddApplicationInput =
  req
    "AddApplicationInput"
    "fixture/AddApplicationInput.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOption -> TestTree
requestAddApplicationCloudWatchLoggingOption =
  req
    "AddApplicationCloudWatchLoggingOption"
    "fixture/AddApplicationCloudWatchLoggingOption.yaml"

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

requestDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfiguration -> TestTree
requestDeleteApplicationInputProcessingConfiguration =
  req
    "DeleteApplicationInputProcessingConfiguration"
    "fixture/DeleteApplicationInputProcessingConfiguration.yaml"

-- Responses

responseAddApplicationOutput :: AddApplicationOutputResponse -> TestTree
responseAddApplicationOutput =
  res
    "AddApplicationOutputResponse"
    "fixture/AddApplicationOutputResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddApplicationOutput)

responseDiscoverInputSchema :: DiscoverInputSchemaResponse -> TestTree
responseDiscoverInputSchema =
  res
    "DiscoverInputSchemaResponse"
    "fixture/DiscoverInputSchemaResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DiscoverInputSchema)

responseDescribeApplication :: DescribeApplicationResponse -> TestTree
responseDescribeApplication =
  res
    "DescribeApplicationResponse"
    "fixture/DescribeApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeApplication)

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication =
  res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartApplication)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSourceResponse -> TestTree
responseDeleteApplicationReferenceDataSource =
  res
    "DeleteApplicationReferenceDataSourceResponse"
    "fixture/DeleteApplicationReferenceDataSourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApplicationReferenceDataSource)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApplication)

responseDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOptionResponse -> TestTree
responseDeleteApplicationCloudWatchLoggingOption =
  res
    "DeleteApplicationCloudWatchLoggingOptionResponse"
    "fixture/DeleteApplicationCloudWatchLoggingOptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApplicationCloudWatchLoggingOption)

responseAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfigurationResponse -> TestTree
responseAddApplicationInputProcessingConfiguration =
  res
    "AddApplicationInputProcessingConfigurationResponse"
    "fixture/AddApplicationInputProcessingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddApplicationInputProcessingConfiguration)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApplication)

responseDeleteApplicationOutput :: DeleteApplicationOutputResponse -> TestTree
responseDeleteApplicationOutput =
  res
    "DeleteApplicationOutputResponse"
    "fixture/DeleteApplicationOutputResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApplicationOutput)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication =
  res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopApplication)

responseAddApplicationReferenceDataSource :: AddApplicationReferenceDataSourceResponse -> TestTree
responseAddApplicationReferenceDataSource =
  res
    "AddApplicationReferenceDataSourceResponse"
    "fixture/AddApplicationReferenceDataSourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddApplicationReferenceDataSource)

responseAddApplicationInput :: AddApplicationInputResponse -> TestTree
responseAddApplicationInput =
  res
    "AddApplicationInputResponse"
    "fixture/AddApplicationInputResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddApplicationInput)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOptionResponse -> TestTree
responseAddApplicationCloudWatchLoggingOption =
  res
    "AddApplicationCloudWatchLoggingOptionResponse"
    "fixture/AddApplicationCloudWatchLoggingOptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddApplicationCloudWatchLoggingOption)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListApplications)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfigurationResponse -> TestTree
responseDeleteApplicationInputProcessingConfiguration =
  res
    "DeleteApplicationInputProcessingConfigurationResponse"
    "fixture/DeleteApplicationInputProcessingConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApplicationInputProcessingConfiguration)

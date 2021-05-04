{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisAnalytics
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDeleteApplicationOutput $
--             newDeleteApplicationOutput
--
--         , requestAddApplicationOutput $
--             newAddApplicationOutput
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestDeleteApplicationCloudWatchLoggingOption $
--             newDeleteApplicationCloudWatchLoggingOption
--
--         , requestAddApplicationCloudWatchLoggingOption $
--             newAddApplicationCloudWatchLoggingOption
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStopApplication $
--             newStopApplication
--
--         , requestStartApplication $
--             newStartApplication
--
--         , requestDescribeApplication $
--             newDescribeApplication
--
--         , requestDiscoverInputSchema $
--             newDiscoverInputSchema
--
--         , requestAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfiguration
--
--         , requestDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfiguration
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestAddApplicationInput $
--             newAddApplicationInput
--
--         , requestAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSource
--
--         , requestDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSource
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseDeleteApplicationOutput $
--             newDeleteApplicationOutputResponse
--
--         , responseAddApplicationOutput $
--             newAddApplicationOutputResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseDeleteApplicationCloudWatchLoggingOption $
--             newDeleteApplicationCloudWatchLoggingOptionResponse
--
--         , responseAddApplicationCloudWatchLoggingOption $
--             newAddApplicationCloudWatchLoggingOptionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStopApplication $
--             newStopApplicationResponse
--
--         , responseStartApplication $
--             newStartApplicationResponse
--
--         , responseDescribeApplication $
--             newDescribeApplicationResponse
--
--         , responseDiscoverInputSchema $
--             newDiscoverInputSchemaResponse
--
--         , responseAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfigurationResponse
--
--         , responseDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfigurationResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseAddApplicationInput $
--             newAddApplicationInputResponse
--
--         , responseAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSourceResponse
--
--         , responseDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSourceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestDeleteApplicationOutput :: DeleteApplicationOutput -> TestTree
requestDeleteApplicationOutput =
  req
    "DeleteApplicationOutput"
    "fixture/DeleteApplicationOutput.yaml"

requestAddApplicationOutput :: AddApplicationOutput -> TestTree
requestAddApplicationOutput =
  req
    "AddApplicationOutput"
    "fixture/AddApplicationOutput.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOption -> TestTree
requestDeleteApplicationCloudWatchLoggingOption =
  req
    "DeleteApplicationCloudWatchLoggingOption"
    "fixture/DeleteApplicationCloudWatchLoggingOption.yaml"

requestAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOption -> TestTree
requestAddApplicationCloudWatchLoggingOption =
  req
    "AddApplicationCloudWatchLoggingOption"
    "fixture/AddApplicationCloudWatchLoggingOption.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestStopApplication :: StopApplication -> TestTree
requestStopApplication =
  req
    "StopApplication"
    "fixture/StopApplication.yaml"

requestStartApplication :: StartApplication -> TestTree
requestStartApplication =
  req
    "StartApplication"
    "fixture/StartApplication.yaml"

requestDescribeApplication :: DescribeApplication -> TestTree
requestDescribeApplication =
  req
    "DescribeApplication"
    "fixture/DescribeApplication.yaml"

requestDiscoverInputSchema :: DiscoverInputSchema -> TestTree
requestDiscoverInputSchema =
  req
    "DiscoverInputSchema"
    "fixture/DiscoverInputSchema.yaml"

requestAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfiguration -> TestTree
requestAddApplicationInputProcessingConfiguration =
  req
    "AddApplicationInputProcessingConfiguration"
    "fixture/AddApplicationInputProcessingConfiguration.yaml"

requestDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfiguration -> TestTree
requestDeleteApplicationInputProcessingConfiguration =
  req
    "DeleteApplicationInputProcessingConfiguration"
    "fixture/DeleteApplicationInputProcessingConfiguration.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestAddApplicationInput :: AddApplicationInput -> TestTree
requestAddApplicationInput =
  req
    "AddApplicationInput"
    "fixture/AddApplicationInput.yaml"

requestAddApplicationReferenceDataSource :: AddApplicationReferenceDataSource -> TestTree
requestAddApplicationReferenceDataSource =
  req
    "AddApplicationReferenceDataSource"
    "fixture/AddApplicationReferenceDataSource.yaml"

requestDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSource -> TestTree
requestDeleteApplicationReferenceDataSource =
  req
    "DeleteApplicationReferenceDataSource"
    "fixture/DeleteApplicationReferenceDataSource.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseDeleteApplicationOutput :: DeleteApplicationOutputResponse -> TestTree
responseDeleteApplicationOutput =
  res
    "DeleteApplicationOutputResponse"
    "fixture/DeleteApplicationOutputResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationOutput)

responseAddApplicationOutput :: AddApplicationOutputResponse -> TestTree
responseAddApplicationOutput =
  res
    "AddApplicationOutputResponse"
    "fixture/AddApplicationOutputResponse.proto"
    defaultService
    (Proxy :: Proxy AddApplicationOutput)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplication)

responseDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOptionResponse -> TestTree
responseDeleteApplicationCloudWatchLoggingOption =
  res
    "DeleteApplicationCloudWatchLoggingOptionResponse"
    "fixture/DeleteApplicationCloudWatchLoggingOptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationCloudWatchLoggingOption)

responseAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOptionResponse -> TestTree
responseAddApplicationCloudWatchLoggingOption =
  res
    "AddApplicationCloudWatchLoggingOptionResponse"
    "fixture/AddApplicationCloudWatchLoggingOptionResponse.proto"
    defaultService
    (Proxy :: Proxy AddApplicationCloudWatchLoggingOption)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication =
  res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StopApplication)

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication =
  res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StartApplication)

responseDescribeApplication :: DescribeApplicationResponse -> TestTree
responseDescribeApplication =
  res
    "DescribeApplicationResponse"
    "fixture/DescribeApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplication)

responseDiscoverInputSchema :: DiscoverInputSchemaResponse -> TestTree
responseDiscoverInputSchema =
  res
    "DiscoverInputSchemaResponse"
    "fixture/DiscoverInputSchemaResponse.proto"
    defaultService
    (Proxy :: Proxy DiscoverInputSchema)

responseAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfigurationResponse -> TestTree
responseAddApplicationInputProcessingConfiguration =
  res
    "AddApplicationInputProcessingConfigurationResponse"
    "fixture/AddApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy AddApplicationInputProcessingConfiguration)

responseDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfigurationResponse -> TestTree
responseDeleteApplicationInputProcessingConfiguration =
  res
    "DeleteApplicationInputProcessingConfigurationResponse"
    "fixture/DeleteApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationInputProcessingConfiguration)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplication)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplications)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)

responseAddApplicationInput :: AddApplicationInputResponse -> TestTree
responseAddApplicationInput =
  res
    "AddApplicationInputResponse"
    "fixture/AddApplicationInputResponse.proto"
    defaultService
    (Proxy :: Proxy AddApplicationInput)

responseAddApplicationReferenceDataSource :: AddApplicationReferenceDataSourceResponse -> TestTree
responseAddApplicationReferenceDataSource =
  res
    "AddApplicationReferenceDataSourceResponse"
    "fixture/AddApplicationReferenceDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy AddApplicationReferenceDataSource)

responseDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSourceResponse -> TestTree
responseDeleteApplicationReferenceDataSource =
  res
    "DeleteApplicationReferenceDataSourceResponse"
    "fixture/DeleteApplicationReferenceDataSourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationReferenceDataSource)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

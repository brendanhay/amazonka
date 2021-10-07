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
--         , requestStartApplication $
--             newStartApplication
--
--         , requestStopApplication $
--             newStopApplication
--
--         , requestDescribeApplication $
--             newDescribeApplication
--
--         , requestDiscoverInputSchema $
--             newDiscoverInputSchema
--
--         , requestDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfiguration
--
--         , requestAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfiguration
--
--         , requestListApplications $
--             newListApplications
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSource
--
--         , requestDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSource
--
--         , requestAddApplicationInput $
--             newAddApplicationInput
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
--         , responseStartApplication $
--             newStartApplicationResponse
--
--         , responseStopApplication $
--             newStopApplicationResponse
--
--         , responseDescribeApplication $
--             newDescribeApplicationResponse
--
--         , responseDiscoverInputSchema $
--             newDiscoverInputSchemaResponse
--
--         , responseDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfigurationResponse
--
--         , responseAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfigurationResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSourceResponse
--
--         , responseDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSourceResponse
--
--         , responseAddApplicationInput $
--             newAddApplicationInputResponse
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

requestStartApplication :: StartApplication -> TestTree
requestStartApplication =
  req
    "StartApplication"
    "fixture/StartApplication.yaml"

requestStopApplication :: StopApplication -> TestTree
requestStopApplication =
  req
    "StopApplication"
    "fixture/StopApplication.yaml"

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

requestDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfiguration -> TestTree
requestDeleteApplicationInputProcessingConfiguration =
  req
    "DeleteApplicationInputProcessingConfiguration"
    "fixture/DeleteApplicationInputProcessingConfiguration.yaml"

requestAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfiguration -> TestTree
requestAddApplicationInputProcessingConfiguration =
  req
    "AddApplicationInputProcessingConfiguration"
    "fixture/AddApplicationInputProcessingConfiguration.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

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

requestAddApplicationInput :: AddApplicationInput -> TestTree
requestAddApplicationInput =
  req
    "AddApplicationInput"
    "fixture/AddApplicationInput.yaml"

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

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication =
  res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StartApplication)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication =
  res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy StopApplication)

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

responseDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfigurationResponse -> TestTree
responseDeleteApplicationInputProcessingConfiguration =
  res
    "DeleteApplicationInputProcessingConfigurationResponse"
    "fixture/DeleteApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationInputProcessingConfiguration)

responseAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfigurationResponse -> TestTree
responseAddApplicationInputProcessingConfiguration =
  res
    "AddApplicationInputProcessingConfigurationResponse"
    "fixture/AddApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy AddApplicationInputProcessingConfiguration)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplications)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)

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

responseAddApplicationInput :: AddApplicationInputResponse -> TestTree
responseAddApplicationInput =
  res
    "AddApplicationInputResponse"
    "fixture/AddApplicationInputResponse.proto"
    defaultService
    (Proxy :: Proxy AddApplicationInput)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

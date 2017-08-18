{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisAnalytics
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.KinesisAnalytics where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.KinesisAnalytics
import Test.AWS.KinesisAnalytics.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddApplicationOutput $
--             addApplicationOutput
--
--         , requestDiscoverInputSchema $
--             discoverInputSchema
--
--         , requestDescribeApplication $
--             describeApplication
--
--         , requestStartApplication $
--             startApplication
--
--         , requestDeleteApplicationReferenceDataSource $
--             deleteApplicationReferenceDataSource
--
--         , requestDeleteApplication $
--             deleteApplication
--
--         , requestUpdateApplication $
--             updateApplication
--
--         , requestDeleteApplicationCloudWatchLoggingOption $
--             deleteApplicationCloudWatchLoggingOption
--
--         , requestCreateApplication $
--             createApplication
--
--         , requestDeleteApplicationOutput $
--             deleteApplicationOutput
--
--         , requestStopApplication $
--             stopApplication
--
--         , requestAddApplicationReferenceDataSource $
--             addApplicationReferenceDataSource
--
--         , requestAddApplicationInput $
--             addApplicationInput
--
--         , requestAddApplicationCloudWatchLoggingOption $
--             addApplicationCloudWatchLoggingOption
--
--         , requestListApplications $
--             listApplications
--
--           ]

--     , testGroup "response"
--         [ responseAddApplicationOutput $
--             addApplicationOutputResponse
--
--         , responseDiscoverInputSchema $
--             discoverInputSchemaResponse
--
--         , responseDescribeApplication $
--             describeApplicationResponse
--
--         , responseStartApplication $
--             startApplicationResponse
--
--         , responseDeleteApplicationReferenceDataSource $
--             deleteApplicationReferenceDataSourceResponse
--
--         , responseDeleteApplication $
--             deleteApplicationResponse
--
--         , responseUpdateApplication $
--             updateApplicationResponse
--
--         , responseDeleteApplicationCloudWatchLoggingOption $
--             deleteApplicationCloudWatchLoggingOptionResponse
--
--         , responseCreateApplication $
--             createApplicationResponse
--
--         , responseDeleteApplicationOutput $
--             deleteApplicationOutputResponse
--
--         , responseStopApplication $
--             stopApplicationResponse
--
--         , responseAddApplicationReferenceDataSource $
--             addApplicationReferenceDataSourceResponse
--
--         , responseAddApplicationInput $
--             addApplicationInputResponse
--
--         , responseAddApplicationCloudWatchLoggingOption $
--             addApplicationCloudWatchLoggingOptionResponse
--
--         , responseListApplications $
--             listApplicationsResponse
--
--           ]
--     ]

-- Requests

requestAddApplicationOutput :: AddApplicationOutput -> TestTree
requestAddApplicationOutput = req
    "AddApplicationOutput"
    "fixture/AddApplicationOutput.yaml"

requestDiscoverInputSchema :: DiscoverInputSchema -> TestTree
requestDiscoverInputSchema = req
    "DiscoverInputSchema"
    "fixture/DiscoverInputSchema.yaml"

requestDescribeApplication :: DescribeApplication -> TestTree
requestDescribeApplication = req
    "DescribeApplication"
    "fixture/DescribeApplication.yaml"

requestStartApplication :: StartApplication -> TestTree
requestStartApplication = req
    "StartApplication"
    "fixture/StartApplication.yaml"

requestDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSource -> TestTree
requestDeleteApplicationReferenceDataSource = req
    "DeleteApplicationReferenceDataSource"
    "fixture/DeleteApplicationReferenceDataSource.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOption -> TestTree
requestDeleteApplicationCloudWatchLoggingOption = req
    "DeleteApplicationCloudWatchLoggingOption"
    "fixture/DeleteApplicationCloudWatchLoggingOption.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestDeleteApplicationOutput :: DeleteApplicationOutput -> TestTree
requestDeleteApplicationOutput = req
    "DeleteApplicationOutput"
    "fixture/DeleteApplicationOutput.yaml"

requestStopApplication :: StopApplication -> TestTree
requestStopApplication = req
    "StopApplication"
    "fixture/StopApplication.yaml"

requestAddApplicationReferenceDataSource :: AddApplicationReferenceDataSource -> TestTree
requestAddApplicationReferenceDataSource = req
    "AddApplicationReferenceDataSource"
    "fixture/AddApplicationReferenceDataSource.yaml"

requestAddApplicationInput :: AddApplicationInput -> TestTree
requestAddApplicationInput = req
    "AddApplicationInput"
    "fixture/AddApplicationInput.yaml"

requestAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOption -> TestTree
requestAddApplicationCloudWatchLoggingOption = req
    "AddApplicationCloudWatchLoggingOption"
    "fixture/AddApplicationCloudWatchLoggingOption.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications = req
    "ListApplications"
    "fixture/ListApplications.yaml"

-- Responses

responseAddApplicationOutput :: AddApplicationOutputResponse -> TestTree
responseAddApplicationOutput = res
    "AddApplicationOutputResponse"
    "fixture/AddApplicationOutputResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy AddApplicationOutput)

responseDiscoverInputSchema :: DiscoverInputSchemaResponse -> TestTree
responseDiscoverInputSchema = res
    "DiscoverInputSchemaResponse"
    "fixture/DiscoverInputSchemaResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy DiscoverInputSchema)

responseDescribeApplication :: DescribeApplicationResponse -> TestTree
responseDescribeApplication = res
    "DescribeApplicationResponse"
    "fixture/DescribeApplicationResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy DescribeApplication)

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication = res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy StartApplication)

responseDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSourceResponse -> TestTree
responseDeleteApplicationReferenceDataSource = res
    "DeleteApplicationReferenceDataSourceResponse"
    "fixture/DeleteApplicationReferenceDataSourceResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy DeleteApplicationReferenceDataSource)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy UpdateApplication)

responseDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOptionResponse -> TestTree
responseDeleteApplicationCloudWatchLoggingOption = res
    "DeleteApplicationCloudWatchLoggingOptionResponse"
    "fixture/DeleteApplicationCloudWatchLoggingOptionResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy DeleteApplicationCloudWatchLoggingOption)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy CreateApplication)

responseDeleteApplicationOutput :: DeleteApplicationOutputResponse -> TestTree
responseDeleteApplicationOutput = res
    "DeleteApplicationOutputResponse"
    "fixture/DeleteApplicationOutputResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy DeleteApplicationOutput)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication = res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy StopApplication)

responseAddApplicationReferenceDataSource :: AddApplicationReferenceDataSourceResponse -> TestTree
responseAddApplicationReferenceDataSource = res
    "AddApplicationReferenceDataSourceResponse"
    "fixture/AddApplicationReferenceDataSourceResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy AddApplicationReferenceDataSource)

responseAddApplicationInput :: AddApplicationInputResponse -> TestTree
responseAddApplicationInput = res
    "AddApplicationInputResponse"
    "fixture/AddApplicationInputResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy AddApplicationInput)

responseAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOptionResponse -> TestTree
responseAddApplicationCloudWatchLoggingOption = res
    "AddApplicationCloudWatchLoggingOptionResponse"
    "fixture/AddApplicationCloudWatchLoggingOptionResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy AddApplicationCloudWatchLoggingOption)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications = res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    kinesisAnalytics
    (Proxy :: Proxy ListApplications)

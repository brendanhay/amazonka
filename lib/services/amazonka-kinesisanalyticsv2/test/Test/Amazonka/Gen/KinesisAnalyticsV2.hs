{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KinesisAnalyticsV2
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.KinesisAnalyticsV2 where

import Amazonka.KinesisAnalyticsV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.KinesisAnalyticsV2.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddApplicationOutput $
--             newAddApplicationOutput
--
--         , requestDiscoverInputSchema $
--             newDiscoverInputSchema
--
--         , requestDescribeApplication $
--             newDescribeApplication
--
--         , requestDeleteApplicationSnapshot $
--             newDeleteApplicationSnapshot
--
--         , requestStartApplication $
--             newStartApplication
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeApplicationVersion $
--             newDescribeApplicationVersion
--
--         , requestDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSource
--
--         , requestCreateApplicationPresignedUrl $
--             newCreateApplicationPresignedUrl
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestDeleteApplicationCloudWatchLoggingOption $
--             newDeleteApplicationCloudWatchLoggingOption
--
--         , requestDescribeApplicationSnapshot $
--             newDescribeApplicationSnapshot
--
--         , requestDeleteApplicationVpcConfiguration $
--             newDeleteApplicationVpcConfiguration
--
--         , requestAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfiguration
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestListApplicationVersions $
--             newListApplicationVersions
--
--         , requestDeleteApplicationOutput $
--             newDeleteApplicationOutput
--
--         , requestRollbackApplication $
--             newRollbackApplication
--
--         , requestListApplicationSnapshots $
--             newListApplicationSnapshots
--
--         , requestStopApplication $
--             newStopApplication
--
--         , requestCreateApplicationSnapshot $
--             newCreateApplicationSnapshot
--
--         , requestAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSource
--
--         , requestAddApplicationInput $
--             newAddApplicationInput
--
--         , requestTagResource $
--             newTagResource
--
--         , requestAddApplicationCloudWatchLoggingOption $
--             newAddApplicationCloudWatchLoggingOption
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfiguration
--
--         , requestAddApplicationVpcConfiguration $
--             newAddApplicationVpcConfiguration
--
--         , requestUpdateApplicationMaintenanceConfiguration $
--             newUpdateApplicationMaintenanceConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAddApplicationOutput $
--             newAddApplicationOutputResponse
--
--         , responseDiscoverInputSchema $
--             newDiscoverInputSchemaResponse
--
--         , responseDescribeApplication $
--             newDescribeApplicationResponse
--
--         , responseDeleteApplicationSnapshot $
--             newDeleteApplicationSnapshotResponse
--
--         , responseStartApplication $
--             newStartApplicationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeApplicationVersion $
--             newDescribeApplicationVersionResponse
--
--         , responseDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSourceResponse
--
--         , responseCreateApplicationPresignedUrl $
--             newCreateApplicationPresignedUrlResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseDeleteApplicationCloudWatchLoggingOption $
--             newDeleteApplicationCloudWatchLoggingOptionResponse
--
--         , responseDescribeApplicationSnapshot $
--             newDescribeApplicationSnapshotResponse
--
--         , responseDeleteApplicationVpcConfiguration $
--             newDeleteApplicationVpcConfigurationResponse
--
--         , responseAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfigurationResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseListApplicationVersions $
--             newListApplicationVersionsResponse
--
--         , responseDeleteApplicationOutput $
--             newDeleteApplicationOutputResponse
--
--         , responseRollbackApplication $
--             newRollbackApplicationResponse
--
--         , responseListApplicationSnapshots $
--             newListApplicationSnapshotsResponse
--
--         , responseStopApplication $
--             newStopApplicationResponse
--
--         , responseCreateApplicationSnapshot $
--             newCreateApplicationSnapshotResponse
--
--         , responseAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSourceResponse
--
--         , responseAddApplicationInput $
--             newAddApplicationInputResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseAddApplicationCloudWatchLoggingOption $
--             newAddApplicationCloudWatchLoggingOptionResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfigurationResponse
--
--         , responseAddApplicationVpcConfiguration $
--             newAddApplicationVpcConfigurationResponse
--
--         , responseUpdateApplicationMaintenanceConfiguration $
--             newUpdateApplicationMaintenanceConfigurationResponse
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

requestDeleteApplicationSnapshot :: DeleteApplicationSnapshot -> TestTree
requestDeleteApplicationSnapshot =
  req
    "DeleteApplicationSnapshot"
    "fixture/DeleteApplicationSnapshot.yaml"

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

requestDescribeApplicationVersion :: DescribeApplicationVersion -> TestTree
requestDescribeApplicationVersion =
  req
    "DescribeApplicationVersion"
    "fixture/DescribeApplicationVersion.yaml"

requestDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSource -> TestTree
requestDeleteApplicationReferenceDataSource =
  req
    "DeleteApplicationReferenceDataSource"
    "fixture/DeleteApplicationReferenceDataSource.yaml"

requestCreateApplicationPresignedUrl :: CreateApplicationPresignedUrl -> TestTree
requestCreateApplicationPresignedUrl =
  req
    "CreateApplicationPresignedUrl"
    "fixture/CreateApplicationPresignedUrl.yaml"

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

requestDescribeApplicationSnapshot :: DescribeApplicationSnapshot -> TestTree
requestDescribeApplicationSnapshot =
  req
    "DescribeApplicationSnapshot"
    "fixture/DescribeApplicationSnapshot.yaml"

requestDeleteApplicationVpcConfiguration :: DeleteApplicationVpcConfiguration -> TestTree
requestDeleteApplicationVpcConfiguration =
  req
    "DeleteApplicationVpcConfiguration"
    "fixture/DeleteApplicationVpcConfiguration.yaml"

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

requestListApplicationVersions :: ListApplicationVersions -> TestTree
requestListApplicationVersions =
  req
    "ListApplicationVersions"
    "fixture/ListApplicationVersions.yaml"

requestDeleteApplicationOutput :: DeleteApplicationOutput -> TestTree
requestDeleteApplicationOutput =
  req
    "DeleteApplicationOutput"
    "fixture/DeleteApplicationOutput.yaml"

requestRollbackApplication :: RollbackApplication -> TestTree
requestRollbackApplication =
  req
    "RollbackApplication"
    "fixture/RollbackApplication.yaml"

requestListApplicationSnapshots :: ListApplicationSnapshots -> TestTree
requestListApplicationSnapshots =
  req
    "ListApplicationSnapshots"
    "fixture/ListApplicationSnapshots.yaml"

requestStopApplication :: StopApplication -> TestTree
requestStopApplication =
  req
    "StopApplication"
    "fixture/StopApplication.yaml"

requestCreateApplicationSnapshot :: CreateApplicationSnapshot -> TestTree
requestCreateApplicationSnapshot =
  req
    "CreateApplicationSnapshot"
    "fixture/CreateApplicationSnapshot.yaml"

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

requestAddApplicationVpcConfiguration :: AddApplicationVpcConfiguration -> TestTree
requestAddApplicationVpcConfiguration =
  req
    "AddApplicationVpcConfiguration"
    "fixture/AddApplicationVpcConfiguration.yaml"

requestUpdateApplicationMaintenanceConfiguration :: UpdateApplicationMaintenanceConfiguration -> TestTree
requestUpdateApplicationMaintenanceConfiguration =
  req
    "UpdateApplicationMaintenanceConfiguration"
    "fixture/UpdateApplicationMaintenanceConfiguration.yaml"

-- Responses

responseAddApplicationOutput :: AddApplicationOutputResponse -> TestTree
responseAddApplicationOutput =
  res
    "AddApplicationOutputResponse"
    "fixture/AddApplicationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationOutput)

responseDiscoverInputSchema :: DiscoverInputSchemaResponse -> TestTree
responseDiscoverInputSchema =
  res
    "DiscoverInputSchemaResponse"
    "fixture/DiscoverInputSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DiscoverInputSchema)

responseDescribeApplication :: DescribeApplicationResponse -> TestTree
responseDescribeApplication =
  res
    "DescribeApplicationResponse"
    "fixture/DescribeApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplication)

responseDeleteApplicationSnapshot :: DeleteApplicationSnapshotResponse -> TestTree
responseDeleteApplicationSnapshot =
  res
    "DeleteApplicationSnapshotResponse"
    "fixture/DeleteApplicationSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationSnapshot)

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication =
  res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartApplication)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeApplicationVersion :: DescribeApplicationVersionResponse -> TestTree
responseDescribeApplicationVersion =
  res
    "DescribeApplicationVersionResponse"
    "fixture/DescribeApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationVersion)

responseDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSourceResponse -> TestTree
responseDeleteApplicationReferenceDataSource =
  res
    "DeleteApplicationReferenceDataSourceResponse"
    "fixture/DeleteApplicationReferenceDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationReferenceDataSource)

responseCreateApplicationPresignedUrl :: CreateApplicationPresignedUrlResponse -> TestTree
responseCreateApplicationPresignedUrl =
  res
    "CreateApplicationPresignedUrlResponse"
    "fixture/CreateApplicationPresignedUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplicationPresignedUrl)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOptionResponse -> TestTree
responseDeleteApplicationCloudWatchLoggingOption =
  res
    "DeleteApplicationCloudWatchLoggingOptionResponse"
    "fixture/DeleteApplicationCloudWatchLoggingOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationCloudWatchLoggingOption)

responseDescribeApplicationSnapshot :: DescribeApplicationSnapshotResponse -> TestTree
responseDescribeApplicationSnapshot =
  res
    "DescribeApplicationSnapshotResponse"
    "fixture/DescribeApplicationSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationSnapshot)

responseDeleteApplicationVpcConfiguration :: DeleteApplicationVpcConfigurationResponse -> TestTree
responseDeleteApplicationVpcConfiguration =
  res
    "DeleteApplicationVpcConfigurationResponse"
    "fixture/DeleteApplicationVpcConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationVpcConfiguration)

responseAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfigurationResponse -> TestTree
responseAddApplicationInputProcessingConfiguration =
  res
    "AddApplicationInputProcessingConfigurationResponse"
    "fixture/AddApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationInputProcessingConfiguration)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseListApplicationVersions :: ListApplicationVersionsResponse -> TestTree
responseListApplicationVersions =
  res
    "ListApplicationVersionsResponse"
    "fixture/ListApplicationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationVersions)

responseDeleteApplicationOutput :: DeleteApplicationOutputResponse -> TestTree
responseDeleteApplicationOutput =
  res
    "DeleteApplicationOutputResponse"
    "fixture/DeleteApplicationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationOutput)

responseRollbackApplication :: RollbackApplicationResponse -> TestTree
responseRollbackApplication =
  res
    "RollbackApplicationResponse"
    "fixture/RollbackApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RollbackApplication)

responseListApplicationSnapshots :: ListApplicationSnapshotsResponse -> TestTree
responseListApplicationSnapshots =
  res
    "ListApplicationSnapshotsResponse"
    "fixture/ListApplicationSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationSnapshots)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication =
  res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopApplication)

responseCreateApplicationSnapshot :: CreateApplicationSnapshotResponse -> TestTree
responseCreateApplicationSnapshot =
  res
    "CreateApplicationSnapshotResponse"
    "fixture/CreateApplicationSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplicationSnapshot)

responseAddApplicationReferenceDataSource :: AddApplicationReferenceDataSourceResponse -> TestTree
responseAddApplicationReferenceDataSource =
  res
    "AddApplicationReferenceDataSourceResponse"
    "fixture/AddApplicationReferenceDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationReferenceDataSource)

responseAddApplicationInput :: AddApplicationInputResponse -> TestTree
responseAddApplicationInput =
  res
    "AddApplicationInputResponse"
    "fixture/AddApplicationInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationInput)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOptionResponse -> TestTree
responseAddApplicationCloudWatchLoggingOption =
  res
    "AddApplicationCloudWatchLoggingOptionResponse"
    "fixture/AddApplicationCloudWatchLoggingOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationCloudWatchLoggingOption)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfigurationResponse -> TestTree
responseDeleteApplicationInputProcessingConfiguration =
  res
    "DeleteApplicationInputProcessingConfigurationResponse"
    "fixture/DeleteApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationInputProcessingConfiguration)

responseAddApplicationVpcConfiguration :: AddApplicationVpcConfigurationResponse -> TestTree
responseAddApplicationVpcConfiguration =
  res
    "AddApplicationVpcConfigurationResponse"
    "fixture/AddApplicationVpcConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationVpcConfiguration)

responseUpdateApplicationMaintenanceConfiguration :: UpdateApplicationMaintenanceConfigurationResponse -> TestTree
responseUpdateApplicationMaintenanceConfiguration =
  res
    "UpdateApplicationMaintenanceConfigurationResponse"
    "fixture/UpdateApplicationMaintenanceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationMaintenanceConfiguration)

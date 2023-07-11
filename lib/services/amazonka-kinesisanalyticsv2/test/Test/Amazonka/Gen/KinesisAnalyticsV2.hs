{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.KinesisAnalyticsV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestAddApplicationCloudWatchLoggingOption $
--             newAddApplicationCloudWatchLoggingOption
--
--         , requestAddApplicationInput $
--             newAddApplicationInput
--
--         , requestAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfiguration
--
--         , requestAddApplicationOutput $
--             newAddApplicationOutput
--
--         , requestAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSource
--
--         , requestAddApplicationVpcConfiguration $
--             newAddApplicationVpcConfiguration
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateApplicationPresignedUrl $
--             newCreateApplicationPresignedUrl
--
--         , requestCreateApplicationSnapshot $
--             newCreateApplicationSnapshot
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteApplicationCloudWatchLoggingOption $
--             newDeleteApplicationCloudWatchLoggingOption
--
--         , requestDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfiguration
--
--         , requestDeleteApplicationOutput $
--             newDeleteApplicationOutput
--
--         , requestDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSource
--
--         , requestDeleteApplicationSnapshot $
--             newDeleteApplicationSnapshot
--
--         , requestDeleteApplicationVpcConfiguration $
--             newDeleteApplicationVpcConfiguration
--
--         , requestDescribeApplication $
--             newDescribeApplication
--
--         , requestDescribeApplicationSnapshot $
--             newDescribeApplicationSnapshot
--
--         , requestDescribeApplicationVersion $
--             newDescribeApplicationVersion
--
--         , requestDiscoverInputSchema $
--             newDiscoverInputSchema
--
--         , requestListApplicationSnapshots $
--             newListApplicationSnapshots
--
--         , requestListApplicationVersions $
--             newListApplicationVersions
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRollbackApplication $
--             newRollbackApplication
--
--         , requestStartApplication $
--             newStartApplication
--
--         , requestStopApplication $
--             newStopApplication
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
--         , requestUpdateApplicationMaintenanceConfiguration $
--             newUpdateApplicationMaintenanceConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAddApplicationCloudWatchLoggingOption $
--             newAddApplicationCloudWatchLoggingOptionResponse
--
--         , responseAddApplicationInput $
--             newAddApplicationInputResponse
--
--         , responseAddApplicationInputProcessingConfiguration $
--             newAddApplicationInputProcessingConfigurationResponse
--
--         , responseAddApplicationOutput $
--             newAddApplicationOutputResponse
--
--         , responseAddApplicationReferenceDataSource $
--             newAddApplicationReferenceDataSourceResponse
--
--         , responseAddApplicationVpcConfiguration $
--             newAddApplicationVpcConfigurationResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateApplicationPresignedUrl $
--             newCreateApplicationPresignedUrlResponse
--
--         , responseCreateApplicationSnapshot $
--             newCreateApplicationSnapshotResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteApplicationCloudWatchLoggingOption $
--             newDeleteApplicationCloudWatchLoggingOptionResponse
--
--         , responseDeleteApplicationInputProcessingConfiguration $
--             newDeleteApplicationInputProcessingConfigurationResponse
--
--         , responseDeleteApplicationOutput $
--             newDeleteApplicationOutputResponse
--
--         , responseDeleteApplicationReferenceDataSource $
--             newDeleteApplicationReferenceDataSourceResponse
--
--         , responseDeleteApplicationSnapshot $
--             newDeleteApplicationSnapshotResponse
--
--         , responseDeleteApplicationVpcConfiguration $
--             newDeleteApplicationVpcConfigurationResponse
--
--         , responseDescribeApplication $
--             newDescribeApplicationResponse
--
--         , responseDescribeApplicationSnapshot $
--             newDescribeApplicationSnapshotResponse
--
--         , responseDescribeApplicationVersion $
--             newDescribeApplicationVersionResponse
--
--         , responseDiscoverInputSchema $
--             newDiscoverInputSchemaResponse
--
--         , responseListApplicationSnapshots $
--             newListApplicationSnapshotsResponse
--
--         , responseListApplicationVersions $
--             newListApplicationVersionsResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRollbackApplication $
--             newRollbackApplicationResponse
--
--         , responseStartApplication $
--             newStartApplicationResponse
--
--         , responseStopApplication $
--             newStopApplicationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseUpdateApplicationMaintenanceConfiguration $
--             newUpdateApplicationMaintenanceConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOption -> TestTree
requestAddApplicationCloudWatchLoggingOption =
  req
    "AddApplicationCloudWatchLoggingOption"
    "fixture/AddApplicationCloudWatchLoggingOption.yaml"

requestAddApplicationInput :: AddApplicationInput -> TestTree
requestAddApplicationInput =
  req
    "AddApplicationInput"
    "fixture/AddApplicationInput.yaml"

requestAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfiguration -> TestTree
requestAddApplicationInputProcessingConfiguration =
  req
    "AddApplicationInputProcessingConfiguration"
    "fixture/AddApplicationInputProcessingConfiguration.yaml"

requestAddApplicationOutput :: AddApplicationOutput -> TestTree
requestAddApplicationOutput =
  req
    "AddApplicationOutput"
    "fixture/AddApplicationOutput.yaml"

requestAddApplicationReferenceDataSource :: AddApplicationReferenceDataSource -> TestTree
requestAddApplicationReferenceDataSource =
  req
    "AddApplicationReferenceDataSource"
    "fixture/AddApplicationReferenceDataSource.yaml"

requestAddApplicationVpcConfiguration :: AddApplicationVpcConfiguration -> TestTree
requestAddApplicationVpcConfiguration =
  req
    "AddApplicationVpcConfiguration"
    "fixture/AddApplicationVpcConfiguration.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateApplicationPresignedUrl :: CreateApplicationPresignedUrl -> TestTree
requestCreateApplicationPresignedUrl =
  req
    "CreateApplicationPresignedUrl"
    "fixture/CreateApplicationPresignedUrl.yaml"

requestCreateApplicationSnapshot :: CreateApplicationSnapshot -> TestTree
requestCreateApplicationSnapshot =
  req
    "CreateApplicationSnapshot"
    "fixture/CreateApplicationSnapshot.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOption -> TestTree
requestDeleteApplicationCloudWatchLoggingOption =
  req
    "DeleteApplicationCloudWatchLoggingOption"
    "fixture/DeleteApplicationCloudWatchLoggingOption.yaml"

requestDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfiguration -> TestTree
requestDeleteApplicationInputProcessingConfiguration =
  req
    "DeleteApplicationInputProcessingConfiguration"
    "fixture/DeleteApplicationInputProcessingConfiguration.yaml"

requestDeleteApplicationOutput :: DeleteApplicationOutput -> TestTree
requestDeleteApplicationOutput =
  req
    "DeleteApplicationOutput"
    "fixture/DeleteApplicationOutput.yaml"

requestDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSource -> TestTree
requestDeleteApplicationReferenceDataSource =
  req
    "DeleteApplicationReferenceDataSource"
    "fixture/DeleteApplicationReferenceDataSource.yaml"

requestDeleteApplicationSnapshot :: DeleteApplicationSnapshot -> TestTree
requestDeleteApplicationSnapshot =
  req
    "DeleteApplicationSnapshot"
    "fixture/DeleteApplicationSnapshot.yaml"

requestDeleteApplicationVpcConfiguration :: DeleteApplicationVpcConfiguration -> TestTree
requestDeleteApplicationVpcConfiguration =
  req
    "DeleteApplicationVpcConfiguration"
    "fixture/DeleteApplicationVpcConfiguration.yaml"

requestDescribeApplication :: DescribeApplication -> TestTree
requestDescribeApplication =
  req
    "DescribeApplication"
    "fixture/DescribeApplication.yaml"

requestDescribeApplicationSnapshot :: DescribeApplicationSnapshot -> TestTree
requestDescribeApplicationSnapshot =
  req
    "DescribeApplicationSnapshot"
    "fixture/DescribeApplicationSnapshot.yaml"

requestDescribeApplicationVersion :: DescribeApplicationVersion -> TestTree
requestDescribeApplicationVersion =
  req
    "DescribeApplicationVersion"
    "fixture/DescribeApplicationVersion.yaml"

requestDiscoverInputSchema :: DiscoverInputSchema -> TestTree
requestDiscoverInputSchema =
  req
    "DiscoverInputSchema"
    "fixture/DiscoverInputSchema.yaml"

requestListApplicationSnapshots :: ListApplicationSnapshots -> TestTree
requestListApplicationSnapshots =
  req
    "ListApplicationSnapshots"
    "fixture/ListApplicationSnapshots.yaml"

requestListApplicationVersions :: ListApplicationVersions -> TestTree
requestListApplicationVersions =
  req
    "ListApplicationVersions"
    "fixture/ListApplicationVersions.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRollbackApplication :: RollbackApplication -> TestTree
requestRollbackApplication =
  req
    "RollbackApplication"
    "fixture/RollbackApplication.yaml"

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

requestUpdateApplicationMaintenanceConfiguration :: UpdateApplicationMaintenanceConfiguration -> TestTree
requestUpdateApplicationMaintenanceConfiguration =
  req
    "UpdateApplicationMaintenanceConfiguration"
    "fixture/UpdateApplicationMaintenanceConfiguration.yaml"

-- Responses

responseAddApplicationCloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOptionResponse -> TestTree
responseAddApplicationCloudWatchLoggingOption =
  res
    "AddApplicationCloudWatchLoggingOptionResponse"
    "fixture/AddApplicationCloudWatchLoggingOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationCloudWatchLoggingOption)

responseAddApplicationInput :: AddApplicationInputResponse -> TestTree
responseAddApplicationInput =
  res
    "AddApplicationInputResponse"
    "fixture/AddApplicationInputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationInput)

responseAddApplicationInputProcessingConfiguration :: AddApplicationInputProcessingConfigurationResponse -> TestTree
responseAddApplicationInputProcessingConfiguration =
  res
    "AddApplicationInputProcessingConfigurationResponse"
    "fixture/AddApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationInputProcessingConfiguration)

responseAddApplicationOutput :: AddApplicationOutputResponse -> TestTree
responseAddApplicationOutput =
  res
    "AddApplicationOutputResponse"
    "fixture/AddApplicationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationOutput)

responseAddApplicationReferenceDataSource :: AddApplicationReferenceDataSourceResponse -> TestTree
responseAddApplicationReferenceDataSource =
  res
    "AddApplicationReferenceDataSourceResponse"
    "fixture/AddApplicationReferenceDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationReferenceDataSource)

responseAddApplicationVpcConfiguration :: AddApplicationVpcConfigurationResponse -> TestTree
responseAddApplicationVpcConfiguration =
  res
    "AddApplicationVpcConfigurationResponse"
    "fixture/AddApplicationVpcConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddApplicationVpcConfiguration)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateApplicationPresignedUrl :: CreateApplicationPresignedUrlResponse -> TestTree
responseCreateApplicationPresignedUrl =
  res
    "CreateApplicationPresignedUrlResponse"
    "fixture/CreateApplicationPresignedUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplicationPresignedUrl)

responseCreateApplicationSnapshot :: CreateApplicationSnapshotResponse -> TestTree
responseCreateApplicationSnapshot =
  res
    "CreateApplicationSnapshotResponse"
    "fixture/CreateApplicationSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplicationSnapshot)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteApplicationCloudWatchLoggingOption :: DeleteApplicationCloudWatchLoggingOptionResponse -> TestTree
responseDeleteApplicationCloudWatchLoggingOption =
  res
    "DeleteApplicationCloudWatchLoggingOptionResponse"
    "fixture/DeleteApplicationCloudWatchLoggingOptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationCloudWatchLoggingOption)

responseDeleteApplicationInputProcessingConfiguration :: DeleteApplicationInputProcessingConfigurationResponse -> TestTree
responseDeleteApplicationInputProcessingConfiguration =
  res
    "DeleteApplicationInputProcessingConfigurationResponse"
    "fixture/DeleteApplicationInputProcessingConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationInputProcessingConfiguration)

responseDeleteApplicationOutput :: DeleteApplicationOutputResponse -> TestTree
responseDeleteApplicationOutput =
  res
    "DeleteApplicationOutputResponse"
    "fixture/DeleteApplicationOutputResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationOutput)

responseDeleteApplicationReferenceDataSource :: DeleteApplicationReferenceDataSourceResponse -> TestTree
responseDeleteApplicationReferenceDataSource =
  res
    "DeleteApplicationReferenceDataSourceResponse"
    "fixture/DeleteApplicationReferenceDataSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationReferenceDataSource)

responseDeleteApplicationSnapshot :: DeleteApplicationSnapshotResponse -> TestTree
responseDeleteApplicationSnapshot =
  res
    "DeleteApplicationSnapshotResponse"
    "fixture/DeleteApplicationSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationSnapshot)

responseDeleteApplicationVpcConfiguration :: DeleteApplicationVpcConfigurationResponse -> TestTree
responseDeleteApplicationVpcConfiguration =
  res
    "DeleteApplicationVpcConfigurationResponse"
    "fixture/DeleteApplicationVpcConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationVpcConfiguration)

responseDescribeApplication :: DescribeApplicationResponse -> TestTree
responseDescribeApplication =
  res
    "DescribeApplicationResponse"
    "fixture/DescribeApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplication)

responseDescribeApplicationSnapshot :: DescribeApplicationSnapshotResponse -> TestTree
responseDescribeApplicationSnapshot =
  res
    "DescribeApplicationSnapshotResponse"
    "fixture/DescribeApplicationSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationSnapshot)

responseDescribeApplicationVersion :: DescribeApplicationVersionResponse -> TestTree
responseDescribeApplicationVersion =
  res
    "DescribeApplicationVersionResponse"
    "fixture/DescribeApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationVersion)

responseDiscoverInputSchema :: DiscoverInputSchemaResponse -> TestTree
responseDiscoverInputSchema =
  res
    "DiscoverInputSchemaResponse"
    "fixture/DiscoverInputSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DiscoverInputSchema)

responseListApplicationSnapshots :: ListApplicationSnapshotsResponse -> TestTree
responseListApplicationSnapshots =
  res
    "ListApplicationSnapshotsResponse"
    "fixture/ListApplicationSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationSnapshots)

responseListApplicationVersions :: ListApplicationVersionsResponse -> TestTree
responseListApplicationVersions =
  res
    "ListApplicationVersionsResponse"
    "fixture/ListApplicationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationVersions)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRollbackApplication :: RollbackApplicationResponse -> TestTree
responseRollbackApplication =
  res
    "RollbackApplicationResponse"
    "fixture/RollbackApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RollbackApplication)

responseStartApplication :: StartApplicationResponse -> TestTree
responseStartApplication =
  res
    "StartApplicationResponse"
    "fixture/StartApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartApplication)

responseStopApplication :: StopApplicationResponse -> TestTree
responseStopApplication =
  res
    "StopApplicationResponse"
    "fixture/StopApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopApplication)

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

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateApplicationMaintenanceConfiguration :: UpdateApplicationMaintenanceConfigurationResponse -> TestTree
responseUpdateApplicationMaintenanceConfiguration =
  res
    "UpdateApplicationMaintenanceConfigurationResponse"
    "fixture/UpdateApplicationMaintenanceConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationMaintenanceConfiguration)

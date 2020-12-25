{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MediaPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MediaPackage where

import Data.Proxy
import Network.AWS.MediaPackage
import Test.AWS.Fixture
import Test.AWS.MediaPackage.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateHarvestJob $
--             mkCreateHarvestJob
--
--         , requestConfigureLogs $
--             mkConfigureLogs
--
--         , requestDescribeOriginEndpoint $
--             mkDescribeOriginEndpoint
--
--         , requestListChannels $
--             mkListChannels
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDeleteChannel $
--             mkDeleteChannel
--
--         , requestUpdateChannel $
--             mkUpdateChannel
--
--         , requestDescribeHarvestJob $
--             mkDescribeHarvestJob
--
--         , requestRotateIngestEndpointCredentials $
--             mkRotateIngestEndpointCredentials
--
--         , requestCreateOriginEndpoint $
--             mkCreateOriginEndpoint
--
--         , requestListOriginEndpoints $
--             mkListOriginEndpoints
--
--         , requestListHarvestJobs $
--             mkListHarvestJobs
--
--         , requestCreateChannel $
--             mkCreateChannel
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDescribeChannel $
--             mkDescribeChannel
--
--         , requestDeleteOriginEndpoint $
--             mkDeleteOriginEndpoint
--
--         , requestUpdateOriginEndpoint $
--             mkUpdateOriginEndpoint
--
--           ]

--     , testGroup "response"
--         [ responseCreateHarvestJob $
--             mkCreateHarvestJobResponse
--
--         , responseConfigureLogs $
--             mkConfigureLogsResponse
--
--         , responseDescribeOriginEndpoint $
--             mkDescribeOriginEndpointResponse
--
--         , responseListChannels $
--             mkListChannelsResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDeleteChannel $
--             mkDeleteChannelResponse
--
--         , responseUpdateChannel $
--             mkUpdateChannelResponse
--
--         , responseDescribeHarvestJob $
--             mkDescribeHarvestJobResponse
--
--         , responseRotateIngestEndpointCredentials $
--             mkRotateIngestEndpointCredentialsResponse
--
--         , responseCreateOriginEndpoint $
--             mkCreateOriginEndpointResponse
--
--         , responseListOriginEndpoints $
--             mkListOriginEndpointsResponse
--
--         , responseListHarvestJobs $
--             mkListHarvestJobsResponse
--
--         , responseCreateChannel $
--             mkCreateChannelResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDescribeChannel $
--             mkDescribeChannelResponse
--
--         , responseDeleteOriginEndpoint $
--             mkDeleteOriginEndpointResponse
--
--         , responseUpdateOriginEndpoint $
--             mkUpdateOriginEndpointResponse
--
--           ]
--     ]

-- Requests

requestCreateHarvestJob :: CreateHarvestJob -> TestTree
requestCreateHarvestJob =
  req
    "CreateHarvestJob"
    "fixture/CreateHarvestJob.yaml"

requestConfigureLogs :: ConfigureLogs -> TestTree
requestConfigureLogs =
  req
    "ConfigureLogs"
    "fixture/ConfigureLogs.yaml"

requestDescribeOriginEndpoint :: DescribeOriginEndpoint -> TestTree
requestDescribeOriginEndpoint =
  req
    "DescribeOriginEndpoint"
    "fixture/DescribeOriginEndpoint.yaml"

requestListChannels :: ListChannels -> TestTree
requestListChannels =
  req
    "ListChannels"
    "fixture/ListChannels.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDeleteChannel :: DeleteChannel -> TestTree
requestDeleteChannel =
  req
    "DeleteChannel"
    "fixture/DeleteChannel.yaml"

requestUpdateChannel :: UpdateChannel -> TestTree
requestUpdateChannel =
  req
    "UpdateChannel"
    "fixture/UpdateChannel.yaml"

requestDescribeHarvestJob :: DescribeHarvestJob -> TestTree
requestDescribeHarvestJob =
  req
    "DescribeHarvestJob"
    "fixture/DescribeHarvestJob.yaml"

requestRotateIngestEndpointCredentials :: RotateIngestEndpointCredentials -> TestTree
requestRotateIngestEndpointCredentials =
  req
    "RotateIngestEndpointCredentials"
    "fixture/RotateIngestEndpointCredentials.yaml"

requestCreateOriginEndpoint :: CreateOriginEndpoint -> TestTree
requestCreateOriginEndpoint =
  req
    "CreateOriginEndpoint"
    "fixture/CreateOriginEndpoint.yaml"

requestListOriginEndpoints :: ListOriginEndpoints -> TestTree
requestListOriginEndpoints =
  req
    "ListOriginEndpoints"
    "fixture/ListOriginEndpoints.yaml"

requestListHarvestJobs :: ListHarvestJobs -> TestTree
requestListHarvestJobs =
  req
    "ListHarvestJobs"
    "fixture/ListHarvestJobs.yaml"

requestCreateChannel :: CreateChannel -> TestTree
requestCreateChannel =
  req
    "CreateChannel"
    "fixture/CreateChannel.yaml"

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

requestDescribeChannel :: DescribeChannel -> TestTree
requestDescribeChannel =
  req
    "DescribeChannel"
    "fixture/DescribeChannel.yaml"

requestDeleteOriginEndpoint :: DeleteOriginEndpoint -> TestTree
requestDeleteOriginEndpoint =
  req
    "DeleteOriginEndpoint"
    "fixture/DeleteOriginEndpoint.yaml"

requestUpdateOriginEndpoint :: UpdateOriginEndpoint -> TestTree
requestUpdateOriginEndpoint =
  req
    "UpdateOriginEndpoint"
    "fixture/UpdateOriginEndpoint.yaml"

-- Responses

responseCreateHarvestJob :: CreateHarvestJobResponse -> TestTree
responseCreateHarvestJob =
  res
    "CreateHarvestJobResponse"
    "fixture/CreateHarvestJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateHarvestJob)

responseConfigureLogs :: ConfigureLogsResponse -> TestTree
responseConfigureLogs =
  res
    "ConfigureLogsResponse"
    "fixture/ConfigureLogsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ConfigureLogs)

responseDescribeOriginEndpoint :: DescribeOriginEndpointResponse -> TestTree
responseDescribeOriginEndpoint =
  res
    "DescribeOriginEndpointResponse"
    "fixture/DescribeOriginEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeOriginEndpoint)

responseListChannels :: ListChannelsResponse -> TestTree
responseListChannels =
  res
    "ListChannelsResponse"
    "fixture/ListChannelsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListChannels)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseDeleteChannel :: DeleteChannelResponse -> TestTree
responseDeleteChannel =
  res
    "DeleteChannelResponse"
    "fixture/DeleteChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteChannel)

responseUpdateChannel :: UpdateChannelResponse -> TestTree
responseUpdateChannel =
  res
    "UpdateChannelResponse"
    "fixture/UpdateChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateChannel)

responseDescribeHarvestJob :: DescribeHarvestJobResponse -> TestTree
responseDescribeHarvestJob =
  res
    "DescribeHarvestJobResponse"
    "fixture/DescribeHarvestJobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHarvestJob)

responseRotateIngestEndpointCredentials :: RotateIngestEndpointCredentialsResponse -> TestTree
responseRotateIngestEndpointCredentials =
  res
    "RotateIngestEndpointCredentialsResponse"
    "fixture/RotateIngestEndpointCredentialsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RotateIngestEndpointCredentials)

responseCreateOriginEndpoint :: CreateOriginEndpointResponse -> TestTree
responseCreateOriginEndpoint =
  res
    "CreateOriginEndpointResponse"
    "fixture/CreateOriginEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateOriginEndpoint)

responseListOriginEndpoints :: ListOriginEndpointsResponse -> TestTree
responseListOriginEndpoints =
  res
    "ListOriginEndpointsResponse"
    "fixture/ListOriginEndpointsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListOriginEndpoints)

responseListHarvestJobs :: ListHarvestJobsResponse -> TestTree
responseListHarvestJobs =
  res
    "ListHarvestJobsResponse"
    "fixture/ListHarvestJobsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHarvestJobs)

responseCreateChannel :: CreateChannelResponse -> TestTree
responseCreateChannel =
  res
    "CreateChannelResponse"
    "fixture/CreateChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateChannel)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseDescribeChannel :: DescribeChannelResponse -> TestTree
responseDescribeChannel =
  res
    "DescribeChannelResponse"
    "fixture/DescribeChannelResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeChannel)

responseDeleteOriginEndpoint :: DeleteOriginEndpointResponse -> TestTree
responseDeleteOriginEndpoint =
  res
    "DeleteOriginEndpointResponse"
    "fixture/DeleteOriginEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteOriginEndpoint)

responseUpdateOriginEndpoint :: UpdateOriginEndpointResponse -> TestTree
responseUpdateOriginEndpoint =
  res
    "UpdateOriginEndpointResponse"
    "fixture/UpdateOriginEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateOriginEndpoint)

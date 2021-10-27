{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.GroundStation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.GroundStation where

import Data.Proxy
import Network.AWS.GroundStation
import Test.AWS.Fixture
import Test.AWS.GroundStation.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteDataflowEndpointGroup $
--             newDeleteDataflowEndpointGroup
--
--         , requestListSatellites $
--             newListSatellites
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetMinuteUsage $
--             newGetMinuteUsage
--
--         , requestDescribeContact $
--             newDescribeContact
--
--         , requestReserveContact $
--             newReserveContact
--
--         , requestCreateMissionProfile $
--             newCreateMissionProfile
--
--         , requestListGroundStations $
--             newListGroundStations
--
--         , requestCreateConfig $
--             newCreateConfig
--
--         , requestListMissionProfiles $
--             newListMissionProfiles
--
--         , requestGetMissionProfile $
--             newGetMissionProfile
--
--         , requestGetConfig $
--             newGetConfig
--
--         , requestListDataflowEndpointGroups $
--             newListDataflowEndpointGroups
--
--         , requestCreateDataflowEndpointGroup $
--             newCreateDataflowEndpointGroup
--
--         , requestGetSatellite $
--             newGetSatellite
--
--         , requestGetDataflowEndpointGroup $
--             newGetDataflowEndpointGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListConfigs $
--             newListConfigs
--
--         , requestUpdateConfig $
--             newUpdateConfig
--
--         , requestDeleteConfig $
--             newDeleteConfig
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateMissionProfile $
--             newUpdateMissionProfile
--
--         , requestDeleteMissionProfile $
--             newDeleteMissionProfile
--
--         , requestCancelContact $
--             newCancelContact
--
--         , requestListContacts $
--             newListContacts
--
--           ]

--     , testGroup "response"
--         [ responseDeleteDataflowEndpointGroup $
--             newDataflowEndpointGroupIdResponse
--
--         , responseListSatellites $
--             newListSatellitesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetMinuteUsage $
--             newGetMinuteUsageResponse
--
--         , responseDescribeContact $
--             newDescribeContactResponse
--
--         , responseReserveContact $
--             newContactIdResponse
--
--         , responseCreateMissionProfile $
--             newMissionProfileIdResponse
--
--         , responseListGroundStations $
--             newListGroundStationsResponse
--
--         , responseCreateConfig $
--             newConfigIdResponse
--
--         , responseListMissionProfiles $
--             newListMissionProfilesResponse
--
--         , responseGetMissionProfile $
--             newGetMissionProfileResponse
--
--         , responseGetConfig $
--             newGetConfigResponse
--
--         , responseListDataflowEndpointGroups $
--             newListDataflowEndpointGroupsResponse
--
--         , responseCreateDataflowEndpointGroup $
--             newDataflowEndpointGroupIdResponse
--
--         , responseGetSatellite $
--             newGetSatelliteResponse
--
--         , responseGetDataflowEndpointGroup $
--             newGetDataflowEndpointGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListConfigs $
--             newListConfigsResponse
--
--         , responseUpdateConfig $
--             newConfigIdResponse
--
--         , responseDeleteConfig $
--             newConfigIdResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateMissionProfile $
--             newMissionProfileIdResponse
--
--         , responseDeleteMissionProfile $
--             newMissionProfileIdResponse
--
--         , responseCancelContact $
--             newContactIdResponse
--
--         , responseListContacts $
--             newListContactsResponse
--
--           ]
--     ]

-- Requests

requestDeleteDataflowEndpointGroup :: DeleteDataflowEndpointGroup -> TestTree
requestDeleteDataflowEndpointGroup =
  req
    "DeleteDataflowEndpointGroup"
    "fixture/DeleteDataflowEndpointGroup.yaml"

requestListSatellites :: ListSatellites -> TestTree
requestListSatellites =
  req
    "ListSatellites"
    "fixture/ListSatellites.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetMinuteUsage :: GetMinuteUsage -> TestTree
requestGetMinuteUsage =
  req
    "GetMinuteUsage"
    "fixture/GetMinuteUsage.yaml"

requestDescribeContact :: DescribeContact -> TestTree
requestDescribeContact =
  req
    "DescribeContact"
    "fixture/DescribeContact.yaml"

requestReserveContact :: ReserveContact -> TestTree
requestReserveContact =
  req
    "ReserveContact"
    "fixture/ReserveContact.yaml"

requestCreateMissionProfile :: CreateMissionProfile -> TestTree
requestCreateMissionProfile =
  req
    "CreateMissionProfile"
    "fixture/CreateMissionProfile.yaml"

requestListGroundStations :: ListGroundStations -> TestTree
requestListGroundStations =
  req
    "ListGroundStations"
    "fixture/ListGroundStations.yaml"

requestCreateConfig :: CreateConfig -> TestTree
requestCreateConfig =
  req
    "CreateConfig"
    "fixture/CreateConfig.yaml"

requestListMissionProfiles :: ListMissionProfiles -> TestTree
requestListMissionProfiles =
  req
    "ListMissionProfiles"
    "fixture/ListMissionProfiles.yaml"

requestGetMissionProfile :: GetMissionProfile -> TestTree
requestGetMissionProfile =
  req
    "GetMissionProfile"
    "fixture/GetMissionProfile.yaml"

requestGetConfig :: GetConfig -> TestTree
requestGetConfig =
  req
    "GetConfig"
    "fixture/GetConfig.yaml"

requestListDataflowEndpointGroups :: ListDataflowEndpointGroups -> TestTree
requestListDataflowEndpointGroups =
  req
    "ListDataflowEndpointGroups"
    "fixture/ListDataflowEndpointGroups.yaml"

requestCreateDataflowEndpointGroup :: CreateDataflowEndpointGroup -> TestTree
requestCreateDataflowEndpointGroup =
  req
    "CreateDataflowEndpointGroup"
    "fixture/CreateDataflowEndpointGroup.yaml"

requestGetSatellite :: GetSatellite -> TestTree
requestGetSatellite =
  req
    "GetSatellite"
    "fixture/GetSatellite.yaml"

requestGetDataflowEndpointGroup :: GetDataflowEndpointGroup -> TestTree
requestGetDataflowEndpointGroup =
  req
    "GetDataflowEndpointGroup"
    "fixture/GetDataflowEndpointGroup.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListConfigs :: ListConfigs -> TestTree
requestListConfigs =
  req
    "ListConfigs"
    "fixture/ListConfigs.yaml"

requestUpdateConfig :: UpdateConfig -> TestTree
requestUpdateConfig =
  req
    "UpdateConfig"
    "fixture/UpdateConfig.yaml"

requestDeleteConfig :: DeleteConfig -> TestTree
requestDeleteConfig =
  req
    "DeleteConfig"
    "fixture/DeleteConfig.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateMissionProfile :: UpdateMissionProfile -> TestTree
requestUpdateMissionProfile =
  req
    "UpdateMissionProfile"
    "fixture/UpdateMissionProfile.yaml"

requestDeleteMissionProfile :: DeleteMissionProfile -> TestTree
requestDeleteMissionProfile =
  req
    "DeleteMissionProfile"
    "fixture/DeleteMissionProfile.yaml"

requestCancelContact :: CancelContact -> TestTree
requestCancelContact =
  req
    "CancelContact"
    "fixture/CancelContact.yaml"

requestListContacts :: ListContacts -> TestTree
requestListContacts =
  req
    "ListContacts"
    "fixture/ListContacts.yaml"

-- Responses

responseDeleteDataflowEndpointGroup :: DataflowEndpointGroupIdResponse -> TestTree
responseDeleteDataflowEndpointGroup =
  res
    "DeleteDataflowEndpointGroupResponse"
    "fixture/DeleteDataflowEndpointGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataflowEndpointGroup)

responseListSatellites :: ListSatellitesResponse -> TestTree
responseListSatellites =
  res
    "ListSatellitesResponse"
    "fixture/ListSatellitesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSatellites)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetMinuteUsage :: GetMinuteUsageResponse -> TestTree
responseGetMinuteUsage =
  res
    "GetMinuteUsageResponse"
    "fixture/GetMinuteUsageResponse.proto"
    defaultService
    (Proxy :: Proxy GetMinuteUsage)

responseDescribeContact :: DescribeContactResponse -> TestTree
responseDescribeContact =
  res
    "DescribeContactResponse"
    "fixture/DescribeContactResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeContact)

responseReserveContact :: ContactIdResponse -> TestTree
responseReserveContact =
  res
    "ReserveContactResponse"
    "fixture/ReserveContactResponse.proto"
    defaultService
    (Proxy :: Proxy ReserveContact)

responseCreateMissionProfile :: MissionProfileIdResponse -> TestTree
responseCreateMissionProfile =
  res
    "CreateMissionProfileResponse"
    "fixture/CreateMissionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMissionProfile)

responseListGroundStations :: ListGroundStationsResponse -> TestTree
responseListGroundStations =
  res
    "ListGroundStationsResponse"
    "fixture/ListGroundStationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroundStations)

responseCreateConfig :: ConfigIdResponse -> TestTree
responseCreateConfig =
  res
    "CreateConfigResponse"
    "fixture/CreateConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfig)

responseListMissionProfiles :: ListMissionProfilesResponse -> TestTree
responseListMissionProfiles =
  res
    "ListMissionProfilesResponse"
    "fixture/ListMissionProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMissionProfiles)

responseGetMissionProfile :: GetMissionProfileResponse -> TestTree
responseGetMissionProfile =
  res
    "GetMissionProfileResponse"
    "fixture/GetMissionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy GetMissionProfile)

responseGetConfig :: GetConfigResponse -> TestTree
responseGetConfig =
  res
    "GetConfigResponse"
    "fixture/GetConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetConfig)

responseListDataflowEndpointGroups :: ListDataflowEndpointGroupsResponse -> TestTree
responseListDataflowEndpointGroups =
  res
    "ListDataflowEndpointGroupsResponse"
    "fixture/ListDataflowEndpointGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDataflowEndpointGroups)

responseCreateDataflowEndpointGroup :: DataflowEndpointGroupIdResponse -> TestTree
responseCreateDataflowEndpointGroup =
  res
    "CreateDataflowEndpointGroupResponse"
    "fixture/CreateDataflowEndpointGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDataflowEndpointGroup)

responseGetSatellite :: GetSatelliteResponse -> TestTree
responseGetSatellite =
  res
    "GetSatelliteResponse"
    "fixture/GetSatelliteResponse.proto"
    defaultService
    (Proxy :: Proxy GetSatellite)

responseGetDataflowEndpointGroup :: GetDataflowEndpointGroupResponse -> TestTree
responseGetDataflowEndpointGroup =
  res
    "GetDataflowEndpointGroupResponse"
    "fixture/GetDataflowEndpointGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetDataflowEndpointGroup)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListConfigs :: ListConfigsResponse -> TestTree
responseListConfigs =
  res
    "ListConfigsResponse"
    "fixture/ListConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConfigs)

responseUpdateConfig :: ConfigIdResponse -> TestTree
responseUpdateConfig =
  res
    "UpdateConfigResponse"
    "fixture/UpdateConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfig)

responseDeleteConfig :: ConfigIdResponse -> TestTree
responseDeleteConfig =
  res
    "DeleteConfigResponse"
    "fixture/DeleteConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfig)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUpdateMissionProfile :: MissionProfileIdResponse -> TestTree
responseUpdateMissionProfile =
  res
    "UpdateMissionProfileResponse"
    "fixture/UpdateMissionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMissionProfile)

responseDeleteMissionProfile :: MissionProfileIdResponse -> TestTree
responseDeleteMissionProfile =
  res
    "DeleteMissionProfileResponse"
    "fixture/DeleteMissionProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMissionProfile)

responseCancelContact :: ContactIdResponse -> TestTree
responseCancelContact =
  res
    "CancelContactResponse"
    "fixture/CancelContactResponse.proto"
    defaultService
    (Proxy :: Proxy CancelContact)

responseListContacts :: ListContactsResponse -> TestTree
responseListContacts =
  res
    "ListContactsResponse"
    "fixture/ListContactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContacts)

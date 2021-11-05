{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Nimble
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Nimble where

import Amazonka.Nimble
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Nimble.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutLaunchProfileMembers $
--             newPutLaunchProfileMembers
--
--         , requestCreateStudio $
--             newCreateStudio
--
--         , requestUpdateLaunchProfileMember $
--             newUpdateLaunchProfileMember
--
--         , requestDeleteLaunchProfileMember $
--             newDeleteLaunchProfileMember
--
--         , requestListLaunchProfiles $
--             newListLaunchProfiles
--
--         , requestCreateLaunchProfile $
--             newCreateLaunchProfile
--
--         , requestListStreamingImages $
--             newListStreamingImages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartStudioSSOConfigurationRepair $
--             newStartStudioSSOConfigurationRepair
--
--         , requestGetLaunchProfileInitialization $
--             newGetLaunchProfileInitialization
--
--         , requestGetLaunchProfile $
--             newGetLaunchProfile
--
--         , requestCreateStudioComponent $
--             newCreateStudioComponent
--
--         , requestGetEula $
--             newGetEula
--
--         , requestListStudioComponents $
--             newListStudioComponents
--
--         , requestAcceptEulas $
--             newAcceptEulas
--
--         , requestCreateStreamingSession $
--             newCreateStreamingSession
--
--         , requestDeleteStudioComponent $
--             newDeleteStudioComponent
--
--         , requestUpdateStudioComponent $
--             newUpdateStudioComponent
--
--         , requestGetStudioMember $
--             newGetStudioMember
--
--         , requestDeleteStudio $
--             newDeleteStudio
--
--         , requestUpdateStudio $
--             newUpdateStudio
--
--         , requestListStudios $
--             newListStudios
--
--         , requestGetStudioComponent $
--             newGetStudioComponent
--
--         , requestListEulas $
--             newListEulas
--
--         , requestGetStreamingSession $
--             newGetStreamingSession
--
--         , requestListLaunchProfileMembers $
--             newListLaunchProfileMembers
--
--         , requestDeleteLaunchProfile $
--             newDeleteLaunchProfile
--
--         , requestUpdateLaunchProfile $
--             newUpdateLaunchProfile
--
--         , requestCreateStreamingImage $
--             newCreateStreamingImage
--
--         , requestCreateStreamingSessionStream $
--             newCreateStreamingSessionStream
--
--         , requestGetLaunchProfileDetails $
--             newGetLaunchProfileDetails
--
--         , requestPutStudioMembers $
--             newPutStudioMembers
--
--         , requestDeleteStreamingImage $
--             newDeleteStreamingImage
--
--         , requestUpdateStreamingImage $
--             newUpdateStreamingImage
--
--         , requestGetStreamingImage $
--             newGetStreamingImage
--
--         , requestListEulaAcceptances $
--             newListEulaAcceptances
--
--         , requestGetStreamingSessionStream $
--             newGetStreamingSessionStream
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetLaunchProfileMember $
--             newGetLaunchProfileMember
--
--         , requestDeleteStreamingSession $
--             newDeleteStreamingSession
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListStreamingSessions $
--             newListStreamingSessions
--
--         , requestGetStudio $
--             newGetStudio
--
--         , requestListStudioMembers $
--             newListStudioMembers
--
--         , requestDeleteStudioMember $
--             newDeleteStudioMember
--
--           ]

--     , testGroup "response"
--         [ responsePutLaunchProfileMembers $
--             newPutLaunchProfileMembersResponse
--
--         , responseCreateStudio $
--             newCreateStudioResponse
--
--         , responseUpdateLaunchProfileMember $
--             newUpdateLaunchProfileMemberResponse
--
--         , responseDeleteLaunchProfileMember $
--             newDeleteLaunchProfileMemberResponse
--
--         , responseListLaunchProfiles $
--             newListLaunchProfilesResponse
--
--         , responseCreateLaunchProfile $
--             newCreateLaunchProfileResponse
--
--         , responseListStreamingImages $
--             newListStreamingImagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartStudioSSOConfigurationRepair $
--             newStartStudioSSOConfigurationRepairResponse
--
--         , responseGetLaunchProfileInitialization $
--             newGetLaunchProfileInitializationResponse
--
--         , responseGetLaunchProfile $
--             newGetLaunchProfileResponse
--
--         , responseCreateStudioComponent $
--             newCreateStudioComponentResponse
--
--         , responseGetEula $
--             newGetEulaResponse
--
--         , responseListStudioComponents $
--             newListStudioComponentsResponse
--
--         , responseAcceptEulas $
--             newAcceptEulasResponse
--
--         , responseCreateStreamingSession $
--             newCreateStreamingSessionResponse
--
--         , responseDeleteStudioComponent $
--             newDeleteStudioComponentResponse
--
--         , responseUpdateStudioComponent $
--             newUpdateStudioComponentResponse
--
--         , responseGetStudioMember $
--             newGetStudioMemberResponse
--
--         , responseDeleteStudio $
--             newDeleteStudioResponse
--
--         , responseUpdateStudio $
--             newUpdateStudioResponse
--
--         , responseListStudios $
--             newListStudiosResponse
--
--         , responseGetStudioComponent $
--             newGetStudioComponentResponse
--
--         , responseListEulas $
--             newListEulasResponse
--
--         , responseGetStreamingSession $
--             newGetStreamingSessionResponse
--
--         , responseListLaunchProfileMembers $
--             newListLaunchProfileMembersResponse
--
--         , responseDeleteLaunchProfile $
--             newDeleteLaunchProfileResponse
--
--         , responseUpdateLaunchProfile $
--             newUpdateLaunchProfileResponse
--
--         , responseCreateStreamingImage $
--             newCreateStreamingImageResponse
--
--         , responseCreateStreamingSessionStream $
--             newCreateStreamingSessionStreamResponse
--
--         , responseGetLaunchProfileDetails $
--             newGetLaunchProfileDetailsResponse
--
--         , responsePutStudioMembers $
--             newPutStudioMembersResponse
--
--         , responseDeleteStreamingImage $
--             newDeleteStreamingImageResponse
--
--         , responseUpdateStreamingImage $
--             newUpdateStreamingImageResponse
--
--         , responseGetStreamingImage $
--             newGetStreamingImageResponse
--
--         , responseListEulaAcceptances $
--             newListEulaAcceptancesResponse
--
--         , responseGetStreamingSessionStream $
--             newGetStreamingSessionStreamResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetLaunchProfileMember $
--             newGetLaunchProfileMemberResponse
--
--         , responseDeleteStreamingSession $
--             newDeleteStreamingSessionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListStreamingSessions $
--             newListStreamingSessionsResponse
--
--         , responseGetStudio $
--             newGetStudioResponse
--
--         , responseListStudioMembers $
--             newListStudioMembersResponse
--
--         , responseDeleteStudioMember $
--             newDeleteStudioMemberResponse
--
--           ]
--     ]

-- Requests

requestPutLaunchProfileMembers :: PutLaunchProfileMembers -> TestTree
requestPutLaunchProfileMembers =
  req
    "PutLaunchProfileMembers"
    "fixture/PutLaunchProfileMembers.yaml"

requestCreateStudio :: CreateStudio -> TestTree
requestCreateStudio =
  req
    "CreateStudio"
    "fixture/CreateStudio.yaml"

requestUpdateLaunchProfileMember :: UpdateLaunchProfileMember -> TestTree
requestUpdateLaunchProfileMember =
  req
    "UpdateLaunchProfileMember"
    "fixture/UpdateLaunchProfileMember.yaml"

requestDeleteLaunchProfileMember :: DeleteLaunchProfileMember -> TestTree
requestDeleteLaunchProfileMember =
  req
    "DeleteLaunchProfileMember"
    "fixture/DeleteLaunchProfileMember.yaml"

requestListLaunchProfiles :: ListLaunchProfiles -> TestTree
requestListLaunchProfiles =
  req
    "ListLaunchProfiles"
    "fixture/ListLaunchProfiles.yaml"

requestCreateLaunchProfile :: CreateLaunchProfile -> TestTree
requestCreateLaunchProfile =
  req
    "CreateLaunchProfile"
    "fixture/CreateLaunchProfile.yaml"

requestListStreamingImages :: ListStreamingImages -> TestTree
requestListStreamingImages =
  req
    "ListStreamingImages"
    "fixture/ListStreamingImages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartStudioSSOConfigurationRepair :: StartStudioSSOConfigurationRepair -> TestTree
requestStartStudioSSOConfigurationRepair =
  req
    "StartStudioSSOConfigurationRepair"
    "fixture/StartStudioSSOConfigurationRepair.yaml"

requestGetLaunchProfileInitialization :: GetLaunchProfileInitialization -> TestTree
requestGetLaunchProfileInitialization =
  req
    "GetLaunchProfileInitialization"
    "fixture/GetLaunchProfileInitialization.yaml"

requestGetLaunchProfile :: GetLaunchProfile -> TestTree
requestGetLaunchProfile =
  req
    "GetLaunchProfile"
    "fixture/GetLaunchProfile.yaml"

requestCreateStudioComponent :: CreateStudioComponent -> TestTree
requestCreateStudioComponent =
  req
    "CreateStudioComponent"
    "fixture/CreateStudioComponent.yaml"

requestGetEula :: GetEula -> TestTree
requestGetEula =
  req
    "GetEula"
    "fixture/GetEula.yaml"

requestListStudioComponents :: ListStudioComponents -> TestTree
requestListStudioComponents =
  req
    "ListStudioComponents"
    "fixture/ListStudioComponents.yaml"

requestAcceptEulas :: AcceptEulas -> TestTree
requestAcceptEulas =
  req
    "AcceptEulas"
    "fixture/AcceptEulas.yaml"

requestCreateStreamingSession :: CreateStreamingSession -> TestTree
requestCreateStreamingSession =
  req
    "CreateStreamingSession"
    "fixture/CreateStreamingSession.yaml"

requestDeleteStudioComponent :: DeleteStudioComponent -> TestTree
requestDeleteStudioComponent =
  req
    "DeleteStudioComponent"
    "fixture/DeleteStudioComponent.yaml"

requestUpdateStudioComponent :: UpdateStudioComponent -> TestTree
requestUpdateStudioComponent =
  req
    "UpdateStudioComponent"
    "fixture/UpdateStudioComponent.yaml"

requestGetStudioMember :: GetStudioMember -> TestTree
requestGetStudioMember =
  req
    "GetStudioMember"
    "fixture/GetStudioMember.yaml"

requestDeleteStudio :: DeleteStudio -> TestTree
requestDeleteStudio =
  req
    "DeleteStudio"
    "fixture/DeleteStudio.yaml"

requestUpdateStudio :: UpdateStudio -> TestTree
requestUpdateStudio =
  req
    "UpdateStudio"
    "fixture/UpdateStudio.yaml"

requestListStudios :: ListStudios -> TestTree
requestListStudios =
  req
    "ListStudios"
    "fixture/ListStudios.yaml"

requestGetStudioComponent :: GetStudioComponent -> TestTree
requestGetStudioComponent =
  req
    "GetStudioComponent"
    "fixture/GetStudioComponent.yaml"

requestListEulas :: ListEulas -> TestTree
requestListEulas =
  req
    "ListEulas"
    "fixture/ListEulas.yaml"

requestGetStreamingSession :: GetStreamingSession -> TestTree
requestGetStreamingSession =
  req
    "GetStreamingSession"
    "fixture/GetStreamingSession.yaml"

requestListLaunchProfileMembers :: ListLaunchProfileMembers -> TestTree
requestListLaunchProfileMembers =
  req
    "ListLaunchProfileMembers"
    "fixture/ListLaunchProfileMembers.yaml"

requestDeleteLaunchProfile :: DeleteLaunchProfile -> TestTree
requestDeleteLaunchProfile =
  req
    "DeleteLaunchProfile"
    "fixture/DeleteLaunchProfile.yaml"

requestUpdateLaunchProfile :: UpdateLaunchProfile -> TestTree
requestUpdateLaunchProfile =
  req
    "UpdateLaunchProfile"
    "fixture/UpdateLaunchProfile.yaml"

requestCreateStreamingImage :: CreateStreamingImage -> TestTree
requestCreateStreamingImage =
  req
    "CreateStreamingImage"
    "fixture/CreateStreamingImage.yaml"

requestCreateStreamingSessionStream :: CreateStreamingSessionStream -> TestTree
requestCreateStreamingSessionStream =
  req
    "CreateStreamingSessionStream"
    "fixture/CreateStreamingSessionStream.yaml"

requestGetLaunchProfileDetails :: GetLaunchProfileDetails -> TestTree
requestGetLaunchProfileDetails =
  req
    "GetLaunchProfileDetails"
    "fixture/GetLaunchProfileDetails.yaml"

requestPutStudioMembers :: PutStudioMembers -> TestTree
requestPutStudioMembers =
  req
    "PutStudioMembers"
    "fixture/PutStudioMembers.yaml"

requestDeleteStreamingImage :: DeleteStreamingImage -> TestTree
requestDeleteStreamingImage =
  req
    "DeleteStreamingImage"
    "fixture/DeleteStreamingImage.yaml"

requestUpdateStreamingImage :: UpdateStreamingImage -> TestTree
requestUpdateStreamingImage =
  req
    "UpdateStreamingImage"
    "fixture/UpdateStreamingImage.yaml"

requestGetStreamingImage :: GetStreamingImage -> TestTree
requestGetStreamingImage =
  req
    "GetStreamingImage"
    "fixture/GetStreamingImage.yaml"

requestListEulaAcceptances :: ListEulaAcceptances -> TestTree
requestListEulaAcceptances =
  req
    "ListEulaAcceptances"
    "fixture/ListEulaAcceptances.yaml"

requestGetStreamingSessionStream :: GetStreamingSessionStream -> TestTree
requestGetStreamingSessionStream =
  req
    "GetStreamingSessionStream"
    "fixture/GetStreamingSessionStream.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetLaunchProfileMember :: GetLaunchProfileMember -> TestTree
requestGetLaunchProfileMember =
  req
    "GetLaunchProfileMember"
    "fixture/GetLaunchProfileMember.yaml"

requestDeleteStreamingSession :: DeleteStreamingSession -> TestTree
requestDeleteStreamingSession =
  req
    "DeleteStreamingSession"
    "fixture/DeleteStreamingSession.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListStreamingSessions :: ListStreamingSessions -> TestTree
requestListStreamingSessions =
  req
    "ListStreamingSessions"
    "fixture/ListStreamingSessions.yaml"

requestGetStudio :: GetStudio -> TestTree
requestGetStudio =
  req
    "GetStudio"
    "fixture/GetStudio.yaml"

requestListStudioMembers :: ListStudioMembers -> TestTree
requestListStudioMembers =
  req
    "ListStudioMembers"
    "fixture/ListStudioMembers.yaml"

requestDeleteStudioMember :: DeleteStudioMember -> TestTree
requestDeleteStudioMember =
  req
    "DeleteStudioMember"
    "fixture/DeleteStudioMember.yaml"

-- Responses

responsePutLaunchProfileMembers :: PutLaunchProfileMembersResponse -> TestTree
responsePutLaunchProfileMembers =
  res
    "PutLaunchProfileMembersResponse"
    "fixture/PutLaunchProfileMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLaunchProfileMembers)

responseCreateStudio :: CreateStudioResponse -> TestTree
responseCreateStudio =
  res
    "CreateStudioResponse"
    "fixture/CreateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudio)

responseUpdateLaunchProfileMember :: UpdateLaunchProfileMemberResponse -> TestTree
responseUpdateLaunchProfileMember =
  res
    "UpdateLaunchProfileMemberResponse"
    "fixture/UpdateLaunchProfileMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchProfileMember)

responseDeleteLaunchProfileMember :: DeleteLaunchProfileMemberResponse -> TestTree
responseDeleteLaunchProfileMember =
  res
    "DeleteLaunchProfileMemberResponse"
    "fixture/DeleteLaunchProfileMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchProfileMember)

responseListLaunchProfiles :: ListLaunchProfilesResponse -> TestTree
responseListLaunchProfiles =
  res
    "ListLaunchProfilesResponse"
    "fixture/ListLaunchProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLaunchProfiles)

responseCreateLaunchProfile :: CreateLaunchProfileResponse -> TestTree
responseCreateLaunchProfile =
  res
    "CreateLaunchProfileResponse"
    "fixture/CreateLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchProfile)

responseListStreamingImages :: ListStreamingImagesResponse -> TestTree
responseListStreamingImages =
  res
    "ListStreamingImagesResponse"
    "fixture/ListStreamingImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamingImages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartStudioSSOConfigurationRepair :: StartStudioSSOConfigurationRepairResponse -> TestTree
responseStartStudioSSOConfigurationRepair =
  res
    "StartStudioSSOConfigurationRepairResponse"
    "fixture/StartStudioSSOConfigurationRepairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStudioSSOConfigurationRepair)

responseGetLaunchProfileInitialization :: GetLaunchProfileInitializationResponse -> TestTree
responseGetLaunchProfileInitialization =
  res
    "GetLaunchProfileInitializationResponse"
    "fixture/GetLaunchProfileInitializationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfileInitialization)

responseGetLaunchProfile :: GetLaunchProfileResponse -> TestTree
responseGetLaunchProfile =
  res
    "GetLaunchProfileResponse"
    "fixture/GetLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfile)

responseCreateStudioComponent :: CreateStudioComponentResponse -> TestTree
responseCreateStudioComponent =
  res
    "CreateStudioComponentResponse"
    "fixture/CreateStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudioComponent)

responseGetEula :: GetEulaResponse -> TestTree
responseGetEula =
  res
    "GetEulaResponse"
    "fixture/GetEulaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEula)

responseListStudioComponents :: ListStudioComponentsResponse -> TestTree
responseListStudioComponents =
  res
    "ListStudioComponentsResponse"
    "fixture/ListStudioComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioComponents)

responseAcceptEulas :: AcceptEulasResponse -> TestTree
responseAcceptEulas =
  res
    "AcceptEulasResponse"
    "fixture/AcceptEulasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptEulas)

responseCreateStreamingSession :: CreateStreamingSessionResponse -> TestTree
responseCreateStreamingSession =
  res
    "CreateStreamingSessionResponse"
    "fixture/CreateStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingSession)

responseDeleteStudioComponent :: DeleteStudioComponentResponse -> TestTree
responseDeleteStudioComponent =
  res
    "DeleteStudioComponentResponse"
    "fixture/DeleteStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioComponent)

responseUpdateStudioComponent :: UpdateStudioComponentResponse -> TestTree
responseUpdateStudioComponent =
  res
    "UpdateStudioComponentResponse"
    "fixture/UpdateStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudioComponent)

responseGetStudioMember :: GetStudioMemberResponse -> TestTree
responseGetStudioMember =
  res
    "GetStudioMemberResponse"
    "fixture/GetStudioMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudioMember)

responseDeleteStudio :: DeleteStudioResponse -> TestTree
responseDeleteStudio =
  res
    "DeleteStudioResponse"
    "fixture/DeleteStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudio)

responseUpdateStudio :: UpdateStudioResponse -> TestTree
responseUpdateStudio =
  res
    "UpdateStudioResponse"
    "fixture/UpdateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudio)

responseListStudios :: ListStudiosResponse -> TestTree
responseListStudios =
  res
    "ListStudiosResponse"
    "fixture/ListStudiosResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudios)

responseGetStudioComponent :: GetStudioComponentResponse -> TestTree
responseGetStudioComponent =
  res
    "GetStudioComponentResponse"
    "fixture/GetStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudioComponent)

responseListEulas :: ListEulasResponse -> TestTree
responseListEulas =
  res
    "ListEulasResponse"
    "fixture/ListEulasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEulas)

responseGetStreamingSession :: GetStreamingSessionResponse -> TestTree
responseGetStreamingSession =
  res
    "GetStreamingSessionResponse"
    "fixture/GetStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingSession)

responseListLaunchProfileMembers :: ListLaunchProfileMembersResponse -> TestTree
responseListLaunchProfileMembers =
  res
    "ListLaunchProfileMembersResponse"
    "fixture/ListLaunchProfileMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLaunchProfileMembers)

responseDeleteLaunchProfile :: DeleteLaunchProfileResponse -> TestTree
responseDeleteLaunchProfile =
  res
    "DeleteLaunchProfileResponse"
    "fixture/DeleteLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchProfile)

responseUpdateLaunchProfile :: UpdateLaunchProfileResponse -> TestTree
responseUpdateLaunchProfile =
  res
    "UpdateLaunchProfileResponse"
    "fixture/UpdateLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchProfile)

responseCreateStreamingImage :: CreateStreamingImageResponse -> TestTree
responseCreateStreamingImage =
  res
    "CreateStreamingImageResponse"
    "fixture/CreateStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingImage)

responseCreateStreamingSessionStream :: CreateStreamingSessionStreamResponse -> TestTree
responseCreateStreamingSessionStream =
  res
    "CreateStreamingSessionStreamResponse"
    "fixture/CreateStreamingSessionStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingSessionStream)

responseGetLaunchProfileDetails :: GetLaunchProfileDetailsResponse -> TestTree
responseGetLaunchProfileDetails =
  res
    "GetLaunchProfileDetailsResponse"
    "fixture/GetLaunchProfileDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfileDetails)

responsePutStudioMembers :: PutStudioMembersResponse -> TestTree
responsePutStudioMembers =
  res
    "PutStudioMembersResponse"
    "fixture/PutStudioMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutStudioMembers)

responseDeleteStreamingImage :: DeleteStreamingImageResponse -> TestTree
responseDeleteStreamingImage =
  res
    "DeleteStreamingImageResponse"
    "fixture/DeleteStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamingImage)

responseUpdateStreamingImage :: UpdateStreamingImageResponse -> TestTree
responseUpdateStreamingImage =
  res
    "UpdateStreamingImageResponse"
    "fixture/UpdateStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStreamingImage)

responseGetStreamingImage :: GetStreamingImageResponse -> TestTree
responseGetStreamingImage =
  res
    "GetStreamingImageResponse"
    "fixture/GetStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingImage)

responseListEulaAcceptances :: ListEulaAcceptancesResponse -> TestTree
responseListEulaAcceptances =
  res
    "ListEulaAcceptancesResponse"
    "fixture/ListEulaAcceptancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEulaAcceptances)

responseGetStreamingSessionStream :: GetStreamingSessionStreamResponse -> TestTree
responseGetStreamingSessionStream =
  res
    "GetStreamingSessionStreamResponse"
    "fixture/GetStreamingSessionStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingSessionStream)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetLaunchProfileMember :: GetLaunchProfileMemberResponse -> TestTree
responseGetLaunchProfileMember =
  res
    "GetLaunchProfileMemberResponse"
    "fixture/GetLaunchProfileMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfileMember)

responseDeleteStreamingSession :: DeleteStreamingSessionResponse -> TestTree
responseDeleteStreamingSession =
  res
    "DeleteStreamingSessionResponse"
    "fixture/DeleteStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamingSession)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListStreamingSessions :: ListStreamingSessionsResponse -> TestTree
responseListStreamingSessions =
  res
    "ListStreamingSessionsResponse"
    "fixture/ListStreamingSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamingSessions)

responseGetStudio :: GetStudioResponse -> TestTree
responseGetStudio =
  res
    "GetStudioResponse"
    "fixture/GetStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudio)

responseListStudioMembers :: ListStudioMembersResponse -> TestTree
responseListStudioMembers =
  res
    "ListStudioMembersResponse"
    "fixture/ListStudioMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioMembers)

responseDeleteStudioMember :: DeleteStudioMemberResponse -> TestTree
responseDeleteStudioMember =
  res
    "DeleteStudioMemberResponse"
    "fixture/DeleteStudioMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioMember)

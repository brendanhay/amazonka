{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Nimble
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Nimble where

import Amazonka.Nimble
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Nimble.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptEulas $
--             newAcceptEulas
--
--         , requestCreateLaunchProfile $
--             newCreateLaunchProfile
--
--         , requestCreateStreamingImage $
--             newCreateStreamingImage
--
--         , requestCreateStreamingSession $
--             newCreateStreamingSession
--
--         , requestCreateStreamingSessionStream $
--             newCreateStreamingSessionStream
--
--         , requestCreateStudio $
--             newCreateStudio
--
--         , requestCreateStudioComponent $
--             newCreateStudioComponent
--
--         , requestDeleteLaunchProfile $
--             newDeleteLaunchProfile
--
--         , requestDeleteLaunchProfileMember $
--             newDeleteLaunchProfileMember
--
--         , requestDeleteStreamingImage $
--             newDeleteStreamingImage
--
--         , requestDeleteStreamingSession $
--             newDeleteStreamingSession
--
--         , requestDeleteStudio $
--             newDeleteStudio
--
--         , requestDeleteStudioComponent $
--             newDeleteStudioComponent
--
--         , requestDeleteStudioMember $
--             newDeleteStudioMember
--
--         , requestGetEula $
--             newGetEula
--
--         , requestGetLaunchProfile $
--             newGetLaunchProfile
--
--         , requestGetLaunchProfileDetails $
--             newGetLaunchProfileDetails
--
--         , requestGetLaunchProfileInitialization $
--             newGetLaunchProfileInitialization
--
--         , requestGetLaunchProfileMember $
--             newGetLaunchProfileMember
--
--         , requestGetStreamingImage $
--             newGetStreamingImage
--
--         , requestGetStreamingSession $
--             newGetStreamingSession
--
--         , requestGetStreamingSessionBackup $
--             newGetStreamingSessionBackup
--
--         , requestGetStreamingSessionStream $
--             newGetStreamingSessionStream
--
--         , requestGetStudio $
--             newGetStudio
--
--         , requestGetStudioComponent $
--             newGetStudioComponent
--
--         , requestGetStudioMember $
--             newGetStudioMember
--
--         , requestListEulaAcceptances $
--             newListEulaAcceptances
--
--         , requestListEulas $
--             newListEulas
--
--         , requestListLaunchProfileMembers $
--             newListLaunchProfileMembers
--
--         , requestListLaunchProfiles $
--             newListLaunchProfiles
--
--         , requestListStreamingImages $
--             newListStreamingImages
--
--         , requestListStreamingSessionBackups $
--             newListStreamingSessionBackups
--
--         , requestListStreamingSessions $
--             newListStreamingSessions
--
--         , requestListStudioComponents $
--             newListStudioComponents
--
--         , requestListStudioMembers $
--             newListStudioMembers
--
--         , requestListStudios $
--             newListStudios
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutLaunchProfileMembers $
--             newPutLaunchProfileMembers
--
--         , requestPutStudioMembers $
--             newPutStudioMembers
--
--         , requestStartStreamingSession $
--             newStartStreamingSession
--
--         , requestStartStudioSSOConfigurationRepair $
--             newStartStudioSSOConfigurationRepair
--
--         , requestStopStreamingSession $
--             newStopStreamingSession
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateLaunchProfile $
--             newUpdateLaunchProfile
--
--         , requestUpdateLaunchProfileMember $
--             newUpdateLaunchProfileMember
--
--         , requestUpdateStreamingImage $
--             newUpdateStreamingImage
--
--         , requestUpdateStudio $
--             newUpdateStudio
--
--         , requestUpdateStudioComponent $
--             newUpdateStudioComponent
--
--           ]

--     , testGroup "response"
--         [ responseAcceptEulas $
--             newAcceptEulasResponse
--
--         , responseCreateLaunchProfile $
--             newCreateLaunchProfileResponse
--
--         , responseCreateStreamingImage $
--             newCreateStreamingImageResponse
--
--         , responseCreateStreamingSession $
--             newCreateStreamingSessionResponse
--
--         , responseCreateStreamingSessionStream $
--             newCreateStreamingSessionStreamResponse
--
--         , responseCreateStudio $
--             newCreateStudioResponse
--
--         , responseCreateStudioComponent $
--             newCreateStudioComponentResponse
--
--         , responseDeleteLaunchProfile $
--             newDeleteLaunchProfileResponse
--
--         , responseDeleteLaunchProfileMember $
--             newDeleteLaunchProfileMemberResponse
--
--         , responseDeleteStreamingImage $
--             newDeleteStreamingImageResponse
--
--         , responseDeleteStreamingSession $
--             newDeleteStreamingSessionResponse
--
--         , responseDeleteStudio $
--             newDeleteStudioResponse
--
--         , responseDeleteStudioComponent $
--             newDeleteStudioComponentResponse
--
--         , responseDeleteStudioMember $
--             newDeleteStudioMemberResponse
--
--         , responseGetEula $
--             newGetEulaResponse
--
--         , responseGetLaunchProfile $
--             newGetLaunchProfileResponse
--
--         , responseGetLaunchProfileDetails $
--             newGetLaunchProfileDetailsResponse
--
--         , responseGetLaunchProfileInitialization $
--             newGetLaunchProfileInitializationResponse
--
--         , responseGetLaunchProfileMember $
--             newGetLaunchProfileMemberResponse
--
--         , responseGetStreamingImage $
--             newGetStreamingImageResponse
--
--         , responseGetStreamingSession $
--             newGetStreamingSessionResponse
--
--         , responseGetStreamingSessionBackup $
--             newGetStreamingSessionBackupResponse
--
--         , responseGetStreamingSessionStream $
--             newGetStreamingSessionStreamResponse
--
--         , responseGetStudio $
--             newGetStudioResponse
--
--         , responseGetStudioComponent $
--             newGetStudioComponentResponse
--
--         , responseGetStudioMember $
--             newGetStudioMemberResponse
--
--         , responseListEulaAcceptances $
--             newListEulaAcceptancesResponse
--
--         , responseListEulas $
--             newListEulasResponse
--
--         , responseListLaunchProfileMembers $
--             newListLaunchProfileMembersResponse
--
--         , responseListLaunchProfiles $
--             newListLaunchProfilesResponse
--
--         , responseListStreamingImages $
--             newListStreamingImagesResponse
--
--         , responseListStreamingSessionBackups $
--             newListStreamingSessionBackupsResponse
--
--         , responseListStreamingSessions $
--             newListStreamingSessionsResponse
--
--         , responseListStudioComponents $
--             newListStudioComponentsResponse
--
--         , responseListStudioMembers $
--             newListStudioMembersResponse
--
--         , responseListStudios $
--             newListStudiosResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutLaunchProfileMembers $
--             newPutLaunchProfileMembersResponse
--
--         , responsePutStudioMembers $
--             newPutStudioMembersResponse
--
--         , responseStartStreamingSession $
--             newStartStreamingSessionResponse
--
--         , responseStartStudioSSOConfigurationRepair $
--             newStartStudioSSOConfigurationRepairResponse
--
--         , responseStopStreamingSession $
--             newStopStreamingSessionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateLaunchProfile $
--             newUpdateLaunchProfileResponse
--
--         , responseUpdateLaunchProfileMember $
--             newUpdateLaunchProfileMemberResponse
--
--         , responseUpdateStreamingImage $
--             newUpdateStreamingImageResponse
--
--         , responseUpdateStudio $
--             newUpdateStudioResponse
--
--         , responseUpdateStudioComponent $
--             newUpdateStudioComponentResponse
--
--           ]
--     ]

-- Requests

requestAcceptEulas :: AcceptEulas -> TestTree
requestAcceptEulas =
  req
    "AcceptEulas"
    "fixture/AcceptEulas.yaml"

requestCreateLaunchProfile :: CreateLaunchProfile -> TestTree
requestCreateLaunchProfile =
  req
    "CreateLaunchProfile"
    "fixture/CreateLaunchProfile.yaml"

requestCreateStreamingImage :: CreateStreamingImage -> TestTree
requestCreateStreamingImage =
  req
    "CreateStreamingImage"
    "fixture/CreateStreamingImage.yaml"

requestCreateStreamingSession :: CreateStreamingSession -> TestTree
requestCreateStreamingSession =
  req
    "CreateStreamingSession"
    "fixture/CreateStreamingSession.yaml"

requestCreateStreamingSessionStream :: CreateStreamingSessionStream -> TestTree
requestCreateStreamingSessionStream =
  req
    "CreateStreamingSessionStream"
    "fixture/CreateStreamingSessionStream.yaml"

requestCreateStudio :: CreateStudio -> TestTree
requestCreateStudio =
  req
    "CreateStudio"
    "fixture/CreateStudio.yaml"

requestCreateStudioComponent :: CreateStudioComponent -> TestTree
requestCreateStudioComponent =
  req
    "CreateStudioComponent"
    "fixture/CreateStudioComponent.yaml"

requestDeleteLaunchProfile :: DeleteLaunchProfile -> TestTree
requestDeleteLaunchProfile =
  req
    "DeleteLaunchProfile"
    "fixture/DeleteLaunchProfile.yaml"

requestDeleteLaunchProfileMember :: DeleteLaunchProfileMember -> TestTree
requestDeleteLaunchProfileMember =
  req
    "DeleteLaunchProfileMember"
    "fixture/DeleteLaunchProfileMember.yaml"

requestDeleteStreamingImage :: DeleteStreamingImage -> TestTree
requestDeleteStreamingImage =
  req
    "DeleteStreamingImage"
    "fixture/DeleteStreamingImage.yaml"

requestDeleteStreamingSession :: DeleteStreamingSession -> TestTree
requestDeleteStreamingSession =
  req
    "DeleteStreamingSession"
    "fixture/DeleteStreamingSession.yaml"

requestDeleteStudio :: DeleteStudio -> TestTree
requestDeleteStudio =
  req
    "DeleteStudio"
    "fixture/DeleteStudio.yaml"

requestDeleteStudioComponent :: DeleteStudioComponent -> TestTree
requestDeleteStudioComponent =
  req
    "DeleteStudioComponent"
    "fixture/DeleteStudioComponent.yaml"

requestDeleteStudioMember :: DeleteStudioMember -> TestTree
requestDeleteStudioMember =
  req
    "DeleteStudioMember"
    "fixture/DeleteStudioMember.yaml"

requestGetEula :: GetEula -> TestTree
requestGetEula =
  req
    "GetEula"
    "fixture/GetEula.yaml"

requestGetLaunchProfile :: GetLaunchProfile -> TestTree
requestGetLaunchProfile =
  req
    "GetLaunchProfile"
    "fixture/GetLaunchProfile.yaml"

requestGetLaunchProfileDetails :: GetLaunchProfileDetails -> TestTree
requestGetLaunchProfileDetails =
  req
    "GetLaunchProfileDetails"
    "fixture/GetLaunchProfileDetails.yaml"

requestGetLaunchProfileInitialization :: GetLaunchProfileInitialization -> TestTree
requestGetLaunchProfileInitialization =
  req
    "GetLaunchProfileInitialization"
    "fixture/GetLaunchProfileInitialization.yaml"

requestGetLaunchProfileMember :: GetLaunchProfileMember -> TestTree
requestGetLaunchProfileMember =
  req
    "GetLaunchProfileMember"
    "fixture/GetLaunchProfileMember.yaml"

requestGetStreamingImage :: GetStreamingImage -> TestTree
requestGetStreamingImage =
  req
    "GetStreamingImage"
    "fixture/GetStreamingImage.yaml"

requestGetStreamingSession :: GetStreamingSession -> TestTree
requestGetStreamingSession =
  req
    "GetStreamingSession"
    "fixture/GetStreamingSession.yaml"

requestGetStreamingSessionBackup :: GetStreamingSessionBackup -> TestTree
requestGetStreamingSessionBackup =
  req
    "GetStreamingSessionBackup"
    "fixture/GetStreamingSessionBackup.yaml"

requestGetStreamingSessionStream :: GetStreamingSessionStream -> TestTree
requestGetStreamingSessionStream =
  req
    "GetStreamingSessionStream"
    "fixture/GetStreamingSessionStream.yaml"

requestGetStudio :: GetStudio -> TestTree
requestGetStudio =
  req
    "GetStudio"
    "fixture/GetStudio.yaml"

requestGetStudioComponent :: GetStudioComponent -> TestTree
requestGetStudioComponent =
  req
    "GetStudioComponent"
    "fixture/GetStudioComponent.yaml"

requestGetStudioMember :: GetStudioMember -> TestTree
requestGetStudioMember =
  req
    "GetStudioMember"
    "fixture/GetStudioMember.yaml"

requestListEulaAcceptances :: ListEulaAcceptances -> TestTree
requestListEulaAcceptances =
  req
    "ListEulaAcceptances"
    "fixture/ListEulaAcceptances.yaml"

requestListEulas :: ListEulas -> TestTree
requestListEulas =
  req
    "ListEulas"
    "fixture/ListEulas.yaml"

requestListLaunchProfileMembers :: ListLaunchProfileMembers -> TestTree
requestListLaunchProfileMembers =
  req
    "ListLaunchProfileMembers"
    "fixture/ListLaunchProfileMembers.yaml"

requestListLaunchProfiles :: ListLaunchProfiles -> TestTree
requestListLaunchProfiles =
  req
    "ListLaunchProfiles"
    "fixture/ListLaunchProfiles.yaml"

requestListStreamingImages :: ListStreamingImages -> TestTree
requestListStreamingImages =
  req
    "ListStreamingImages"
    "fixture/ListStreamingImages.yaml"

requestListStreamingSessionBackups :: ListStreamingSessionBackups -> TestTree
requestListStreamingSessionBackups =
  req
    "ListStreamingSessionBackups"
    "fixture/ListStreamingSessionBackups.yaml"

requestListStreamingSessions :: ListStreamingSessions -> TestTree
requestListStreamingSessions =
  req
    "ListStreamingSessions"
    "fixture/ListStreamingSessions.yaml"

requestListStudioComponents :: ListStudioComponents -> TestTree
requestListStudioComponents =
  req
    "ListStudioComponents"
    "fixture/ListStudioComponents.yaml"

requestListStudioMembers :: ListStudioMembers -> TestTree
requestListStudioMembers =
  req
    "ListStudioMembers"
    "fixture/ListStudioMembers.yaml"

requestListStudios :: ListStudios -> TestTree
requestListStudios =
  req
    "ListStudios"
    "fixture/ListStudios.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutLaunchProfileMembers :: PutLaunchProfileMembers -> TestTree
requestPutLaunchProfileMembers =
  req
    "PutLaunchProfileMembers"
    "fixture/PutLaunchProfileMembers.yaml"

requestPutStudioMembers :: PutStudioMembers -> TestTree
requestPutStudioMembers =
  req
    "PutStudioMembers"
    "fixture/PutStudioMembers.yaml"

requestStartStreamingSession :: StartStreamingSession -> TestTree
requestStartStreamingSession =
  req
    "StartStreamingSession"
    "fixture/StartStreamingSession.yaml"

requestStartStudioSSOConfigurationRepair :: StartStudioSSOConfigurationRepair -> TestTree
requestStartStudioSSOConfigurationRepair =
  req
    "StartStudioSSOConfigurationRepair"
    "fixture/StartStudioSSOConfigurationRepair.yaml"

requestStopStreamingSession :: StopStreamingSession -> TestTree
requestStopStreamingSession =
  req
    "StopStreamingSession"
    "fixture/StopStreamingSession.yaml"

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

requestUpdateLaunchProfile :: UpdateLaunchProfile -> TestTree
requestUpdateLaunchProfile =
  req
    "UpdateLaunchProfile"
    "fixture/UpdateLaunchProfile.yaml"

requestUpdateLaunchProfileMember :: UpdateLaunchProfileMember -> TestTree
requestUpdateLaunchProfileMember =
  req
    "UpdateLaunchProfileMember"
    "fixture/UpdateLaunchProfileMember.yaml"

requestUpdateStreamingImage :: UpdateStreamingImage -> TestTree
requestUpdateStreamingImage =
  req
    "UpdateStreamingImage"
    "fixture/UpdateStreamingImage.yaml"

requestUpdateStudio :: UpdateStudio -> TestTree
requestUpdateStudio =
  req
    "UpdateStudio"
    "fixture/UpdateStudio.yaml"

requestUpdateStudioComponent :: UpdateStudioComponent -> TestTree
requestUpdateStudioComponent =
  req
    "UpdateStudioComponent"
    "fixture/UpdateStudioComponent.yaml"

-- Responses

responseAcceptEulas :: AcceptEulasResponse -> TestTree
responseAcceptEulas =
  res
    "AcceptEulasResponse"
    "fixture/AcceptEulasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptEulas)

responseCreateLaunchProfile :: CreateLaunchProfileResponse -> TestTree
responseCreateLaunchProfile =
  res
    "CreateLaunchProfileResponse"
    "fixture/CreateLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLaunchProfile)

responseCreateStreamingImage :: CreateStreamingImageResponse -> TestTree
responseCreateStreamingImage =
  res
    "CreateStreamingImageResponse"
    "fixture/CreateStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingImage)

responseCreateStreamingSession :: CreateStreamingSessionResponse -> TestTree
responseCreateStreamingSession =
  res
    "CreateStreamingSessionResponse"
    "fixture/CreateStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingSession)

responseCreateStreamingSessionStream :: CreateStreamingSessionStreamResponse -> TestTree
responseCreateStreamingSessionStream =
  res
    "CreateStreamingSessionStreamResponse"
    "fixture/CreateStreamingSessionStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingSessionStream)

responseCreateStudio :: CreateStudioResponse -> TestTree
responseCreateStudio =
  res
    "CreateStudioResponse"
    "fixture/CreateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudio)

responseCreateStudioComponent :: CreateStudioComponentResponse -> TestTree
responseCreateStudioComponent =
  res
    "CreateStudioComponentResponse"
    "fixture/CreateStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStudioComponent)

responseDeleteLaunchProfile :: DeleteLaunchProfileResponse -> TestTree
responseDeleteLaunchProfile =
  res
    "DeleteLaunchProfileResponse"
    "fixture/DeleteLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchProfile)

responseDeleteLaunchProfileMember :: DeleteLaunchProfileMemberResponse -> TestTree
responseDeleteLaunchProfileMember =
  res
    "DeleteLaunchProfileMemberResponse"
    "fixture/DeleteLaunchProfileMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLaunchProfileMember)

responseDeleteStreamingImage :: DeleteStreamingImageResponse -> TestTree
responseDeleteStreamingImage =
  res
    "DeleteStreamingImageResponse"
    "fixture/DeleteStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamingImage)

responseDeleteStreamingSession :: DeleteStreamingSessionResponse -> TestTree
responseDeleteStreamingSession =
  res
    "DeleteStreamingSessionResponse"
    "fixture/DeleteStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStreamingSession)

responseDeleteStudio :: DeleteStudioResponse -> TestTree
responseDeleteStudio =
  res
    "DeleteStudioResponse"
    "fixture/DeleteStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudio)

responseDeleteStudioComponent :: DeleteStudioComponentResponse -> TestTree
responseDeleteStudioComponent =
  res
    "DeleteStudioComponentResponse"
    "fixture/DeleteStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioComponent)

responseDeleteStudioMember :: DeleteStudioMemberResponse -> TestTree
responseDeleteStudioMember =
  res
    "DeleteStudioMemberResponse"
    "fixture/DeleteStudioMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStudioMember)

responseGetEula :: GetEulaResponse -> TestTree
responseGetEula =
  res
    "GetEulaResponse"
    "fixture/GetEulaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEula)

responseGetLaunchProfile :: GetLaunchProfileResponse -> TestTree
responseGetLaunchProfile =
  res
    "GetLaunchProfileResponse"
    "fixture/GetLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfile)

responseGetLaunchProfileDetails :: GetLaunchProfileDetailsResponse -> TestTree
responseGetLaunchProfileDetails =
  res
    "GetLaunchProfileDetailsResponse"
    "fixture/GetLaunchProfileDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfileDetails)

responseGetLaunchProfileInitialization :: GetLaunchProfileInitializationResponse -> TestTree
responseGetLaunchProfileInitialization =
  res
    "GetLaunchProfileInitializationResponse"
    "fixture/GetLaunchProfileInitializationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfileInitialization)

responseGetLaunchProfileMember :: GetLaunchProfileMemberResponse -> TestTree
responseGetLaunchProfileMember =
  res
    "GetLaunchProfileMemberResponse"
    "fixture/GetLaunchProfileMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLaunchProfileMember)

responseGetStreamingImage :: GetStreamingImageResponse -> TestTree
responseGetStreamingImage =
  res
    "GetStreamingImageResponse"
    "fixture/GetStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingImage)

responseGetStreamingSession :: GetStreamingSessionResponse -> TestTree
responseGetStreamingSession =
  res
    "GetStreamingSessionResponse"
    "fixture/GetStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingSession)

responseGetStreamingSessionBackup :: GetStreamingSessionBackupResponse -> TestTree
responseGetStreamingSessionBackup =
  res
    "GetStreamingSessionBackupResponse"
    "fixture/GetStreamingSessionBackupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingSessionBackup)

responseGetStreamingSessionStream :: GetStreamingSessionStreamResponse -> TestTree
responseGetStreamingSessionStream =
  res
    "GetStreamingSessionStreamResponse"
    "fixture/GetStreamingSessionStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStreamingSessionStream)

responseGetStudio :: GetStudioResponse -> TestTree
responseGetStudio =
  res
    "GetStudioResponse"
    "fixture/GetStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudio)

responseGetStudioComponent :: GetStudioComponentResponse -> TestTree
responseGetStudioComponent =
  res
    "GetStudioComponentResponse"
    "fixture/GetStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudioComponent)

responseGetStudioMember :: GetStudioMemberResponse -> TestTree
responseGetStudioMember =
  res
    "GetStudioMemberResponse"
    "fixture/GetStudioMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStudioMember)

responseListEulaAcceptances :: ListEulaAcceptancesResponse -> TestTree
responseListEulaAcceptances =
  res
    "ListEulaAcceptancesResponse"
    "fixture/ListEulaAcceptancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEulaAcceptances)

responseListEulas :: ListEulasResponse -> TestTree
responseListEulas =
  res
    "ListEulasResponse"
    "fixture/ListEulasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEulas)

responseListLaunchProfileMembers :: ListLaunchProfileMembersResponse -> TestTree
responseListLaunchProfileMembers =
  res
    "ListLaunchProfileMembersResponse"
    "fixture/ListLaunchProfileMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLaunchProfileMembers)

responseListLaunchProfiles :: ListLaunchProfilesResponse -> TestTree
responseListLaunchProfiles =
  res
    "ListLaunchProfilesResponse"
    "fixture/ListLaunchProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLaunchProfiles)

responseListStreamingImages :: ListStreamingImagesResponse -> TestTree
responseListStreamingImages =
  res
    "ListStreamingImagesResponse"
    "fixture/ListStreamingImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamingImages)

responseListStreamingSessionBackups :: ListStreamingSessionBackupsResponse -> TestTree
responseListStreamingSessionBackups =
  res
    "ListStreamingSessionBackupsResponse"
    "fixture/ListStreamingSessionBackupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamingSessionBackups)

responseListStreamingSessions :: ListStreamingSessionsResponse -> TestTree
responseListStreamingSessions =
  res
    "ListStreamingSessionsResponse"
    "fixture/ListStreamingSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStreamingSessions)

responseListStudioComponents :: ListStudioComponentsResponse -> TestTree
responseListStudioComponents =
  res
    "ListStudioComponentsResponse"
    "fixture/ListStudioComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioComponents)

responseListStudioMembers :: ListStudioMembersResponse -> TestTree
responseListStudioMembers =
  res
    "ListStudioMembersResponse"
    "fixture/ListStudioMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudioMembers)

responseListStudios :: ListStudiosResponse -> TestTree
responseListStudios =
  res
    "ListStudiosResponse"
    "fixture/ListStudiosResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStudios)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutLaunchProfileMembers :: PutLaunchProfileMembersResponse -> TestTree
responsePutLaunchProfileMembers =
  res
    "PutLaunchProfileMembersResponse"
    "fixture/PutLaunchProfileMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutLaunchProfileMembers)

responsePutStudioMembers :: PutStudioMembersResponse -> TestTree
responsePutStudioMembers =
  res
    "PutStudioMembersResponse"
    "fixture/PutStudioMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutStudioMembers)

responseStartStreamingSession :: StartStreamingSessionResponse -> TestTree
responseStartStreamingSession =
  res
    "StartStreamingSessionResponse"
    "fixture/StartStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStreamingSession)

responseStartStudioSSOConfigurationRepair :: StartStudioSSOConfigurationRepairResponse -> TestTree
responseStartStudioSSOConfigurationRepair =
  res
    "StartStudioSSOConfigurationRepairResponse"
    "fixture/StartStudioSSOConfigurationRepairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStudioSSOConfigurationRepair)

responseStopStreamingSession :: StopStreamingSessionResponse -> TestTree
responseStopStreamingSession =
  res
    "StopStreamingSessionResponse"
    "fixture/StopStreamingSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopStreamingSession)

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

responseUpdateLaunchProfile :: UpdateLaunchProfileResponse -> TestTree
responseUpdateLaunchProfile =
  res
    "UpdateLaunchProfileResponse"
    "fixture/UpdateLaunchProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchProfile)

responseUpdateLaunchProfileMember :: UpdateLaunchProfileMemberResponse -> TestTree
responseUpdateLaunchProfileMember =
  res
    "UpdateLaunchProfileMemberResponse"
    "fixture/UpdateLaunchProfileMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLaunchProfileMember)

responseUpdateStreamingImage :: UpdateStreamingImageResponse -> TestTree
responseUpdateStreamingImage =
  res
    "UpdateStreamingImageResponse"
    "fixture/UpdateStreamingImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStreamingImage)

responseUpdateStudio :: UpdateStudioResponse -> TestTree
responseUpdateStudio =
  res
    "UpdateStudioResponse"
    "fixture/UpdateStudioResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudio)

responseUpdateStudioComponent :: UpdateStudioComponentResponse -> TestTree
responseUpdateStudioComponent =
  res
    "UpdateStudioComponentResponse"
    "fixture/UpdateStudioComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStudioComponent)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RolesAnywhere
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RolesAnywhere where

import Amazonka.RolesAnywhere
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RolesAnywhere.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateProfile $
--             newCreateProfile
--
--         , requestCreateTrustAnchor $
--             newCreateTrustAnchor
--
--         , requestDeleteCrl $
--             newDeleteCrl
--
--         , requestDeleteProfile $
--             newDeleteProfile
--
--         , requestDeleteTrustAnchor $
--             newDeleteTrustAnchor
--
--         , requestDisableCrl $
--             newDisableCrl
--
--         , requestDisableProfile $
--             newDisableProfile
--
--         , requestDisableTrustAnchor $
--             newDisableTrustAnchor
--
--         , requestEnableCrl $
--             newEnableCrl
--
--         , requestEnableProfile $
--             newEnableProfile
--
--         , requestEnableTrustAnchor $
--             newEnableTrustAnchor
--
--         , requestGetCrl $
--             newGetCrl
--
--         , requestGetProfile $
--             newGetProfile
--
--         , requestGetSubject $
--             newGetSubject
--
--         , requestGetTrustAnchor $
--             newGetTrustAnchor
--
--         , requestImportCrl $
--             newImportCrl
--
--         , requestListCrls $
--             newListCrls
--
--         , requestListProfiles $
--             newListProfiles
--
--         , requestListSubjects $
--             newListSubjects
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTrustAnchors $
--             newListTrustAnchors
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCrl $
--             newUpdateCrl
--
--         , requestUpdateProfile $
--             newUpdateProfile
--
--         , requestUpdateTrustAnchor $
--             newUpdateTrustAnchor
--
--           ]

--     , testGroup "response"
--         [ responseCreateProfile $
--             newProfileDetailResponse
--
--         , responseCreateTrustAnchor $
--             newTrustAnchorDetailResponse
--
--         , responseDeleteCrl $
--             newCrlDetailResponse
--
--         , responseDeleteProfile $
--             newProfileDetailResponse
--
--         , responseDeleteTrustAnchor $
--             newTrustAnchorDetailResponse
--
--         , responseDisableCrl $
--             newCrlDetailResponse
--
--         , responseDisableProfile $
--             newProfileDetailResponse
--
--         , responseDisableTrustAnchor $
--             newTrustAnchorDetailResponse
--
--         , responseEnableCrl $
--             newCrlDetailResponse
--
--         , responseEnableProfile $
--             newProfileDetailResponse
--
--         , responseEnableTrustAnchor $
--             newTrustAnchorDetailResponse
--
--         , responseGetCrl $
--             newCrlDetailResponse
--
--         , responseGetProfile $
--             newProfileDetailResponse
--
--         , responseGetSubject $
--             newGetSubjectResponse
--
--         , responseGetTrustAnchor $
--             newTrustAnchorDetailResponse
--
--         , responseImportCrl $
--             newCrlDetailResponse
--
--         , responseListCrls $
--             newListCrlsResponse
--
--         , responseListProfiles $
--             newListProfilesResponse
--
--         , responseListSubjects $
--             newListSubjectsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTrustAnchors $
--             newListTrustAnchorsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCrl $
--             newCrlDetailResponse
--
--         , responseUpdateProfile $
--             newProfileDetailResponse
--
--         , responseUpdateTrustAnchor $
--             newTrustAnchorDetailResponse
--
--           ]
--     ]

-- Requests

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestCreateTrustAnchor :: CreateTrustAnchor -> TestTree
requestCreateTrustAnchor =
  req
    "CreateTrustAnchor"
    "fixture/CreateTrustAnchor.yaml"

requestDeleteCrl :: DeleteCrl -> TestTree
requestDeleteCrl =
  req
    "DeleteCrl"
    "fixture/DeleteCrl.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestDeleteTrustAnchor :: DeleteTrustAnchor -> TestTree
requestDeleteTrustAnchor =
  req
    "DeleteTrustAnchor"
    "fixture/DeleteTrustAnchor.yaml"

requestDisableCrl :: DisableCrl -> TestTree
requestDisableCrl =
  req
    "DisableCrl"
    "fixture/DisableCrl.yaml"

requestDisableProfile :: DisableProfile -> TestTree
requestDisableProfile =
  req
    "DisableProfile"
    "fixture/DisableProfile.yaml"

requestDisableTrustAnchor :: DisableTrustAnchor -> TestTree
requestDisableTrustAnchor =
  req
    "DisableTrustAnchor"
    "fixture/DisableTrustAnchor.yaml"

requestEnableCrl :: EnableCrl -> TestTree
requestEnableCrl =
  req
    "EnableCrl"
    "fixture/EnableCrl.yaml"

requestEnableProfile :: EnableProfile -> TestTree
requestEnableProfile =
  req
    "EnableProfile"
    "fixture/EnableProfile.yaml"

requestEnableTrustAnchor :: EnableTrustAnchor -> TestTree
requestEnableTrustAnchor =
  req
    "EnableTrustAnchor"
    "fixture/EnableTrustAnchor.yaml"

requestGetCrl :: GetCrl -> TestTree
requestGetCrl =
  req
    "GetCrl"
    "fixture/GetCrl.yaml"

requestGetProfile :: GetProfile -> TestTree
requestGetProfile =
  req
    "GetProfile"
    "fixture/GetProfile.yaml"

requestGetSubject :: GetSubject -> TestTree
requestGetSubject =
  req
    "GetSubject"
    "fixture/GetSubject.yaml"

requestGetTrustAnchor :: GetTrustAnchor -> TestTree
requestGetTrustAnchor =
  req
    "GetTrustAnchor"
    "fixture/GetTrustAnchor.yaml"

requestImportCrl :: ImportCrl -> TestTree
requestImportCrl =
  req
    "ImportCrl"
    "fixture/ImportCrl.yaml"

requestListCrls :: ListCrls -> TestTree
requestListCrls =
  req
    "ListCrls"
    "fixture/ListCrls.yaml"

requestListProfiles :: ListProfiles -> TestTree
requestListProfiles =
  req
    "ListProfiles"
    "fixture/ListProfiles.yaml"

requestListSubjects :: ListSubjects -> TestTree
requestListSubjects =
  req
    "ListSubjects"
    "fixture/ListSubjects.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTrustAnchors :: ListTrustAnchors -> TestTree
requestListTrustAnchors =
  req
    "ListTrustAnchors"
    "fixture/ListTrustAnchors.yaml"

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

requestUpdateCrl :: UpdateCrl -> TestTree
requestUpdateCrl =
  req
    "UpdateCrl"
    "fixture/UpdateCrl.yaml"

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile =
  req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

requestUpdateTrustAnchor :: UpdateTrustAnchor -> TestTree
requestUpdateTrustAnchor =
  req
    "UpdateTrustAnchor"
    "fixture/UpdateTrustAnchor.yaml"

-- Responses

responseCreateProfile :: ProfileDetailResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfile)

responseCreateTrustAnchor :: TrustAnchorDetailResponse -> TestTree
responseCreateTrustAnchor =
  res
    "CreateTrustAnchorResponse"
    "fixture/CreateTrustAnchorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrustAnchor)

responseDeleteCrl :: CrlDetailResponse -> TestTree
responseDeleteCrl =
  res
    "DeleteCrlResponse"
    "fixture/DeleteCrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCrl)

responseDeleteProfile :: ProfileDetailResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfile)

responseDeleteTrustAnchor :: TrustAnchorDetailResponse -> TestTree
responseDeleteTrustAnchor =
  res
    "DeleteTrustAnchorResponse"
    "fixture/DeleteTrustAnchorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrustAnchor)

responseDisableCrl :: CrlDetailResponse -> TestTree
responseDisableCrl =
  res
    "DisableCrlResponse"
    "fixture/DisableCrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableCrl)

responseDisableProfile :: ProfileDetailResponse -> TestTree
responseDisableProfile =
  res
    "DisableProfileResponse"
    "fixture/DisableProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableProfile)

responseDisableTrustAnchor :: TrustAnchorDetailResponse -> TestTree
responseDisableTrustAnchor =
  res
    "DisableTrustAnchorResponse"
    "fixture/DisableTrustAnchorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableTrustAnchor)

responseEnableCrl :: CrlDetailResponse -> TestTree
responseEnableCrl =
  res
    "EnableCrlResponse"
    "fixture/EnableCrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableCrl)

responseEnableProfile :: ProfileDetailResponse -> TestTree
responseEnableProfile =
  res
    "EnableProfileResponse"
    "fixture/EnableProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableProfile)

responseEnableTrustAnchor :: TrustAnchorDetailResponse -> TestTree
responseEnableTrustAnchor =
  res
    "EnableTrustAnchorResponse"
    "fixture/EnableTrustAnchorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableTrustAnchor)

responseGetCrl :: CrlDetailResponse -> TestTree
responseGetCrl =
  res
    "GetCrlResponse"
    "fixture/GetCrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCrl)

responseGetProfile :: ProfileDetailResponse -> TestTree
responseGetProfile =
  res
    "GetProfileResponse"
    "fixture/GetProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfile)

responseGetSubject :: GetSubjectResponse -> TestTree
responseGetSubject =
  res
    "GetSubjectResponse"
    "fixture/GetSubjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubject)

responseGetTrustAnchor :: TrustAnchorDetailResponse -> TestTree
responseGetTrustAnchor =
  res
    "GetTrustAnchorResponse"
    "fixture/GetTrustAnchorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrustAnchor)

responseImportCrl :: CrlDetailResponse -> TestTree
responseImportCrl =
  res
    "ImportCrlResponse"
    "fixture/ImportCrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportCrl)

responseListCrls :: ListCrlsResponse -> TestTree
responseListCrls =
  res
    "ListCrlsResponse"
    "fixture/ListCrlsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCrls)

responseListProfiles :: ListProfilesResponse -> TestTree
responseListProfiles =
  res
    "ListProfilesResponse"
    "fixture/ListProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfiles)

responseListSubjects :: ListSubjectsResponse -> TestTree
responseListSubjects =
  res
    "ListSubjectsResponse"
    "fixture/ListSubjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubjects)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTrustAnchors :: ListTrustAnchorsResponse -> TestTree
responseListTrustAnchors =
  res
    "ListTrustAnchorsResponse"
    "fixture/ListTrustAnchorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrustAnchors)

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

responseUpdateCrl :: CrlDetailResponse -> TestTree
responseUpdateCrl =
  res
    "UpdateCrlResponse"
    "fixture/UpdateCrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCrl)

responseUpdateProfile :: ProfileDetailResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfile)

responseUpdateTrustAnchor :: TrustAnchorDetailResponse -> TestTree
responseUpdateTrustAnchor =
  res
    "UpdateTrustAnchorResponse"
    "fixture/UpdateTrustAnchorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrustAnchor)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Signer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Signer where

import qualified Data.Proxy as Proxy
import Network.AWS.Signer
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Signer.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartSigningJob $
--             newStartSigningJob
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRevokeSigningProfile $
--             newRevokeSigningProfile
--
--         , requestCancelSigningProfile $
--             newCancelSigningProfile
--
--         , requestPutSigningProfile $
--             newPutSigningProfile
--
--         , requestAddProfilePermission $
--             newAddProfilePermission
--
--         , requestListSigningProfiles $
--             newListSigningProfiles
--
--         , requestListProfilePermissions $
--             newListProfilePermissions
--
--         , requestRevokeSignature $
--             newRevokeSignature
--
--         , requestGetSigningPlatform $
--             newGetSigningPlatform
--
--         , requestListSigningPlatforms $
--             newListSigningPlatforms
--
--         , requestListSigningJobs $
--             newListSigningJobs
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestRemoveProfilePermission $
--             newRemoveProfilePermission
--
--         , requestGetSigningProfile $
--             newGetSigningProfile
--
--         , requestDescribeSigningJob $
--             newDescribeSigningJob
--
--           ]

--     , testGroup "response"
--         [ responseStartSigningJob $
--             newStartSigningJobResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRevokeSigningProfile $
--             newRevokeSigningProfileResponse
--
--         , responseCancelSigningProfile $
--             newCancelSigningProfileResponse
--
--         , responsePutSigningProfile $
--             newPutSigningProfileResponse
--
--         , responseAddProfilePermission $
--             newAddProfilePermissionResponse
--
--         , responseListSigningProfiles $
--             newListSigningProfilesResponse
--
--         , responseListProfilePermissions $
--             newListProfilePermissionsResponse
--
--         , responseRevokeSignature $
--             newRevokeSignatureResponse
--
--         , responseGetSigningPlatform $
--             newGetSigningPlatformResponse
--
--         , responseListSigningPlatforms $
--             newListSigningPlatformsResponse
--
--         , responseListSigningJobs $
--             newListSigningJobsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseRemoveProfilePermission $
--             newRemoveProfilePermissionResponse
--
--         , responseGetSigningProfile $
--             newGetSigningProfileResponse
--
--         , responseDescribeSigningJob $
--             newDescribeSigningJobResponse
--
--           ]
--     ]

-- Requests

requestStartSigningJob :: StartSigningJob -> TestTree
requestStartSigningJob =
  req
    "StartSigningJob"
    "fixture/StartSigningJob.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRevokeSigningProfile :: RevokeSigningProfile -> TestTree
requestRevokeSigningProfile =
  req
    "RevokeSigningProfile"
    "fixture/RevokeSigningProfile.yaml"

requestCancelSigningProfile :: CancelSigningProfile -> TestTree
requestCancelSigningProfile =
  req
    "CancelSigningProfile"
    "fixture/CancelSigningProfile.yaml"

requestPutSigningProfile :: PutSigningProfile -> TestTree
requestPutSigningProfile =
  req
    "PutSigningProfile"
    "fixture/PutSigningProfile.yaml"

requestAddProfilePermission :: AddProfilePermission -> TestTree
requestAddProfilePermission =
  req
    "AddProfilePermission"
    "fixture/AddProfilePermission.yaml"

requestListSigningProfiles :: ListSigningProfiles -> TestTree
requestListSigningProfiles =
  req
    "ListSigningProfiles"
    "fixture/ListSigningProfiles.yaml"

requestListProfilePermissions :: ListProfilePermissions -> TestTree
requestListProfilePermissions =
  req
    "ListProfilePermissions"
    "fixture/ListProfilePermissions.yaml"

requestRevokeSignature :: RevokeSignature -> TestTree
requestRevokeSignature =
  req
    "RevokeSignature"
    "fixture/RevokeSignature.yaml"

requestGetSigningPlatform :: GetSigningPlatform -> TestTree
requestGetSigningPlatform =
  req
    "GetSigningPlatform"
    "fixture/GetSigningPlatform.yaml"

requestListSigningPlatforms :: ListSigningPlatforms -> TestTree
requestListSigningPlatforms =
  req
    "ListSigningPlatforms"
    "fixture/ListSigningPlatforms.yaml"

requestListSigningJobs :: ListSigningJobs -> TestTree
requestListSigningJobs =
  req
    "ListSigningJobs"
    "fixture/ListSigningJobs.yaml"

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

requestRemoveProfilePermission :: RemoveProfilePermission -> TestTree
requestRemoveProfilePermission =
  req
    "RemoveProfilePermission"
    "fixture/RemoveProfilePermission.yaml"

requestGetSigningProfile :: GetSigningProfile -> TestTree
requestGetSigningProfile =
  req
    "GetSigningProfile"
    "fixture/GetSigningProfile.yaml"

requestDescribeSigningJob :: DescribeSigningJob -> TestTree
requestDescribeSigningJob =
  req
    "DescribeSigningJob"
    "fixture/DescribeSigningJob.yaml"

-- Responses

responseStartSigningJob :: StartSigningJobResponse -> TestTree
responseStartSigningJob =
  res
    "StartSigningJobResponse"
    "fixture/StartSigningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSigningJob)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRevokeSigningProfile :: RevokeSigningProfileResponse -> TestTree
responseRevokeSigningProfile =
  res
    "RevokeSigningProfileResponse"
    "fixture/RevokeSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSigningProfile)

responseCancelSigningProfile :: CancelSigningProfileResponse -> TestTree
responseCancelSigningProfile =
  res
    "CancelSigningProfileResponse"
    "fixture/CancelSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSigningProfile)

responsePutSigningProfile :: PutSigningProfileResponse -> TestTree
responsePutSigningProfile =
  res
    "PutSigningProfileResponse"
    "fixture/PutSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSigningProfile)

responseAddProfilePermission :: AddProfilePermissionResponse -> TestTree
responseAddProfilePermission =
  res
    "AddProfilePermissionResponse"
    "fixture/AddProfilePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddProfilePermission)

responseListSigningProfiles :: ListSigningProfilesResponse -> TestTree
responseListSigningProfiles =
  res
    "ListSigningProfilesResponse"
    "fixture/ListSigningProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSigningProfiles)

responseListProfilePermissions :: ListProfilePermissionsResponse -> TestTree
responseListProfilePermissions =
  res
    "ListProfilePermissionsResponse"
    "fixture/ListProfilePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfilePermissions)

responseRevokeSignature :: RevokeSignatureResponse -> TestTree
responseRevokeSignature =
  res
    "RevokeSignatureResponse"
    "fixture/RevokeSignatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSignature)

responseGetSigningPlatform :: GetSigningPlatformResponse -> TestTree
responseGetSigningPlatform =
  res
    "GetSigningPlatformResponse"
    "fixture/GetSigningPlatformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSigningPlatform)

responseListSigningPlatforms :: ListSigningPlatformsResponse -> TestTree
responseListSigningPlatforms =
  res
    "ListSigningPlatformsResponse"
    "fixture/ListSigningPlatformsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSigningPlatforms)

responseListSigningJobs :: ListSigningJobsResponse -> TestTree
responseListSigningJobs =
  res
    "ListSigningJobsResponse"
    "fixture/ListSigningJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSigningJobs)

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

responseRemoveProfilePermission :: RemoveProfilePermissionResponse -> TestTree
responseRemoveProfilePermission =
  res
    "RemoveProfilePermissionResponse"
    "fixture/RemoveProfilePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveProfilePermission)

responseGetSigningProfile :: GetSigningProfileResponse -> TestTree
responseGetSigningProfile =
  res
    "GetSigningProfileResponse"
    "fixture/GetSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSigningProfile)

responseDescribeSigningJob :: DescribeSigningJobResponse -> TestTree
responseDescribeSigningJob =
  res
    "DescribeSigningJobResponse"
    "fixture/DescribeSigningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSigningJob)

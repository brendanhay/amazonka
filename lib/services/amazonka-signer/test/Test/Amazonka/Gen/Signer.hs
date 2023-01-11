{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Signer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Signer where

import Amazonka.Signer
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Signer.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddProfilePermission $
--             newAddProfilePermission
--
--         , requestCancelSigningProfile $
--             newCancelSigningProfile
--
--         , requestDescribeSigningJob $
--             newDescribeSigningJob
--
--         , requestGetSigningPlatform $
--             newGetSigningPlatform
--
--         , requestGetSigningProfile $
--             newGetSigningProfile
--
--         , requestListProfilePermissions $
--             newListProfilePermissions
--
--         , requestListSigningJobs $
--             newListSigningJobs
--
--         , requestListSigningPlatforms $
--             newListSigningPlatforms
--
--         , requestListSigningProfiles $
--             newListSigningProfiles
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutSigningProfile $
--             newPutSigningProfile
--
--         , requestRemoveProfilePermission $
--             newRemoveProfilePermission
--
--         , requestRevokeSignature $
--             newRevokeSignature
--
--         , requestRevokeSigningProfile $
--             newRevokeSigningProfile
--
--         , requestStartSigningJob $
--             newStartSigningJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseAddProfilePermission $
--             newAddProfilePermissionResponse
--
--         , responseCancelSigningProfile $
--             newCancelSigningProfileResponse
--
--         , responseDescribeSigningJob $
--             newDescribeSigningJobResponse
--
--         , responseGetSigningPlatform $
--             newGetSigningPlatformResponse
--
--         , responseGetSigningProfile $
--             newGetSigningProfileResponse
--
--         , responseListProfilePermissions $
--             newListProfilePermissionsResponse
--
--         , responseListSigningJobs $
--             newListSigningJobsResponse
--
--         , responseListSigningPlatforms $
--             newListSigningPlatformsResponse
--
--         , responseListSigningProfiles $
--             newListSigningProfilesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutSigningProfile $
--             newPutSigningProfileResponse
--
--         , responseRemoveProfilePermission $
--             newRemoveProfilePermissionResponse
--
--         , responseRevokeSignature $
--             newRevokeSignatureResponse
--
--         , responseRevokeSigningProfile $
--             newRevokeSigningProfileResponse
--
--         , responseStartSigningJob $
--             newStartSigningJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestAddProfilePermission :: AddProfilePermission -> TestTree
requestAddProfilePermission =
  req
    "AddProfilePermission"
    "fixture/AddProfilePermission.yaml"

requestCancelSigningProfile :: CancelSigningProfile -> TestTree
requestCancelSigningProfile =
  req
    "CancelSigningProfile"
    "fixture/CancelSigningProfile.yaml"

requestDescribeSigningJob :: DescribeSigningJob -> TestTree
requestDescribeSigningJob =
  req
    "DescribeSigningJob"
    "fixture/DescribeSigningJob.yaml"

requestGetSigningPlatform :: GetSigningPlatform -> TestTree
requestGetSigningPlatform =
  req
    "GetSigningPlatform"
    "fixture/GetSigningPlatform.yaml"

requestGetSigningProfile :: GetSigningProfile -> TestTree
requestGetSigningProfile =
  req
    "GetSigningProfile"
    "fixture/GetSigningProfile.yaml"

requestListProfilePermissions :: ListProfilePermissions -> TestTree
requestListProfilePermissions =
  req
    "ListProfilePermissions"
    "fixture/ListProfilePermissions.yaml"

requestListSigningJobs :: ListSigningJobs -> TestTree
requestListSigningJobs =
  req
    "ListSigningJobs"
    "fixture/ListSigningJobs.yaml"

requestListSigningPlatforms :: ListSigningPlatforms -> TestTree
requestListSigningPlatforms =
  req
    "ListSigningPlatforms"
    "fixture/ListSigningPlatforms.yaml"

requestListSigningProfiles :: ListSigningProfiles -> TestTree
requestListSigningProfiles =
  req
    "ListSigningProfiles"
    "fixture/ListSigningProfiles.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutSigningProfile :: PutSigningProfile -> TestTree
requestPutSigningProfile =
  req
    "PutSigningProfile"
    "fixture/PutSigningProfile.yaml"

requestRemoveProfilePermission :: RemoveProfilePermission -> TestTree
requestRemoveProfilePermission =
  req
    "RemoveProfilePermission"
    "fixture/RemoveProfilePermission.yaml"

requestRevokeSignature :: RevokeSignature -> TestTree
requestRevokeSignature =
  req
    "RevokeSignature"
    "fixture/RevokeSignature.yaml"

requestRevokeSigningProfile :: RevokeSigningProfile -> TestTree
requestRevokeSigningProfile =
  req
    "RevokeSigningProfile"
    "fixture/RevokeSigningProfile.yaml"

requestStartSigningJob :: StartSigningJob -> TestTree
requestStartSigningJob =
  req
    "StartSigningJob"
    "fixture/StartSigningJob.yaml"

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

-- Responses

responseAddProfilePermission :: AddProfilePermissionResponse -> TestTree
responseAddProfilePermission =
  res
    "AddProfilePermissionResponse"
    "fixture/AddProfilePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddProfilePermission)

responseCancelSigningProfile :: CancelSigningProfileResponse -> TestTree
responseCancelSigningProfile =
  res
    "CancelSigningProfileResponse"
    "fixture/CancelSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelSigningProfile)

responseDescribeSigningJob :: DescribeSigningJobResponse -> TestTree
responseDescribeSigningJob =
  res
    "DescribeSigningJobResponse"
    "fixture/DescribeSigningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSigningJob)

responseGetSigningPlatform :: GetSigningPlatformResponse -> TestTree
responseGetSigningPlatform =
  res
    "GetSigningPlatformResponse"
    "fixture/GetSigningPlatformResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSigningPlatform)

responseGetSigningProfile :: GetSigningProfileResponse -> TestTree
responseGetSigningProfile =
  res
    "GetSigningProfileResponse"
    "fixture/GetSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSigningProfile)

responseListProfilePermissions :: ListProfilePermissionsResponse -> TestTree
responseListProfilePermissions =
  res
    "ListProfilePermissionsResponse"
    "fixture/ListProfilePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfilePermissions)

responseListSigningJobs :: ListSigningJobsResponse -> TestTree
responseListSigningJobs =
  res
    "ListSigningJobsResponse"
    "fixture/ListSigningJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSigningJobs)

responseListSigningPlatforms :: ListSigningPlatformsResponse -> TestTree
responseListSigningPlatforms =
  res
    "ListSigningPlatformsResponse"
    "fixture/ListSigningPlatformsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSigningPlatforms)

responseListSigningProfiles :: ListSigningProfilesResponse -> TestTree
responseListSigningProfiles =
  res
    "ListSigningProfilesResponse"
    "fixture/ListSigningProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSigningProfiles)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutSigningProfile :: PutSigningProfileResponse -> TestTree
responsePutSigningProfile =
  res
    "PutSigningProfileResponse"
    "fixture/PutSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSigningProfile)

responseRemoveProfilePermission :: RemoveProfilePermissionResponse -> TestTree
responseRemoveProfilePermission =
  res
    "RemoveProfilePermissionResponse"
    "fixture/RemoveProfilePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveProfilePermission)

responseRevokeSignature :: RevokeSignatureResponse -> TestTree
responseRevokeSignature =
  res
    "RevokeSignatureResponse"
    "fixture/RevokeSignatureResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSignature)

responseRevokeSigningProfile :: RevokeSigningProfileResponse -> TestTree
responseRevokeSigningProfile =
  res
    "RevokeSigningProfileResponse"
    "fixture/RevokeSigningProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RevokeSigningProfile)

responseStartSigningJob :: StartSigningJobResponse -> TestTree
responseStartSigningJob =
  res
    "StartSigningJobResponse"
    "fixture/StartSigningJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSigningJob)

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

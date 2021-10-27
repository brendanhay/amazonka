{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.VoiceId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.VoiceId where

import Data.Proxy
import Network.AWS.VoiceId
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.VoiceId.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeFraudsterRegistrationJob $
--             newDescribeFraudsterRegistrationJob
--
--         , requestDeleteSpeaker $
--             newDeleteSpeaker
--
--         , requestListSpeakers $
--             newListSpeakers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestEvaluateSession $
--             newEvaluateSession
--
--         , requestDescribeSpeakerEnrollmentJob $
--             newDescribeSpeakerEnrollmentJob
--
--         , requestDeleteFraudster $
--             newDeleteFraudster
--
--         , requestListFraudsterRegistrationJobs $
--             newListFraudsterRegistrationJobs
--
--         , requestDescribeFraudster $
--             newDescribeFraudster
--
--         , requestListSpeakerEnrollmentJobs $
--             newListSpeakerEnrollmentJobs
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestStartFraudsterRegistrationJob $
--             newStartFraudsterRegistrationJob
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestTagResource $
--             newTagResource
--
--         , requestStartSpeakerEnrollmentJob $
--             newStartSpeakerEnrollmentJob
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestOptOutSpeaker $
--             newOptOutSpeaker
--
--         , requestDescribeSpeaker $
--             newDescribeSpeaker
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--         , requestListDomains $
--             newListDomains
--
--           ]

--     , testGroup "response"
--         [ responseDescribeFraudsterRegistrationJob $
--             newDescribeFraudsterRegistrationJobResponse
--
--         , responseDeleteSpeaker $
--             newDeleteSpeakerResponse
--
--         , responseListSpeakers $
--             newListSpeakersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseEvaluateSession $
--             newEvaluateSessionResponse
--
--         , responseDescribeSpeakerEnrollmentJob $
--             newDescribeSpeakerEnrollmentJobResponse
--
--         , responseDeleteFraudster $
--             newDeleteFraudsterResponse
--
--         , responseListFraudsterRegistrationJobs $
--             newListFraudsterRegistrationJobsResponse
--
--         , responseDescribeFraudster $
--             newDescribeFraudsterResponse
--
--         , responseListSpeakerEnrollmentJobs $
--             newListSpeakerEnrollmentJobsResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseStartFraudsterRegistrationJob $
--             newStartFraudsterRegistrationJobResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseStartSpeakerEnrollmentJob $
--             newStartSpeakerEnrollmentJobResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseOptOutSpeaker $
--             newOptOutSpeakerResponse
--
--         , responseDescribeSpeaker $
--             newDescribeSpeakerResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--           ]
--     ]

-- Requests

requestDescribeFraudsterRegistrationJob :: DescribeFraudsterRegistrationJob -> TestTree
requestDescribeFraudsterRegistrationJob =
  req
    "DescribeFraudsterRegistrationJob"
    "fixture/DescribeFraudsterRegistrationJob.yaml"

requestDeleteSpeaker :: DeleteSpeaker -> TestTree
requestDeleteSpeaker =
  req
    "DeleteSpeaker"
    "fixture/DeleteSpeaker.yaml"

requestListSpeakers :: ListSpeakers -> TestTree
requestListSpeakers =
  req
    "ListSpeakers"
    "fixture/ListSpeakers.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestEvaluateSession :: EvaluateSession -> TestTree
requestEvaluateSession =
  req
    "EvaluateSession"
    "fixture/EvaluateSession.yaml"

requestDescribeSpeakerEnrollmentJob :: DescribeSpeakerEnrollmentJob -> TestTree
requestDescribeSpeakerEnrollmentJob =
  req
    "DescribeSpeakerEnrollmentJob"
    "fixture/DescribeSpeakerEnrollmentJob.yaml"

requestDeleteFraudster :: DeleteFraudster -> TestTree
requestDeleteFraudster =
  req
    "DeleteFraudster"
    "fixture/DeleteFraudster.yaml"

requestListFraudsterRegistrationJobs :: ListFraudsterRegistrationJobs -> TestTree
requestListFraudsterRegistrationJobs =
  req
    "ListFraudsterRegistrationJobs"
    "fixture/ListFraudsterRegistrationJobs.yaml"

requestDescribeFraudster :: DescribeFraudster -> TestTree
requestDescribeFraudster =
  req
    "DescribeFraudster"
    "fixture/DescribeFraudster.yaml"

requestListSpeakerEnrollmentJobs :: ListSpeakerEnrollmentJobs -> TestTree
requestListSpeakerEnrollmentJobs =
  req
    "ListSpeakerEnrollmentJobs"
    "fixture/ListSpeakerEnrollmentJobs.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestStartFraudsterRegistrationJob :: StartFraudsterRegistrationJob -> TestTree
requestStartFraudsterRegistrationJob =
  req
    "StartFraudsterRegistrationJob"
    "fixture/StartFraudsterRegistrationJob.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestStartSpeakerEnrollmentJob :: StartSpeakerEnrollmentJob -> TestTree
requestStartSpeakerEnrollmentJob =
  req
    "StartSpeakerEnrollmentJob"
    "fixture/StartSpeakerEnrollmentJob.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestOptOutSpeaker :: OptOutSpeaker -> TestTree
requestOptOutSpeaker =
  req
    "OptOutSpeaker"
    "fixture/OptOutSpeaker.yaml"

requestDescribeSpeaker :: DescribeSpeaker -> TestTree
requestDescribeSpeaker =
  req
    "DescribeSpeaker"
    "fixture/DescribeSpeaker.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

-- Responses

responseDescribeFraudsterRegistrationJob :: DescribeFraudsterRegistrationJobResponse -> TestTree
responseDescribeFraudsterRegistrationJob =
  res
    "DescribeFraudsterRegistrationJobResponse"
    "fixture/DescribeFraudsterRegistrationJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFraudsterRegistrationJob)

responseDeleteSpeaker :: DeleteSpeakerResponse -> TestTree
responseDeleteSpeaker =
  res
    "DeleteSpeakerResponse"
    "fixture/DeleteSpeakerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSpeaker)

responseListSpeakers :: ListSpeakersResponse -> TestTree
responseListSpeakers =
  res
    "ListSpeakersResponse"
    "fixture/ListSpeakersResponse.proto"
    defaultService
    (Proxy :: Proxy ListSpeakers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseEvaluateSession :: EvaluateSessionResponse -> TestTree
responseEvaluateSession =
  res
    "EvaluateSessionResponse"
    "fixture/EvaluateSessionResponse.proto"
    defaultService
    (Proxy :: Proxy EvaluateSession)

responseDescribeSpeakerEnrollmentJob :: DescribeSpeakerEnrollmentJobResponse -> TestTree
responseDescribeSpeakerEnrollmentJob =
  res
    "DescribeSpeakerEnrollmentJobResponse"
    "fixture/DescribeSpeakerEnrollmentJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpeakerEnrollmentJob)

responseDeleteFraudster :: DeleteFraudsterResponse -> TestTree
responseDeleteFraudster =
  res
    "DeleteFraudsterResponse"
    "fixture/DeleteFraudsterResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFraudster)

responseListFraudsterRegistrationJobs :: ListFraudsterRegistrationJobsResponse -> TestTree
responseListFraudsterRegistrationJobs =
  res
    "ListFraudsterRegistrationJobsResponse"
    "fixture/ListFraudsterRegistrationJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFraudsterRegistrationJobs)

responseDescribeFraudster :: DescribeFraudsterResponse -> TestTree
responseDescribeFraudster =
  res
    "DescribeFraudsterResponse"
    "fixture/DescribeFraudsterResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFraudster)

responseListSpeakerEnrollmentJobs :: ListSpeakerEnrollmentJobsResponse -> TestTree
responseListSpeakerEnrollmentJobs =
  res
    "ListSpeakerEnrollmentJobsResponse"
    "fixture/ListSpeakerEnrollmentJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSpeakerEnrollmentJobs)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomain)

responseStartFraudsterRegistrationJob :: StartFraudsterRegistrationJobResponse -> TestTree
responseStartFraudsterRegistrationJob =
  res
    "StartFraudsterRegistrationJobResponse"
    "fixture/StartFraudsterRegistrationJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartFraudsterRegistrationJob)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDomain)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseStartSpeakerEnrollmentJob :: StartSpeakerEnrollmentJobResponse -> TestTree
responseStartSpeakerEnrollmentJob =
  res
    "StartSpeakerEnrollmentJobResponse"
    "fixture/StartSpeakerEnrollmentJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartSpeakerEnrollmentJob)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseOptOutSpeaker :: OptOutSpeakerResponse -> TestTree
responseOptOutSpeaker =
  res
    "OptOutSpeakerResponse"
    "fixture/OptOutSpeakerResponse.proto"
    defaultService
    (Proxy :: Proxy OptOutSpeaker)

responseDescribeSpeaker :: DescribeSpeakerResponse -> TestTree
responseDescribeSpeaker =
  res
    "DescribeSpeakerResponse"
    "fixture/DescribeSpeakerResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSpeaker)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomain)

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDomains)

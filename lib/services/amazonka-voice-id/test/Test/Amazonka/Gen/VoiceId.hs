{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.VoiceId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.VoiceId where

import Amazonka.VoiceId
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.VoiceId.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDomain $
--             newCreateDomain
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteFraudster $
--             newDeleteFraudster
--
--         , requestDeleteSpeaker $
--             newDeleteSpeaker
--
--         , requestDescribeDomain $
--             newDescribeDomain
--
--         , requestDescribeFraudster $
--             newDescribeFraudster
--
--         , requestDescribeFraudsterRegistrationJob $
--             newDescribeFraudsterRegistrationJob
--
--         , requestDescribeSpeaker $
--             newDescribeSpeaker
--
--         , requestDescribeSpeakerEnrollmentJob $
--             newDescribeSpeakerEnrollmentJob
--
--         , requestEvaluateSession $
--             newEvaluateSession
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListFraudsterRegistrationJobs $
--             newListFraudsterRegistrationJobs
--
--         , requestListSpeakerEnrollmentJobs $
--             newListSpeakerEnrollmentJobs
--
--         , requestListSpeakers $
--             newListSpeakers
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestOptOutSpeaker $
--             newOptOutSpeaker
--
--         , requestStartFraudsterRegistrationJob $
--             newStartFraudsterRegistrationJob
--
--         , requestStartSpeakerEnrollmentJob $
--             newStartSpeakerEnrollmentJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--           ]

--     , testGroup "response"
--         [ responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteFraudster $
--             newDeleteFraudsterResponse
--
--         , responseDeleteSpeaker $
--             newDeleteSpeakerResponse
--
--         , responseDescribeDomain $
--             newDescribeDomainResponse
--
--         , responseDescribeFraudster $
--             newDescribeFraudsterResponse
--
--         , responseDescribeFraudsterRegistrationJob $
--             newDescribeFraudsterRegistrationJobResponse
--
--         , responseDescribeSpeaker $
--             newDescribeSpeakerResponse
--
--         , responseDescribeSpeakerEnrollmentJob $
--             newDescribeSpeakerEnrollmentJobResponse
--
--         , responseEvaluateSession $
--             newEvaluateSessionResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListFraudsterRegistrationJobs $
--             newListFraudsterRegistrationJobsResponse
--
--         , responseListSpeakerEnrollmentJobs $
--             newListSpeakerEnrollmentJobsResponse
--
--         , responseListSpeakers $
--             newListSpeakersResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseOptOutSpeaker $
--             newOptOutSpeakerResponse
--
--         , responseStartFraudsterRegistrationJob $
--             newStartFraudsterRegistrationJobResponse
--
--         , responseStartSpeakerEnrollmentJob $
--             newStartSpeakerEnrollmentJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--           ]
--     ]

-- Requests

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteFraudster :: DeleteFraudster -> TestTree
requestDeleteFraudster =
  req
    "DeleteFraudster"
    "fixture/DeleteFraudster.yaml"

requestDeleteSpeaker :: DeleteSpeaker -> TestTree
requestDeleteSpeaker =
  req
    "DeleteSpeaker"
    "fixture/DeleteSpeaker.yaml"

requestDescribeDomain :: DescribeDomain -> TestTree
requestDescribeDomain =
  req
    "DescribeDomain"
    "fixture/DescribeDomain.yaml"

requestDescribeFraudster :: DescribeFraudster -> TestTree
requestDescribeFraudster =
  req
    "DescribeFraudster"
    "fixture/DescribeFraudster.yaml"

requestDescribeFraudsterRegistrationJob :: DescribeFraudsterRegistrationJob -> TestTree
requestDescribeFraudsterRegistrationJob =
  req
    "DescribeFraudsterRegistrationJob"
    "fixture/DescribeFraudsterRegistrationJob.yaml"

requestDescribeSpeaker :: DescribeSpeaker -> TestTree
requestDescribeSpeaker =
  req
    "DescribeSpeaker"
    "fixture/DescribeSpeaker.yaml"

requestDescribeSpeakerEnrollmentJob :: DescribeSpeakerEnrollmentJob -> TestTree
requestDescribeSpeakerEnrollmentJob =
  req
    "DescribeSpeakerEnrollmentJob"
    "fixture/DescribeSpeakerEnrollmentJob.yaml"

requestEvaluateSession :: EvaluateSession -> TestTree
requestEvaluateSession =
  req
    "EvaluateSession"
    "fixture/EvaluateSession.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListFraudsterRegistrationJobs :: ListFraudsterRegistrationJobs -> TestTree
requestListFraudsterRegistrationJobs =
  req
    "ListFraudsterRegistrationJobs"
    "fixture/ListFraudsterRegistrationJobs.yaml"

requestListSpeakerEnrollmentJobs :: ListSpeakerEnrollmentJobs -> TestTree
requestListSpeakerEnrollmentJobs =
  req
    "ListSpeakerEnrollmentJobs"
    "fixture/ListSpeakerEnrollmentJobs.yaml"

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

requestOptOutSpeaker :: OptOutSpeaker -> TestTree
requestOptOutSpeaker =
  req
    "OptOutSpeaker"
    "fixture/OptOutSpeaker.yaml"

requestStartFraudsterRegistrationJob :: StartFraudsterRegistrationJob -> TestTree
requestStartFraudsterRegistrationJob =
  req
    "StartFraudsterRegistrationJob"
    "fixture/StartFraudsterRegistrationJob.yaml"

requestStartSpeakerEnrollmentJob :: StartSpeakerEnrollmentJob -> TestTree
requestStartSpeakerEnrollmentJob =
  req
    "StartSpeakerEnrollmentJob"
    "fixture/StartSpeakerEnrollmentJob.yaml"

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

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

-- Responses

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteFraudster :: DeleteFraudsterResponse -> TestTree
responseDeleteFraudster =
  res
    "DeleteFraudsterResponse"
    "fixture/DeleteFraudsterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFraudster)

responseDeleteSpeaker :: DeleteSpeakerResponse -> TestTree
responseDeleteSpeaker =
  res
    "DeleteSpeakerResponse"
    "fixture/DeleteSpeakerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSpeaker)

responseDescribeDomain :: DescribeDomainResponse -> TestTree
responseDescribeDomain =
  res
    "DescribeDomainResponse"
    "fixture/DescribeDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDomain)

responseDescribeFraudster :: DescribeFraudsterResponse -> TestTree
responseDescribeFraudster =
  res
    "DescribeFraudsterResponse"
    "fixture/DescribeFraudsterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFraudster)

responseDescribeFraudsterRegistrationJob :: DescribeFraudsterRegistrationJobResponse -> TestTree
responseDescribeFraudsterRegistrationJob =
  res
    "DescribeFraudsterRegistrationJobResponse"
    "fixture/DescribeFraudsterRegistrationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFraudsterRegistrationJob)

responseDescribeSpeaker :: DescribeSpeakerResponse -> TestTree
responseDescribeSpeaker =
  res
    "DescribeSpeakerResponse"
    "fixture/DescribeSpeakerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpeaker)

responseDescribeSpeakerEnrollmentJob :: DescribeSpeakerEnrollmentJobResponse -> TestTree
responseDescribeSpeakerEnrollmentJob =
  res
    "DescribeSpeakerEnrollmentJobResponse"
    "fixture/DescribeSpeakerEnrollmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpeakerEnrollmentJob)

responseEvaluateSession :: EvaluateSessionResponse -> TestTree
responseEvaluateSession =
  res
    "EvaluateSessionResponse"
    "fixture/EvaluateSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EvaluateSession)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListFraudsterRegistrationJobs :: ListFraudsterRegistrationJobsResponse -> TestTree
responseListFraudsterRegistrationJobs =
  res
    "ListFraudsterRegistrationJobsResponse"
    "fixture/ListFraudsterRegistrationJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFraudsterRegistrationJobs)

responseListSpeakerEnrollmentJobs :: ListSpeakerEnrollmentJobsResponse -> TestTree
responseListSpeakerEnrollmentJobs =
  res
    "ListSpeakerEnrollmentJobsResponse"
    "fixture/ListSpeakerEnrollmentJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSpeakerEnrollmentJobs)

responseListSpeakers :: ListSpeakersResponse -> TestTree
responseListSpeakers =
  res
    "ListSpeakersResponse"
    "fixture/ListSpeakersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSpeakers)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseOptOutSpeaker :: OptOutSpeakerResponse -> TestTree
responseOptOutSpeaker =
  res
    "OptOutSpeakerResponse"
    "fixture/OptOutSpeakerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OptOutSpeaker)

responseStartFraudsterRegistrationJob :: StartFraudsterRegistrationJobResponse -> TestTree
responseStartFraudsterRegistrationJob =
  res
    "StartFraudsterRegistrationJobResponse"
    "fixture/StartFraudsterRegistrationJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFraudsterRegistrationJob)

responseStartSpeakerEnrollmentJob :: StartSpeakerEnrollmentJobResponse -> TestTree
responseStartSpeakerEnrollmentJob =
  res
    "StartSpeakerEnrollmentJobResponse"
    "fixture/StartSpeakerEnrollmentJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartSpeakerEnrollmentJob)

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

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomain)

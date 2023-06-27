{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.VoiceId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestAssociateFraudster $
--             newAssociateFraudster
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateWatchlist $
--             newCreateWatchlist
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
--         , requestDeleteWatchlist $
--             newDeleteWatchlist
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
--         , requestDescribeWatchlist $
--             newDescribeWatchlist
--
--         , requestDisassociateFraudster $
--             newDisassociateFraudster
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
--         , requestListFraudsters $
--             newListFraudsters
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
--         , requestListWatchlists $
--             newListWatchlists
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
--         , requestUpdateWatchlist $
--             newUpdateWatchlist
--
--           ]

--     , testGroup "response"
--         [ responseAssociateFraudster $
--             newAssociateFraudsterResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateWatchlist $
--             newCreateWatchlistResponse
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
--         , responseDeleteWatchlist $
--             newDeleteWatchlistResponse
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
--         , responseDescribeWatchlist $
--             newDescribeWatchlistResponse
--
--         , responseDisassociateFraudster $
--             newDisassociateFraudsterResponse
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
--         , responseListFraudsters $
--             newListFraudstersResponse
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
--         , responseListWatchlists $
--             newListWatchlistsResponse
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
--         , responseUpdateWatchlist $
--             newUpdateWatchlistResponse
--
--           ]
--     ]

-- Requests

requestAssociateFraudster :: AssociateFraudster -> TestTree
requestAssociateFraudster =
  req
    "AssociateFraudster"
    "fixture/AssociateFraudster.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestCreateWatchlist :: CreateWatchlist -> TestTree
requestCreateWatchlist =
  req
    "CreateWatchlist"
    "fixture/CreateWatchlist.yaml"

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

requestDeleteWatchlist :: DeleteWatchlist -> TestTree
requestDeleteWatchlist =
  req
    "DeleteWatchlist"
    "fixture/DeleteWatchlist.yaml"

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

requestDescribeWatchlist :: DescribeWatchlist -> TestTree
requestDescribeWatchlist =
  req
    "DescribeWatchlist"
    "fixture/DescribeWatchlist.yaml"

requestDisassociateFraudster :: DisassociateFraudster -> TestTree
requestDisassociateFraudster =
  req
    "DisassociateFraudster"
    "fixture/DisassociateFraudster.yaml"

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

requestListFraudsters :: ListFraudsters -> TestTree
requestListFraudsters =
  req
    "ListFraudsters"
    "fixture/ListFraudsters.yaml"

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

requestListWatchlists :: ListWatchlists -> TestTree
requestListWatchlists =
  req
    "ListWatchlists"
    "fixture/ListWatchlists.yaml"

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

requestUpdateWatchlist :: UpdateWatchlist -> TestTree
requestUpdateWatchlist =
  req
    "UpdateWatchlist"
    "fixture/UpdateWatchlist.yaml"

-- Responses

responseAssociateFraudster :: AssociateFraudsterResponse -> TestTree
responseAssociateFraudster =
  res
    "AssociateFraudsterResponse"
    "fixture/AssociateFraudsterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFraudster)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseCreateWatchlist :: CreateWatchlistResponse -> TestTree
responseCreateWatchlist =
  res
    "CreateWatchlistResponse"
    "fixture/CreateWatchlistResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWatchlist)

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

responseDeleteWatchlist :: DeleteWatchlistResponse -> TestTree
responseDeleteWatchlist =
  res
    "DeleteWatchlistResponse"
    "fixture/DeleteWatchlistResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWatchlist)

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

responseDescribeWatchlist :: DescribeWatchlistResponse -> TestTree
responseDescribeWatchlist =
  res
    "DescribeWatchlistResponse"
    "fixture/DescribeWatchlistResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWatchlist)

responseDisassociateFraudster :: DisassociateFraudsterResponse -> TestTree
responseDisassociateFraudster =
  res
    "DisassociateFraudsterResponse"
    "fixture/DisassociateFraudsterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFraudster)

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

responseListFraudsters :: ListFraudstersResponse -> TestTree
responseListFraudsters =
  res
    "ListFraudstersResponse"
    "fixture/ListFraudstersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFraudsters)

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

responseListWatchlists :: ListWatchlistsResponse -> TestTree
responseListWatchlists =
  res
    "ListWatchlistsResponse"
    "fixture/ListWatchlistsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWatchlists)

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

responseUpdateWatchlist :: UpdateWatchlistResponse -> TestTree
responseUpdateWatchlist =
  res
    "UpdateWatchlistResponse"
    "fixture/UpdateWatchlistResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWatchlist)

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.OAM
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.OAM where

import Amazonka.OAM
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.OAM.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateLink $
--             newCreateLink
--
--         , requestCreateSink $
--             newCreateSink
--
--         , requestDeleteLink $
--             newDeleteLink
--
--         , requestDeleteSink $
--             newDeleteSink
--
--         , requestGetLink $
--             newGetLink
--
--         , requestGetSink $
--             newGetSink
--
--         , requestGetSinkPolicy $
--             newGetSinkPolicy
--
--         , requestListAttachedLinks $
--             newListAttachedLinks
--
--         , requestListLinks $
--             newListLinks
--
--         , requestListSinks $
--             newListSinks
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutSinkPolicy $
--             newPutSinkPolicy
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateLink $
--             newUpdateLink
--
--           ]

--     , testGroup "response"
--         [ responseCreateLink $
--             newCreateLinkResponse
--
--         , responseCreateSink $
--             newCreateSinkResponse
--
--         , responseDeleteLink $
--             newDeleteLinkResponse
--
--         , responseDeleteSink $
--             newDeleteSinkResponse
--
--         , responseGetLink $
--             newGetLinkResponse
--
--         , responseGetSink $
--             newGetSinkResponse
--
--         , responseGetSinkPolicy $
--             newGetSinkPolicyResponse
--
--         , responseListAttachedLinks $
--             newListAttachedLinksResponse
--
--         , responseListLinks $
--             newListLinksResponse
--
--         , responseListSinks $
--             newListSinksResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutSinkPolicy $
--             newPutSinkPolicyResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateLink $
--             newUpdateLinkResponse
--
--           ]
--     ]

-- Requests

requestCreateLink :: CreateLink -> TestTree
requestCreateLink =
  req
    "CreateLink"
    "fixture/CreateLink.yaml"

requestCreateSink :: CreateSink -> TestTree
requestCreateSink =
  req
    "CreateSink"
    "fixture/CreateSink.yaml"

requestDeleteLink :: DeleteLink -> TestTree
requestDeleteLink =
  req
    "DeleteLink"
    "fixture/DeleteLink.yaml"

requestDeleteSink :: DeleteSink -> TestTree
requestDeleteSink =
  req
    "DeleteSink"
    "fixture/DeleteSink.yaml"

requestGetLink :: GetLink -> TestTree
requestGetLink =
  req
    "GetLink"
    "fixture/GetLink.yaml"

requestGetSink :: GetSink -> TestTree
requestGetSink =
  req
    "GetSink"
    "fixture/GetSink.yaml"

requestGetSinkPolicy :: GetSinkPolicy -> TestTree
requestGetSinkPolicy =
  req
    "GetSinkPolicy"
    "fixture/GetSinkPolicy.yaml"

requestListAttachedLinks :: ListAttachedLinks -> TestTree
requestListAttachedLinks =
  req
    "ListAttachedLinks"
    "fixture/ListAttachedLinks.yaml"

requestListLinks :: ListLinks -> TestTree
requestListLinks =
  req
    "ListLinks"
    "fixture/ListLinks.yaml"

requestListSinks :: ListSinks -> TestTree
requestListSinks =
  req
    "ListSinks"
    "fixture/ListSinks.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutSinkPolicy :: PutSinkPolicy -> TestTree
requestPutSinkPolicy =
  req
    "PutSinkPolicy"
    "fixture/PutSinkPolicy.yaml"

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

requestUpdateLink :: UpdateLink -> TestTree
requestUpdateLink =
  req
    "UpdateLink"
    "fixture/UpdateLink.yaml"

-- Responses

responseCreateLink :: CreateLinkResponse -> TestTree
responseCreateLink =
  res
    "CreateLinkResponse"
    "fixture/CreateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLink)

responseCreateSink :: CreateSinkResponse -> TestTree
responseCreateSink =
  res
    "CreateSinkResponse"
    "fixture/CreateSinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSink)

responseDeleteLink :: DeleteLinkResponse -> TestTree
responseDeleteLink =
  res
    "DeleteLinkResponse"
    "fixture/DeleteLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLink)

responseDeleteSink :: DeleteSinkResponse -> TestTree
responseDeleteSink =
  res
    "DeleteSinkResponse"
    "fixture/DeleteSinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSink)

responseGetLink :: GetLinkResponse -> TestTree
responseGetLink =
  res
    "GetLinkResponse"
    "fixture/GetLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLink)

responseGetSink :: GetSinkResponse -> TestTree
responseGetSink =
  res
    "GetSinkResponse"
    "fixture/GetSinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSink)

responseGetSinkPolicy :: GetSinkPolicyResponse -> TestTree
responseGetSinkPolicy =
  res
    "GetSinkPolicyResponse"
    "fixture/GetSinkPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSinkPolicy)

responseListAttachedLinks :: ListAttachedLinksResponse -> TestTree
responseListAttachedLinks =
  res
    "ListAttachedLinksResponse"
    "fixture/ListAttachedLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttachedLinks)

responseListLinks :: ListLinksResponse -> TestTree
responseListLinks =
  res
    "ListLinksResponse"
    "fixture/ListLinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLinks)

responseListSinks :: ListSinksResponse -> TestTree
responseListSinks =
  res
    "ListSinksResponse"
    "fixture/ListSinksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSinks)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutSinkPolicy :: PutSinkPolicyResponse -> TestTree
responsePutSinkPolicy =
  res
    "PutSinkPolicyResponse"
    "fixture/PutSinkPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSinkPolicy)

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

responseUpdateLink :: UpdateLinkResponse -> TestTree
responseUpdateLink =
  res
    "UpdateLinkResponse"
    "fixture/UpdateLinkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLink)

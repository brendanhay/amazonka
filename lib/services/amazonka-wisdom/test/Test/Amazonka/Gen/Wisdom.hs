{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Wisdom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Wisdom where

import Amazonka.Wisdom
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Wisdom.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateAssistant $
--             newCreateAssistant
--
--         , requestCreateAssistantAssociation $
--             newCreateAssistantAssociation
--
--         , requestCreateContent $
--             newCreateContent
--
--         , requestCreateKnowledgeBase $
--             newCreateKnowledgeBase
--
--         , requestCreateSession $
--             newCreateSession
--
--         , requestDeleteAssistant $
--             newDeleteAssistant
--
--         , requestDeleteAssistantAssociation $
--             newDeleteAssistantAssociation
--
--         , requestDeleteContent $
--             newDeleteContent
--
--         , requestDeleteKnowledgeBase $
--             newDeleteKnowledgeBase
--
--         , requestGetAssistant $
--             newGetAssistant
--
--         , requestGetAssistantAssociation $
--             newGetAssistantAssociation
--
--         , requestGetContent $
--             newGetContent
--
--         , requestGetContentSummary $
--             newGetContentSummary
--
--         , requestGetKnowledgeBase $
--             newGetKnowledgeBase
--
--         , requestGetRecommendations $
--             newGetRecommendations
--
--         , requestGetSession $
--             newGetSession
--
--         , requestListAssistantAssociations $
--             newListAssistantAssociations
--
--         , requestListAssistants $
--             newListAssistants
--
--         , requestListContents $
--             newListContents
--
--         , requestListKnowledgeBases $
--             newListKnowledgeBases
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestNotifyRecommendationsReceived $
--             newNotifyRecommendationsReceived
--
--         , requestQueryAssistant $
--             newQueryAssistant
--
--         , requestRemoveKnowledgeBaseTemplateUri $
--             newRemoveKnowledgeBaseTemplateUri
--
--         , requestSearchContent $
--             newSearchContent
--
--         , requestSearchSessions $
--             newSearchSessions
--
--         , requestStartContentUpload $
--             newStartContentUpload
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateContent $
--             newUpdateContent
--
--         , requestUpdateKnowledgeBaseTemplateUri $
--             newUpdateKnowledgeBaseTemplateUri
--
--           ]

--     , testGroup "response"
--         [ responseCreateAssistant $
--             newCreateAssistantResponse
--
--         , responseCreateAssistantAssociation $
--             newCreateAssistantAssociationResponse
--
--         , responseCreateContent $
--             newCreateContentResponse
--
--         , responseCreateKnowledgeBase $
--             newCreateKnowledgeBaseResponse
--
--         , responseCreateSession $
--             newCreateSessionResponse
--
--         , responseDeleteAssistant $
--             newDeleteAssistantResponse
--
--         , responseDeleteAssistantAssociation $
--             newDeleteAssistantAssociationResponse
--
--         , responseDeleteContent $
--             newDeleteContentResponse
--
--         , responseDeleteKnowledgeBase $
--             newDeleteKnowledgeBaseResponse
--
--         , responseGetAssistant $
--             newGetAssistantResponse
--
--         , responseGetAssistantAssociation $
--             newGetAssistantAssociationResponse
--
--         , responseGetContent $
--             newGetContentResponse
--
--         , responseGetContentSummary $
--             newGetContentSummaryResponse
--
--         , responseGetKnowledgeBase $
--             newGetKnowledgeBaseResponse
--
--         , responseGetRecommendations $
--             newGetRecommendationsResponse
--
--         , responseGetSession $
--             newGetSessionResponse
--
--         , responseListAssistantAssociations $
--             newListAssistantAssociationsResponse
--
--         , responseListAssistants $
--             newListAssistantsResponse
--
--         , responseListContents $
--             newListContentsResponse
--
--         , responseListKnowledgeBases $
--             newListKnowledgeBasesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseNotifyRecommendationsReceived $
--             newNotifyRecommendationsReceivedResponse
--
--         , responseQueryAssistant $
--             newQueryAssistantResponse
--
--         , responseRemoveKnowledgeBaseTemplateUri $
--             newRemoveKnowledgeBaseTemplateUriResponse
--
--         , responseSearchContent $
--             newSearchContentResponse
--
--         , responseSearchSessions $
--             newSearchSessionsResponse
--
--         , responseStartContentUpload $
--             newStartContentUploadResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateContent $
--             newUpdateContentResponse
--
--         , responseUpdateKnowledgeBaseTemplateUri $
--             newUpdateKnowledgeBaseTemplateUriResponse
--
--           ]
--     ]

-- Requests

requestCreateAssistant :: CreateAssistant -> TestTree
requestCreateAssistant =
  req
    "CreateAssistant"
    "fixture/CreateAssistant.yaml"

requestCreateAssistantAssociation :: CreateAssistantAssociation -> TestTree
requestCreateAssistantAssociation =
  req
    "CreateAssistantAssociation"
    "fixture/CreateAssistantAssociation.yaml"

requestCreateContent :: CreateContent -> TestTree
requestCreateContent =
  req
    "CreateContent"
    "fixture/CreateContent.yaml"

requestCreateKnowledgeBase :: CreateKnowledgeBase -> TestTree
requestCreateKnowledgeBase =
  req
    "CreateKnowledgeBase"
    "fixture/CreateKnowledgeBase.yaml"

requestCreateSession :: CreateSession -> TestTree
requestCreateSession =
  req
    "CreateSession"
    "fixture/CreateSession.yaml"

requestDeleteAssistant :: DeleteAssistant -> TestTree
requestDeleteAssistant =
  req
    "DeleteAssistant"
    "fixture/DeleteAssistant.yaml"

requestDeleteAssistantAssociation :: DeleteAssistantAssociation -> TestTree
requestDeleteAssistantAssociation =
  req
    "DeleteAssistantAssociation"
    "fixture/DeleteAssistantAssociation.yaml"

requestDeleteContent :: DeleteContent -> TestTree
requestDeleteContent =
  req
    "DeleteContent"
    "fixture/DeleteContent.yaml"

requestDeleteKnowledgeBase :: DeleteKnowledgeBase -> TestTree
requestDeleteKnowledgeBase =
  req
    "DeleteKnowledgeBase"
    "fixture/DeleteKnowledgeBase.yaml"

requestGetAssistant :: GetAssistant -> TestTree
requestGetAssistant =
  req
    "GetAssistant"
    "fixture/GetAssistant.yaml"

requestGetAssistantAssociation :: GetAssistantAssociation -> TestTree
requestGetAssistantAssociation =
  req
    "GetAssistantAssociation"
    "fixture/GetAssistantAssociation.yaml"

requestGetContent :: GetContent -> TestTree
requestGetContent =
  req
    "GetContent"
    "fixture/GetContent.yaml"

requestGetContentSummary :: GetContentSummary -> TestTree
requestGetContentSummary =
  req
    "GetContentSummary"
    "fixture/GetContentSummary.yaml"

requestGetKnowledgeBase :: GetKnowledgeBase -> TestTree
requestGetKnowledgeBase =
  req
    "GetKnowledgeBase"
    "fixture/GetKnowledgeBase.yaml"

requestGetRecommendations :: GetRecommendations -> TestTree
requestGetRecommendations =
  req
    "GetRecommendations"
    "fixture/GetRecommendations.yaml"

requestGetSession :: GetSession -> TestTree
requestGetSession =
  req
    "GetSession"
    "fixture/GetSession.yaml"

requestListAssistantAssociations :: ListAssistantAssociations -> TestTree
requestListAssistantAssociations =
  req
    "ListAssistantAssociations"
    "fixture/ListAssistantAssociations.yaml"

requestListAssistants :: ListAssistants -> TestTree
requestListAssistants =
  req
    "ListAssistants"
    "fixture/ListAssistants.yaml"

requestListContents :: ListContents -> TestTree
requestListContents =
  req
    "ListContents"
    "fixture/ListContents.yaml"

requestListKnowledgeBases :: ListKnowledgeBases -> TestTree
requestListKnowledgeBases =
  req
    "ListKnowledgeBases"
    "fixture/ListKnowledgeBases.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestNotifyRecommendationsReceived :: NotifyRecommendationsReceived -> TestTree
requestNotifyRecommendationsReceived =
  req
    "NotifyRecommendationsReceived"
    "fixture/NotifyRecommendationsReceived.yaml"

requestQueryAssistant :: QueryAssistant -> TestTree
requestQueryAssistant =
  req
    "QueryAssistant"
    "fixture/QueryAssistant.yaml"

requestRemoveKnowledgeBaseTemplateUri :: RemoveKnowledgeBaseTemplateUri -> TestTree
requestRemoveKnowledgeBaseTemplateUri =
  req
    "RemoveKnowledgeBaseTemplateUri"
    "fixture/RemoveKnowledgeBaseTemplateUri.yaml"

requestSearchContent :: SearchContent -> TestTree
requestSearchContent =
  req
    "SearchContent"
    "fixture/SearchContent.yaml"

requestSearchSessions :: SearchSessions -> TestTree
requestSearchSessions =
  req
    "SearchSessions"
    "fixture/SearchSessions.yaml"

requestStartContentUpload :: StartContentUpload -> TestTree
requestStartContentUpload =
  req
    "StartContentUpload"
    "fixture/StartContentUpload.yaml"

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

requestUpdateContent :: UpdateContent -> TestTree
requestUpdateContent =
  req
    "UpdateContent"
    "fixture/UpdateContent.yaml"

requestUpdateKnowledgeBaseTemplateUri :: UpdateKnowledgeBaseTemplateUri -> TestTree
requestUpdateKnowledgeBaseTemplateUri =
  req
    "UpdateKnowledgeBaseTemplateUri"
    "fixture/UpdateKnowledgeBaseTemplateUri.yaml"

-- Responses

responseCreateAssistant :: CreateAssistantResponse -> TestTree
responseCreateAssistant =
  res
    "CreateAssistantResponse"
    "fixture/CreateAssistantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssistant)

responseCreateAssistantAssociation :: CreateAssistantAssociationResponse -> TestTree
responseCreateAssistantAssociation =
  res
    "CreateAssistantAssociationResponse"
    "fixture/CreateAssistantAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAssistantAssociation)

responseCreateContent :: CreateContentResponse -> TestTree
responseCreateContent =
  res
    "CreateContentResponse"
    "fixture/CreateContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContent)

responseCreateKnowledgeBase :: CreateKnowledgeBaseResponse -> TestTree
responseCreateKnowledgeBase =
  res
    "CreateKnowledgeBaseResponse"
    "fixture/CreateKnowledgeBaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKnowledgeBase)

responseCreateSession :: CreateSessionResponse -> TestTree
responseCreateSession =
  res
    "CreateSessionResponse"
    "fixture/CreateSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSession)

responseDeleteAssistant :: DeleteAssistantResponse -> TestTree
responseDeleteAssistant =
  res
    "DeleteAssistantResponse"
    "fixture/DeleteAssistantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssistant)

responseDeleteAssistantAssociation :: DeleteAssistantAssociationResponse -> TestTree
responseDeleteAssistantAssociation =
  res
    "DeleteAssistantAssociationResponse"
    "fixture/DeleteAssistantAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAssistantAssociation)

responseDeleteContent :: DeleteContentResponse -> TestTree
responseDeleteContent =
  res
    "DeleteContentResponse"
    "fixture/DeleteContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContent)

responseDeleteKnowledgeBase :: DeleteKnowledgeBaseResponse -> TestTree
responseDeleteKnowledgeBase =
  res
    "DeleteKnowledgeBaseResponse"
    "fixture/DeleteKnowledgeBaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKnowledgeBase)

responseGetAssistant :: GetAssistantResponse -> TestTree
responseGetAssistant =
  res
    "GetAssistantResponse"
    "fixture/GetAssistantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssistant)

responseGetAssistantAssociation :: GetAssistantAssociationResponse -> TestTree
responseGetAssistantAssociation =
  res
    "GetAssistantAssociationResponse"
    "fixture/GetAssistantAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssistantAssociation)

responseGetContent :: GetContentResponse -> TestTree
responseGetContent =
  res
    "GetContentResponse"
    "fixture/GetContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContent)

responseGetContentSummary :: GetContentSummaryResponse -> TestTree
responseGetContentSummary =
  res
    "GetContentSummaryResponse"
    "fixture/GetContentSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContentSummary)

responseGetKnowledgeBase :: GetKnowledgeBaseResponse -> TestTree
responseGetKnowledgeBase =
  res
    "GetKnowledgeBaseResponse"
    "fixture/GetKnowledgeBaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKnowledgeBase)

responseGetRecommendations :: GetRecommendationsResponse -> TestTree
responseGetRecommendations =
  res
    "GetRecommendationsResponse"
    "fixture/GetRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommendations)

responseGetSession :: GetSessionResponse -> TestTree
responseGetSession =
  res
    "GetSessionResponse"
    "fixture/GetSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSession)

responseListAssistantAssociations :: ListAssistantAssociationsResponse -> TestTree
responseListAssistantAssociations =
  res
    "ListAssistantAssociationsResponse"
    "fixture/ListAssistantAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssistantAssociations)

responseListAssistants :: ListAssistantsResponse -> TestTree
responseListAssistants =
  res
    "ListAssistantsResponse"
    "fixture/ListAssistantsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssistants)

responseListContents :: ListContentsResponse -> TestTree
responseListContents =
  res
    "ListContentsResponse"
    "fixture/ListContentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContents)

responseListKnowledgeBases :: ListKnowledgeBasesResponse -> TestTree
responseListKnowledgeBases =
  res
    "ListKnowledgeBasesResponse"
    "fixture/ListKnowledgeBasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKnowledgeBases)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseNotifyRecommendationsReceived :: NotifyRecommendationsReceivedResponse -> TestTree
responseNotifyRecommendationsReceived =
  res
    "NotifyRecommendationsReceivedResponse"
    "fixture/NotifyRecommendationsReceivedResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyRecommendationsReceived)

responseQueryAssistant :: QueryAssistantResponse -> TestTree
responseQueryAssistant =
  res
    "QueryAssistantResponse"
    "fixture/QueryAssistantResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy QueryAssistant)

responseRemoveKnowledgeBaseTemplateUri :: RemoveKnowledgeBaseTemplateUriResponse -> TestTree
responseRemoveKnowledgeBaseTemplateUri =
  res
    "RemoveKnowledgeBaseTemplateUriResponse"
    "fixture/RemoveKnowledgeBaseTemplateUriResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveKnowledgeBaseTemplateUri)

responseSearchContent :: SearchContentResponse -> TestTree
responseSearchContent =
  res
    "SearchContentResponse"
    "fixture/SearchContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchContent)

responseSearchSessions :: SearchSessionsResponse -> TestTree
responseSearchSessions =
  res
    "SearchSessionsResponse"
    "fixture/SearchSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSessions)

responseStartContentUpload :: StartContentUploadResponse -> TestTree
responseStartContentUpload =
  res
    "StartContentUploadResponse"
    "fixture/StartContentUploadResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartContentUpload)

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

responseUpdateContent :: UpdateContentResponse -> TestTree
responseUpdateContent =
  res
    "UpdateContentResponse"
    "fixture/UpdateContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContent)

responseUpdateKnowledgeBaseTemplateUri :: UpdateKnowledgeBaseTemplateUriResponse -> TestTree
responseUpdateKnowledgeBaseTemplateUri =
  res
    "UpdateKnowledgeBaseTemplateUriResponse"
    "fixture/UpdateKnowledgeBaseTemplateUriResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKnowledgeBaseTemplateUri)

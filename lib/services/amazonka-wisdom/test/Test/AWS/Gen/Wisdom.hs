{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Wisdom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Wisdom where

import Data.Proxy
import Network.AWS.Wisdom
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Wisdom.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListAssistantAssociations $
--             newListAssistantAssociations
--
--         , requestGetRecommendations $
--             newGetRecommendations
--
--         , requestSearchContent $
--             newSearchContent
--
--         , requestRemoveKnowledgeBaseTemplateUri $
--             newRemoveKnowledgeBaseTemplateUri
--
--         , requestGetAssistant $
--             newGetAssistant
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListAssistants $
--             newListAssistants
--
--         , requestUpdateKnowledgeBaseTemplateUri $
--             newUpdateKnowledgeBaseTemplateUri
--
--         , requestListContents $
--             newListContents
--
--         , requestDeleteContent $
--             newDeleteContent
--
--         , requestUpdateContent $
--             newUpdateContent
--
--         , requestCreateAssistant $
--             newCreateAssistant
--
--         , requestGetContentSummary $
--             newGetContentSummary
--
--         , requestNotifyRecommendationsReceived $
--             newNotifyRecommendationsReceived
--
--         , requestDeleteAssistantAssociation $
--             newDeleteAssistantAssociation
--
--         , requestGetContent $
--             newGetContent
--
--         , requestStartContentUpload $
--             newStartContentUpload
--
--         , requestCreateSession $
--             newCreateSession
--
--         , requestCreateContent $
--             newCreateContent
--
--         , requestDeleteAssistant $
--             newDeleteAssistant
--
--         , requestGetSession $
--             newGetSession
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateKnowledgeBase $
--             newCreateKnowledgeBase
--
--         , requestGetAssistantAssociation $
--             newGetAssistantAssociation
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestSearchSessions $
--             newSearchSessions
--
--         , requestListKnowledgeBases $
--             newListKnowledgeBases
--
--         , requestQueryAssistant $
--             newQueryAssistant
--
--         , requestDeleteKnowledgeBase $
--             newDeleteKnowledgeBase
--
--         , requestCreateAssistantAssociation $
--             newCreateAssistantAssociation
--
--         , requestGetKnowledgeBase $
--             newGetKnowledgeBase
--
--           ]

--     , testGroup "response"
--         [ responseListAssistantAssociations $
--             newListAssistantAssociationsResponse
--
--         , responseGetRecommendations $
--             newGetRecommendationsResponse
--
--         , responseSearchContent $
--             newSearchContentResponse
--
--         , responseRemoveKnowledgeBaseTemplateUri $
--             newRemoveKnowledgeBaseTemplateUriResponse
--
--         , responseGetAssistant $
--             newGetAssistantResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListAssistants $
--             newListAssistantsResponse
--
--         , responseUpdateKnowledgeBaseTemplateUri $
--             newUpdateKnowledgeBaseTemplateUriResponse
--
--         , responseListContents $
--             newListContentsResponse
--
--         , responseDeleteContent $
--             newDeleteContentResponse
--
--         , responseUpdateContent $
--             newUpdateContentResponse
--
--         , responseCreateAssistant $
--             newCreateAssistantResponse
--
--         , responseGetContentSummary $
--             newGetContentSummaryResponse
--
--         , responseNotifyRecommendationsReceived $
--             newNotifyRecommendationsReceivedResponse
--
--         , responseDeleteAssistantAssociation $
--             newDeleteAssistantAssociationResponse
--
--         , responseGetContent $
--             newGetContentResponse
--
--         , responseStartContentUpload $
--             newStartContentUploadResponse
--
--         , responseCreateSession $
--             newCreateSessionResponse
--
--         , responseCreateContent $
--             newCreateContentResponse
--
--         , responseDeleteAssistant $
--             newDeleteAssistantResponse
--
--         , responseGetSession $
--             newGetSessionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateKnowledgeBase $
--             newCreateKnowledgeBaseResponse
--
--         , responseGetAssistantAssociation $
--             newGetAssistantAssociationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseSearchSessions $
--             newSearchSessionsResponse
--
--         , responseListKnowledgeBases $
--             newListKnowledgeBasesResponse
--
--         , responseQueryAssistant $
--             newQueryAssistantResponse
--
--         , responseDeleteKnowledgeBase $
--             newDeleteKnowledgeBaseResponse
--
--         , responseCreateAssistantAssociation $
--             newCreateAssistantAssociationResponse
--
--         , responseGetKnowledgeBase $
--             newGetKnowledgeBaseResponse
--
--           ]
--     ]

-- Requests

requestListAssistantAssociations :: ListAssistantAssociations -> TestTree
requestListAssistantAssociations =
  req
    "ListAssistantAssociations"
    "fixture/ListAssistantAssociations.yaml"

requestGetRecommendations :: GetRecommendations -> TestTree
requestGetRecommendations =
  req
    "GetRecommendations"
    "fixture/GetRecommendations.yaml"

requestSearchContent :: SearchContent -> TestTree
requestSearchContent =
  req
    "SearchContent"
    "fixture/SearchContent.yaml"

requestRemoveKnowledgeBaseTemplateUri :: RemoveKnowledgeBaseTemplateUri -> TestTree
requestRemoveKnowledgeBaseTemplateUri =
  req
    "RemoveKnowledgeBaseTemplateUri"
    "fixture/RemoveKnowledgeBaseTemplateUri.yaml"

requestGetAssistant :: GetAssistant -> TestTree
requestGetAssistant =
  req
    "GetAssistant"
    "fixture/GetAssistant.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListAssistants :: ListAssistants -> TestTree
requestListAssistants =
  req
    "ListAssistants"
    "fixture/ListAssistants.yaml"

requestUpdateKnowledgeBaseTemplateUri :: UpdateKnowledgeBaseTemplateUri -> TestTree
requestUpdateKnowledgeBaseTemplateUri =
  req
    "UpdateKnowledgeBaseTemplateUri"
    "fixture/UpdateKnowledgeBaseTemplateUri.yaml"

requestListContents :: ListContents -> TestTree
requestListContents =
  req
    "ListContents"
    "fixture/ListContents.yaml"

requestDeleteContent :: DeleteContent -> TestTree
requestDeleteContent =
  req
    "DeleteContent"
    "fixture/DeleteContent.yaml"

requestUpdateContent :: UpdateContent -> TestTree
requestUpdateContent =
  req
    "UpdateContent"
    "fixture/UpdateContent.yaml"

requestCreateAssistant :: CreateAssistant -> TestTree
requestCreateAssistant =
  req
    "CreateAssistant"
    "fixture/CreateAssistant.yaml"

requestGetContentSummary :: GetContentSummary -> TestTree
requestGetContentSummary =
  req
    "GetContentSummary"
    "fixture/GetContentSummary.yaml"

requestNotifyRecommendationsReceived :: NotifyRecommendationsReceived -> TestTree
requestNotifyRecommendationsReceived =
  req
    "NotifyRecommendationsReceived"
    "fixture/NotifyRecommendationsReceived.yaml"

requestDeleteAssistantAssociation :: DeleteAssistantAssociation -> TestTree
requestDeleteAssistantAssociation =
  req
    "DeleteAssistantAssociation"
    "fixture/DeleteAssistantAssociation.yaml"

requestGetContent :: GetContent -> TestTree
requestGetContent =
  req
    "GetContent"
    "fixture/GetContent.yaml"

requestStartContentUpload :: StartContentUpload -> TestTree
requestStartContentUpload =
  req
    "StartContentUpload"
    "fixture/StartContentUpload.yaml"

requestCreateSession :: CreateSession -> TestTree
requestCreateSession =
  req
    "CreateSession"
    "fixture/CreateSession.yaml"

requestCreateContent :: CreateContent -> TestTree
requestCreateContent =
  req
    "CreateContent"
    "fixture/CreateContent.yaml"

requestDeleteAssistant :: DeleteAssistant -> TestTree
requestDeleteAssistant =
  req
    "DeleteAssistant"
    "fixture/DeleteAssistant.yaml"

requestGetSession :: GetSession -> TestTree
requestGetSession =
  req
    "GetSession"
    "fixture/GetSession.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateKnowledgeBase :: CreateKnowledgeBase -> TestTree
requestCreateKnowledgeBase =
  req
    "CreateKnowledgeBase"
    "fixture/CreateKnowledgeBase.yaml"

requestGetAssistantAssociation :: GetAssistantAssociation -> TestTree
requestGetAssistantAssociation =
  req
    "GetAssistantAssociation"
    "fixture/GetAssistantAssociation.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestSearchSessions :: SearchSessions -> TestTree
requestSearchSessions =
  req
    "SearchSessions"
    "fixture/SearchSessions.yaml"

requestListKnowledgeBases :: ListKnowledgeBases -> TestTree
requestListKnowledgeBases =
  req
    "ListKnowledgeBases"
    "fixture/ListKnowledgeBases.yaml"

requestQueryAssistant :: QueryAssistant -> TestTree
requestQueryAssistant =
  req
    "QueryAssistant"
    "fixture/QueryAssistant.yaml"

requestDeleteKnowledgeBase :: DeleteKnowledgeBase -> TestTree
requestDeleteKnowledgeBase =
  req
    "DeleteKnowledgeBase"
    "fixture/DeleteKnowledgeBase.yaml"

requestCreateAssistantAssociation :: CreateAssistantAssociation -> TestTree
requestCreateAssistantAssociation =
  req
    "CreateAssistantAssociation"
    "fixture/CreateAssistantAssociation.yaml"

requestGetKnowledgeBase :: GetKnowledgeBase -> TestTree
requestGetKnowledgeBase =
  req
    "GetKnowledgeBase"
    "fixture/GetKnowledgeBase.yaml"

-- Responses

responseListAssistantAssociations :: ListAssistantAssociationsResponse -> TestTree
responseListAssistantAssociations =
  res
    "ListAssistantAssociationsResponse"
    "fixture/ListAssistantAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssistantAssociations)

responseGetRecommendations :: GetRecommendationsResponse -> TestTree
responseGetRecommendations =
  res
    "GetRecommendationsResponse"
    "fixture/GetRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRecommendations)

responseSearchContent :: SearchContentResponse -> TestTree
responseSearchContent =
  res
    "SearchContentResponse"
    "fixture/SearchContentResponse.proto"
    defaultService
    (Proxy :: Proxy SearchContent)

responseRemoveKnowledgeBaseTemplateUri :: RemoveKnowledgeBaseTemplateUriResponse -> TestTree
responseRemoveKnowledgeBaseTemplateUri =
  res
    "RemoveKnowledgeBaseTemplateUriResponse"
    "fixture/RemoveKnowledgeBaseTemplateUriResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveKnowledgeBaseTemplateUri)

responseGetAssistant :: GetAssistantResponse -> TestTree
responseGetAssistant =
  res
    "GetAssistantResponse"
    "fixture/GetAssistantResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssistant)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListAssistants :: ListAssistantsResponse -> TestTree
responseListAssistants =
  res
    "ListAssistantsResponse"
    "fixture/ListAssistantsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssistants)

responseUpdateKnowledgeBaseTemplateUri :: UpdateKnowledgeBaseTemplateUriResponse -> TestTree
responseUpdateKnowledgeBaseTemplateUri =
  res
    "UpdateKnowledgeBaseTemplateUriResponse"
    "fixture/UpdateKnowledgeBaseTemplateUriResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateKnowledgeBaseTemplateUri)

responseListContents :: ListContentsResponse -> TestTree
responseListContents =
  res
    "ListContentsResponse"
    "fixture/ListContentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListContents)

responseDeleteContent :: DeleteContentResponse -> TestTree
responseDeleteContent =
  res
    "DeleteContentResponse"
    "fixture/DeleteContentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContent)

responseUpdateContent :: UpdateContentResponse -> TestTree
responseUpdateContent =
  res
    "UpdateContentResponse"
    "fixture/UpdateContentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContent)

responseCreateAssistant :: CreateAssistantResponse -> TestTree
responseCreateAssistant =
  res
    "CreateAssistantResponse"
    "fixture/CreateAssistantResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAssistant)

responseGetContentSummary :: GetContentSummaryResponse -> TestTree
responseGetContentSummary =
  res
    "GetContentSummaryResponse"
    "fixture/GetContentSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetContentSummary)

responseNotifyRecommendationsReceived :: NotifyRecommendationsReceivedResponse -> TestTree
responseNotifyRecommendationsReceived =
  res
    "NotifyRecommendationsReceivedResponse"
    "fixture/NotifyRecommendationsReceivedResponse.proto"
    defaultService
    (Proxy :: Proxy NotifyRecommendationsReceived)

responseDeleteAssistantAssociation :: DeleteAssistantAssociationResponse -> TestTree
responseDeleteAssistantAssociation =
  res
    "DeleteAssistantAssociationResponse"
    "fixture/DeleteAssistantAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssistantAssociation)

responseGetContent :: GetContentResponse -> TestTree
responseGetContent =
  res
    "GetContentResponse"
    "fixture/GetContentResponse.proto"
    defaultService
    (Proxy :: Proxy GetContent)

responseStartContentUpload :: StartContentUploadResponse -> TestTree
responseStartContentUpload =
  res
    "StartContentUploadResponse"
    "fixture/StartContentUploadResponse.proto"
    defaultService
    (Proxy :: Proxy StartContentUpload)

responseCreateSession :: CreateSessionResponse -> TestTree
responseCreateSession =
  res
    "CreateSessionResponse"
    "fixture/CreateSessionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSession)

responseCreateContent :: CreateContentResponse -> TestTree
responseCreateContent =
  res
    "CreateContentResponse"
    "fixture/CreateContentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContent)

responseDeleteAssistant :: DeleteAssistantResponse -> TestTree
responseDeleteAssistant =
  res
    "DeleteAssistantResponse"
    "fixture/DeleteAssistantResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAssistant)

responseGetSession :: GetSessionResponse -> TestTree
responseGetSession =
  res
    "GetSessionResponse"
    "fixture/GetSessionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSession)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateKnowledgeBase :: CreateKnowledgeBaseResponse -> TestTree
responseCreateKnowledgeBase =
  res
    "CreateKnowledgeBaseResponse"
    "fixture/CreateKnowledgeBaseResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKnowledgeBase)

responseGetAssistantAssociation :: GetAssistantAssociationResponse -> TestTree
responseGetAssistantAssociation =
  res
    "GetAssistantAssociationResponse"
    "fixture/GetAssistantAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssistantAssociation)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseSearchSessions :: SearchSessionsResponse -> TestTree
responseSearchSessions =
  res
    "SearchSessionsResponse"
    "fixture/SearchSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy SearchSessions)

responseListKnowledgeBases :: ListKnowledgeBasesResponse -> TestTree
responseListKnowledgeBases =
  res
    "ListKnowledgeBasesResponse"
    "fixture/ListKnowledgeBasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListKnowledgeBases)

responseQueryAssistant :: QueryAssistantResponse -> TestTree
responseQueryAssistant =
  res
    "QueryAssistantResponse"
    "fixture/QueryAssistantResponse.proto"
    defaultService
    (Proxy :: Proxy QueryAssistant)

responseDeleteKnowledgeBase :: DeleteKnowledgeBaseResponse -> TestTree
responseDeleteKnowledgeBase =
  res
    "DeleteKnowledgeBaseResponse"
    "fixture/DeleteKnowledgeBaseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKnowledgeBase)

responseCreateAssistantAssociation :: CreateAssistantAssociationResponse -> TestTree
responseCreateAssistantAssociation =
  res
    "CreateAssistantAssociationResponse"
    "fixture/CreateAssistantAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAssistantAssociation)

responseGetKnowledgeBase :: GetKnowledgeBaseResponse -> TestTree
responseGetKnowledgeBase =
  res
    "GetKnowledgeBaseResponse"
    "fixture/GetKnowledgeBaseResponse.proto"
    defaultService
    (Proxy :: Proxy GetKnowledgeBase)

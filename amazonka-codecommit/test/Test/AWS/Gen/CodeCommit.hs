{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeCommit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CodeCommit where

import Data.Proxy
import Network.AWS.CodeCommit
import Test.AWS.CodeCommit.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestMergePullRequestByFastForward $
--             mergePullRequestByFastForward
--
--         , requestUpdateRepositoryName $
--             updateRepositoryName
--
--         , requestPostCommentForPullRequest $
--             postCommentForPullRequest
--
--         , requestGetCommit $
--             getCommit
--
--         , requestGetBranch $
--             getBranch
--
--         , requestGetDifferences $
--             getDifferences
--
--         , requestGetPullRequest $
--             getPullRequest
--
--         , requestListPullRequests $
--             listPullRequests
--
--         , requestGetComment $
--             getComment
--
--         , requestDeleteBranch $
--             deleteBranch
--
--         , requestUpdateRepositoryDescription $
--             updateRepositoryDescription
--
--         , requestCreateBranch $
--             createBranch
--
--         , requestCreatePullRequest $
--             createPullRequest
--
--         , requestListBranches $
--             listBranches
--
--         , requestUpdatePullRequestDescription $
--             updatePullRequestDescription
--
--         , requestListRepositories $
--             listRepositories
--
--         , requestCreateRepository $
--             createRepository
--
--         , requestUpdateDefaultBranch $
--             updateDefaultBranch
--
--         , requestPostCommentReply $
--             postCommentReply
--
--         , requestGetRepository $
--             getRepository
--
--         , requestGetRepositoryTriggers $
--             getRepositoryTriggers
--
--         , requestPutFile $
--             putFile
--
--         , requestGetCommentsForComparedCommit $
--             getCommentsForComparedCommit
--
--         , requestTestRepositoryTriggers $
--             testRepositoryTriggers
--
--         , requestUpdateComment $
--             updateComment
--
--         , requestPostCommentForComparedCommit $
--             postCommentForComparedCommit
--
--         , requestUpdatePullRequestTitle $
--             updatePullRequestTitle
--
--         , requestGetBlob $
--             getBlob
--
--         , requestPutRepositoryTriggers $
--             putRepositoryTriggers
--
--         , requestGetMergeConflicts $
--             getMergeConflicts
--
--         , requestDeleteRepository $
--             deleteRepository
--
--         , requestDeleteCommentContent $
--             deleteCommentContent
--
--         , requestDescribePullRequestEvents $
--             describePullRequestEvents
--
--         , requestBatchGetRepositories $
--             batchGetRepositories
--
--         , requestGetCommentsForPullRequest $
--             getCommentsForPullRequest
--
--         , requestUpdatePullRequestStatus $
--             updatePullRequestStatus
--
--           ]

--     , testGroup "response"
--         [ responseMergePullRequestByFastForward $
--             mergePullRequestByFastForwardResponse
--
--         , responseUpdateRepositoryName $
--             updateRepositoryNameResponse
--
--         , responsePostCommentForPullRequest $
--             postCommentForPullRequestResponse
--
--         , responseGetCommit $
--             getCommitResponse
--
--         , responseGetBranch $
--             getBranchResponse
--
--         , responseGetDifferences $
--             getDifferencesResponse
--
--         , responseGetPullRequest $
--             getPullRequestResponse
--
--         , responseListPullRequests $
--             listPullRequestsResponse
--
--         , responseGetComment $
--             getCommentResponse
--
--         , responseDeleteBranch $
--             deleteBranchResponse
--
--         , responseUpdateRepositoryDescription $
--             updateRepositoryDescriptionResponse
--
--         , responseCreateBranch $
--             createBranchResponse
--
--         , responseCreatePullRequest $
--             createPullRequestResponse
--
--         , responseListBranches $
--             listBranchesResponse
--
--         , responseUpdatePullRequestDescription $
--             updatePullRequestDescriptionResponse
--
--         , responseListRepositories $
--             listRepositoriesResponse
--
--         , responseCreateRepository $
--             createRepositoryResponse
--
--         , responseUpdateDefaultBranch $
--             updateDefaultBranchResponse
--
--         , responsePostCommentReply $
--             postCommentReplyResponse
--
--         , responseGetRepository $
--             getRepositoryResponse
--
--         , responseGetRepositoryTriggers $
--             getRepositoryTriggersResponse
--
--         , responsePutFile $
--             putFileResponse
--
--         , responseGetCommentsForComparedCommit $
--             getCommentsForComparedCommitResponse
--
--         , responseTestRepositoryTriggers $
--             testRepositoryTriggersResponse
--
--         , responseUpdateComment $
--             updateCommentResponse
--
--         , responsePostCommentForComparedCommit $
--             postCommentForComparedCommitResponse
--
--         , responseUpdatePullRequestTitle $
--             updatePullRequestTitleResponse
--
--         , responseGetBlob $
--             getBlobResponse
--
--         , responsePutRepositoryTriggers $
--             putRepositoryTriggersResponse
--
--         , responseGetMergeConflicts $
--             getMergeConflictsResponse
--
--         , responseDeleteRepository $
--             deleteRepositoryResponse
--
--         , responseDeleteCommentContent $
--             deleteCommentContentResponse
--
--         , responseDescribePullRequestEvents $
--             describePullRequestEventsResponse
--
--         , responseBatchGetRepositories $
--             batchGetRepositoriesResponse
--
--         , responseGetCommentsForPullRequest $
--             getCommentsForPullRequestResponse
--
--         , responseUpdatePullRequestStatus $
--             updatePullRequestStatusResponse
--
--           ]
--     ]

-- Requests

requestMergePullRequestByFastForward :: MergePullRequestByFastForward -> TestTree
requestMergePullRequestByFastForward = req
    "MergePullRequestByFastForward"
    "fixture/MergePullRequestByFastForward.yaml"

requestUpdateRepositoryName :: UpdateRepositoryName -> TestTree
requestUpdateRepositoryName = req
    "UpdateRepositoryName"
    "fixture/UpdateRepositoryName.yaml"

requestPostCommentForPullRequest :: PostCommentForPullRequest -> TestTree
requestPostCommentForPullRequest = req
    "PostCommentForPullRequest"
    "fixture/PostCommentForPullRequest.yaml"

requestGetCommit :: GetCommit -> TestTree
requestGetCommit = req
    "GetCommit"
    "fixture/GetCommit.yaml"

requestGetBranch :: GetBranch -> TestTree
requestGetBranch = req
    "GetBranch"
    "fixture/GetBranch.yaml"

requestGetDifferences :: GetDifferences -> TestTree
requestGetDifferences = req
    "GetDifferences"
    "fixture/GetDifferences.yaml"

requestGetPullRequest :: GetPullRequest -> TestTree
requestGetPullRequest = req
    "GetPullRequest"
    "fixture/GetPullRequest.yaml"

requestListPullRequests :: ListPullRequests -> TestTree
requestListPullRequests = req
    "ListPullRequests"
    "fixture/ListPullRequests.yaml"

requestGetComment :: GetComment -> TestTree
requestGetComment = req
    "GetComment"
    "fixture/GetComment.yaml"

requestDeleteBranch :: DeleteBranch -> TestTree
requestDeleteBranch = req
    "DeleteBranch"
    "fixture/DeleteBranch.yaml"

requestUpdateRepositoryDescription :: UpdateRepositoryDescription -> TestTree
requestUpdateRepositoryDescription = req
    "UpdateRepositoryDescription"
    "fixture/UpdateRepositoryDescription.yaml"

requestCreateBranch :: CreateBranch -> TestTree
requestCreateBranch = req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

requestCreatePullRequest :: CreatePullRequest -> TestTree
requestCreatePullRequest = req
    "CreatePullRequest"
    "fixture/CreatePullRequest.yaml"

requestListBranches :: ListBranches -> TestTree
requestListBranches = req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestUpdatePullRequestDescription :: UpdatePullRequestDescription -> TestTree
requestUpdatePullRequestDescription = req
    "UpdatePullRequestDescription"
    "fixture/UpdatePullRequestDescription.yaml"

requestListRepositories :: ListRepositories -> TestTree
requestListRepositories = req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository = req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestUpdateDefaultBranch :: UpdateDefaultBranch -> TestTree
requestUpdateDefaultBranch = req
    "UpdateDefaultBranch"
    "fixture/UpdateDefaultBranch.yaml"

requestPostCommentReply :: PostCommentReply -> TestTree
requestPostCommentReply = req
    "PostCommentReply"
    "fixture/PostCommentReply.yaml"

requestGetRepository :: GetRepository -> TestTree
requestGetRepository = req
    "GetRepository"
    "fixture/GetRepository.yaml"

requestGetRepositoryTriggers :: GetRepositoryTriggers -> TestTree
requestGetRepositoryTriggers = req
    "GetRepositoryTriggers"
    "fixture/GetRepositoryTriggers.yaml"

requestPutFile :: PutFile -> TestTree
requestPutFile = req
    "PutFile"
    "fixture/PutFile.yaml"

requestGetCommentsForComparedCommit :: GetCommentsForComparedCommit -> TestTree
requestGetCommentsForComparedCommit = req
    "GetCommentsForComparedCommit"
    "fixture/GetCommentsForComparedCommit.yaml"

requestTestRepositoryTriggers :: TestRepositoryTriggers -> TestTree
requestTestRepositoryTriggers = req
    "TestRepositoryTriggers"
    "fixture/TestRepositoryTriggers.yaml"

requestUpdateComment :: UpdateComment -> TestTree
requestUpdateComment = req
    "UpdateComment"
    "fixture/UpdateComment.yaml"

requestPostCommentForComparedCommit :: PostCommentForComparedCommit -> TestTree
requestPostCommentForComparedCommit = req
    "PostCommentForComparedCommit"
    "fixture/PostCommentForComparedCommit.yaml"

requestUpdatePullRequestTitle :: UpdatePullRequestTitle -> TestTree
requestUpdatePullRequestTitle = req
    "UpdatePullRequestTitle"
    "fixture/UpdatePullRequestTitle.yaml"

requestGetBlob :: GetBlob -> TestTree
requestGetBlob = req
    "GetBlob"
    "fixture/GetBlob.yaml"

requestPutRepositoryTriggers :: PutRepositoryTriggers -> TestTree
requestPutRepositoryTriggers = req
    "PutRepositoryTriggers"
    "fixture/PutRepositoryTriggers.yaml"

requestGetMergeConflicts :: GetMergeConflicts -> TestTree
requestGetMergeConflicts = req
    "GetMergeConflicts"
    "fixture/GetMergeConflicts.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository = req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestDeleteCommentContent :: DeleteCommentContent -> TestTree
requestDeleteCommentContent = req
    "DeleteCommentContent"
    "fixture/DeleteCommentContent.yaml"

requestDescribePullRequestEvents :: DescribePullRequestEvents -> TestTree
requestDescribePullRequestEvents = req
    "DescribePullRequestEvents"
    "fixture/DescribePullRequestEvents.yaml"

requestBatchGetRepositories :: BatchGetRepositories -> TestTree
requestBatchGetRepositories = req
    "BatchGetRepositories"
    "fixture/BatchGetRepositories.yaml"

requestGetCommentsForPullRequest :: GetCommentsForPullRequest -> TestTree
requestGetCommentsForPullRequest = req
    "GetCommentsForPullRequest"
    "fixture/GetCommentsForPullRequest.yaml"

requestUpdatePullRequestStatus :: UpdatePullRequestStatus -> TestTree
requestUpdatePullRequestStatus = req
    "UpdatePullRequestStatus"
    "fixture/UpdatePullRequestStatus.yaml"

-- Responses

responseMergePullRequestByFastForward :: MergePullRequestByFastForwardResponse -> TestTree
responseMergePullRequestByFastForward = res
    "MergePullRequestByFastForwardResponse"
    "fixture/MergePullRequestByFastForwardResponse.proto"
    codeCommit
    (Proxy :: Proxy MergePullRequestByFastForward)

responseUpdateRepositoryName :: UpdateRepositoryNameResponse -> TestTree
responseUpdateRepositoryName = res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateRepositoryName)

responsePostCommentForPullRequest :: PostCommentForPullRequestResponse -> TestTree
responsePostCommentForPullRequest = res
    "PostCommentForPullRequestResponse"
    "fixture/PostCommentForPullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy PostCommentForPullRequest)

responseGetCommit :: GetCommitResponse -> TestTree
responseGetCommit = res
    "GetCommitResponse"
    "fixture/GetCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy GetCommit)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch = res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy GetBranch)

responseGetDifferences :: GetDifferencesResponse -> TestTree
responseGetDifferences = res
    "GetDifferencesResponse"
    "fixture/GetDifferencesResponse.proto"
    codeCommit
    (Proxy :: Proxy GetDifferences)

responseGetPullRequest :: GetPullRequestResponse -> TestTree
responseGetPullRequest = res
    "GetPullRequestResponse"
    "fixture/GetPullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy GetPullRequest)

responseListPullRequests :: ListPullRequestsResponse -> TestTree
responseListPullRequests = res
    "ListPullRequestsResponse"
    "fixture/ListPullRequestsResponse.proto"
    codeCommit
    (Proxy :: Proxy ListPullRequests)

responseGetComment :: GetCommentResponse -> TestTree
responseGetComment = res
    "GetCommentResponse"
    "fixture/GetCommentResponse.proto"
    codeCommit
    (Proxy :: Proxy GetComment)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch = res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteBranch)

responseUpdateRepositoryDescription :: UpdateRepositoryDescriptionResponse -> TestTree
responseUpdateRepositoryDescription = res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateRepositoryDescription)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch = res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateBranch)

responseCreatePullRequest :: CreatePullRequestResponse -> TestTree
responseCreatePullRequest = res
    "CreatePullRequestResponse"
    "fixture/CreatePullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy CreatePullRequest)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches = res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListBranches)

responseUpdatePullRequestDescription :: UpdatePullRequestDescriptionResponse -> TestTree
responseUpdatePullRequestDescription = res
    "UpdatePullRequestDescriptionResponse"
    "fixture/UpdatePullRequestDescriptionResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestDescription)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories = res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListRepositories)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository = res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateRepository)

responseUpdateDefaultBranch :: UpdateDefaultBranchResponse -> TestTree
responseUpdateDefaultBranch = res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateDefaultBranch)

responsePostCommentReply :: PostCommentReplyResponse -> TestTree
responsePostCommentReply = res
    "PostCommentReplyResponse"
    "fixture/PostCommentReplyResponse.proto"
    codeCommit
    (Proxy :: Proxy PostCommentReply)

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository = res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy GetRepository)

responseGetRepositoryTriggers :: GetRepositoryTriggersResponse -> TestTree
responseGetRepositoryTriggers = res
    "GetRepositoryTriggersResponse"
    "fixture/GetRepositoryTriggersResponse.proto"
    codeCommit
    (Proxy :: Proxy GetRepositoryTriggers)

responsePutFile :: PutFileResponse -> TestTree
responsePutFile = res
    "PutFileResponse"
    "fixture/PutFileResponse.proto"
    codeCommit
    (Proxy :: Proxy PutFile)

responseGetCommentsForComparedCommit :: GetCommentsForComparedCommitResponse -> TestTree
responseGetCommentsForComparedCommit = res
    "GetCommentsForComparedCommitResponse"
    "fixture/GetCommentsForComparedCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy GetCommentsForComparedCommit)

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers = res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    codeCommit
    (Proxy :: Proxy TestRepositoryTriggers)

responseUpdateComment :: UpdateCommentResponse -> TestTree
responseUpdateComment = res
    "UpdateCommentResponse"
    "fixture/UpdateCommentResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateComment)

responsePostCommentForComparedCommit :: PostCommentForComparedCommitResponse -> TestTree
responsePostCommentForComparedCommit = res
    "PostCommentForComparedCommitResponse"
    "fixture/PostCommentForComparedCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy PostCommentForComparedCommit)

responseUpdatePullRequestTitle :: UpdatePullRequestTitleResponse -> TestTree
responseUpdatePullRequestTitle = res
    "UpdatePullRequestTitleResponse"
    "fixture/UpdatePullRequestTitleResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestTitle)

responseGetBlob :: GetBlobResponse -> TestTree
responseGetBlob = res
    "GetBlobResponse"
    "fixture/GetBlobResponse.proto"
    codeCommit
    (Proxy :: Proxy GetBlob)

responsePutRepositoryTriggers :: PutRepositoryTriggersResponse -> TestTree
responsePutRepositoryTriggers = res
    "PutRepositoryTriggersResponse"
    "fixture/PutRepositoryTriggersResponse.proto"
    codeCommit
    (Proxy :: Proxy PutRepositoryTriggers)

responseGetMergeConflicts :: GetMergeConflictsResponse -> TestTree
responseGetMergeConflicts = res
    "GetMergeConflictsResponse"
    "fixture/GetMergeConflictsResponse.proto"
    codeCommit
    (Proxy :: Proxy GetMergeConflicts)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository = res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteRepository)

responseDeleteCommentContent :: DeleteCommentContentResponse -> TestTree
responseDeleteCommentContent = res
    "DeleteCommentContentResponse"
    "fixture/DeleteCommentContentResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteCommentContent)

responseDescribePullRequestEvents :: DescribePullRequestEventsResponse -> TestTree
responseDescribePullRequestEvents = res
    "DescribePullRequestEventsResponse"
    "fixture/DescribePullRequestEventsResponse.proto"
    codeCommit
    (Proxy :: Proxy DescribePullRequestEvents)

responseBatchGetRepositories :: BatchGetRepositoriesResponse -> TestTree
responseBatchGetRepositories = res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchGetRepositories)

responseGetCommentsForPullRequest :: GetCommentsForPullRequestResponse -> TestTree
responseGetCommentsForPullRequest = res
    "GetCommentsForPullRequestResponse"
    "fixture/GetCommentsForPullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy GetCommentsForPullRequest)

responseUpdatePullRequestStatus :: UpdatePullRequestStatusResponse -> TestTree
responseUpdatePullRequestStatus = res
    "UpdatePullRequestStatusResponse"
    "fixture/UpdatePullRequestStatusResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestStatus)

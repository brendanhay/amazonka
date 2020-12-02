{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         , requestMergeBranchesBySquash $
--             mergeBranchesBySquash
--
--         , requestGetCommit $
--             getCommit
--
--         , requestBatchAssociateApprovalRuleTemplateWithRepositories $
--             batchAssociateApprovalRuleTemplateWithRepositories
--
--         , requestGetCommentReactions $
--             getCommentReactions
--
--         , requestGetApprovalRuleTemplate $
--             getApprovalRuleTemplate
--
--         , requestDisassociateApprovalRuleTemplateFromRepository $
--             disassociateApprovalRuleTemplateFromRepository
--
--         , requestGetBranch $
--             getBranch
--
--         , requestGetDifferences $
--             getDifferences
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestGetPullRequest $
--             getPullRequest
--
--         , requestOverridePullRequestApprovalRules $
--             overridePullRequestApprovalRules
--
--         , requestListPullRequests $
--             listPullRequests
--
--         , requestCreateCommit $
--             createCommit
--
--         , requestUpdatePullRequestApprovalState $
--             updatePullRequestApprovalState
--
--         , requestEvaluatePullRequestApprovalRules $
--             evaluatePullRequestApprovalRules
--
--         , requestGetComment $
--             getComment
--
--         , requestCreateApprovalRuleTemplate $
--             createApprovalRuleTemplate
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
--         , requestGetFolder $
--             getFolder
--
--         , requestCreatePullRequest $
--             createPullRequest
--
--         , requestDeleteApprovalRuleTemplate $
--             deleteApprovalRuleTemplate
--
--         , requestListBranches $
--             listBranches
--
--         , requestBatchGetCommits $
--             batchGetCommits
--
--         , requestPutCommentReaction $
--             putCommentReaction
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
--         , requestGetMergeOptions $
--             getMergeOptions
--
--         , requestCreatePullRequestApprovalRule $
--             createPullRequestApprovalRule
--
--         , requestPostCommentReply $
--             postCommentReply
--
--         , requestUpdateApprovalRuleTemplateContent $
--             updateApprovalRuleTemplateContent
--
--         , requestCreateUnreferencedMergeCommit $
--             createUnreferencedMergeCommit
--
--         , requestListRepositoriesForApprovalRuleTemplate $
--             listRepositoriesForApprovalRuleTemplate
--
--         , requestGetRepository $
--             getRepository
--
--         , requestBatchDescribeMergeConflicts $
--             batchDescribeMergeConflicts
--
--         , requestDeletePullRequestApprovalRule $
--             deletePullRequestApprovalRule
--
--         , requestGetRepositoryTriggers $
--             getRepositoryTriggers
--
--         , requestUpdateApprovalRuleTemplateName $
--             updateApprovalRuleTemplateName
--
--         , requestPutFile $
--             putFile
--
--         , requestDeleteFile $
--             deleteFile
--
--         , requestGetCommentsForComparedCommit $
--             getCommentsForComparedCommit
--
--         , requestGetMergeCommit $
--             getMergeCommit
--
--         , requestTestRepositoryTriggers $
--             testRepositoryTriggers
--
--         , requestMergePullRequestBySquash $
--             mergePullRequestBySquash
--
--         , requestUpdateComment $
--             updateComment
--
--         , requestPostCommentForComparedCommit $
--             postCommentForComparedCommit
--
--         , requestMergeBranchesByFastForward $
--             mergeBranchesByFastForward
--
--         , requestUpdatePullRequestTitle $
--             updatePullRequestTitle
--
--         , requestBatchDisassociateApprovalRuleTemplateFromRepositories $
--             batchDisassociateApprovalRuleTemplateFromRepositories
--
--         , requestUpdatePullRequestApprovalRuleContent $
--             updatePullRequestApprovalRuleContent
--
--         , requestGetBlob $
--             getBlob
--
--         , requestAssociateApprovalRuleTemplateWithRepository $
--             associateApprovalRuleTemplateWithRepository
--
--         , requestPutRepositoryTriggers $
--             putRepositoryTriggers
--
--         , requestListApprovalRuleTemplates $
--             listApprovalRuleTemplates
--
--         , requestDescribeMergeConflicts $
--             describeMergeConflicts
--
--         , requestTagResource $
--             tagResource
--
--         , requestMergeBranchesByThreeWay $
--             mergeBranchesByThreeWay
--
--         , requestGetFile $
--             getFile
--
--         , requestUntagResource $
--             untagResource
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
--         , requestMergePullRequestByThreeWay $
--             mergePullRequestByThreeWay
--
--         , requestDescribePullRequestEvents $
--             describePullRequestEvents
--
--         , requestBatchGetRepositories $
--             batchGetRepositories
--
--         , requestUpdateApprovalRuleTemplateDescription $
--             updateApprovalRuleTemplateDescription
--
--         , requestGetPullRequestOverrideState $
--             getPullRequestOverrideState
--
--         , requestGetPullRequestApprovalStates $
--             getPullRequestApprovalStates
--
--         , requestGetCommentsForPullRequest $
--             getCommentsForPullRequest
--
--         , requestUpdatePullRequestStatus $
--             updatePullRequestStatus
--
--         , requestListAssociatedApprovalRuleTemplatesForRepository $
--             listAssociatedApprovalRuleTemplatesForRepository
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
--         , responseMergeBranchesBySquash $
--             mergeBranchesBySquashResponse
--
--         , responseGetCommit $
--             getCommitResponse
--
--         , responseBatchAssociateApprovalRuleTemplateWithRepositories $
--             batchAssociateApprovalRuleTemplateWithRepositoriesResponse
--
--         , responseGetCommentReactions $
--             getCommentReactionsResponse
--
--         , responseGetApprovalRuleTemplate $
--             getApprovalRuleTemplateResponse
--
--         , responseDisassociateApprovalRuleTemplateFromRepository $
--             disassociateApprovalRuleTemplateFromRepositoryResponse
--
--         , responseGetBranch $
--             getBranchResponse
--
--         , responseGetDifferences $
--             getDifferencesResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseGetPullRequest $
--             getPullRequestResponse
--
--         , responseOverridePullRequestApprovalRules $
--             overridePullRequestApprovalRulesResponse
--
--         , responseListPullRequests $
--             listPullRequestsResponse
--
--         , responseCreateCommit $
--             createCommitResponse
--
--         , responseUpdatePullRequestApprovalState $
--             updatePullRequestApprovalStateResponse
--
--         , responseEvaluatePullRequestApprovalRules $
--             evaluatePullRequestApprovalRulesResponse
--
--         , responseGetComment $
--             getCommentResponse
--
--         , responseCreateApprovalRuleTemplate $
--             createApprovalRuleTemplateResponse
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
--         , responseGetFolder $
--             getFolderResponse
--
--         , responseCreatePullRequest $
--             createPullRequestResponse
--
--         , responseDeleteApprovalRuleTemplate $
--             deleteApprovalRuleTemplateResponse
--
--         , responseListBranches $
--             listBranchesResponse
--
--         , responseBatchGetCommits $
--             batchGetCommitsResponse
--
--         , responsePutCommentReaction $
--             putCommentReactionResponse
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
--         , responseGetMergeOptions $
--             getMergeOptionsResponse
--
--         , responseCreatePullRequestApprovalRule $
--             createPullRequestApprovalRuleResponse
--
--         , responsePostCommentReply $
--             postCommentReplyResponse
--
--         , responseUpdateApprovalRuleTemplateContent $
--             updateApprovalRuleTemplateContentResponse
--
--         , responseCreateUnreferencedMergeCommit $
--             createUnreferencedMergeCommitResponse
--
--         , responseListRepositoriesForApprovalRuleTemplate $
--             listRepositoriesForApprovalRuleTemplateResponse
--
--         , responseGetRepository $
--             getRepositoryResponse
--
--         , responseBatchDescribeMergeConflicts $
--             batchDescribeMergeConflictsResponse
--
--         , responseDeletePullRequestApprovalRule $
--             deletePullRequestApprovalRuleResponse
--
--         , responseGetRepositoryTriggers $
--             getRepositoryTriggersResponse
--
--         , responseUpdateApprovalRuleTemplateName $
--             updateApprovalRuleTemplateNameResponse
--
--         , responsePutFile $
--             putFileResponse
--
--         , responseDeleteFile $
--             deleteFileResponse
--
--         , responseGetCommentsForComparedCommit $
--             getCommentsForComparedCommitResponse
--
--         , responseGetMergeCommit $
--             getMergeCommitResponse
--
--         , responseTestRepositoryTriggers $
--             testRepositoryTriggersResponse
--
--         , responseMergePullRequestBySquash $
--             mergePullRequestBySquashResponse
--
--         , responseUpdateComment $
--             updateCommentResponse
--
--         , responsePostCommentForComparedCommit $
--             postCommentForComparedCommitResponse
--
--         , responseMergeBranchesByFastForward $
--             mergeBranchesByFastForwardResponse
--
--         , responseUpdatePullRequestTitle $
--             updatePullRequestTitleResponse
--
--         , responseBatchDisassociateApprovalRuleTemplateFromRepositories $
--             batchDisassociateApprovalRuleTemplateFromRepositoriesResponse
--
--         , responseUpdatePullRequestApprovalRuleContent $
--             updatePullRequestApprovalRuleContentResponse
--
--         , responseGetBlob $
--             getBlobResponse
--
--         , responseAssociateApprovalRuleTemplateWithRepository $
--             associateApprovalRuleTemplateWithRepositoryResponse
--
--         , responsePutRepositoryTriggers $
--             putRepositoryTriggersResponse
--
--         , responseListApprovalRuleTemplates $
--             listApprovalRuleTemplatesResponse
--
--         , responseDescribeMergeConflicts $
--             describeMergeConflictsResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseMergeBranchesByThreeWay $
--             mergeBranchesByThreeWayResponse
--
--         , responseGetFile $
--             getFileResponse
--
--         , responseUntagResource $
--             untagResourceResponse
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
--         , responseMergePullRequestByThreeWay $
--             mergePullRequestByThreeWayResponse
--
--         , responseDescribePullRequestEvents $
--             describePullRequestEventsResponse
--
--         , responseBatchGetRepositories $
--             batchGetRepositoriesResponse
--
--         , responseUpdateApprovalRuleTemplateDescription $
--             updateApprovalRuleTemplateDescriptionResponse
--
--         , responseGetPullRequestOverrideState $
--             getPullRequestOverrideStateResponse
--
--         , responseGetPullRequestApprovalStates $
--             getPullRequestApprovalStatesResponse
--
--         , responseGetCommentsForPullRequest $
--             getCommentsForPullRequestResponse
--
--         , responseUpdatePullRequestStatus $
--             updatePullRequestStatusResponse
--
--         , responseListAssociatedApprovalRuleTemplatesForRepository $
--             listAssociatedApprovalRuleTemplatesForRepositoryResponse
--
--           ]
--     ]

-- Requests

requestMergePullRequestByFastForward :: MergePullRequestByFastForward -> TestTree
requestMergePullRequestByFastForward =
  req
    "MergePullRequestByFastForward"
    "fixture/MergePullRequestByFastForward.yaml"

requestUpdateRepositoryName :: UpdateRepositoryName -> TestTree
requestUpdateRepositoryName =
  req
    "UpdateRepositoryName"
    "fixture/UpdateRepositoryName.yaml"

requestPostCommentForPullRequest :: PostCommentForPullRequest -> TestTree
requestPostCommentForPullRequest =
  req
    "PostCommentForPullRequest"
    "fixture/PostCommentForPullRequest.yaml"

requestMergeBranchesBySquash :: MergeBranchesBySquash -> TestTree
requestMergeBranchesBySquash =
  req
    "MergeBranchesBySquash"
    "fixture/MergeBranchesBySquash.yaml"

requestGetCommit :: GetCommit -> TestTree
requestGetCommit =
  req
    "GetCommit"
    "fixture/GetCommit.yaml"

requestBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositories -> TestTree
requestBatchAssociateApprovalRuleTemplateWithRepositories =
  req
    "BatchAssociateApprovalRuleTemplateWithRepositories"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositories.yaml"

requestGetCommentReactions :: GetCommentReactions -> TestTree
requestGetCommentReactions =
  req
    "GetCommentReactions"
    "fixture/GetCommentReactions.yaml"

requestGetApprovalRuleTemplate :: GetApprovalRuleTemplate -> TestTree
requestGetApprovalRuleTemplate =
  req
    "GetApprovalRuleTemplate"
    "fixture/GetApprovalRuleTemplate.yaml"

requestDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepository -> TestTree
requestDisassociateApprovalRuleTemplateFromRepository =
  req
    "DisassociateApprovalRuleTemplateFromRepository"
    "fixture/DisassociateApprovalRuleTemplateFromRepository.yaml"

requestGetBranch :: GetBranch -> TestTree
requestGetBranch =
  req
    "GetBranch"
    "fixture/GetBranch.yaml"

requestGetDifferences :: GetDifferences -> TestTree
requestGetDifferences =
  req
    "GetDifferences"
    "fixture/GetDifferences.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetPullRequest :: GetPullRequest -> TestTree
requestGetPullRequest =
  req
    "GetPullRequest"
    "fixture/GetPullRequest.yaml"

requestOverridePullRequestApprovalRules :: OverridePullRequestApprovalRules -> TestTree
requestOverridePullRequestApprovalRules =
  req
    "OverridePullRequestApprovalRules"
    "fixture/OverridePullRequestApprovalRules.yaml"

requestListPullRequests :: ListPullRequests -> TestTree
requestListPullRequests =
  req
    "ListPullRequests"
    "fixture/ListPullRequests.yaml"

requestCreateCommit :: CreateCommit -> TestTree
requestCreateCommit =
  req
    "CreateCommit"
    "fixture/CreateCommit.yaml"

requestUpdatePullRequestApprovalState :: UpdatePullRequestApprovalState -> TestTree
requestUpdatePullRequestApprovalState =
  req
    "UpdatePullRequestApprovalState"
    "fixture/UpdatePullRequestApprovalState.yaml"

requestEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRules -> TestTree
requestEvaluatePullRequestApprovalRules =
  req
    "EvaluatePullRequestApprovalRules"
    "fixture/EvaluatePullRequestApprovalRules.yaml"

requestGetComment :: GetComment -> TestTree
requestGetComment =
  req
    "GetComment"
    "fixture/GetComment.yaml"

requestCreateApprovalRuleTemplate :: CreateApprovalRuleTemplate -> TestTree
requestCreateApprovalRuleTemplate =
  req
    "CreateApprovalRuleTemplate"
    "fixture/CreateApprovalRuleTemplate.yaml"

requestDeleteBranch :: DeleteBranch -> TestTree
requestDeleteBranch =
  req
    "DeleteBranch"
    "fixture/DeleteBranch.yaml"

requestUpdateRepositoryDescription :: UpdateRepositoryDescription -> TestTree
requestUpdateRepositoryDescription =
  req
    "UpdateRepositoryDescription"
    "fixture/UpdateRepositoryDescription.yaml"

requestCreateBranch :: CreateBranch -> TestTree
requestCreateBranch =
  req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder =
  req
    "GetFolder"
    "fixture/GetFolder.yaml"

requestCreatePullRequest :: CreatePullRequest -> TestTree
requestCreatePullRequest =
  req
    "CreatePullRequest"
    "fixture/CreatePullRequest.yaml"

requestDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplate -> TestTree
requestDeleteApprovalRuleTemplate =
  req
    "DeleteApprovalRuleTemplate"
    "fixture/DeleteApprovalRuleTemplate.yaml"

requestListBranches :: ListBranches -> TestTree
requestListBranches =
  req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestBatchGetCommits :: BatchGetCommits -> TestTree
requestBatchGetCommits =
  req
    "BatchGetCommits"
    "fixture/BatchGetCommits.yaml"

requestPutCommentReaction :: PutCommentReaction -> TestTree
requestPutCommentReaction =
  req
    "PutCommentReaction"
    "fixture/PutCommentReaction.yaml"

requestUpdatePullRequestDescription :: UpdatePullRequestDescription -> TestTree
requestUpdatePullRequestDescription =
  req
    "UpdatePullRequestDescription"
    "fixture/UpdatePullRequestDescription.yaml"

requestListRepositories :: ListRepositories -> TestTree
requestListRepositories =
  req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestUpdateDefaultBranch :: UpdateDefaultBranch -> TestTree
requestUpdateDefaultBranch =
  req
    "UpdateDefaultBranch"
    "fixture/UpdateDefaultBranch.yaml"

requestGetMergeOptions :: GetMergeOptions -> TestTree
requestGetMergeOptions =
  req
    "GetMergeOptions"
    "fixture/GetMergeOptions.yaml"

requestCreatePullRequestApprovalRule :: CreatePullRequestApprovalRule -> TestTree
requestCreatePullRequestApprovalRule =
  req
    "CreatePullRequestApprovalRule"
    "fixture/CreatePullRequestApprovalRule.yaml"

requestPostCommentReply :: PostCommentReply -> TestTree
requestPostCommentReply =
  req
    "PostCommentReply"
    "fixture/PostCommentReply.yaml"

requestUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContent -> TestTree
requestUpdateApprovalRuleTemplateContent =
  req
    "UpdateApprovalRuleTemplateContent"
    "fixture/UpdateApprovalRuleTemplateContent.yaml"

requestCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommit -> TestTree
requestCreateUnreferencedMergeCommit =
  req
    "CreateUnreferencedMergeCommit"
    "fixture/CreateUnreferencedMergeCommit.yaml"

requestListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplate -> TestTree
requestListRepositoriesForApprovalRuleTemplate =
  req
    "ListRepositoriesForApprovalRuleTemplate"
    "fixture/ListRepositoriesForApprovalRuleTemplate.yaml"

requestGetRepository :: GetRepository -> TestTree
requestGetRepository =
  req
    "GetRepository"
    "fixture/GetRepository.yaml"

requestBatchDescribeMergeConflicts :: BatchDescribeMergeConflicts -> TestTree
requestBatchDescribeMergeConflicts =
  req
    "BatchDescribeMergeConflicts"
    "fixture/BatchDescribeMergeConflicts.yaml"

requestDeletePullRequestApprovalRule :: DeletePullRequestApprovalRule -> TestTree
requestDeletePullRequestApprovalRule =
  req
    "DeletePullRequestApprovalRule"
    "fixture/DeletePullRequestApprovalRule.yaml"

requestGetRepositoryTriggers :: GetRepositoryTriggers -> TestTree
requestGetRepositoryTriggers =
  req
    "GetRepositoryTriggers"
    "fixture/GetRepositoryTriggers.yaml"

requestUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateName -> TestTree
requestUpdateApprovalRuleTemplateName =
  req
    "UpdateApprovalRuleTemplateName"
    "fixture/UpdateApprovalRuleTemplateName.yaml"

requestPutFile :: PutFile -> TestTree
requestPutFile =
  req
    "PutFile"
    "fixture/PutFile.yaml"

requestDeleteFile :: DeleteFile -> TestTree
requestDeleteFile =
  req
    "DeleteFile"
    "fixture/DeleteFile.yaml"

requestGetCommentsForComparedCommit :: GetCommentsForComparedCommit -> TestTree
requestGetCommentsForComparedCommit =
  req
    "GetCommentsForComparedCommit"
    "fixture/GetCommentsForComparedCommit.yaml"

requestGetMergeCommit :: GetMergeCommit -> TestTree
requestGetMergeCommit =
  req
    "GetMergeCommit"
    "fixture/GetMergeCommit.yaml"

requestTestRepositoryTriggers :: TestRepositoryTriggers -> TestTree
requestTestRepositoryTriggers =
  req
    "TestRepositoryTriggers"
    "fixture/TestRepositoryTriggers.yaml"

requestMergePullRequestBySquash :: MergePullRequestBySquash -> TestTree
requestMergePullRequestBySquash =
  req
    "MergePullRequestBySquash"
    "fixture/MergePullRequestBySquash.yaml"

requestUpdateComment :: UpdateComment -> TestTree
requestUpdateComment =
  req
    "UpdateComment"
    "fixture/UpdateComment.yaml"

requestPostCommentForComparedCommit :: PostCommentForComparedCommit -> TestTree
requestPostCommentForComparedCommit =
  req
    "PostCommentForComparedCommit"
    "fixture/PostCommentForComparedCommit.yaml"

requestMergeBranchesByFastForward :: MergeBranchesByFastForward -> TestTree
requestMergeBranchesByFastForward =
  req
    "MergeBranchesByFastForward"
    "fixture/MergeBranchesByFastForward.yaml"

requestUpdatePullRequestTitle :: UpdatePullRequestTitle -> TestTree
requestUpdatePullRequestTitle =
  req
    "UpdatePullRequestTitle"
    "fixture/UpdatePullRequestTitle.yaml"

requestBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositories -> TestTree
requestBatchDisassociateApprovalRuleTemplateFromRepositories =
  req
    "BatchDisassociateApprovalRuleTemplateFromRepositories"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositories.yaml"

requestUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContent -> TestTree
requestUpdatePullRequestApprovalRuleContent =
  req
    "UpdatePullRequestApprovalRuleContent"
    "fixture/UpdatePullRequestApprovalRuleContent.yaml"

requestGetBlob :: GetBlob -> TestTree
requestGetBlob =
  req
    "GetBlob"
    "fixture/GetBlob.yaml"

requestAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepository -> TestTree
requestAssociateApprovalRuleTemplateWithRepository =
  req
    "AssociateApprovalRuleTemplateWithRepository"
    "fixture/AssociateApprovalRuleTemplateWithRepository.yaml"

requestPutRepositoryTriggers :: PutRepositoryTriggers -> TestTree
requestPutRepositoryTriggers =
  req
    "PutRepositoryTriggers"
    "fixture/PutRepositoryTriggers.yaml"

requestListApprovalRuleTemplates :: ListApprovalRuleTemplates -> TestTree
requestListApprovalRuleTemplates =
  req
    "ListApprovalRuleTemplates"
    "fixture/ListApprovalRuleTemplates.yaml"

requestDescribeMergeConflicts :: DescribeMergeConflicts -> TestTree
requestDescribeMergeConflicts =
  req
    "DescribeMergeConflicts"
    "fixture/DescribeMergeConflicts.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestMergeBranchesByThreeWay :: MergeBranchesByThreeWay -> TestTree
requestMergeBranchesByThreeWay =
  req
    "MergeBranchesByThreeWay"
    "fixture/MergeBranchesByThreeWay.yaml"

requestGetFile :: GetFile -> TestTree
requestGetFile =
  req
    "GetFile"
    "fixture/GetFile.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetMergeConflicts :: GetMergeConflicts -> TestTree
requestGetMergeConflicts =
  req
    "GetMergeConflicts"
    "fixture/GetMergeConflicts.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestDeleteCommentContent :: DeleteCommentContent -> TestTree
requestDeleteCommentContent =
  req
    "DeleteCommentContent"
    "fixture/DeleteCommentContent.yaml"

requestMergePullRequestByThreeWay :: MergePullRequestByThreeWay -> TestTree
requestMergePullRequestByThreeWay =
  req
    "MergePullRequestByThreeWay"
    "fixture/MergePullRequestByThreeWay.yaml"

requestDescribePullRequestEvents :: DescribePullRequestEvents -> TestTree
requestDescribePullRequestEvents =
  req
    "DescribePullRequestEvents"
    "fixture/DescribePullRequestEvents.yaml"

requestBatchGetRepositories :: BatchGetRepositories -> TestTree
requestBatchGetRepositories =
  req
    "BatchGetRepositories"
    "fixture/BatchGetRepositories.yaml"

requestUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescription -> TestTree
requestUpdateApprovalRuleTemplateDescription =
  req
    "UpdateApprovalRuleTemplateDescription"
    "fixture/UpdateApprovalRuleTemplateDescription.yaml"

requestGetPullRequestOverrideState :: GetPullRequestOverrideState -> TestTree
requestGetPullRequestOverrideState =
  req
    "GetPullRequestOverrideState"
    "fixture/GetPullRequestOverrideState.yaml"

requestGetPullRequestApprovalStates :: GetPullRequestApprovalStates -> TestTree
requestGetPullRequestApprovalStates =
  req
    "GetPullRequestApprovalStates"
    "fixture/GetPullRequestApprovalStates.yaml"

requestGetCommentsForPullRequest :: GetCommentsForPullRequest -> TestTree
requestGetCommentsForPullRequest =
  req
    "GetCommentsForPullRequest"
    "fixture/GetCommentsForPullRequest.yaml"

requestUpdatePullRequestStatus :: UpdatePullRequestStatus -> TestTree
requestUpdatePullRequestStatus =
  req
    "UpdatePullRequestStatus"
    "fixture/UpdatePullRequestStatus.yaml"

requestListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepository -> TestTree
requestListAssociatedApprovalRuleTemplatesForRepository =
  req
    "ListAssociatedApprovalRuleTemplatesForRepository"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepository.yaml"

-- Responses

responseMergePullRequestByFastForward :: MergePullRequestByFastForwardResponse -> TestTree
responseMergePullRequestByFastForward =
  res
    "MergePullRequestByFastForwardResponse"
    "fixture/MergePullRequestByFastForwardResponse.proto"
    codeCommit
    (Proxy :: Proxy MergePullRequestByFastForward)

responseUpdateRepositoryName :: UpdateRepositoryNameResponse -> TestTree
responseUpdateRepositoryName =
  res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateRepositoryName)

responsePostCommentForPullRequest :: PostCommentForPullRequestResponse -> TestTree
responsePostCommentForPullRequest =
  res
    "PostCommentForPullRequestResponse"
    "fixture/PostCommentForPullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy PostCommentForPullRequest)

responseMergeBranchesBySquash :: MergeBranchesBySquashResponse -> TestTree
responseMergeBranchesBySquash =
  res
    "MergeBranchesBySquashResponse"
    "fixture/MergeBranchesBySquashResponse.proto"
    codeCommit
    (Proxy :: Proxy MergeBranchesBySquash)

responseGetCommit :: GetCommitResponse -> TestTree
responseGetCommit =
  res
    "GetCommitResponse"
    "fixture/GetCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy GetCommit)

responseBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> TestTree
responseBatchAssociateApprovalRuleTemplateWithRepositories =
  res
    "BatchAssociateApprovalRuleTemplateWithRepositoriesResponse"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchAssociateApprovalRuleTemplateWithRepositories)

responseGetCommentReactions :: GetCommentReactionsResponse -> TestTree
responseGetCommentReactions =
  res
    "GetCommentReactionsResponse"
    "fixture/GetCommentReactionsResponse.proto"
    codeCommit
    (Proxy :: Proxy GetCommentReactions)

responseGetApprovalRuleTemplate :: GetApprovalRuleTemplateResponse -> TestTree
responseGetApprovalRuleTemplate =
  res
    "GetApprovalRuleTemplateResponse"
    "fixture/GetApprovalRuleTemplateResponse.proto"
    codeCommit
    (Proxy :: Proxy GetApprovalRuleTemplate)

responseDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepositoryResponse -> TestTree
responseDisassociateApprovalRuleTemplateFromRepository =
  res
    "DisassociateApprovalRuleTemplateFromRepositoryResponse"
    "fixture/DisassociateApprovalRuleTemplateFromRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy DisassociateApprovalRuleTemplateFromRepository)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch =
  res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy GetBranch)

responseGetDifferences :: GetDifferencesResponse -> TestTree
responseGetDifferences =
  res
    "GetDifferencesResponse"
    "fixture/GetDifferencesResponse.proto"
    codeCommit
    (Proxy :: Proxy GetDifferences)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    codeCommit
    (Proxy :: Proxy ListTagsForResource)

responseGetPullRequest :: GetPullRequestResponse -> TestTree
responseGetPullRequest =
  res
    "GetPullRequestResponse"
    "fixture/GetPullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy GetPullRequest)

responseOverridePullRequestApprovalRules :: OverridePullRequestApprovalRulesResponse -> TestTree
responseOverridePullRequestApprovalRules =
  res
    "OverridePullRequestApprovalRulesResponse"
    "fixture/OverridePullRequestApprovalRulesResponse.proto"
    codeCommit
    (Proxy :: Proxy OverridePullRequestApprovalRules)

responseListPullRequests :: ListPullRequestsResponse -> TestTree
responseListPullRequests =
  res
    "ListPullRequestsResponse"
    "fixture/ListPullRequestsResponse.proto"
    codeCommit
    (Proxy :: Proxy ListPullRequests)

responseCreateCommit :: CreateCommitResponse -> TestTree
responseCreateCommit =
  res
    "CreateCommitResponse"
    "fixture/CreateCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateCommit)

responseUpdatePullRequestApprovalState :: UpdatePullRequestApprovalStateResponse -> TestTree
responseUpdatePullRequestApprovalState =
  res
    "UpdatePullRequestApprovalStateResponse"
    "fixture/UpdatePullRequestApprovalStateResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestApprovalState)

responseEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRulesResponse -> TestTree
responseEvaluatePullRequestApprovalRules =
  res
    "EvaluatePullRequestApprovalRulesResponse"
    "fixture/EvaluatePullRequestApprovalRulesResponse.proto"
    codeCommit
    (Proxy :: Proxy EvaluatePullRequestApprovalRules)

responseGetComment :: GetCommentResponse -> TestTree
responseGetComment =
  res
    "GetCommentResponse"
    "fixture/GetCommentResponse.proto"
    codeCommit
    (Proxy :: Proxy GetComment)

responseCreateApprovalRuleTemplate :: CreateApprovalRuleTemplateResponse -> TestTree
responseCreateApprovalRuleTemplate =
  res
    "CreateApprovalRuleTemplateResponse"
    "fixture/CreateApprovalRuleTemplateResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateApprovalRuleTemplate)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteBranch)

responseUpdateRepositoryDescription :: UpdateRepositoryDescriptionResponse -> TestTree
responseUpdateRepositoryDescription =
  res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateRepositoryDescription)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch =
  res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateBranch)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    codeCommit
    (Proxy :: Proxy GetFolder)

responseCreatePullRequest :: CreatePullRequestResponse -> TestTree
responseCreatePullRequest =
  res
    "CreatePullRequestResponse"
    "fixture/CreatePullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy CreatePullRequest)

responseDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplateResponse -> TestTree
responseDeleteApprovalRuleTemplate =
  res
    "DeleteApprovalRuleTemplateResponse"
    "fixture/DeleteApprovalRuleTemplateResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteApprovalRuleTemplate)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListBranches)

responseBatchGetCommits :: BatchGetCommitsResponse -> TestTree
responseBatchGetCommits =
  res
    "BatchGetCommitsResponse"
    "fixture/BatchGetCommitsResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchGetCommits)

responsePutCommentReaction :: PutCommentReactionResponse -> TestTree
responsePutCommentReaction =
  res
    "PutCommentReactionResponse"
    "fixture/PutCommentReactionResponse.proto"
    codeCommit
    (Proxy :: Proxy PutCommentReaction)

responseUpdatePullRequestDescription :: UpdatePullRequestDescriptionResponse -> TestTree
responseUpdatePullRequestDescription =
  res
    "UpdatePullRequestDescriptionResponse"
    "fixture/UpdatePullRequestDescriptionResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestDescription)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListRepositories)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateRepository)

responseUpdateDefaultBranch :: UpdateDefaultBranchResponse -> TestTree
responseUpdateDefaultBranch =
  res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateDefaultBranch)

responseGetMergeOptions :: GetMergeOptionsResponse -> TestTree
responseGetMergeOptions =
  res
    "GetMergeOptionsResponse"
    "fixture/GetMergeOptionsResponse.proto"
    codeCommit
    (Proxy :: Proxy GetMergeOptions)

responseCreatePullRequestApprovalRule :: CreatePullRequestApprovalRuleResponse -> TestTree
responseCreatePullRequestApprovalRule =
  res
    "CreatePullRequestApprovalRuleResponse"
    "fixture/CreatePullRequestApprovalRuleResponse.proto"
    codeCommit
    (Proxy :: Proxy CreatePullRequestApprovalRule)

responsePostCommentReply :: PostCommentReplyResponse -> TestTree
responsePostCommentReply =
  res
    "PostCommentReplyResponse"
    "fixture/PostCommentReplyResponse.proto"
    codeCommit
    (Proxy :: Proxy PostCommentReply)

responseUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContentResponse -> TestTree
responseUpdateApprovalRuleTemplateContent =
  res
    "UpdateApprovalRuleTemplateContentResponse"
    "fixture/UpdateApprovalRuleTemplateContentResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateApprovalRuleTemplateContent)

responseCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommitResponse -> TestTree
responseCreateUnreferencedMergeCommit =
  res
    "CreateUnreferencedMergeCommitResponse"
    "fixture/CreateUnreferencedMergeCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateUnreferencedMergeCommit)

responseListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplateResponse -> TestTree
responseListRepositoriesForApprovalRuleTemplate =
  res
    "ListRepositoriesForApprovalRuleTemplateResponse"
    "fixture/ListRepositoriesForApprovalRuleTemplateResponse.proto"
    codeCommit
    (Proxy :: Proxy ListRepositoriesForApprovalRuleTemplate)

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository =
  res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy GetRepository)

responseBatchDescribeMergeConflicts :: BatchDescribeMergeConflictsResponse -> TestTree
responseBatchDescribeMergeConflicts =
  res
    "BatchDescribeMergeConflictsResponse"
    "fixture/BatchDescribeMergeConflictsResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchDescribeMergeConflicts)

responseDeletePullRequestApprovalRule :: DeletePullRequestApprovalRuleResponse -> TestTree
responseDeletePullRequestApprovalRule =
  res
    "DeletePullRequestApprovalRuleResponse"
    "fixture/DeletePullRequestApprovalRuleResponse.proto"
    codeCommit
    (Proxy :: Proxy DeletePullRequestApprovalRule)

responseGetRepositoryTriggers :: GetRepositoryTriggersResponse -> TestTree
responseGetRepositoryTriggers =
  res
    "GetRepositoryTriggersResponse"
    "fixture/GetRepositoryTriggersResponse.proto"
    codeCommit
    (Proxy :: Proxy GetRepositoryTriggers)

responseUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateNameResponse -> TestTree
responseUpdateApprovalRuleTemplateName =
  res
    "UpdateApprovalRuleTemplateNameResponse"
    "fixture/UpdateApprovalRuleTemplateNameResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateApprovalRuleTemplateName)

responsePutFile :: PutFileResponse -> TestTree
responsePutFile =
  res
    "PutFileResponse"
    "fixture/PutFileResponse.proto"
    codeCommit
    (Proxy :: Proxy PutFile)

responseDeleteFile :: DeleteFileResponse -> TestTree
responseDeleteFile =
  res
    "DeleteFileResponse"
    "fixture/DeleteFileResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteFile)

responseGetCommentsForComparedCommit :: GetCommentsForComparedCommitResponse -> TestTree
responseGetCommentsForComparedCommit =
  res
    "GetCommentsForComparedCommitResponse"
    "fixture/GetCommentsForComparedCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy GetCommentsForComparedCommit)

responseGetMergeCommit :: GetMergeCommitResponse -> TestTree
responseGetMergeCommit =
  res
    "GetMergeCommitResponse"
    "fixture/GetMergeCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy GetMergeCommit)

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers =
  res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    codeCommit
    (Proxy :: Proxy TestRepositoryTriggers)

responseMergePullRequestBySquash :: MergePullRequestBySquashResponse -> TestTree
responseMergePullRequestBySquash =
  res
    "MergePullRequestBySquashResponse"
    "fixture/MergePullRequestBySquashResponse.proto"
    codeCommit
    (Proxy :: Proxy MergePullRequestBySquash)

responseUpdateComment :: UpdateCommentResponse -> TestTree
responseUpdateComment =
  res
    "UpdateCommentResponse"
    "fixture/UpdateCommentResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateComment)

responsePostCommentForComparedCommit :: PostCommentForComparedCommitResponse -> TestTree
responsePostCommentForComparedCommit =
  res
    "PostCommentForComparedCommitResponse"
    "fixture/PostCommentForComparedCommitResponse.proto"
    codeCommit
    (Proxy :: Proxy PostCommentForComparedCommit)

responseMergeBranchesByFastForward :: MergeBranchesByFastForwardResponse -> TestTree
responseMergeBranchesByFastForward =
  res
    "MergeBranchesByFastForwardResponse"
    "fixture/MergeBranchesByFastForwardResponse.proto"
    codeCommit
    (Proxy :: Proxy MergeBranchesByFastForward)

responseUpdatePullRequestTitle :: UpdatePullRequestTitleResponse -> TestTree
responseUpdatePullRequestTitle =
  res
    "UpdatePullRequestTitleResponse"
    "fixture/UpdatePullRequestTitleResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestTitle)

responseBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> TestTree
responseBatchDisassociateApprovalRuleTemplateFromRepositories =
  res
    "BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchDisassociateApprovalRuleTemplateFromRepositories)

responseUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContentResponse -> TestTree
responseUpdatePullRequestApprovalRuleContent =
  res
    "UpdatePullRequestApprovalRuleContentResponse"
    "fixture/UpdatePullRequestApprovalRuleContentResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestApprovalRuleContent)

responseGetBlob :: GetBlobResponse -> TestTree
responseGetBlob =
  res
    "GetBlobResponse"
    "fixture/GetBlobResponse.proto"
    codeCommit
    (Proxy :: Proxy GetBlob)

responseAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepositoryResponse -> TestTree
responseAssociateApprovalRuleTemplateWithRepository =
  res
    "AssociateApprovalRuleTemplateWithRepositoryResponse"
    "fixture/AssociateApprovalRuleTemplateWithRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy AssociateApprovalRuleTemplateWithRepository)

responsePutRepositoryTriggers :: PutRepositoryTriggersResponse -> TestTree
responsePutRepositoryTriggers =
  res
    "PutRepositoryTriggersResponse"
    "fixture/PutRepositoryTriggersResponse.proto"
    codeCommit
    (Proxy :: Proxy PutRepositoryTriggers)

responseListApprovalRuleTemplates :: ListApprovalRuleTemplatesResponse -> TestTree
responseListApprovalRuleTemplates =
  res
    "ListApprovalRuleTemplatesResponse"
    "fixture/ListApprovalRuleTemplatesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListApprovalRuleTemplates)

responseDescribeMergeConflicts :: DescribeMergeConflictsResponse -> TestTree
responseDescribeMergeConflicts =
  res
    "DescribeMergeConflictsResponse"
    "fixture/DescribeMergeConflictsResponse.proto"
    codeCommit
    (Proxy :: Proxy DescribeMergeConflicts)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    codeCommit
    (Proxy :: Proxy TagResource)

responseMergeBranchesByThreeWay :: MergeBranchesByThreeWayResponse -> TestTree
responseMergeBranchesByThreeWay =
  res
    "MergeBranchesByThreeWayResponse"
    "fixture/MergeBranchesByThreeWayResponse.proto"
    codeCommit
    (Proxy :: Proxy MergeBranchesByThreeWay)

responseGetFile :: GetFileResponse -> TestTree
responseGetFile =
  res
    "GetFileResponse"
    "fixture/GetFileResponse.proto"
    codeCommit
    (Proxy :: Proxy GetFile)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    codeCommit
    (Proxy :: Proxy UntagResource)

responseGetMergeConflicts :: GetMergeConflictsResponse -> TestTree
responseGetMergeConflicts =
  res
    "GetMergeConflictsResponse"
    "fixture/GetMergeConflictsResponse.proto"
    codeCommit
    (Proxy :: Proxy GetMergeConflicts)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteRepository)

responseDeleteCommentContent :: DeleteCommentContentResponse -> TestTree
responseDeleteCommentContent =
  res
    "DeleteCommentContentResponse"
    "fixture/DeleteCommentContentResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteCommentContent)

responseMergePullRequestByThreeWay :: MergePullRequestByThreeWayResponse -> TestTree
responseMergePullRequestByThreeWay =
  res
    "MergePullRequestByThreeWayResponse"
    "fixture/MergePullRequestByThreeWayResponse.proto"
    codeCommit
    (Proxy :: Proxy MergePullRequestByThreeWay)

responseDescribePullRequestEvents :: DescribePullRequestEventsResponse -> TestTree
responseDescribePullRequestEvents =
  res
    "DescribePullRequestEventsResponse"
    "fixture/DescribePullRequestEventsResponse.proto"
    codeCommit
    (Proxy :: Proxy DescribePullRequestEvents)

responseBatchGetRepositories :: BatchGetRepositoriesResponse -> TestTree
responseBatchGetRepositories =
  res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchGetRepositories)

responseUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescriptionResponse -> TestTree
responseUpdateApprovalRuleTemplateDescription =
  res
    "UpdateApprovalRuleTemplateDescriptionResponse"
    "fixture/UpdateApprovalRuleTemplateDescriptionResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateApprovalRuleTemplateDescription)

responseGetPullRequestOverrideState :: GetPullRequestOverrideStateResponse -> TestTree
responseGetPullRequestOverrideState =
  res
    "GetPullRequestOverrideStateResponse"
    "fixture/GetPullRequestOverrideStateResponse.proto"
    codeCommit
    (Proxy :: Proxy GetPullRequestOverrideState)

responseGetPullRequestApprovalStates :: GetPullRequestApprovalStatesResponse -> TestTree
responseGetPullRequestApprovalStates =
  res
    "GetPullRequestApprovalStatesResponse"
    "fixture/GetPullRequestApprovalStatesResponse.proto"
    codeCommit
    (Proxy :: Proxy GetPullRequestApprovalStates)

responseGetCommentsForPullRequest :: GetCommentsForPullRequestResponse -> TestTree
responseGetCommentsForPullRequest =
  res
    "GetCommentsForPullRequestResponse"
    "fixture/GetCommentsForPullRequestResponse.proto"
    codeCommit
    (Proxy :: Proxy GetCommentsForPullRequest)

responseUpdatePullRequestStatus :: UpdatePullRequestStatusResponse -> TestTree
responseUpdatePullRequestStatus =
  res
    "UpdatePullRequestStatusResponse"
    "fixture/UpdatePullRequestStatusResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdatePullRequestStatus)

responseListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> TestTree
responseListAssociatedApprovalRuleTemplatesForRepository =
  res
    "ListAssociatedApprovalRuleTemplatesForRepositoryResponse"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy ListAssociatedApprovalRuleTemplatesForRepository)

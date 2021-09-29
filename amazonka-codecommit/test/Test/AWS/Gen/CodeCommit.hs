{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeCommit
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestPutFile $
--             newPutFile
--
--         , requestGetRepositoryTriggers $
--             newGetRepositoryTriggers
--
--         , requestBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositories
--
--         , requestDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepository
--
--         , requestUpdateRepositoryName $
--             newUpdateRepositoryName
--
--         , requestBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflicts
--
--         , requestListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplate
--
--         , requestDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRule
--
--         , requestGetRepository $
--             newGetRepository
--
--         , requestGetCommentsForPullRequest $
--             newGetCommentsForPullRequest
--
--         , requestGetPullRequestOverrideState $
--             newGetPullRequestOverrideState
--
--         , requestPostCommentReply $
--             newPostCommentReply
--
--         , requestUpdatePullRequestStatus $
--             newUpdatePullRequestStatus
--
--         , requestMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWay
--
--         , requestUpdateDefaultBranch $
--             newUpdateDefaultBranch
--
--         , requestBatchGetRepositories $
--             newBatchGetRepositories
--
--         , requestGetMergeOptions $
--             newGetMergeOptions
--
--         , requestPutCommentReaction $
--             newPutCommentReaction
--
--         , requestGetMergeConflicts $
--             newGetMergeConflicts
--
--         , requestUpdatePullRequestDescription $
--             newUpdatePullRequestDescription
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplate
--
--         , requestGetFile $
--             newGetFile
--
--         , requestListApprovalRuleTemplates $
--             newListApprovalRuleTemplates
--
--         , requestTagResource $
--             newTagResource
--
--         , requestMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWay
--
--         , requestListBranches $
--             newListBranches
--
--         , requestPutRepositoryTriggers $
--             newPutRepositoryTriggers
--
--         , requestCreateBranch $
--             newCreateBranch
--
--         , requestUpdatePullRequestApprovalRuleContent $
--             newUpdatePullRequestApprovalRuleContent
--
--         , requestUpdatePullRequestTitle $
--             newUpdatePullRequestTitle
--
--         , requestUpdateRepositoryDescription $
--             newUpdateRepositoryDescription
--
--         , requestGetPullRequest $
--             newGetPullRequest
--
--         , requestGetDifferences $
--             newGetDifferences
--
--         , requestOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRules
--
--         , requestUpdateComment $
--             newUpdateComment
--
--         , requestDeleteFile $
--             newDeleteFile
--
--         , requestUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateName
--
--         , requestGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommit
--
--         , requestTestRepositoryTriggers $
--             newTestRepositoryTriggers
--
--         , requestGetMergeCommit $
--             newGetMergeCommit
--
--         , requestGetCommit $
--             newGetCommit
--
--         , requestGetCommentReactions $
--             newGetCommentReactions
--
--         , requestGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplate
--
--         , requestMergePullRequestByFastForward $
--             newMergePullRequestByFastForward
--
--         , requestPostCommentForPullRequest $
--             newPostCommentForPullRequest
--
--         , requestMergeBranchesBySquash $
--             newMergeBranchesBySquash
--
--         , requestCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommit
--
--         , requestCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRule
--
--         , requestGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStates
--
--         , requestListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepository
--
--         , requestUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContent
--
--         , requestDescribePullRequestEvents $
--             newDescribePullRequestEvents
--
--         , requestListRepositories $
--             newListRepositories
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescription
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDeleteCommentContent $
--             newDeleteCommentContent
--
--         , requestBatchGetCommits $
--             newBatchGetCommits
--
--         , requestDescribeMergeConflicts $
--             newDescribeMergeConflicts
--
--         , requestCreatePullRequest $
--             newCreatePullRequest
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalState
--
--         , requestCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplate
--
--         , requestDeleteBranch $
--             newDeleteBranch
--
--         , requestCreateCommit $
--             newCreateCommit
--
--         , requestGetComment $
--             newGetComment
--
--         , requestEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRules
--
--         , requestAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepository
--
--         , requestListPullRequests $
--             newListPullRequests
--
--         , requestBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositories
--
--         , requestGetBlob $
--             newGetBlob
--
--         , requestMergePullRequestBySquash $
--             newMergePullRequestBySquash
--
--         , requestPostCommentForComparedCommit $
--             newPostCommentForComparedCommit
--
--         , requestMergeBranchesByFastForward $
--             newMergeBranchesByFastForward
--
--         , requestGetBranch $
--             newGetBranch
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responsePutFile $
--             newPutFileResponse
--
--         , responseGetRepositoryTriggers $
--             newGetRepositoryTriggersResponse
--
--         , responseBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
--
--         , responseDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepositoryResponse
--
--         , responseUpdateRepositoryName $
--             newUpdateRepositoryNameResponse
--
--         , responseBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflictsResponse
--
--         , responseListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplateResponse
--
--         , responseDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRuleResponse
--
--         , responseGetRepository $
--             newGetRepositoryResponse
--
--         , responseGetCommentsForPullRequest $
--             newGetCommentsForPullRequestResponse
--
--         , responseGetPullRequestOverrideState $
--             newGetPullRequestOverrideStateResponse
--
--         , responsePostCommentReply $
--             newPostCommentReplyResponse
--
--         , responseUpdatePullRequestStatus $
--             newUpdatePullRequestStatusResponse
--
--         , responseMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWayResponse
--
--         , responseUpdateDefaultBranch $
--             newUpdateDefaultBranchResponse
--
--         , responseBatchGetRepositories $
--             newBatchGetRepositoriesResponse
--
--         , responseGetMergeOptions $
--             newGetMergeOptionsResponse
--
--         , responsePutCommentReaction $
--             newPutCommentReactionResponse
--
--         , responseGetMergeConflicts $
--             newGetMergeConflictsResponse
--
--         , responseUpdatePullRequestDescription $
--             newUpdatePullRequestDescriptionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplateResponse
--
--         , responseGetFile $
--             newGetFileResponse
--
--         , responseListApprovalRuleTemplates $
--             newListApprovalRuleTemplatesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWayResponse
--
--         , responseListBranches $
--             newListBranchesResponse
--
--         , responsePutRepositoryTriggers $
--             newPutRepositoryTriggersResponse
--
--         , responseCreateBranch $
--             newCreateBranchResponse
--
--         , responseUpdatePullRequestApprovalRuleContent $
--             newUpdatePullRequestApprovalRuleContentResponse
--
--         , responseUpdatePullRequestTitle $
--             newUpdatePullRequestTitleResponse
--
--         , responseUpdateRepositoryDescription $
--             newUpdateRepositoryDescriptionResponse
--
--         , responseGetPullRequest $
--             newGetPullRequestResponse
--
--         , responseGetDifferences $
--             newGetDifferencesResponse
--
--         , responseOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRulesResponse
--
--         , responseUpdateComment $
--             newUpdateCommentResponse
--
--         , responseDeleteFile $
--             newDeleteFileResponse
--
--         , responseUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateNameResponse
--
--         , responseGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommitResponse
--
--         , responseTestRepositoryTriggers $
--             newTestRepositoryTriggersResponse
--
--         , responseGetMergeCommit $
--             newGetMergeCommitResponse
--
--         , responseGetCommit $
--             newGetCommitResponse
--
--         , responseGetCommentReactions $
--             newGetCommentReactionsResponse
--
--         , responseGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplateResponse
--
--         , responseMergePullRequestByFastForward $
--             newMergePullRequestByFastForwardResponse
--
--         , responsePostCommentForPullRequest $
--             newPostCommentForPullRequestResponse
--
--         , responseMergeBranchesBySquash $
--             newMergeBranchesBySquashResponse
--
--         , responseCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommitResponse
--
--         , responseCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRuleResponse
--
--         , responseGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStatesResponse
--
--         , responseListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepositoryResponse
--
--         , responseUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContentResponse
--
--         , responseDescribePullRequestEvents $
--             newDescribePullRequestEventsResponse
--
--         , responseListRepositories $
--             newListRepositoriesResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescriptionResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDeleteCommentContent $
--             newDeleteCommentContentResponse
--
--         , responseBatchGetCommits $
--             newBatchGetCommitsResponse
--
--         , responseDescribeMergeConflicts $
--             newDescribeMergeConflictsResponse
--
--         , responseCreatePullRequest $
--             newCreatePullRequestResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalStateResponse
--
--         , responseCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplateResponse
--
--         , responseDeleteBranch $
--             newDeleteBranchResponse
--
--         , responseCreateCommit $
--             newCreateCommitResponse
--
--         , responseGetComment $
--             newGetCommentResponse
--
--         , responseEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRulesResponse
--
--         , responseAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepositoryResponse
--
--         , responseListPullRequests $
--             newListPullRequestsResponse
--
--         , responseBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
--
--         , responseGetBlob $
--             newGetBlobResponse
--
--         , responseMergePullRequestBySquash $
--             newMergePullRequestBySquashResponse
--
--         , responsePostCommentForComparedCommit $
--             newPostCommentForComparedCommitResponse
--
--         , responseMergeBranchesByFastForward $
--             newMergeBranchesByFastForwardResponse
--
--         , responseGetBranch $
--             newGetBranchResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestPutFile :: PutFile -> TestTree
requestPutFile =
  req
    "PutFile"
    "fixture/PutFile.yaml"

requestGetRepositoryTriggers :: GetRepositoryTriggers -> TestTree
requestGetRepositoryTriggers =
  req
    "GetRepositoryTriggers"
    "fixture/GetRepositoryTriggers.yaml"

requestBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositories -> TestTree
requestBatchAssociateApprovalRuleTemplateWithRepositories =
  req
    "BatchAssociateApprovalRuleTemplateWithRepositories"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositories.yaml"

requestDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepository -> TestTree
requestDisassociateApprovalRuleTemplateFromRepository =
  req
    "DisassociateApprovalRuleTemplateFromRepository"
    "fixture/DisassociateApprovalRuleTemplateFromRepository.yaml"

requestUpdateRepositoryName :: UpdateRepositoryName -> TestTree
requestUpdateRepositoryName =
  req
    "UpdateRepositoryName"
    "fixture/UpdateRepositoryName.yaml"

requestBatchDescribeMergeConflicts :: BatchDescribeMergeConflicts -> TestTree
requestBatchDescribeMergeConflicts =
  req
    "BatchDescribeMergeConflicts"
    "fixture/BatchDescribeMergeConflicts.yaml"

requestListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplate -> TestTree
requestListRepositoriesForApprovalRuleTemplate =
  req
    "ListRepositoriesForApprovalRuleTemplate"
    "fixture/ListRepositoriesForApprovalRuleTemplate.yaml"

requestDeletePullRequestApprovalRule :: DeletePullRequestApprovalRule -> TestTree
requestDeletePullRequestApprovalRule =
  req
    "DeletePullRequestApprovalRule"
    "fixture/DeletePullRequestApprovalRule.yaml"

requestGetRepository :: GetRepository -> TestTree
requestGetRepository =
  req
    "GetRepository"
    "fixture/GetRepository.yaml"

requestGetCommentsForPullRequest :: GetCommentsForPullRequest -> TestTree
requestGetCommentsForPullRequest =
  req
    "GetCommentsForPullRequest"
    "fixture/GetCommentsForPullRequest.yaml"

requestGetPullRequestOverrideState :: GetPullRequestOverrideState -> TestTree
requestGetPullRequestOverrideState =
  req
    "GetPullRequestOverrideState"
    "fixture/GetPullRequestOverrideState.yaml"

requestPostCommentReply :: PostCommentReply -> TestTree
requestPostCommentReply =
  req
    "PostCommentReply"
    "fixture/PostCommentReply.yaml"

requestUpdatePullRequestStatus :: UpdatePullRequestStatus -> TestTree
requestUpdatePullRequestStatus =
  req
    "UpdatePullRequestStatus"
    "fixture/UpdatePullRequestStatus.yaml"

requestMergePullRequestByThreeWay :: MergePullRequestByThreeWay -> TestTree
requestMergePullRequestByThreeWay =
  req
    "MergePullRequestByThreeWay"
    "fixture/MergePullRequestByThreeWay.yaml"

requestUpdateDefaultBranch :: UpdateDefaultBranch -> TestTree
requestUpdateDefaultBranch =
  req
    "UpdateDefaultBranch"
    "fixture/UpdateDefaultBranch.yaml"

requestBatchGetRepositories :: BatchGetRepositories -> TestTree
requestBatchGetRepositories =
  req
    "BatchGetRepositories"
    "fixture/BatchGetRepositories.yaml"

requestGetMergeOptions :: GetMergeOptions -> TestTree
requestGetMergeOptions =
  req
    "GetMergeOptions"
    "fixture/GetMergeOptions.yaml"

requestPutCommentReaction :: PutCommentReaction -> TestTree
requestPutCommentReaction =
  req
    "PutCommentReaction"
    "fixture/PutCommentReaction.yaml"

requestGetMergeConflicts :: GetMergeConflicts -> TestTree
requestGetMergeConflicts =
  req
    "GetMergeConflicts"
    "fixture/GetMergeConflicts.yaml"

requestUpdatePullRequestDescription :: UpdatePullRequestDescription -> TestTree
requestUpdatePullRequestDescription =
  req
    "UpdatePullRequestDescription"
    "fixture/UpdatePullRequestDescription.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplate -> TestTree
requestDeleteApprovalRuleTemplate =
  req
    "DeleteApprovalRuleTemplate"
    "fixture/DeleteApprovalRuleTemplate.yaml"

requestGetFile :: GetFile -> TestTree
requestGetFile =
  req
    "GetFile"
    "fixture/GetFile.yaml"

requestListApprovalRuleTemplates :: ListApprovalRuleTemplates -> TestTree
requestListApprovalRuleTemplates =
  req
    "ListApprovalRuleTemplates"
    "fixture/ListApprovalRuleTemplates.yaml"

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

requestListBranches :: ListBranches -> TestTree
requestListBranches =
  req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestPutRepositoryTriggers :: PutRepositoryTriggers -> TestTree
requestPutRepositoryTriggers =
  req
    "PutRepositoryTriggers"
    "fixture/PutRepositoryTriggers.yaml"

requestCreateBranch :: CreateBranch -> TestTree
requestCreateBranch =
  req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

requestUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContent -> TestTree
requestUpdatePullRequestApprovalRuleContent =
  req
    "UpdatePullRequestApprovalRuleContent"
    "fixture/UpdatePullRequestApprovalRuleContent.yaml"

requestUpdatePullRequestTitle :: UpdatePullRequestTitle -> TestTree
requestUpdatePullRequestTitle =
  req
    "UpdatePullRequestTitle"
    "fixture/UpdatePullRequestTitle.yaml"

requestUpdateRepositoryDescription :: UpdateRepositoryDescription -> TestTree
requestUpdateRepositoryDescription =
  req
    "UpdateRepositoryDescription"
    "fixture/UpdateRepositoryDescription.yaml"

requestGetPullRequest :: GetPullRequest -> TestTree
requestGetPullRequest =
  req
    "GetPullRequest"
    "fixture/GetPullRequest.yaml"

requestGetDifferences :: GetDifferences -> TestTree
requestGetDifferences =
  req
    "GetDifferences"
    "fixture/GetDifferences.yaml"

requestOverridePullRequestApprovalRules :: OverridePullRequestApprovalRules -> TestTree
requestOverridePullRequestApprovalRules =
  req
    "OverridePullRequestApprovalRules"
    "fixture/OverridePullRequestApprovalRules.yaml"

requestUpdateComment :: UpdateComment -> TestTree
requestUpdateComment =
  req
    "UpdateComment"
    "fixture/UpdateComment.yaml"

requestDeleteFile :: DeleteFile -> TestTree
requestDeleteFile =
  req
    "DeleteFile"
    "fixture/DeleteFile.yaml"

requestUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateName -> TestTree
requestUpdateApprovalRuleTemplateName =
  req
    "UpdateApprovalRuleTemplateName"
    "fixture/UpdateApprovalRuleTemplateName.yaml"

requestGetCommentsForComparedCommit :: GetCommentsForComparedCommit -> TestTree
requestGetCommentsForComparedCommit =
  req
    "GetCommentsForComparedCommit"
    "fixture/GetCommentsForComparedCommit.yaml"

requestTestRepositoryTriggers :: TestRepositoryTriggers -> TestTree
requestTestRepositoryTriggers =
  req
    "TestRepositoryTriggers"
    "fixture/TestRepositoryTriggers.yaml"

requestGetMergeCommit :: GetMergeCommit -> TestTree
requestGetMergeCommit =
  req
    "GetMergeCommit"
    "fixture/GetMergeCommit.yaml"

requestGetCommit :: GetCommit -> TestTree
requestGetCommit =
  req
    "GetCommit"
    "fixture/GetCommit.yaml"

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

requestMergePullRequestByFastForward :: MergePullRequestByFastForward -> TestTree
requestMergePullRequestByFastForward =
  req
    "MergePullRequestByFastForward"
    "fixture/MergePullRequestByFastForward.yaml"

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

requestCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommit -> TestTree
requestCreateUnreferencedMergeCommit =
  req
    "CreateUnreferencedMergeCommit"
    "fixture/CreateUnreferencedMergeCommit.yaml"

requestCreatePullRequestApprovalRule :: CreatePullRequestApprovalRule -> TestTree
requestCreatePullRequestApprovalRule =
  req
    "CreatePullRequestApprovalRule"
    "fixture/CreatePullRequestApprovalRule.yaml"

requestGetPullRequestApprovalStates :: GetPullRequestApprovalStates -> TestTree
requestGetPullRequestApprovalStates =
  req
    "GetPullRequestApprovalStates"
    "fixture/GetPullRequestApprovalStates.yaml"

requestListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepository -> TestTree
requestListAssociatedApprovalRuleTemplatesForRepository =
  req
    "ListAssociatedApprovalRuleTemplatesForRepository"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepository.yaml"

requestUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContent -> TestTree
requestUpdateApprovalRuleTemplateContent =
  req
    "UpdateApprovalRuleTemplateContent"
    "fixture/UpdateApprovalRuleTemplateContent.yaml"

requestDescribePullRequestEvents :: DescribePullRequestEvents -> TestTree
requestDescribePullRequestEvents =
  req
    "DescribePullRequestEvents"
    "fixture/DescribePullRequestEvents.yaml"

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

requestUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescription -> TestTree
requestUpdateApprovalRuleTemplateDescription =
  req
    "UpdateApprovalRuleTemplateDescription"
    "fixture/UpdateApprovalRuleTemplateDescription.yaml"

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

requestBatchGetCommits :: BatchGetCommits -> TestTree
requestBatchGetCommits =
  req
    "BatchGetCommits"
    "fixture/BatchGetCommits.yaml"

requestDescribeMergeConflicts :: DescribeMergeConflicts -> TestTree
requestDescribeMergeConflicts =
  req
    "DescribeMergeConflicts"
    "fixture/DescribeMergeConflicts.yaml"

requestCreatePullRequest :: CreatePullRequest -> TestTree
requestCreatePullRequest =
  req
    "CreatePullRequest"
    "fixture/CreatePullRequest.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder =
  req
    "GetFolder"
    "fixture/GetFolder.yaml"

requestUpdatePullRequestApprovalState :: UpdatePullRequestApprovalState -> TestTree
requestUpdatePullRequestApprovalState =
  req
    "UpdatePullRequestApprovalState"
    "fixture/UpdatePullRequestApprovalState.yaml"

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

requestCreateCommit :: CreateCommit -> TestTree
requestCreateCommit =
  req
    "CreateCommit"
    "fixture/CreateCommit.yaml"

requestGetComment :: GetComment -> TestTree
requestGetComment =
  req
    "GetComment"
    "fixture/GetComment.yaml"

requestEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRules -> TestTree
requestEvaluatePullRequestApprovalRules =
  req
    "EvaluatePullRequestApprovalRules"
    "fixture/EvaluatePullRequestApprovalRules.yaml"

requestAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepository -> TestTree
requestAssociateApprovalRuleTemplateWithRepository =
  req
    "AssociateApprovalRuleTemplateWithRepository"
    "fixture/AssociateApprovalRuleTemplateWithRepository.yaml"

requestListPullRequests :: ListPullRequests -> TestTree
requestListPullRequests =
  req
    "ListPullRequests"
    "fixture/ListPullRequests.yaml"

requestBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositories -> TestTree
requestBatchDisassociateApprovalRuleTemplateFromRepositories =
  req
    "BatchDisassociateApprovalRuleTemplateFromRepositories"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositories.yaml"

requestGetBlob :: GetBlob -> TestTree
requestGetBlob =
  req
    "GetBlob"
    "fixture/GetBlob.yaml"

requestMergePullRequestBySquash :: MergePullRequestBySquash -> TestTree
requestMergePullRequestBySquash =
  req
    "MergePullRequestBySquash"
    "fixture/MergePullRequestBySquash.yaml"

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

requestGetBranch :: GetBranch -> TestTree
requestGetBranch =
  req
    "GetBranch"
    "fixture/GetBranch.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responsePutFile :: PutFileResponse -> TestTree
responsePutFile =
  res
    "PutFileResponse"
    "fixture/PutFileResponse.proto"
    defaultService
    (Proxy :: Proxy PutFile)

responseGetRepositoryTriggers :: GetRepositoryTriggersResponse -> TestTree
responseGetRepositoryTriggers =
  res
    "GetRepositoryTriggersResponse"
    "fixture/GetRepositoryTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy GetRepositoryTriggers)

responseBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> TestTree
responseBatchAssociateApprovalRuleTemplateWithRepositories =
  res
    "BatchAssociateApprovalRuleTemplateWithRepositoriesResponse"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateApprovalRuleTemplateWithRepositories)

responseDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepositoryResponse -> TestTree
responseDisassociateApprovalRuleTemplateFromRepository =
  res
    "DisassociateApprovalRuleTemplateFromRepositoryResponse"
    "fixture/DisassociateApprovalRuleTemplateFromRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateApprovalRuleTemplateFromRepository)

responseUpdateRepositoryName :: UpdateRepositoryNameResponse -> TestTree
responseUpdateRepositoryName =
  res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRepositoryName)

responseBatchDescribeMergeConflicts :: BatchDescribeMergeConflictsResponse -> TestTree
responseBatchDescribeMergeConflicts =
  res
    "BatchDescribeMergeConflictsResponse"
    "fixture/BatchDescribeMergeConflictsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDescribeMergeConflicts)

responseListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplateResponse -> TestTree
responseListRepositoriesForApprovalRuleTemplate =
  res
    "ListRepositoriesForApprovalRuleTemplateResponse"
    "fixture/ListRepositoriesForApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy ListRepositoriesForApprovalRuleTemplate)

responseDeletePullRequestApprovalRule :: DeletePullRequestApprovalRuleResponse -> TestTree
responseDeletePullRequestApprovalRule =
  res
    "DeletePullRequestApprovalRuleResponse"
    "fixture/DeletePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePullRequestApprovalRule)

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository =
  res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetRepository)

responseGetCommentsForPullRequest :: GetCommentsForPullRequestResponse -> TestTree
responseGetCommentsForPullRequest =
  res
    "GetCommentsForPullRequestResponse"
    "fixture/GetCommentsForPullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommentsForPullRequest)

responseGetPullRequestOverrideState :: GetPullRequestOverrideStateResponse -> TestTree
responseGetPullRequestOverrideState =
  res
    "GetPullRequestOverrideStateResponse"
    "fixture/GetPullRequestOverrideStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetPullRequestOverrideState)

responsePostCommentReply :: PostCommentReplyResponse -> TestTree
responsePostCommentReply =
  res
    "PostCommentReplyResponse"
    "fixture/PostCommentReplyResponse.proto"
    defaultService
    (Proxy :: Proxy PostCommentReply)

responseUpdatePullRequestStatus :: UpdatePullRequestStatusResponse -> TestTree
responseUpdatePullRequestStatus =
  res
    "UpdatePullRequestStatusResponse"
    "fixture/UpdatePullRequestStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestStatus)

responseMergePullRequestByThreeWay :: MergePullRequestByThreeWayResponse -> TestTree
responseMergePullRequestByThreeWay =
  res
    "MergePullRequestByThreeWayResponse"
    "fixture/MergePullRequestByThreeWayResponse.proto"
    defaultService
    (Proxy :: Proxy MergePullRequestByThreeWay)

responseUpdateDefaultBranch :: UpdateDefaultBranchResponse -> TestTree
responseUpdateDefaultBranch =
  res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDefaultBranch)

responseBatchGetRepositories :: BatchGetRepositoriesResponse -> TestTree
responseBatchGetRepositories =
  res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetRepositories)

responseGetMergeOptions :: GetMergeOptionsResponse -> TestTree
responseGetMergeOptions =
  res
    "GetMergeOptionsResponse"
    "fixture/GetMergeOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMergeOptions)

responsePutCommentReaction :: PutCommentReactionResponse -> TestTree
responsePutCommentReaction =
  res
    "PutCommentReactionResponse"
    "fixture/PutCommentReactionResponse.proto"
    defaultService
    (Proxy :: Proxy PutCommentReaction)

responseGetMergeConflicts :: GetMergeConflictsResponse -> TestTree
responseGetMergeConflicts =
  res
    "GetMergeConflictsResponse"
    "fixture/GetMergeConflictsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMergeConflicts)

responseUpdatePullRequestDescription :: UpdatePullRequestDescriptionResponse -> TestTree
responseUpdatePullRequestDescription =
  res
    "UpdatePullRequestDescriptionResponse"
    "fixture/UpdatePullRequestDescriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestDescription)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplateResponse -> TestTree
responseDeleteApprovalRuleTemplate =
  res
    "DeleteApprovalRuleTemplateResponse"
    "fixture/DeleteApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApprovalRuleTemplate)

responseGetFile :: GetFileResponse -> TestTree
responseGetFile =
  res
    "GetFileResponse"
    "fixture/GetFileResponse.proto"
    defaultService
    (Proxy :: Proxy GetFile)

responseListApprovalRuleTemplates :: ListApprovalRuleTemplatesResponse -> TestTree
responseListApprovalRuleTemplates =
  res
    "ListApprovalRuleTemplatesResponse"
    "fixture/ListApprovalRuleTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListApprovalRuleTemplates)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseMergeBranchesByThreeWay :: MergeBranchesByThreeWayResponse -> TestTree
responseMergeBranchesByThreeWay =
  res
    "MergeBranchesByThreeWayResponse"
    "fixture/MergeBranchesByThreeWayResponse.proto"
    defaultService
    (Proxy :: Proxy MergeBranchesByThreeWay)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBranches)

responsePutRepositoryTriggers :: PutRepositoryTriggersResponse -> TestTree
responsePutRepositoryTriggers =
  res
    "PutRepositoryTriggersResponse"
    "fixture/PutRepositoryTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy PutRepositoryTriggers)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch =
  res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBranch)

responseUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContentResponse -> TestTree
responseUpdatePullRequestApprovalRuleContent =
  res
    "UpdatePullRequestApprovalRuleContentResponse"
    "fixture/UpdatePullRequestApprovalRuleContentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestApprovalRuleContent)

responseUpdatePullRequestTitle :: UpdatePullRequestTitleResponse -> TestTree
responseUpdatePullRequestTitle =
  res
    "UpdatePullRequestTitleResponse"
    "fixture/UpdatePullRequestTitleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestTitle)

responseUpdateRepositoryDescription :: UpdateRepositoryDescriptionResponse -> TestTree
responseUpdateRepositoryDescription =
  res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRepositoryDescription)

responseGetPullRequest :: GetPullRequestResponse -> TestTree
responseGetPullRequest =
  res
    "GetPullRequestResponse"
    "fixture/GetPullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy GetPullRequest)

responseGetDifferences :: GetDifferencesResponse -> TestTree
responseGetDifferences =
  res
    "GetDifferencesResponse"
    "fixture/GetDifferencesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDifferences)

responseOverridePullRequestApprovalRules :: OverridePullRequestApprovalRulesResponse -> TestTree
responseOverridePullRequestApprovalRules =
  res
    "OverridePullRequestApprovalRulesResponse"
    "fixture/OverridePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy :: Proxy OverridePullRequestApprovalRules)

responseUpdateComment :: UpdateCommentResponse -> TestTree
responseUpdateComment =
  res
    "UpdateCommentResponse"
    "fixture/UpdateCommentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateComment)

responseDeleteFile :: DeleteFileResponse -> TestTree
responseDeleteFile =
  res
    "DeleteFileResponse"
    "fixture/DeleteFileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFile)

responseUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateNameResponse -> TestTree
responseUpdateApprovalRuleTemplateName =
  res
    "UpdateApprovalRuleTemplateNameResponse"
    "fixture/UpdateApprovalRuleTemplateNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApprovalRuleTemplateName)

responseGetCommentsForComparedCommit :: GetCommentsForComparedCommitResponse -> TestTree
responseGetCommentsForComparedCommit =
  res
    "GetCommentsForComparedCommitResponse"
    "fixture/GetCommentsForComparedCommitResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommentsForComparedCommit)

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers =
  res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy TestRepositoryTriggers)

responseGetMergeCommit :: GetMergeCommitResponse -> TestTree
responseGetMergeCommit =
  res
    "GetMergeCommitResponse"
    "fixture/GetMergeCommitResponse.proto"
    defaultService
    (Proxy :: Proxy GetMergeCommit)

responseGetCommit :: GetCommitResponse -> TestTree
responseGetCommit =
  res
    "GetCommitResponse"
    "fixture/GetCommitResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommit)

responseGetCommentReactions :: GetCommentReactionsResponse -> TestTree
responseGetCommentReactions =
  res
    "GetCommentReactionsResponse"
    "fixture/GetCommentReactionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommentReactions)

responseGetApprovalRuleTemplate :: GetApprovalRuleTemplateResponse -> TestTree
responseGetApprovalRuleTemplate =
  res
    "GetApprovalRuleTemplateResponse"
    "fixture/GetApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetApprovalRuleTemplate)

responseMergePullRequestByFastForward :: MergePullRequestByFastForwardResponse -> TestTree
responseMergePullRequestByFastForward =
  res
    "MergePullRequestByFastForwardResponse"
    "fixture/MergePullRequestByFastForwardResponse.proto"
    defaultService
    (Proxy :: Proxy MergePullRequestByFastForward)

responsePostCommentForPullRequest :: PostCommentForPullRequestResponse -> TestTree
responsePostCommentForPullRequest =
  res
    "PostCommentForPullRequestResponse"
    "fixture/PostCommentForPullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy PostCommentForPullRequest)

responseMergeBranchesBySquash :: MergeBranchesBySquashResponse -> TestTree
responseMergeBranchesBySquash =
  res
    "MergeBranchesBySquashResponse"
    "fixture/MergeBranchesBySquashResponse.proto"
    defaultService
    (Proxy :: Proxy MergeBranchesBySquash)

responseCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommitResponse -> TestTree
responseCreateUnreferencedMergeCommit =
  res
    "CreateUnreferencedMergeCommitResponse"
    "fixture/CreateUnreferencedMergeCommitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUnreferencedMergeCommit)

responseCreatePullRequestApprovalRule :: CreatePullRequestApprovalRuleResponse -> TestTree
responseCreatePullRequestApprovalRule =
  res
    "CreatePullRequestApprovalRuleResponse"
    "fixture/CreatePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePullRequestApprovalRule)

responseGetPullRequestApprovalStates :: GetPullRequestApprovalStatesResponse -> TestTree
responseGetPullRequestApprovalStates =
  res
    "GetPullRequestApprovalStatesResponse"
    "fixture/GetPullRequestApprovalStatesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPullRequestApprovalStates)

responseListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> TestTree
responseListAssociatedApprovalRuleTemplatesForRepository =
  res
    "ListAssociatedApprovalRuleTemplatesForRepositoryResponse"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedApprovalRuleTemplatesForRepository)

responseUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContentResponse -> TestTree
responseUpdateApprovalRuleTemplateContent =
  res
    "UpdateApprovalRuleTemplateContentResponse"
    "fixture/UpdateApprovalRuleTemplateContentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApprovalRuleTemplateContent)

responseDescribePullRequestEvents :: DescribePullRequestEventsResponse -> TestTree
responseDescribePullRequestEvents =
  res
    "DescribePullRequestEventsResponse"
    "fixture/DescribePullRequestEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePullRequestEvents)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRepositories)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRepository)

responseUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescriptionResponse -> TestTree
responseUpdateApprovalRuleTemplateDescription =
  res
    "UpdateApprovalRuleTemplateDescriptionResponse"
    "fixture/UpdateApprovalRuleTemplateDescriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApprovalRuleTemplateDescription)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRepository)

responseDeleteCommentContent :: DeleteCommentContentResponse -> TestTree
responseDeleteCommentContent =
  res
    "DeleteCommentContentResponse"
    "fixture/DeleteCommentContentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCommentContent)

responseBatchGetCommits :: BatchGetCommitsResponse -> TestTree
responseBatchGetCommits =
  res
    "BatchGetCommitsResponse"
    "fixture/BatchGetCommitsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetCommits)

responseDescribeMergeConflicts :: DescribeMergeConflictsResponse -> TestTree
responseDescribeMergeConflicts =
  res
    "DescribeMergeConflictsResponse"
    "fixture/DescribeMergeConflictsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMergeConflicts)

responseCreatePullRequest :: CreatePullRequestResponse -> TestTree
responseCreatePullRequest =
  res
    "CreatePullRequestResponse"
    "fixture/CreatePullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePullRequest)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy :: Proxy GetFolder)

responseUpdatePullRequestApprovalState :: UpdatePullRequestApprovalStateResponse -> TestTree
responseUpdatePullRequestApprovalState =
  res
    "UpdatePullRequestApprovalStateResponse"
    "fixture/UpdatePullRequestApprovalStateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestApprovalState)

responseCreateApprovalRuleTemplate :: CreateApprovalRuleTemplateResponse -> TestTree
responseCreateApprovalRuleTemplate =
  res
    "CreateApprovalRuleTemplateResponse"
    "fixture/CreateApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApprovalRuleTemplate)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBranch)

responseCreateCommit :: CreateCommitResponse -> TestTree
responseCreateCommit =
  res
    "CreateCommitResponse"
    "fixture/CreateCommitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCommit)

responseGetComment :: GetCommentResponse -> TestTree
responseGetComment =
  res
    "GetCommentResponse"
    "fixture/GetCommentResponse.proto"
    defaultService
    (Proxy :: Proxy GetComment)

responseEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRulesResponse -> TestTree
responseEvaluatePullRequestApprovalRules =
  res
    "EvaluatePullRequestApprovalRulesResponse"
    "fixture/EvaluatePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy :: Proxy EvaluatePullRequestApprovalRules)

responseAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepositoryResponse -> TestTree
responseAssociateApprovalRuleTemplateWithRepository =
  res
    "AssociateApprovalRuleTemplateWithRepositoryResponse"
    "fixture/AssociateApprovalRuleTemplateWithRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateApprovalRuleTemplateWithRepository)

responseListPullRequests :: ListPullRequestsResponse -> TestTree
responseListPullRequests =
  res
    "ListPullRequestsResponse"
    "fixture/ListPullRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPullRequests)

responseBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> TestTree
responseBatchDisassociateApprovalRuleTemplateFromRepositories =
  res
    "BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDisassociateApprovalRuleTemplateFromRepositories)

responseGetBlob :: GetBlobResponse -> TestTree
responseGetBlob =
  res
    "GetBlobResponse"
    "fixture/GetBlobResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlob)

responseMergePullRequestBySquash :: MergePullRequestBySquashResponse -> TestTree
responseMergePullRequestBySquash =
  res
    "MergePullRequestBySquashResponse"
    "fixture/MergePullRequestBySquashResponse.proto"
    defaultService
    (Proxy :: Proxy MergePullRequestBySquash)

responsePostCommentForComparedCommit :: PostCommentForComparedCommitResponse -> TestTree
responsePostCommentForComparedCommit =
  res
    "PostCommentForComparedCommitResponse"
    "fixture/PostCommentForComparedCommitResponse.proto"
    defaultService
    (Proxy :: Proxy PostCommentForComparedCommit)

responseMergeBranchesByFastForward :: MergeBranchesByFastForwardResponse -> TestTree
responseMergeBranchesByFastForward =
  res
    "MergeBranchesByFastForwardResponse"
    "fixture/MergeBranchesByFastForwardResponse.proto"
    defaultService
    (Proxy :: Proxy MergeBranchesByFastForward)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch =
  res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    defaultService
    (Proxy :: Proxy GetBranch)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

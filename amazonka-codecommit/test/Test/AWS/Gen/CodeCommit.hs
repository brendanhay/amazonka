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
--         [ requestBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositories
--
--         , requestPutFile $
--             newPutFile
--
--         , requestGetRepositoryTriggers $
--             newGetRepositoryTriggers
--
--         , requestDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepository
--
--         , requestDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRule
--
--         , requestListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplate
--
--         , requestUpdateRepositoryName $
--             newUpdateRepositoryName
--
--         , requestBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflicts
--
--         , requestGetRepository $
--             newGetRepository
--
--         , requestUpdatePullRequestStatus $
--             newUpdatePullRequestStatus
--
--         , requestPostCommentReply $
--             newPostCommentReply
--
--         , requestGetPullRequestOverrideState $
--             newGetPullRequestOverrideState
--
--         , requestGetCommentsForPullRequest $
--             newGetCommentsForPullRequest
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
--         , requestMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWay
--
--         , requestUpdatePullRequestDescription $
--             newUpdatePullRequestDescription
--
--         , requestGetMergeConflicts $
--             newGetMergeConflicts
--
--         , requestPutCommentReaction $
--             newPutCommentReaction
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplate
--
--         , requestListBranches $
--             newListBranches
--
--         , requestListApprovalRuleTemplates $
--             newListApprovalRuleTemplates
--
--         , requestPutRepositoryTriggers $
--             newPutRepositoryTriggers
--
--         , requestCreateBranch $
--             newCreateBranch
--
--         , requestMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWay
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetFile $
--             newGetFile
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
--         , requestOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRules
--
--         , requestGetPullRequest $
--             newGetPullRequest
--
--         , requestUpdateComment $
--             newUpdateComment
--
--         , requestGetDifferences $
--             newGetDifferences
--
--         , requestGetMergeCommit $
--             newGetMergeCommit
--
--         , requestGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplate
--
--         , requestGetCommit $
--             newGetCommit
--
--         , requestUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateName
--
--         , requestGetCommentReactions $
--             newGetCommentReactions
--
--         , requestTestRepositoryTriggers $
--             newTestRepositoryTriggers
--
--         , requestDeleteFile $
--             newDeleteFile
--
--         , requestGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommit
--
--         , requestMergeBranchesBySquash $
--             newMergeBranchesBySquash
--
--         , requestPostCommentForPullRequest $
--             newPostCommentForPullRequest
--
--         , requestMergePullRequestByFastForward $
--             newMergePullRequestByFastForward
--
--         , requestCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRule
--
--         , requestCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommit
--
--         , requestListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepository
--
--         , requestGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStates
--
--         , requestUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContent
--
--         , requestListRepositories $
--             newListRepositories
--
--         , requestUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescription
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestDescribePullRequestEvents $
--             newDescribePullRequestEvents
--
--         , requestDeleteCommentContent $
--             newDeleteCommentContent
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDescribeMergeConflicts $
--             newDescribeMergeConflicts
--
--         , requestBatchGetCommits $
--             newBatchGetCommits
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestCreatePullRequest $
--             newCreatePullRequest
--
--         , requestEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRules
--
--         , requestUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalState
--
--         , requestCreateCommit $
--             newCreateCommit
--
--         , requestAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepository
--
--         , requestGetBlob $
--             newGetBlob
--
--         , requestCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplate
--
--         , requestListPullRequests $
--             newListPullRequests
--
--         , requestDeleteBranch $
--             newDeleteBranch
--
--         , requestBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositories
--
--         , requestGetComment $
--             newGetComment
--
--         , requestGetBranch $
--             newGetBranch
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestMergeBranchesByFastForward $
--             newMergeBranchesByFastForward
--
--         , requestPostCommentForComparedCommit $
--             newPostCommentForComparedCommit
--
--         , requestMergePullRequestBySquash $
--             newMergePullRequestBySquash
--
--           ]

--     , testGroup "response"
--         [ responseBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
--
--         , responsePutFile $
--             newPutFileResponse
--
--         , responseGetRepositoryTriggers $
--             newGetRepositoryTriggersResponse
--
--         , responseDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepositoryResponse
--
--         , responseDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRuleResponse
--
--         , responseListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplateResponse
--
--         , responseUpdateRepositoryName $
--             newUpdateRepositoryNameResponse
--
--         , responseBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflictsResponse
--
--         , responseGetRepository $
--             newGetRepositoryResponse
--
--         , responseUpdatePullRequestStatus $
--             newUpdatePullRequestStatusResponse
--
--         , responsePostCommentReply $
--             newPostCommentReplyResponse
--
--         , responseGetPullRequestOverrideState $
--             newGetPullRequestOverrideStateResponse
--
--         , responseGetCommentsForPullRequest $
--             newGetCommentsForPullRequestResponse
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
--         , responseMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWayResponse
--
--         , responseUpdatePullRequestDescription $
--             newUpdatePullRequestDescriptionResponse
--
--         , responseGetMergeConflicts $
--             newGetMergeConflictsResponse
--
--         , responsePutCommentReaction $
--             newPutCommentReactionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplateResponse
--
--         , responseListBranches $
--             newListBranchesResponse
--
--         , responseListApprovalRuleTemplates $
--             newListApprovalRuleTemplatesResponse
--
--         , responsePutRepositoryTriggers $
--             newPutRepositoryTriggersResponse
--
--         , responseCreateBranch $
--             newCreateBranchResponse
--
--         , responseMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWayResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetFile $
--             newGetFileResponse
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
--         , responseOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRulesResponse
--
--         , responseGetPullRequest $
--             newGetPullRequestResponse
--
--         , responseUpdateComment $
--             newUpdateCommentResponse
--
--         , responseGetDifferences $
--             newGetDifferencesResponse
--
--         , responseGetMergeCommit $
--             newGetMergeCommitResponse
--
--         , responseGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplateResponse
--
--         , responseGetCommit $
--             newGetCommitResponse
--
--         , responseUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateNameResponse
--
--         , responseGetCommentReactions $
--             newGetCommentReactionsResponse
--
--         , responseTestRepositoryTriggers $
--             newTestRepositoryTriggersResponse
--
--         , responseDeleteFile $
--             newDeleteFileResponse
--
--         , responseGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommitResponse
--
--         , responseMergeBranchesBySquash $
--             newMergeBranchesBySquashResponse
--
--         , responsePostCommentForPullRequest $
--             newPostCommentForPullRequestResponse
--
--         , responseMergePullRequestByFastForward $
--             newMergePullRequestByFastForwardResponse
--
--         , responseCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRuleResponse
--
--         , responseCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommitResponse
--
--         , responseListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepositoryResponse
--
--         , responseGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStatesResponse
--
--         , responseUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContentResponse
--
--         , responseListRepositories $
--             newListRepositoriesResponse
--
--         , responseUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescriptionResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseDescribePullRequestEvents $
--             newDescribePullRequestEventsResponse
--
--         , responseDeleteCommentContent $
--             newDeleteCommentContentResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDescribeMergeConflicts $
--             newDescribeMergeConflictsResponse
--
--         , responseBatchGetCommits $
--             newBatchGetCommitsResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseCreatePullRequest $
--             newCreatePullRequestResponse
--
--         , responseEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRulesResponse
--
--         , responseUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalStateResponse
--
--         , responseCreateCommit $
--             newCreateCommitResponse
--
--         , responseAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepositoryResponse
--
--         , responseGetBlob $
--             newGetBlobResponse
--
--         , responseCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplateResponse
--
--         , responseListPullRequests $
--             newListPullRequestsResponse
--
--         , responseDeleteBranch $
--             newDeleteBranchResponse
--
--         , responseBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
--
--         , responseGetComment $
--             newGetCommentResponse
--
--         , responseGetBranch $
--             newGetBranchResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseMergeBranchesByFastForward $
--             newMergeBranchesByFastForwardResponse
--
--         , responsePostCommentForComparedCommit $
--             newPostCommentForComparedCommitResponse
--
--         , responseMergePullRequestBySquash $
--             newMergePullRequestBySquashResponse
--
--           ]
--     ]

-- Requests

requestBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositories -> TestTree
requestBatchAssociateApprovalRuleTemplateWithRepositories =
  req
    "BatchAssociateApprovalRuleTemplateWithRepositories"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositories.yaml"

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

requestDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepository -> TestTree
requestDisassociateApprovalRuleTemplateFromRepository =
  req
    "DisassociateApprovalRuleTemplateFromRepository"
    "fixture/DisassociateApprovalRuleTemplateFromRepository.yaml"

requestDeletePullRequestApprovalRule :: DeletePullRequestApprovalRule -> TestTree
requestDeletePullRequestApprovalRule =
  req
    "DeletePullRequestApprovalRule"
    "fixture/DeletePullRequestApprovalRule.yaml"

requestListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplate -> TestTree
requestListRepositoriesForApprovalRuleTemplate =
  req
    "ListRepositoriesForApprovalRuleTemplate"
    "fixture/ListRepositoriesForApprovalRuleTemplate.yaml"

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

requestGetRepository :: GetRepository -> TestTree
requestGetRepository =
  req
    "GetRepository"
    "fixture/GetRepository.yaml"

requestUpdatePullRequestStatus :: UpdatePullRequestStatus -> TestTree
requestUpdatePullRequestStatus =
  req
    "UpdatePullRequestStatus"
    "fixture/UpdatePullRequestStatus.yaml"

requestPostCommentReply :: PostCommentReply -> TestTree
requestPostCommentReply =
  req
    "PostCommentReply"
    "fixture/PostCommentReply.yaml"

requestGetPullRequestOverrideState :: GetPullRequestOverrideState -> TestTree
requestGetPullRequestOverrideState =
  req
    "GetPullRequestOverrideState"
    "fixture/GetPullRequestOverrideState.yaml"

requestGetCommentsForPullRequest :: GetCommentsForPullRequest -> TestTree
requestGetCommentsForPullRequest =
  req
    "GetCommentsForPullRequest"
    "fixture/GetCommentsForPullRequest.yaml"

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

requestMergePullRequestByThreeWay :: MergePullRequestByThreeWay -> TestTree
requestMergePullRequestByThreeWay =
  req
    "MergePullRequestByThreeWay"
    "fixture/MergePullRequestByThreeWay.yaml"

requestUpdatePullRequestDescription :: UpdatePullRequestDescription -> TestTree
requestUpdatePullRequestDescription =
  req
    "UpdatePullRequestDescription"
    "fixture/UpdatePullRequestDescription.yaml"

requestGetMergeConflicts :: GetMergeConflicts -> TestTree
requestGetMergeConflicts =
  req
    "GetMergeConflicts"
    "fixture/GetMergeConflicts.yaml"

requestPutCommentReaction :: PutCommentReaction -> TestTree
requestPutCommentReaction =
  req
    "PutCommentReaction"
    "fixture/PutCommentReaction.yaml"

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

requestListBranches :: ListBranches -> TestTree
requestListBranches =
  req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestListApprovalRuleTemplates :: ListApprovalRuleTemplates -> TestTree
requestListApprovalRuleTemplates =
  req
    "ListApprovalRuleTemplates"
    "fixture/ListApprovalRuleTemplates.yaml"

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

requestMergeBranchesByThreeWay :: MergeBranchesByThreeWay -> TestTree
requestMergeBranchesByThreeWay =
  req
    "MergeBranchesByThreeWay"
    "fixture/MergeBranchesByThreeWay.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetFile :: GetFile -> TestTree
requestGetFile =
  req
    "GetFile"
    "fixture/GetFile.yaml"

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

requestOverridePullRequestApprovalRules :: OverridePullRequestApprovalRules -> TestTree
requestOverridePullRequestApprovalRules =
  req
    "OverridePullRequestApprovalRules"
    "fixture/OverridePullRequestApprovalRules.yaml"

requestGetPullRequest :: GetPullRequest -> TestTree
requestGetPullRequest =
  req
    "GetPullRequest"
    "fixture/GetPullRequest.yaml"

requestUpdateComment :: UpdateComment -> TestTree
requestUpdateComment =
  req
    "UpdateComment"
    "fixture/UpdateComment.yaml"

requestGetDifferences :: GetDifferences -> TestTree
requestGetDifferences =
  req
    "GetDifferences"
    "fixture/GetDifferences.yaml"

requestGetMergeCommit :: GetMergeCommit -> TestTree
requestGetMergeCommit =
  req
    "GetMergeCommit"
    "fixture/GetMergeCommit.yaml"

requestGetApprovalRuleTemplate :: GetApprovalRuleTemplate -> TestTree
requestGetApprovalRuleTemplate =
  req
    "GetApprovalRuleTemplate"
    "fixture/GetApprovalRuleTemplate.yaml"

requestGetCommit :: GetCommit -> TestTree
requestGetCommit =
  req
    "GetCommit"
    "fixture/GetCommit.yaml"

requestUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateName -> TestTree
requestUpdateApprovalRuleTemplateName =
  req
    "UpdateApprovalRuleTemplateName"
    "fixture/UpdateApprovalRuleTemplateName.yaml"

requestGetCommentReactions :: GetCommentReactions -> TestTree
requestGetCommentReactions =
  req
    "GetCommentReactions"
    "fixture/GetCommentReactions.yaml"

requestTestRepositoryTriggers :: TestRepositoryTriggers -> TestTree
requestTestRepositoryTriggers =
  req
    "TestRepositoryTriggers"
    "fixture/TestRepositoryTriggers.yaml"

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

requestMergeBranchesBySquash :: MergeBranchesBySquash -> TestTree
requestMergeBranchesBySquash =
  req
    "MergeBranchesBySquash"
    "fixture/MergeBranchesBySquash.yaml"

requestPostCommentForPullRequest :: PostCommentForPullRequest -> TestTree
requestPostCommentForPullRequest =
  req
    "PostCommentForPullRequest"
    "fixture/PostCommentForPullRequest.yaml"

requestMergePullRequestByFastForward :: MergePullRequestByFastForward -> TestTree
requestMergePullRequestByFastForward =
  req
    "MergePullRequestByFastForward"
    "fixture/MergePullRequestByFastForward.yaml"

requestCreatePullRequestApprovalRule :: CreatePullRequestApprovalRule -> TestTree
requestCreatePullRequestApprovalRule =
  req
    "CreatePullRequestApprovalRule"
    "fixture/CreatePullRequestApprovalRule.yaml"

requestCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommit -> TestTree
requestCreateUnreferencedMergeCommit =
  req
    "CreateUnreferencedMergeCommit"
    "fixture/CreateUnreferencedMergeCommit.yaml"

requestListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepository -> TestTree
requestListAssociatedApprovalRuleTemplatesForRepository =
  req
    "ListAssociatedApprovalRuleTemplatesForRepository"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepository.yaml"

requestGetPullRequestApprovalStates :: GetPullRequestApprovalStates -> TestTree
requestGetPullRequestApprovalStates =
  req
    "GetPullRequestApprovalStates"
    "fixture/GetPullRequestApprovalStates.yaml"

requestUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContent -> TestTree
requestUpdateApprovalRuleTemplateContent =
  req
    "UpdateApprovalRuleTemplateContent"
    "fixture/UpdateApprovalRuleTemplateContent.yaml"

requestListRepositories :: ListRepositories -> TestTree
requestListRepositories =
  req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

requestUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescription -> TestTree
requestUpdateApprovalRuleTemplateDescription =
  req
    "UpdateApprovalRuleTemplateDescription"
    "fixture/UpdateApprovalRuleTemplateDescription.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestDescribePullRequestEvents :: DescribePullRequestEvents -> TestTree
requestDescribePullRequestEvents =
  req
    "DescribePullRequestEvents"
    "fixture/DescribePullRequestEvents.yaml"

requestDeleteCommentContent :: DeleteCommentContent -> TestTree
requestDeleteCommentContent =
  req
    "DeleteCommentContent"
    "fixture/DeleteCommentContent.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository =
  req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestDescribeMergeConflicts :: DescribeMergeConflicts -> TestTree
requestDescribeMergeConflicts =
  req
    "DescribeMergeConflicts"
    "fixture/DescribeMergeConflicts.yaml"

requestBatchGetCommits :: BatchGetCommits -> TestTree
requestBatchGetCommits =
  req
    "BatchGetCommits"
    "fixture/BatchGetCommits.yaml"

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

requestEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRules -> TestTree
requestEvaluatePullRequestApprovalRules =
  req
    "EvaluatePullRequestApprovalRules"
    "fixture/EvaluatePullRequestApprovalRules.yaml"

requestUpdatePullRequestApprovalState :: UpdatePullRequestApprovalState -> TestTree
requestUpdatePullRequestApprovalState =
  req
    "UpdatePullRequestApprovalState"
    "fixture/UpdatePullRequestApprovalState.yaml"

requestCreateCommit :: CreateCommit -> TestTree
requestCreateCommit =
  req
    "CreateCommit"
    "fixture/CreateCommit.yaml"

requestAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepository -> TestTree
requestAssociateApprovalRuleTemplateWithRepository =
  req
    "AssociateApprovalRuleTemplateWithRepository"
    "fixture/AssociateApprovalRuleTemplateWithRepository.yaml"

requestGetBlob :: GetBlob -> TestTree
requestGetBlob =
  req
    "GetBlob"
    "fixture/GetBlob.yaml"

requestCreateApprovalRuleTemplate :: CreateApprovalRuleTemplate -> TestTree
requestCreateApprovalRuleTemplate =
  req
    "CreateApprovalRuleTemplate"
    "fixture/CreateApprovalRuleTemplate.yaml"

requestListPullRequests :: ListPullRequests -> TestTree
requestListPullRequests =
  req
    "ListPullRequests"
    "fixture/ListPullRequests.yaml"

requestDeleteBranch :: DeleteBranch -> TestTree
requestDeleteBranch =
  req
    "DeleteBranch"
    "fixture/DeleteBranch.yaml"

requestBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositories -> TestTree
requestBatchDisassociateApprovalRuleTemplateFromRepositories =
  req
    "BatchDisassociateApprovalRuleTemplateFromRepositories"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositories.yaml"

requestGetComment :: GetComment -> TestTree
requestGetComment =
  req
    "GetComment"
    "fixture/GetComment.yaml"

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

requestMergeBranchesByFastForward :: MergeBranchesByFastForward -> TestTree
requestMergeBranchesByFastForward =
  req
    "MergeBranchesByFastForward"
    "fixture/MergeBranchesByFastForward.yaml"

requestPostCommentForComparedCommit :: PostCommentForComparedCommit -> TestTree
requestPostCommentForComparedCommit =
  req
    "PostCommentForComparedCommit"
    "fixture/PostCommentForComparedCommit.yaml"

requestMergePullRequestBySquash :: MergePullRequestBySquash -> TestTree
requestMergePullRequestBySquash =
  req
    "MergePullRequestBySquash"
    "fixture/MergePullRequestBySquash.yaml"

-- Responses

responseBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> TestTree
responseBatchAssociateApprovalRuleTemplateWithRepositories =
  res
    "BatchAssociateApprovalRuleTemplateWithRepositoriesResponse"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateApprovalRuleTemplateWithRepositories)

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

responseDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepositoryResponse -> TestTree
responseDisassociateApprovalRuleTemplateFromRepository =
  res
    "DisassociateApprovalRuleTemplateFromRepositoryResponse"
    "fixture/DisassociateApprovalRuleTemplateFromRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateApprovalRuleTemplateFromRepository)

responseDeletePullRequestApprovalRule :: DeletePullRequestApprovalRuleResponse -> TestTree
responseDeletePullRequestApprovalRule =
  res
    "DeletePullRequestApprovalRuleResponse"
    "fixture/DeletePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePullRequestApprovalRule)

responseListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplateResponse -> TestTree
responseListRepositoriesForApprovalRuleTemplate =
  res
    "ListRepositoriesForApprovalRuleTemplateResponse"
    "fixture/ListRepositoriesForApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy ListRepositoriesForApprovalRuleTemplate)

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

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository =
  res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy GetRepository)

responseUpdatePullRequestStatus :: UpdatePullRequestStatusResponse -> TestTree
responseUpdatePullRequestStatus =
  res
    "UpdatePullRequestStatusResponse"
    "fixture/UpdatePullRequestStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestStatus)

responsePostCommentReply :: PostCommentReplyResponse -> TestTree
responsePostCommentReply =
  res
    "PostCommentReplyResponse"
    "fixture/PostCommentReplyResponse.proto"
    defaultService
    (Proxy :: Proxy PostCommentReply)

responseGetPullRequestOverrideState :: GetPullRequestOverrideStateResponse -> TestTree
responseGetPullRequestOverrideState =
  res
    "GetPullRequestOverrideStateResponse"
    "fixture/GetPullRequestOverrideStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetPullRequestOverrideState)

responseGetCommentsForPullRequest :: GetCommentsForPullRequestResponse -> TestTree
responseGetCommentsForPullRequest =
  res
    "GetCommentsForPullRequestResponse"
    "fixture/GetCommentsForPullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommentsForPullRequest)

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

responseMergePullRequestByThreeWay :: MergePullRequestByThreeWayResponse -> TestTree
responseMergePullRequestByThreeWay =
  res
    "MergePullRequestByThreeWayResponse"
    "fixture/MergePullRequestByThreeWayResponse.proto"
    defaultService
    (Proxy :: Proxy MergePullRequestByThreeWay)

responseUpdatePullRequestDescription :: UpdatePullRequestDescriptionResponse -> TestTree
responseUpdatePullRequestDescription =
  res
    "UpdatePullRequestDescriptionResponse"
    "fixture/UpdatePullRequestDescriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestDescription)

responseGetMergeConflicts :: GetMergeConflictsResponse -> TestTree
responseGetMergeConflicts =
  res
    "GetMergeConflictsResponse"
    "fixture/GetMergeConflictsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMergeConflicts)

responsePutCommentReaction :: PutCommentReactionResponse -> TestTree
responsePutCommentReaction =
  res
    "PutCommentReactionResponse"
    "fixture/PutCommentReactionResponse.proto"
    defaultService
    (Proxy :: Proxy PutCommentReaction)

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

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBranches)

responseListApprovalRuleTemplates :: ListApprovalRuleTemplatesResponse -> TestTree
responseListApprovalRuleTemplates =
  res
    "ListApprovalRuleTemplatesResponse"
    "fixture/ListApprovalRuleTemplatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListApprovalRuleTemplates)

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

responseMergeBranchesByThreeWay :: MergeBranchesByThreeWayResponse -> TestTree
responseMergeBranchesByThreeWay =
  res
    "MergeBranchesByThreeWayResponse"
    "fixture/MergeBranchesByThreeWayResponse.proto"
    defaultService
    (Proxy :: Proxy MergeBranchesByThreeWay)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetFile :: GetFileResponse -> TestTree
responseGetFile =
  res
    "GetFileResponse"
    "fixture/GetFileResponse.proto"
    defaultService
    (Proxy :: Proxy GetFile)

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

responseOverridePullRequestApprovalRules :: OverridePullRequestApprovalRulesResponse -> TestTree
responseOverridePullRequestApprovalRules =
  res
    "OverridePullRequestApprovalRulesResponse"
    "fixture/OverridePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy :: Proxy OverridePullRequestApprovalRules)

responseGetPullRequest :: GetPullRequestResponse -> TestTree
responseGetPullRequest =
  res
    "GetPullRequestResponse"
    "fixture/GetPullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy GetPullRequest)

responseUpdateComment :: UpdateCommentResponse -> TestTree
responseUpdateComment =
  res
    "UpdateCommentResponse"
    "fixture/UpdateCommentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateComment)

responseGetDifferences :: GetDifferencesResponse -> TestTree
responseGetDifferences =
  res
    "GetDifferencesResponse"
    "fixture/GetDifferencesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDifferences)

responseGetMergeCommit :: GetMergeCommitResponse -> TestTree
responseGetMergeCommit =
  res
    "GetMergeCommitResponse"
    "fixture/GetMergeCommitResponse.proto"
    defaultService
    (Proxy :: Proxy GetMergeCommit)

responseGetApprovalRuleTemplate :: GetApprovalRuleTemplateResponse -> TestTree
responseGetApprovalRuleTemplate =
  res
    "GetApprovalRuleTemplateResponse"
    "fixture/GetApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy GetApprovalRuleTemplate)

responseGetCommit :: GetCommitResponse -> TestTree
responseGetCommit =
  res
    "GetCommitResponse"
    "fixture/GetCommitResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommit)

responseUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateNameResponse -> TestTree
responseUpdateApprovalRuleTemplateName =
  res
    "UpdateApprovalRuleTemplateNameResponse"
    "fixture/UpdateApprovalRuleTemplateNameResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApprovalRuleTemplateName)

responseGetCommentReactions :: GetCommentReactionsResponse -> TestTree
responseGetCommentReactions =
  res
    "GetCommentReactionsResponse"
    "fixture/GetCommentReactionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommentReactions)

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers =
  res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    defaultService
    (Proxy :: Proxy TestRepositoryTriggers)

responseDeleteFile :: DeleteFileResponse -> TestTree
responseDeleteFile =
  res
    "DeleteFileResponse"
    "fixture/DeleteFileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFile)

responseGetCommentsForComparedCommit :: GetCommentsForComparedCommitResponse -> TestTree
responseGetCommentsForComparedCommit =
  res
    "GetCommentsForComparedCommitResponse"
    "fixture/GetCommentsForComparedCommitResponse.proto"
    defaultService
    (Proxy :: Proxy GetCommentsForComparedCommit)

responseMergeBranchesBySquash :: MergeBranchesBySquashResponse -> TestTree
responseMergeBranchesBySquash =
  res
    "MergeBranchesBySquashResponse"
    "fixture/MergeBranchesBySquashResponse.proto"
    defaultService
    (Proxy :: Proxy MergeBranchesBySquash)

responsePostCommentForPullRequest :: PostCommentForPullRequestResponse -> TestTree
responsePostCommentForPullRequest =
  res
    "PostCommentForPullRequestResponse"
    "fixture/PostCommentForPullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy PostCommentForPullRequest)

responseMergePullRequestByFastForward :: MergePullRequestByFastForwardResponse -> TestTree
responseMergePullRequestByFastForward =
  res
    "MergePullRequestByFastForwardResponse"
    "fixture/MergePullRequestByFastForwardResponse.proto"
    defaultService
    (Proxy :: Proxy MergePullRequestByFastForward)

responseCreatePullRequestApprovalRule :: CreatePullRequestApprovalRuleResponse -> TestTree
responseCreatePullRequestApprovalRule =
  res
    "CreatePullRequestApprovalRuleResponse"
    "fixture/CreatePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePullRequestApprovalRule)

responseCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommitResponse -> TestTree
responseCreateUnreferencedMergeCommit =
  res
    "CreateUnreferencedMergeCommitResponse"
    "fixture/CreateUnreferencedMergeCommitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUnreferencedMergeCommit)

responseListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> TestTree
responseListAssociatedApprovalRuleTemplatesForRepository =
  res
    "ListAssociatedApprovalRuleTemplatesForRepositoryResponse"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedApprovalRuleTemplatesForRepository)

responseGetPullRequestApprovalStates :: GetPullRequestApprovalStatesResponse -> TestTree
responseGetPullRequestApprovalStates =
  res
    "GetPullRequestApprovalStatesResponse"
    "fixture/GetPullRequestApprovalStatesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPullRequestApprovalStates)

responseUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContentResponse -> TestTree
responseUpdateApprovalRuleTemplateContent =
  res
    "UpdateApprovalRuleTemplateContentResponse"
    "fixture/UpdateApprovalRuleTemplateContentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApprovalRuleTemplateContent)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy ListRepositories)

responseUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescriptionResponse -> TestTree
responseUpdateApprovalRuleTemplateDescription =
  res
    "UpdateApprovalRuleTemplateDescriptionResponse"
    "fixture/UpdateApprovalRuleTemplateDescriptionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApprovalRuleTemplateDescription)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRepository)

responseDescribePullRequestEvents :: DescribePullRequestEventsResponse -> TestTree
responseDescribePullRequestEvents =
  res
    "DescribePullRequestEventsResponse"
    "fixture/DescribePullRequestEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePullRequestEvents)

responseDeleteCommentContent :: DeleteCommentContentResponse -> TestTree
responseDeleteCommentContent =
  res
    "DeleteCommentContentResponse"
    "fixture/DeleteCommentContentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCommentContent)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRepository)

responseDescribeMergeConflicts :: DescribeMergeConflictsResponse -> TestTree
responseDescribeMergeConflicts =
  res
    "DescribeMergeConflictsResponse"
    "fixture/DescribeMergeConflictsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMergeConflicts)

responseBatchGetCommits :: BatchGetCommitsResponse -> TestTree
responseBatchGetCommits =
  res
    "BatchGetCommitsResponse"
    "fixture/BatchGetCommitsResponse.proto"
    defaultService
    (Proxy :: Proxy BatchGetCommits)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy :: Proxy GetFolder)

responseCreatePullRequest :: CreatePullRequestResponse -> TestTree
responseCreatePullRequest =
  res
    "CreatePullRequestResponse"
    "fixture/CreatePullRequestResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePullRequest)

responseEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRulesResponse -> TestTree
responseEvaluatePullRequestApprovalRules =
  res
    "EvaluatePullRequestApprovalRulesResponse"
    "fixture/EvaluatePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy :: Proxy EvaluatePullRequestApprovalRules)

responseUpdatePullRequestApprovalState :: UpdatePullRequestApprovalStateResponse -> TestTree
responseUpdatePullRequestApprovalState =
  res
    "UpdatePullRequestApprovalStateResponse"
    "fixture/UpdatePullRequestApprovalStateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePullRequestApprovalState)

responseCreateCommit :: CreateCommitResponse -> TestTree
responseCreateCommit =
  res
    "CreateCommitResponse"
    "fixture/CreateCommitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCommit)

responseAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepositoryResponse -> TestTree
responseAssociateApprovalRuleTemplateWithRepository =
  res
    "AssociateApprovalRuleTemplateWithRepositoryResponse"
    "fixture/AssociateApprovalRuleTemplateWithRepositoryResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateApprovalRuleTemplateWithRepository)

responseGetBlob :: GetBlobResponse -> TestTree
responseGetBlob =
  res
    "GetBlobResponse"
    "fixture/GetBlobResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlob)

responseCreateApprovalRuleTemplate :: CreateApprovalRuleTemplateResponse -> TestTree
responseCreateApprovalRuleTemplate =
  res
    "CreateApprovalRuleTemplateResponse"
    "fixture/CreateApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApprovalRuleTemplate)

responseListPullRequests :: ListPullRequestsResponse -> TestTree
responseListPullRequests =
  res
    "ListPullRequestsResponse"
    "fixture/ListPullRequestsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPullRequests)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBranch)

responseBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> TestTree
responseBatchDisassociateApprovalRuleTemplateFromRepositories =
  res
    "BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDisassociateApprovalRuleTemplateFromRepositories)

responseGetComment :: GetCommentResponse -> TestTree
responseGetComment =
  res
    "GetCommentResponse"
    "fixture/GetCommentResponse.proto"
    defaultService
    (Proxy :: Proxy GetComment)

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

responseMergeBranchesByFastForward :: MergeBranchesByFastForwardResponse -> TestTree
responseMergeBranchesByFastForward =
  res
    "MergeBranchesByFastForwardResponse"
    "fixture/MergeBranchesByFastForwardResponse.proto"
    defaultService
    (Proxy :: Proxy MergeBranchesByFastForward)

responsePostCommentForComparedCommit :: PostCommentForComparedCommitResponse -> TestTree
responsePostCommentForComparedCommit =
  res
    "PostCommentForComparedCommitResponse"
    "fixture/PostCommentForComparedCommitResponse.proto"
    defaultService
    (Proxy :: Proxy PostCommentForComparedCommit)

responseMergePullRequestBySquash :: MergePullRequestBySquashResponse -> TestTree
responseMergePullRequestBySquash =
  res
    "MergePullRequestBySquashResponse"
    "fixture/MergePullRequestBySquashResponse.proto"
    defaultService
    (Proxy :: Proxy MergePullRequestBySquash)

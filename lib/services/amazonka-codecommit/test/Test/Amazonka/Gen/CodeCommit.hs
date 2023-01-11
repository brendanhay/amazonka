{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeCommit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeCommit where

import Amazonka.CodeCommit
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeCommit.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepository
--
--         , requestBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositories
--
--         , requestBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflicts
--
--         , requestBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositories
--
--         , requestBatchGetCommits $
--             newBatchGetCommits
--
--         , requestBatchGetRepositories $
--             newBatchGetRepositories
--
--         , requestCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplate
--
--         , requestCreateBranch $
--             newCreateBranch
--
--         , requestCreateCommit $
--             newCreateCommit
--
--         , requestCreatePullRequest $
--             newCreatePullRequest
--
--         , requestCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRule
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommit
--
--         , requestDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplate
--
--         , requestDeleteBranch $
--             newDeleteBranch
--
--         , requestDeleteCommentContent $
--             newDeleteCommentContent
--
--         , requestDeleteFile $
--             newDeleteFile
--
--         , requestDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRule
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDescribeMergeConflicts $
--             newDescribeMergeConflicts
--
--         , requestDescribePullRequestEvents $
--             newDescribePullRequestEvents
--
--         , requestDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepository
--
--         , requestEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRules
--
--         , requestGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplate
--
--         , requestGetBlob $
--             newGetBlob
--
--         , requestGetBranch $
--             newGetBranch
--
--         , requestGetComment $
--             newGetComment
--
--         , requestGetCommentReactions $
--             newGetCommentReactions
--
--         , requestGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommit
--
--         , requestGetCommentsForPullRequest $
--             newGetCommentsForPullRequest
--
--         , requestGetCommit $
--             newGetCommit
--
--         , requestGetDifferences $
--             newGetDifferences
--
--         , requestGetFile $
--             newGetFile
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestGetMergeCommit $
--             newGetMergeCommit
--
--         , requestGetMergeConflicts $
--             newGetMergeConflicts
--
--         , requestGetMergeOptions $
--             newGetMergeOptions
--
--         , requestGetPullRequest $
--             newGetPullRequest
--
--         , requestGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStates
--
--         , requestGetPullRequestOverrideState $
--             newGetPullRequestOverrideState
--
--         , requestGetRepository $
--             newGetRepository
--
--         , requestGetRepositoryTriggers $
--             newGetRepositoryTriggers
--
--         , requestListApprovalRuleTemplates $
--             newListApprovalRuleTemplates
--
--         , requestListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepository
--
--         , requestListBranches $
--             newListBranches
--
--         , requestListPullRequests $
--             newListPullRequests
--
--         , requestListRepositories $
--             newListRepositories
--
--         , requestListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplate
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestMergeBranchesByFastForward $
--             newMergeBranchesByFastForward
--
--         , requestMergeBranchesBySquash $
--             newMergeBranchesBySquash
--
--         , requestMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWay
--
--         , requestMergePullRequestByFastForward $
--             newMergePullRequestByFastForward
--
--         , requestMergePullRequestBySquash $
--             newMergePullRequestBySquash
--
--         , requestMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWay
--
--         , requestOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRules
--
--         , requestPostCommentForComparedCommit $
--             newPostCommentForComparedCommit
--
--         , requestPostCommentForPullRequest $
--             newPostCommentForPullRequest
--
--         , requestPostCommentReply $
--             newPostCommentReply
--
--         , requestPutCommentReaction $
--             newPutCommentReaction
--
--         , requestPutFile $
--             newPutFile
--
--         , requestPutRepositoryTriggers $
--             newPutRepositoryTriggers
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestRepositoryTriggers $
--             newTestRepositoryTriggers
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContent
--
--         , requestUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescription
--
--         , requestUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateName
--
--         , requestUpdateComment $
--             newUpdateComment
--
--         , requestUpdateDefaultBranch $
--             newUpdateDefaultBranch
--
--         , requestUpdatePullRequestApprovalRuleContent $
--             newUpdatePullRequestApprovalRuleContent
--
--         , requestUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalState
--
--         , requestUpdatePullRequestDescription $
--             newUpdatePullRequestDescription
--
--         , requestUpdatePullRequestStatus $
--             newUpdatePullRequestStatus
--
--         , requestUpdatePullRequestTitle $
--             newUpdatePullRequestTitle
--
--         , requestUpdateRepositoryDescription $
--             newUpdateRepositoryDescription
--
--         , requestUpdateRepositoryName $
--             newUpdateRepositoryName
--
--           ]

--     , testGroup "response"
--         [ responseAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepositoryResponse
--
--         , responseBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
--
--         , responseBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflictsResponse
--
--         , responseBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
--
--         , responseBatchGetCommits $
--             newBatchGetCommitsResponse
--
--         , responseBatchGetRepositories $
--             newBatchGetRepositoriesResponse
--
--         , responseCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplateResponse
--
--         , responseCreateBranch $
--             newCreateBranchResponse
--
--         , responseCreateCommit $
--             newCreateCommitResponse
--
--         , responseCreatePullRequest $
--             newCreatePullRequestResponse
--
--         , responseCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRuleResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommitResponse
--
--         , responseDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplateResponse
--
--         , responseDeleteBranch $
--             newDeleteBranchResponse
--
--         , responseDeleteCommentContent $
--             newDeleteCommentContentResponse
--
--         , responseDeleteFile $
--             newDeleteFileResponse
--
--         , responseDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRuleResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDescribeMergeConflicts $
--             newDescribeMergeConflictsResponse
--
--         , responseDescribePullRequestEvents $
--             newDescribePullRequestEventsResponse
--
--         , responseDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepositoryResponse
--
--         , responseEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRulesResponse
--
--         , responseGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplateResponse
--
--         , responseGetBlob $
--             newGetBlobResponse
--
--         , responseGetBranch $
--             newGetBranchResponse
--
--         , responseGetComment $
--             newGetCommentResponse
--
--         , responseGetCommentReactions $
--             newGetCommentReactionsResponse
--
--         , responseGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommitResponse
--
--         , responseGetCommentsForPullRequest $
--             newGetCommentsForPullRequestResponse
--
--         , responseGetCommit $
--             newGetCommitResponse
--
--         , responseGetDifferences $
--             newGetDifferencesResponse
--
--         , responseGetFile $
--             newGetFileResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseGetMergeCommit $
--             newGetMergeCommitResponse
--
--         , responseGetMergeConflicts $
--             newGetMergeConflictsResponse
--
--         , responseGetMergeOptions $
--             newGetMergeOptionsResponse
--
--         , responseGetPullRequest $
--             newGetPullRequestResponse
--
--         , responseGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStatesResponse
--
--         , responseGetPullRequestOverrideState $
--             newGetPullRequestOverrideStateResponse
--
--         , responseGetRepository $
--             newGetRepositoryResponse
--
--         , responseGetRepositoryTriggers $
--             newGetRepositoryTriggersResponse
--
--         , responseListApprovalRuleTemplates $
--             newListApprovalRuleTemplatesResponse
--
--         , responseListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepositoryResponse
--
--         , responseListBranches $
--             newListBranchesResponse
--
--         , responseListPullRequests $
--             newListPullRequestsResponse
--
--         , responseListRepositories $
--             newListRepositoriesResponse
--
--         , responseListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplateResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseMergeBranchesByFastForward $
--             newMergeBranchesByFastForwardResponse
--
--         , responseMergeBranchesBySquash $
--             newMergeBranchesBySquashResponse
--
--         , responseMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWayResponse
--
--         , responseMergePullRequestByFastForward $
--             newMergePullRequestByFastForwardResponse
--
--         , responseMergePullRequestBySquash $
--             newMergePullRequestBySquashResponse
--
--         , responseMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWayResponse
--
--         , responseOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRulesResponse
--
--         , responsePostCommentForComparedCommit $
--             newPostCommentForComparedCommitResponse
--
--         , responsePostCommentForPullRequest $
--             newPostCommentForPullRequestResponse
--
--         , responsePostCommentReply $
--             newPostCommentReplyResponse
--
--         , responsePutCommentReaction $
--             newPutCommentReactionResponse
--
--         , responsePutFile $
--             newPutFileResponse
--
--         , responsePutRepositoryTriggers $
--             newPutRepositoryTriggersResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestRepositoryTriggers $
--             newTestRepositoryTriggersResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContentResponse
--
--         , responseUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescriptionResponse
--
--         , responseUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateNameResponse
--
--         , responseUpdateComment $
--             newUpdateCommentResponse
--
--         , responseUpdateDefaultBranch $
--             newUpdateDefaultBranchResponse
--
--         , responseUpdatePullRequestApprovalRuleContent $
--             newUpdatePullRequestApprovalRuleContentResponse
--
--         , responseUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalStateResponse
--
--         , responseUpdatePullRequestDescription $
--             newUpdatePullRequestDescriptionResponse
--
--         , responseUpdatePullRequestStatus $
--             newUpdatePullRequestStatusResponse
--
--         , responseUpdatePullRequestTitle $
--             newUpdatePullRequestTitleResponse
--
--         , responseUpdateRepositoryDescription $
--             newUpdateRepositoryDescriptionResponse
--
--         , responseUpdateRepositoryName $
--             newUpdateRepositoryNameResponse
--
--           ]
--     ]

-- Requests

requestAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepository -> TestTree
requestAssociateApprovalRuleTemplateWithRepository =
  req
    "AssociateApprovalRuleTemplateWithRepository"
    "fixture/AssociateApprovalRuleTemplateWithRepository.yaml"

requestBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositories -> TestTree
requestBatchAssociateApprovalRuleTemplateWithRepositories =
  req
    "BatchAssociateApprovalRuleTemplateWithRepositories"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositories.yaml"

requestBatchDescribeMergeConflicts :: BatchDescribeMergeConflicts -> TestTree
requestBatchDescribeMergeConflicts =
  req
    "BatchDescribeMergeConflicts"
    "fixture/BatchDescribeMergeConflicts.yaml"

requestBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositories -> TestTree
requestBatchDisassociateApprovalRuleTemplateFromRepositories =
  req
    "BatchDisassociateApprovalRuleTemplateFromRepositories"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositories.yaml"

requestBatchGetCommits :: BatchGetCommits -> TestTree
requestBatchGetCommits =
  req
    "BatchGetCommits"
    "fixture/BatchGetCommits.yaml"

requestBatchGetRepositories :: BatchGetRepositories -> TestTree
requestBatchGetRepositories =
  req
    "BatchGetRepositories"
    "fixture/BatchGetRepositories.yaml"

requestCreateApprovalRuleTemplate :: CreateApprovalRuleTemplate -> TestTree
requestCreateApprovalRuleTemplate =
  req
    "CreateApprovalRuleTemplate"
    "fixture/CreateApprovalRuleTemplate.yaml"

requestCreateBranch :: CreateBranch -> TestTree
requestCreateBranch =
  req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

requestCreateCommit :: CreateCommit -> TestTree
requestCreateCommit =
  req
    "CreateCommit"
    "fixture/CreateCommit.yaml"

requestCreatePullRequest :: CreatePullRequest -> TestTree
requestCreatePullRequest =
  req
    "CreatePullRequest"
    "fixture/CreatePullRequest.yaml"

requestCreatePullRequestApprovalRule :: CreatePullRequestApprovalRule -> TestTree
requestCreatePullRequestApprovalRule =
  req
    "CreatePullRequestApprovalRule"
    "fixture/CreatePullRequestApprovalRule.yaml"

requestCreateRepository :: CreateRepository -> TestTree
requestCreateRepository =
  req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

requestCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommit -> TestTree
requestCreateUnreferencedMergeCommit =
  req
    "CreateUnreferencedMergeCommit"
    "fixture/CreateUnreferencedMergeCommit.yaml"

requestDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplate -> TestTree
requestDeleteApprovalRuleTemplate =
  req
    "DeleteApprovalRuleTemplate"
    "fixture/DeleteApprovalRuleTemplate.yaml"

requestDeleteBranch :: DeleteBranch -> TestTree
requestDeleteBranch =
  req
    "DeleteBranch"
    "fixture/DeleteBranch.yaml"

requestDeleteCommentContent :: DeleteCommentContent -> TestTree
requestDeleteCommentContent =
  req
    "DeleteCommentContent"
    "fixture/DeleteCommentContent.yaml"

requestDeleteFile :: DeleteFile -> TestTree
requestDeleteFile =
  req
    "DeleteFile"
    "fixture/DeleteFile.yaml"

requestDeletePullRequestApprovalRule :: DeletePullRequestApprovalRule -> TestTree
requestDeletePullRequestApprovalRule =
  req
    "DeletePullRequestApprovalRule"
    "fixture/DeletePullRequestApprovalRule.yaml"

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

requestDescribePullRequestEvents :: DescribePullRequestEvents -> TestTree
requestDescribePullRequestEvents =
  req
    "DescribePullRequestEvents"
    "fixture/DescribePullRequestEvents.yaml"

requestDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepository -> TestTree
requestDisassociateApprovalRuleTemplateFromRepository =
  req
    "DisassociateApprovalRuleTemplateFromRepository"
    "fixture/DisassociateApprovalRuleTemplateFromRepository.yaml"

requestEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRules -> TestTree
requestEvaluatePullRequestApprovalRules =
  req
    "EvaluatePullRequestApprovalRules"
    "fixture/EvaluatePullRequestApprovalRules.yaml"

requestGetApprovalRuleTemplate :: GetApprovalRuleTemplate -> TestTree
requestGetApprovalRuleTemplate =
  req
    "GetApprovalRuleTemplate"
    "fixture/GetApprovalRuleTemplate.yaml"

requestGetBlob :: GetBlob -> TestTree
requestGetBlob =
  req
    "GetBlob"
    "fixture/GetBlob.yaml"

requestGetBranch :: GetBranch -> TestTree
requestGetBranch =
  req
    "GetBranch"
    "fixture/GetBranch.yaml"

requestGetComment :: GetComment -> TestTree
requestGetComment =
  req
    "GetComment"
    "fixture/GetComment.yaml"

requestGetCommentReactions :: GetCommentReactions -> TestTree
requestGetCommentReactions =
  req
    "GetCommentReactions"
    "fixture/GetCommentReactions.yaml"

requestGetCommentsForComparedCommit :: GetCommentsForComparedCommit -> TestTree
requestGetCommentsForComparedCommit =
  req
    "GetCommentsForComparedCommit"
    "fixture/GetCommentsForComparedCommit.yaml"

requestGetCommentsForPullRequest :: GetCommentsForPullRequest -> TestTree
requestGetCommentsForPullRequest =
  req
    "GetCommentsForPullRequest"
    "fixture/GetCommentsForPullRequest.yaml"

requestGetCommit :: GetCommit -> TestTree
requestGetCommit =
  req
    "GetCommit"
    "fixture/GetCommit.yaml"

requestGetDifferences :: GetDifferences -> TestTree
requestGetDifferences =
  req
    "GetDifferences"
    "fixture/GetDifferences.yaml"

requestGetFile :: GetFile -> TestTree
requestGetFile =
  req
    "GetFile"
    "fixture/GetFile.yaml"

requestGetFolder :: GetFolder -> TestTree
requestGetFolder =
  req
    "GetFolder"
    "fixture/GetFolder.yaml"

requestGetMergeCommit :: GetMergeCommit -> TestTree
requestGetMergeCommit =
  req
    "GetMergeCommit"
    "fixture/GetMergeCommit.yaml"

requestGetMergeConflicts :: GetMergeConflicts -> TestTree
requestGetMergeConflicts =
  req
    "GetMergeConflicts"
    "fixture/GetMergeConflicts.yaml"

requestGetMergeOptions :: GetMergeOptions -> TestTree
requestGetMergeOptions =
  req
    "GetMergeOptions"
    "fixture/GetMergeOptions.yaml"

requestGetPullRequest :: GetPullRequest -> TestTree
requestGetPullRequest =
  req
    "GetPullRequest"
    "fixture/GetPullRequest.yaml"

requestGetPullRequestApprovalStates :: GetPullRequestApprovalStates -> TestTree
requestGetPullRequestApprovalStates =
  req
    "GetPullRequestApprovalStates"
    "fixture/GetPullRequestApprovalStates.yaml"

requestGetPullRequestOverrideState :: GetPullRequestOverrideState -> TestTree
requestGetPullRequestOverrideState =
  req
    "GetPullRequestOverrideState"
    "fixture/GetPullRequestOverrideState.yaml"

requestGetRepository :: GetRepository -> TestTree
requestGetRepository =
  req
    "GetRepository"
    "fixture/GetRepository.yaml"

requestGetRepositoryTriggers :: GetRepositoryTriggers -> TestTree
requestGetRepositoryTriggers =
  req
    "GetRepositoryTriggers"
    "fixture/GetRepositoryTriggers.yaml"

requestListApprovalRuleTemplates :: ListApprovalRuleTemplates -> TestTree
requestListApprovalRuleTemplates =
  req
    "ListApprovalRuleTemplates"
    "fixture/ListApprovalRuleTemplates.yaml"

requestListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepository -> TestTree
requestListAssociatedApprovalRuleTemplatesForRepository =
  req
    "ListAssociatedApprovalRuleTemplatesForRepository"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepository.yaml"

requestListBranches :: ListBranches -> TestTree
requestListBranches =
  req
    "ListBranches"
    "fixture/ListBranches.yaml"

requestListPullRequests :: ListPullRequests -> TestTree
requestListPullRequests =
  req
    "ListPullRequests"
    "fixture/ListPullRequests.yaml"

requestListRepositories :: ListRepositories -> TestTree
requestListRepositories =
  req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

requestListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplate -> TestTree
requestListRepositoriesForApprovalRuleTemplate =
  req
    "ListRepositoriesForApprovalRuleTemplate"
    "fixture/ListRepositoriesForApprovalRuleTemplate.yaml"

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

requestMergeBranchesBySquash :: MergeBranchesBySquash -> TestTree
requestMergeBranchesBySquash =
  req
    "MergeBranchesBySquash"
    "fixture/MergeBranchesBySquash.yaml"

requestMergeBranchesByThreeWay :: MergeBranchesByThreeWay -> TestTree
requestMergeBranchesByThreeWay =
  req
    "MergeBranchesByThreeWay"
    "fixture/MergeBranchesByThreeWay.yaml"

requestMergePullRequestByFastForward :: MergePullRequestByFastForward -> TestTree
requestMergePullRequestByFastForward =
  req
    "MergePullRequestByFastForward"
    "fixture/MergePullRequestByFastForward.yaml"

requestMergePullRequestBySquash :: MergePullRequestBySquash -> TestTree
requestMergePullRequestBySquash =
  req
    "MergePullRequestBySquash"
    "fixture/MergePullRequestBySquash.yaml"

requestMergePullRequestByThreeWay :: MergePullRequestByThreeWay -> TestTree
requestMergePullRequestByThreeWay =
  req
    "MergePullRequestByThreeWay"
    "fixture/MergePullRequestByThreeWay.yaml"

requestOverridePullRequestApprovalRules :: OverridePullRequestApprovalRules -> TestTree
requestOverridePullRequestApprovalRules =
  req
    "OverridePullRequestApprovalRules"
    "fixture/OverridePullRequestApprovalRules.yaml"

requestPostCommentForComparedCommit :: PostCommentForComparedCommit -> TestTree
requestPostCommentForComparedCommit =
  req
    "PostCommentForComparedCommit"
    "fixture/PostCommentForComparedCommit.yaml"

requestPostCommentForPullRequest :: PostCommentForPullRequest -> TestTree
requestPostCommentForPullRequest =
  req
    "PostCommentForPullRequest"
    "fixture/PostCommentForPullRequest.yaml"

requestPostCommentReply :: PostCommentReply -> TestTree
requestPostCommentReply =
  req
    "PostCommentReply"
    "fixture/PostCommentReply.yaml"

requestPutCommentReaction :: PutCommentReaction -> TestTree
requestPutCommentReaction =
  req
    "PutCommentReaction"
    "fixture/PutCommentReaction.yaml"

requestPutFile :: PutFile -> TestTree
requestPutFile =
  req
    "PutFile"
    "fixture/PutFile.yaml"

requestPutRepositoryTriggers :: PutRepositoryTriggers -> TestTree
requestPutRepositoryTriggers =
  req
    "PutRepositoryTriggers"
    "fixture/PutRepositoryTriggers.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestRepositoryTriggers :: TestRepositoryTriggers -> TestTree
requestTestRepositoryTriggers =
  req
    "TestRepositoryTriggers"
    "fixture/TestRepositoryTriggers.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContent -> TestTree
requestUpdateApprovalRuleTemplateContent =
  req
    "UpdateApprovalRuleTemplateContent"
    "fixture/UpdateApprovalRuleTemplateContent.yaml"

requestUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescription -> TestTree
requestUpdateApprovalRuleTemplateDescription =
  req
    "UpdateApprovalRuleTemplateDescription"
    "fixture/UpdateApprovalRuleTemplateDescription.yaml"

requestUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateName -> TestTree
requestUpdateApprovalRuleTemplateName =
  req
    "UpdateApprovalRuleTemplateName"
    "fixture/UpdateApprovalRuleTemplateName.yaml"

requestUpdateComment :: UpdateComment -> TestTree
requestUpdateComment =
  req
    "UpdateComment"
    "fixture/UpdateComment.yaml"

requestUpdateDefaultBranch :: UpdateDefaultBranch -> TestTree
requestUpdateDefaultBranch =
  req
    "UpdateDefaultBranch"
    "fixture/UpdateDefaultBranch.yaml"

requestUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContent -> TestTree
requestUpdatePullRequestApprovalRuleContent =
  req
    "UpdatePullRequestApprovalRuleContent"
    "fixture/UpdatePullRequestApprovalRuleContent.yaml"

requestUpdatePullRequestApprovalState :: UpdatePullRequestApprovalState -> TestTree
requestUpdatePullRequestApprovalState =
  req
    "UpdatePullRequestApprovalState"
    "fixture/UpdatePullRequestApprovalState.yaml"

requestUpdatePullRequestDescription :: UpdatePullRequestDescription -> TestTree
requestUpdatePullRequestDescription =
  req
    "UpdatePullRequestDescription"
    "fixture/UpdatePullRequestDescription.yaml"

requestUpdatePullRequestStatus :: UpdatePullRequestStatus -> TestTree
requestUpdatePullRequestStatus =
  req
    "UpdatePullRequestStatus"
    "fixture/UpdatePullRequestStatus.yaml"

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

requestUpdateRepositoryName :: UpdateRepositoryName -> TestTree
requestUpdateRepositoryName =
  req
    "UpdateRepositoryName"
    "fixture/UpdateRepositoryName.yaml"

-- Responses

responseAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepositoryResponse -> TestTree
responseAssociateApprovalRuleTemplateWithRepository =
  res
    "AssociateApprovalRuleTemplateWithRepositoryResponse"
    "fixture/AssociateApprovalRuleTemplateWithRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateApprovalRuleTemplateWithRepository)

responseBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> TestTree
responseBatchAssociateApprovalRuleTemplateWithRepositories =
  res
    "BatchAssociateApprovalRuleTemplateWithRepositoriesResponse"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateApprovalRuleTemplateWithRepositories)

responseBatchDescribeMergeConflicts :: BatchDescribeMergeConflictsResponse -> TestTree
responseBatchDescribeMergeConflicts =
  res
    "BatchDescribeMergeConflictsResponse"
    "fixture/BatchDescribeMergeConflictsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDescribeMergeConflicts)

responseBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> TestTree
responseBatchDisassociateApprovalRuleTemplateFromRepositories =
  res
    "BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateApprovalRuleTemplateFromRepositories)

responseBatchGetCommits :: BatchGetCommitsResponse -> TestTree
responseBatchGetCommits =
  res
    "BatchGetCommitsResponse"
    "fixture/BatchGetCommitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCommits)

responseBatchGetRepositories :: BatchGetRepositoriesResponse -> TestTree
responseBatchGetRepositories =
  res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetRepositories)

responseCreateApprovalRuleTemplate :: CreateApprovalRuleTemplateResponse -> TestTree
responseCreateApprovalRuleTemplate =
  res
    "CreateApprovalRuleTemplateResponse"
    "fixture/CreateApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApprovalRuleTemplate)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch =
  res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBranch)

responseCreateCommit :: CreateCommitResponse -> TestTree
responseCreateCommit =
  res
    "CreateCommitResponse"
    "fixture/CreateCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCommit)

responseCreatePullRequest :: CreatePullRequestResponse -> TestTree
responseCreatePullRequest =
  res
    "CreatePullRequestResponse"
    "fixture/CreatePullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePullRequest)

responseCreatePullRequestApprovalRule :: CreatePullRequestApprovalRuleResponse -> TestTree
responseCreatePullRequestApprovalRule =
  res
    "CreatePullRequestApprovalRuleResponse"
    "fixture/CreatePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePullRequestApprovalRule)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

responseCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommitResponse -> TestTree
responseCreateUnreferencedMergeCommit =
  res
    "CreateUnreferencedMergeCommitResponse"
    "fixture/CreateUnreferencedMergeCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUnreferencedMergeCommit)

responseDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplateResponse -> TestTree
responseDeleteApprovalRuleTemplate =
  res
    "DeleteApprovalRuleTemplateResponse"
    "fixture/DeleteApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApprovalRuleTemplate)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBranch)

responseDeleteCommentContent :: DeleteCommentContentResponse -> TestTree
responseDeleteCommentContent =
  res
    "DeleteCommentContentResponse"
    "fixture/DeleteCommentContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCommentContent)

responseDeleteFile :: DeleteFileResponse -> TestTree
responseDeleteFile =
  res
    "DeleteFileResponse"
    "fixture/DeleteFileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFile)

responseDeletePullRequestApprovalRule :: DeletePullRequestApprovalRuleResponse -> TestTree
responseDeletePullRequestApprovalRule =
  res
    "DeletePullRequestApprovalRuleResponse"
    "fixture/DeletePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePullRequestApprovalRule)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responseDescribeMergeConflicts :: DescribeMergeConflictsResponse -> TestTree
responseDescribeMergeConflicts =
  res
    "DescribeMergeConflictsResponse"
    "fixture/DescribeMergeConflictsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMergeConflicts)

responseDescribePullRequestEvents :: DescribePullRequestEventsResponse -> TestTree
responseDescribePullRequestEvents =
  res
    "DescribePullRequestEventsResponse"
    "fixture/DescribePullRequestEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePullRequestEvents)

responseDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepositoryResponse -> TestTree
responseDisassociateApprovalRuleTemplateFromRepository =
  res
    "DisassociateApprovalRuleTemplateFromRepositoryResponse"
    "fixture/DisassociateApprovalRuleTemplateFromRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateApprovalRuleTemplateFromRepository)

responseEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRulesResponse -> TestTree
responseEvaluatePullRequestApprovalRules =
  res
    "EvaluatePullRequestApprovalRulesResponse"
    "fixture/EvaluatePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EvaluatePullRequestApprovalRules)

responseGetApprovalRuleTemplate :: GetApprovalRuleTemplateResponse -> TestTree
responseGetApprovalRuleTemplate =
  res
    "GetApprovalRuleTemplateResponse"
    "fixture/GetApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApprovalRuleTemplate)

responseGetBlob :: GetBlobResponse -> TestTree
responseGetBlob =
  res
    "GetBlobResponse"
    "fixture/GetBlobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlob)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch =
  res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBranch)

responseGetComment :: GetCommentResponse -> TestTree
responseGetComment =
  res
    "GetCommentResponse"
    "fixture/GetCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComment)

responseGetCommentReactions :: GetCommentReactionsResponse -> TestTree
responseGetCommentReactions =
  res
    "GetCommentReactionsResponse"
    "fixture/GetCommentReactionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommentReactions)

responseGetCommentsForComparedCommit :: GetCommentsForComparedCommitResponse -> TestTree
responseGetCommentsForComparedCommit =
  res
    "GetCommentsForComparedCommitResponse"
    "fixture/GetCommentsForComparedCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommentsForComparedCommit)

responseGetCommentsForPullRequest :: GetCommentsForPullRequestResponse -> TestTree
responseGetCommentsForPullRequest =
  res
    "GetCommentsForPullRequestResponse"
    "fixture/GetCommentsForPullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommentsForPullRequest)

responseGetCommit :: GetCommitResponse -> TestTree
responseGetCommit =
  res
    "GetCommitResponse"
    "fixture/GetCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommit)

responseGetDifferences :: GetDifferencesResponse -> TestTree
responseGetDifferences =
  res
    "GetDifferencesResponse"
    "fixture/GetDifferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDifferences)

responseGetFile :: GetFileResponse -> TestTree
responseGetFile =
  res
    "GetFileResponse"
    "fixture/GetFileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFile)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFolder)

responseGetMergeCommit :: GetMergeCommitResponse -> TestTree
responseGetMergeCommit =
  res
    "GetMergeCommitResponse"
    "fixture/GetMergeCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMergeCommit)

responseGetMergeConflicts :: GetMergeConflictsResponse -> TestTree
responseGetMergeConflicts =
  res
    "GetMergeConflictsResponse"
    "fixture/GetMergeConflictsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMergeConflicts)

responseGetMergeOptions :: GetMergeOptionsResponse -> TestTree
responseGetMergeOptions =
  res
    "GetMergeOptionsResponse"
    "fixture/GetMergeOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMergeOptions)

responseGetPullRequest :: GetPullRequestResponse -> TestTree
responseGetPullRequest =
  res
    "GetPullRequestResponse"
    "fixture/GetPullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPullRequest)

responseGetPullRequestApprovalStates :: GetPullRequestApprovalStatesResponse -> TestTree
responseGetPullRequestApprovalStates =
  res
    "GetPullRequestApprovalStatesResponse"
    "fixture/GetPullRequestApprovalStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPullRequestApprovalStates)

responseGetPullRequestOverrideState :: GetPullRequestOverrideStateResponse -> TestTree
responseGetPullRequestOverrideState =
  res
    "GetPullRequestOverrideStateResponse"
    "fixture/GetPullRequestOverrideStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPullRequestOverrideState)

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository =
  res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepository)

responseGetRepositoryTriggers :: GetRepositoryTriggersResponse -> TestTree
responseGetRepositoryTriggers =
  res
    "GetRepositoryTriggersResponse"
    "fixture/GetRepositoryTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryTriggers)

responseListApprovalRuleTemplates :: ListApprovalRuleTemplatesResponse -> TestTree
responseListApprovalRuleTemplates =
  res
    "ListApprovalRuleTemplatesResponse"
    "fixture/ListApprovalRuleTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApprovalRuleTemplates)

responseListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> TestTree
responseListAssociatedApprovalRuleTemplatesForRepository =
  res
    "ListAssociatedApprovalRuleTemplatesForRepositoryResponse"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedApprovalRuleTemplatesForRepository)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBranches)

responseListPullRequests :: ListPullRequestsResponse -> TestTree
responseListPullRequests =
  res
    "ListPullRequestsResponse"
    "fixture/ListPullRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPullRequests)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositories)

responseListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplateResponse -> TestTree
responseListRepositoriesForApprovalRuleTemplate =
  res
    "ListRepositoriesForApprovalRuleTemplateResponse"
    "fixture/ListRepositoriesForApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositoriesForApprovalRuleTemplate)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseMergeBranchesByFastForward :: MergeBranchesByFastForwardResponse -> TestTree
responseMergeBranchesByFastForward =
  res
    "MergeBranchesByFastForwardResponse"
    "fixture/MergeBranchesByFastForwardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeBranchesByFastForward)

responseMergeBranchesBySquash :: MergeBranchesBySquashResponse -> TestTree
responseMergeBranchesBySquash =
  res
    "MergeBranchesBySquashResponse"
    "fixture/MergeBranchesBySquashResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeBranchesBySquash)

responseMergeBranchesByThreeWay :: MergeBranchesByThreeWayResponse -> TestTree
responseMergeBranchesByThreeWay =
  res
    "MergeBranchesByThreeWayResponse"
    "fixture/MergeBranchesByThreeWayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeBranchesByThreeWay)

responseMergePullRequestByFastForward :: MergePullRequestByFastForwardResponse -> TestTree
responseMergePullRequestByFastForward =
  res
    "MergePullRequestByFastForwardResponse"
    "fixture/MergePullRequestByFastForwardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergePullRequestByFastForward)

responseMergePullRequestBySquash :: MergePullRequestBySquashResponse -> TestTree
responseMergePullRequestBySquash =
  res
    "MergePullRequestBySquashResponse"
    "fixture/MergePullRequestBySquashResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergePullRequestBySquash)

responseMergePullRequestByThreeWay :: MergePullRequestByThreeWayResponse -> TestTree
responseMergePullRequestByThreeWay =
  res
    "MergePullRequestByThreeWayResponse"
    "fixture/MergePullRequestByThreeWayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergePullRequestByThreeWay)

responseOverridePullRequestApprovalRules :: OverridePullRequestApprovalRulesResponse -> TestTree
responseOverridePullRequestApprovalRules =
  res
    "OverridePullRequestApprovalRulesResponse"
    "fixture/OverridePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OverridePullRequestApprovalRules)

responsePostCommentForComparedCommit :: PostCommentForComparedCommitResponse -> TestTree
responsePostCommentForComparedCommit =
  res
    "PostCommentForComparedCommitResponse"
    "fixture/PostCommentForComparedCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostCommentForComparedCommit)

responsePostCommentForPullRequest :: PostCommentForPullRequestResponse -> TestTree
responsePostCommentForPullRequest =
  res
    "PostCommentForPullRequestResponse"
    "fixture/PostCommentForPullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostCommentForPullRequest)

responsePostCommentReply :: PostCommentReplyResponse -> TestTree
responsePostCommentReply =
  res
    "PostCommentReplyResponse"
    "fixture/PostCommentReplyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostCommentReply)

responsePutCommentReaction :: PutCommentReactionResponse -> TestTree
responsePutCommentReaction =
  res
    "PutCommentReactionResponse"
    "fixture/PutCommentReactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutCommentReaction)

responsePutFile :: PutFileResponse -> TestTree
responsePutFile =
  res
    "PutFileResponse"
    "fixture/PutFileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFile)

responsePutRepositoryTriggers :: PutRepositoryTriggersResponse -> TestTree
responsePutRepositoryTriggers =
  res
    "PutRepositoryTriggersResponse"
    "fixture/PutRepositoryTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRepositoryTriggers)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers =
  res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestRepositoryTriggers)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContentResponse -> TestTree
responseUpdateApprovalRuleTemplateContent =
  res
    "UpdateApprovalRuleTemplateContentResponse"
    "fixture/UpdateApprovalRuleTemplateContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApprovalRuleTemplateContent)

responseUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescriptionResponse -> TestTree
responseUpdateApprovalRuleTemplateDescription =
  res
    "UpdateApprovalRuleTemplateDescriptionResponse"
    "fixture/UpdateApprovalRuleTemplateDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApprovalRuleTemplateDescription)

responseUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateNameResponse -> TestTree
responseUpdateApprovalRuleTemplateName =
  res
    "UpdateApprovalRuleTemplateNameResponse"
    "fixture/UpdateApprovalRuleTemplateNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApprovalRuleTemplateName)

responseUpdateComment :: UpdateCommentResponse -> TestTree
responseUpdateComment =
  res
    "UpdateCommentResponse"
    "fixture/UpdateCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComment)

responseUpdateDefaultBranch :: UpdateDefaultBranchResponse -> TestTree
responseUpdateDefaultBranch =
  res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDefaultBranch)

responseUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContentResponse -> TestTree
responseUpdatePullRequestApprovalRuleContent =
  res
    "UpdatePullRequestApprovalRuleContentResponse"
    "fixture/UpdatePullRequestApprovalRuleContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestApprovalRuleContent)

responseUpdatePullRequestApprovalState :: UpdatePullRequestApprovalStateResponse -> TestTree
responseUpdatePullRequestApprovalState =
  res
    "UpdatePullRequestApprovalStateResponse"
    "fixture/UpdatePullRequestApprovalStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestApprovalState)

responseUpdatePullRequestDescription :: UpdatePullRequestDescriptionResponse -> TestTree
responseUpdatePullRequestDescription =
  res
    "UpdatePullRequestDescriptionResponse"
    "fixture/UpdatePullRequestDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestDescription)

responseUpdatePullRequestStatus :: UpdatePullRequestStatusResponse -> TestTree
responseUpdatePullRequestStatus =
  res
    "UpdatePullRequestStatusResponse"
    "fixture/UpdatePullRequestStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestStatus)

responseUpdatePullRequestTitle :: UpdatePullRequestTitleResponse -> TestTree
responseUpdatePullRequestTitle =
  res
    "UpdatePullRequestTitleResponse"
    "fixture/UpdatePullRequestTitleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestTitle)

responseUpdateRepositoryDescription :: UpdateRepositoryDescriptionResponse -> TestTree
responseUpdateRepositoryDescription =
  res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRepositoryDescription)

responseUpdateRepositoryName :: UpdateRepositoryNameResponse -> TestTree
responseUpdateRepositoryName =
  res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRepositoryName)

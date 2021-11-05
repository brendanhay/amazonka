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

import Amazonka.CodeCommit
import qualified Data.Proxy as Proxy
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
--             newMergePullRequestByFastForward
--
--         , requestUpdateRepositoryName $
--             newUpdateRepositoryName
--
--         , requestPostCommentForPullRequest $
--             newPostCommentForPullRequest
--
--         , requestMergeBranchesBySquash $
--             newMergeBranchesBySquash
--
--         , requestGetCommit $
--             newGetCommit
--
--         , requestBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositories
--
--         , requestGetCommentReactions $
--             newGetCommentReactions
--
--         , requestGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplate
--
--         , requestDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepository
--
--         , requestGetBranch $
--             newGetBranch
--
--         , requestGetDifferences $
--             newGetDifferences
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetPullRequest $
--             newGetPullRequest
--
--         , requestOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRules
--
--         , requestListPullRequests $
--             newListPullRequests
--
--         , requestCreateCommit $
--             newCreateCommit
--
--         , requestUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalState
--
--         , requestEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRules
--
--         , requestGetComment $
--             newGetComment
--
--         , requestCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplate
--
--         , requestDeleteBranch $
--             newDeleteBranch
--
--         , requestUpdateRepositoryDescription $
--             newUpdateRepositoryDescription
--
--         , requestCreateBranch $
--             newCreateBranch
--
--         , requestGetFolder $
--             newGetFolder
--
--         , requestCreatePullRequest $
--             newCreatePullRequest
--
--         , requestDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplate
--
--         , requestListBranches $
--             newListBranches
--
--         , requestBatchGetCommits $
--             newBatchGetCommits
--
--         , requestPutCommentReaction $
--             newPutCommentReaction
--
--         , requestUpdatePullRequestDescription $
--             newUpdatePullRequestDescription
--
--         , requestListRepositories $
--             newListRepositories
--
--         , requestCreateRepository $
--             newCreateRepository
--
--         , requestUpdateDefaultBranch $
--             newUpdateDefaultBranch
--
--         , requestGetMergeOptions $
--             newGetMergeOptions
--
--         , requestCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRule
--
--         , requestPostCommentReply $
--             newPostCommentReply
--
--         , requestUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContent
--
--         , requestCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommit
--
--         , requestListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplate
--
--         , requestGetRepository $
--             newGetRepository
--
--         , requestBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflicts
--
--         , requestDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRule
--
--         , requestGetRepositoryTriggers $
--             newGetRepositoryTriggers
--
--         , requestUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateName
--
--         , requestPutFile $
--             newPutFile
--
--         , requestDeleteFile $
--             newDeleteFile
--
--         , requestGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommit
--
--         , requestGetMergeCommit $
--             newGetMergeCommit
--
--         , requestTestRepositoryTriggers $
--             newTestRepositoryTriggers
--
--         , requestMergePullRequestBySquash $
--             newMergePullRequestBySquash
--
--         , requestUpdateComment $
--             newUpdateComment
--
--         , requestPostCommentForComparedCommit $
--             newPostCommentForComparedCommit
--
--         , requestMergeBranchesByFastForward $
--             newMergeBranchesByFastForward
--
--         , requestUpdatePullRequestTitle $
--             newUpdatePullRequestTitle
--
--         , requestBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositories
--
--         , requestUpdatePullRequestApprovalRuleContent $
--             newUpdatePullRequestApprovalRuleContent
--
--         , requestGetBlob $
--             newGetBlob
--
--         , requestAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepository
--
--         , requestPutRepositoryTriggers $
--             newPutRepositoryTriggers
--
--         , requestListApprovalRuleTemplates $
--             newListApprovalRuleTemplates
--
--         , requestDescribeMergeConflicts $
--             newDescribeMergeConflicts
--
--         , requestTagResource $
--             newTagResource
--
--         , requestMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWay
--
--         , requestGetFile $
--             newGetFile
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetMergeConflicts $
--             newGetMergeConflicts
--
--         , requestDeleteRepository $
--             newDeleteRepository
--
--         , requestDeleteCommentContent $
--             newDeleteCommentContent
--
--         , requestMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWay
--
--         , requestDescribePullRequestEvents $
--             newDescribePullRequestEvents
--
--         , requestBatchGetRepositories $
--             newBatchGetRepositories
--
--         , requestUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescription
--
--         , requestGetPullRequestOverrideState $
--             newGetPullRequestOverrideState
--
--         , requestGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStates
--
--         , requestGetCommentsForPullRequest $
--             newGetCommentsForPullRequest
--
--         , requestUpdatePullRequestStatus $
--             newUpdatePullRequestStatus
--
--         , requestListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepository
--
--           ]

--     , testGroup "response"
--         [ responseMergePullRequestByFastForward $
--             newMergePullRequestByFastForwardResponse
--
--         , responseUpdateRepositoryName $
--             newUpdateRepositoryNameResponse
--
--         , responsePostCommentForPullRequest $
--             newPostCommentForPullRequestResponse
--
--         , responseMergeBranchesBySquash $
--             newMergeBranchesBySquashResponse
--
--         , responseGetCommit $
--             newGetCommitResponse
--
--         , responseBatchAssociateApprovalRuleTemplateWithRepositories $
--             newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
--
--         , responseGetCommentReactions $
--             newGetCommentReactionsResponse
--
--         , responseGetApprovalRuleTemplate $
--             newGetApprovalRuleTemplateResponse
--
--         , responseDisassociateApprovalRuleTemplateFromRepository $
--             newDisassociateApprovalRuleTemplateFromRepositoryResponse
--
--         , responseGetBranch $
--             newGetBranchResponse
--
--         , responseGetDifferences $
--             newGetDifferencesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetPullRequest $
--             newGetPullRequestResponse
--
--         , responseOverridePullRequestApprovalRules $
--             newOverridePullRequestApprovalRulesResponse
--
--         , responseListPullRequests $
--             newListPullRequestsResponse
--
--         , responseCreateCommit $
--             newCreateCommitResponse
--
--         , responseUpdatePullRequestApprovalState $
--             newUpdatePullRequestApprovalStateResponse
--
--         , responseEvaluatePullRequestApprovalRules $
--             newEvaluatePullRequestApprovalRulesResponse
--
--         , responseGetComment $
--             newGetCommentResponse
--
--         , responseCreateApprovalRuleTemplate $
--             newCreateApprovalRuleTemplateResponse
--
--         , responseDeleteBranch $
--             newDeleteBranchResponse
--
--         , responseUpdateRepositoryDescription $
--             newUpdateRepositoryDescriptionResponse
--
--         , responseCreateBranch $
--             newCreateBranchResponse
--
--         , responseGetFolder $
--             newGetFolderResponse
--
--         , responseCreatePullRequest $
--             newCreatePullRequestResponse
--
--         , responseDeleteApprovalRuleTemplate $
--             newDeleteApprovalRuleTemplateResponse
--
--         , responseListBranches $
--             newListBranchesResponse
--
--         , responseBatchGetCommits $
--             newBatchGetCommitsResponse
--
--         , responsePutCommentReaction $
--             newPutCommentReactionResponse
--
--         , responseUpdatePullRequestDescription $
--             newUpdatePullRequestDescriptionResponse
--
--         , responseListRepositories $
--             newListRepositoriesResponse
--
--         , responseCreateRepository $
--             newCreateRepositoryResponse
--
--         , responseUpdateDefaultBranch $
--             newUpdateDefaultBranchResponse
--
--         , responseGetMergeOptions $
--             newGetMergeOptionsResponse
--
--         , responseCreatePullRequestApprovalRule $
--             newCreatePullRequestApprovalRuleResponse
--
--         , responsePostCommentReply $
--             newPostCommentReplyResponse
--
--         , responseUpdateApprovalRuleTemplateContent $
--             newUpdateApprovalRuleTemplateContentResponse
--
--         , responseCreateUnreferencedMergeCommit $
--             newCreateUnreferencedMergeCommitResponse
--
--         , responseListRepositoriesForApprovalRuleTemplate $
--             newListRepositoriesForApprovalRuleTemplateResponse
--
--         , responseGetRepository $
--             newGetRepositoryResponse
--
--         , responseBatchDescribeMergeConflicts $
--             newBatchDescribeMergeConflictsResponse
--
--         , responseDeletePullRequestApprovalRule $
--             newDeletePullRequestApprovalRuleResponse
--
--         , responseGetRepositoryTriggers $
--             newGetRepositoryTriggersResponse
--
--         , responseUpdateApprovalRuleTemplateName $
--             newUpdateApprovalRuleTemplateNameResponse
--
--         , responsePutFile $
--             newPutFileResponse
--
--         , responseDeleteFile $
--             newDeleteFileResponse
--
--         , responseGetCommentsForComparedCommit $
--             newGetCommentsForComparedCommitResponse
--
--         , responseGetMergeCommit $
--             newGetMergeCommitResponse
--
--         , responseTestRepositoryTriggers $
--             newTestRepositoryTriggersResponse
--
--         , responseMergePullRequestBySquash $
--             newMergePullRequestBySquashResponse
--
--         , responseUpdateComment $
--             newUpdateCommentResponse
--
--         , responsePostCommentForComparedCommit $
--             newPostCommentForComparedCommitResponse
--
--         , responseMergeBranchesByFastForward $
--             newMergeBranchesByFastForwardResponse
--
--         , responseUpdatePullRequestTitle $
--             newUpdatePullRequestTitleResponse
--
--         , responseBatchDisassociateApprovalRuleTemplateFromRepositories $
--             newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
--
--         , responseUpdatePullRequestApprovalRuleContent $
--             newUpdatePullRequestApprovalRuleContentResponse
--
--         , responseGetBlob $
--             newGetBlobResponse
--
--         , responseAssociateApprovalRuleTemplateWithRepository $
--             newAssociateApprovalRuleTemplateWithRepositoryResponse
--
--         , responsePutRepositoryTriggers $
--             newPutRepositoryTriggersResponse
--
--         , responseListApprovalRuleTemplates $
--             newListApprovalRuleTemplatesResponse
--
--         , responseDescribeMergeConflicts $
--             newDescribeMergeConflictsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseMergeBranchesByThreeWay $
--             newMergeBranchesByThreeWayResponse
--
--         , responseGetFile $
--             newGetFileResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetMergeConflicts $
--             newGetMergeConflictsResponse
--
--         , responseDeleteRepository $
--             newDeleteRepositoryResponse
--
--         , responseDeleteCommentContent $
--             newDeleteCommentContentResponse
--
--         , responseMergePullRequestByThreeWay $
--             newMergePullRequestByThreeWayResponse
--
--         , responseDescribePullRequestEvents $
--             newDescribePullRequestEventsResponse
--
--         , responseBatchGetRepositories $
--             newBatchGetRepositoriesResponse
--
--         , responseUpdateApprovalRuleTemplateDescription $
--             newUpdateApprovalRuleTemplateDescriptionResponse
--
--         , responseGetPullRequestOverrideState $
--             newGetPullRequestOverrideStateResponse
--
--         , responseGetPullRequestApprovalStates $
--             newGetPullRequestApprovalStatesResponse
--
--         , responseGetCommentsForPullRequest $
--             newGetCommentsForPullRequestResponse
--
--         , responseUpdatePullRequestStatus $
--             newUpdatePullRequestStatusResponse
--
--         , responseListAssociatedApprovalRuleTemplatesForRepository $
--             newListAssociatedApprovalRuleTemplatesForRepositoryResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergePullRequestByFastForward)

responseUpdateRepositoryName :: UpdateRepositoryNameResponse -> TestTree
responseUpdateRepositoryName =
  res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRepositoryName)

responsePostCommentForPullRequest :: PostCommentForPullRequestResponse -> TestTree
responsePostCommentForPullRequest =
  res
    "PostCommentForPullRequestResponse"
    "fixture/PostCommentForPullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostCommentForPullRequest)

responseMergeBranchesBySquash :: MergeBranchesBySquashResponse -> TestTree
responseMergeBranchesBySquash =
  res
    "MergeBranchesBySquashResponse"
    "fixture/MergeBranchesBySquashResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeBranchesBySquash)

responseGetCommit :: GetCommitResponse -> TestTree
responseGetCommit =
  res
    "GetCommitResponse"
    "fixture/GetCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommit)

responseBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> TestTree
responseBatchAssociateApprovalRuleTemplateWithRepositories =
  res
    "BatchAssociateApprovalRuleTemplateWithRepositoriesResponse"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateApprovalRuleTemplateWithRepositories)

responseGetCommentReactions :: GetCommentReactionsResponse -> TestTree
responseGetCommentReactions =
  res
    "GetCommentReactionsResponse"
    "fixture/GetCommentReactionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommentReactions)

responseGetApprovalRuleTemplate :: GetApprovalRuleTemplateResponse -> TestTree
responseGetApprovalRuleTemplate =
  res
    "GetApprovalRuleTemplateResponse"
    "fixture/GetApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApprovalRuleTemplate)

responseDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepositoryResponse -> TestTree
responseDisassociateApprovalRuleTemplateFromRepository =
  res
    "DisassociateApprovalRuleTemplateFromRepositoryResponse"
    "fixture/DisassociateApprovalRuleTemplateFromRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateApprovalRuleTemplateFromRepository)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch =
  res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBranch)

responseGetDifferences :: GetDifferencesResponse -> TestTree
responseGetDifferences =
  res
    "GetDifferencesResponse"
    "fixture/GetDifferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDifferences)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetPullRequest :: GetPullRequestResponse -> TestTree
responseGetPullRequest =
  res
    "GetPullRequestResponse"
    "fixture/GetPullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPullRequest)

responseOverridePullRequestApprovalRules :: OverridePullRequestApprovalRulesResponse -> TestTree
responseOverridePullRequestApprovalRules =
  res
    "OverridePullRequestApprovalRulesResponse"
    "fixture/OverridePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OverridePullRequestApprovalRules)

responseListPullRequests :: ListPullRequestsResponse -> TestTree
responseListPullRequests =
  res
    "ListPullRequestsResponse"
    "fixture/ListPullRequestsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPullRequests)

responseCreateCommit :: CreateCommitResponse -> TestTree
responseCreateCommit =
  res
    "CreateCommitResponse"
    "fixture/CreateCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCommit)

responseUpdatePullRequestApprovalState :: UpdatePullRequestApprovalStateResponse -> TestTree
responseUpdatePullRequestApprovalState =
  res
    "UpdatePullRequestApprovalStateResponse"
    "fixture/UpdatePullRequestApprovalStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestApprovalState)

responseEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRulesResponse -> TestTree
responseEvaluatePullRequestApprovalRules =
  res
    "EvaluatePullRequestApprovalRulesResponse"
    "fixture/EvaluatePullRequestApprovalRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EvaluatePullRequestApprovalRules)

responseGetComment :: GetCommentResponse -> TestTree
responseGetComment =
  res
    "GetCommentResponse"
    "fixture/GetCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComment)

responseCreateApprovalRuleTemplate :: CreateApprovalRuleTemplateResponse -> TestTree
responseCreateApprovalRuleTemplate =
  res
    "CreateApprovalRuleTemplateResponse"
    "fixture/CreateApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApprovalRuleTemplate)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBranch)

responseUpdateRepositoryDescription :: UpdateRepositoryDescriptionResponse -> TestTree
responseUpdateRepositoryDescription =
  res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRepositoryDescription)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch =
  res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBranch)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFolder)

responseCreatePullRequest :: CreatePullRequestResponse -> TestTree
responseCreatePullRequest =
  res
    "CreatePullRequestResponse"
    "fixture/CreatePullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePullRequest)

responseDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplateResponse -> TestTree
responseDeleteApprovalRuleTemplate =
  res
    "DeleteApprovalRuleTemplateResponse"
    "fixture/DeleteApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApprovalRuleTemplate)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBranches)

responseBatchGetCommits :: BatchGetCommitsResponse -> TestTree
responseBatchGetCommits =
  res
    "BatchGetCommitsResponse"
    "fixture/BatchGetCommitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetCommits)

responsePutCommentReaction :: PutCommentReactionResponse -> TestTree
responsePutCommentReaction =
  res
    "PutCommentReactionResponse"
    "fixture/PutCommentReactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutCommentReaction)

responseUpdatePullRequestDescription :: UpdatePullRequestDescriptionResponse -> TestTree
responseUpdatePullRequestDescription =
  res
    "UpdatePullRequestDescriptionResponse"
    "fixture/UpdatePullRequestDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestDescription)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositories)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRepository)

responseUpdateDefaultBranch :: UpdateDefaultBranchResponse -> TestTree
responseUpdateDefaultBranch =
  res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDefaultBranch)

responseGetMergeOptions :: GetMergeOptionsResponse -> TestTree
responseGetMergeOptions =
  res
    "GetMergeOptionsResponse"
    "fixture/GetMergeOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMergeOptions)

responseCreatePullRequestApprovalRule :: CreatePullRequestApprovalRuleResponse -> TestTree
responseCreatePullRequestApprovalRule =
  res
    "CreatePullRequestApprovalRuleResponse"
    "fixture/CreatePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePullRequestApprovalRule)

responsePostCommentReply :: PostCommentReplyResponse -> TestTree
responsePostCommentReply =
  res
    "PostCommentReplyResponse"
    "fixture/PostCommentReplyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostCommentReply)

responseUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContentResponse -> TestTree
responseUpdateApprovalRuleTemplateContent =
  res
    "UpdateApprovalRuleTemplateContentResponse"
    "fixture/UpdateApprovalRuleTemplateContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApprovalRuleTemplateContent)

responseCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommitResponse -> TestTree
responseCreateUnreferencedMergeCommit =
  res
    "CreateUnreferencedMergeCommitResponse"
    "fixture/CreateUnreferencedMergeCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUnreferencedMergeCommit)

responseListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplateResponse -> TestTree
responseListRepositoriesForApprovalRuleTemplate =
  res
    "ListRepositoriesForApprovalRuleTemplateResponse"
    "fixture/ListRepositoriesForApprovalRuleTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRepositoriesForApprovalRuleTemplate)

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository =
  res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepository)

responseBatchDescribeMergeConflicts :: BatchDescribeMergeConflictsResponse -> TestTree
responseBatchDescribeMergeConflicts =
  res
    "BatchDescribeMergeConflictsResponse"
    "fixture/BatchDescribeMergeConflictsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDescribeMergeConflicts)

responseDeletePullRequestApprovalRule :: DeletePullRequestApprovalRuleResponse -> TestTree
responseDeletePullRequestApprovalRule =
  res
    "DeletePullRequestApprovalRuleResponse"
    "fixture/DeletePullRequestApprovalRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePullRequestApprovalRule)

responseGetRepositoryTriggers :: GetRepositoryTriggersResponse -> TestTree
responseGetRepositoryTriggers =
  res
    "GetRepositoryTriggersResponse"
    "fixture/GetRepositoryTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRepositoryTriggers)

responseUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateNameResponse -> TestTree
responseUpdateApprovalRuleTemplateName =
  res
    "UpdateApprovalRuleTemplateNameResponse"
    "fixture/UpdateApprovalRuleTemplateNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApprovalRuleTemplateName)

responsePutFile :: PutFileResponse -> TestTree
responsePutFile =
  res
    "PutFileResponse"
    "fixture/PutFileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutFile)

responseDeleteFile :: DeleteFileResponse -> TestTree
responseDeleteFile =
  res
    "DeleteFileResponse"
    "fixture/DeleteFileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFile)

responseGetCommentsForComparedCommit :: GetCommentsForComparedCommitResponse -> TestTree
responseGetCommentsForComparedCommit =
  res
    "GetCommentsForComparedCommitResponse"
    "fixture/GetCommentsForComparedCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommentsForComparedCommit)

responseGetMergeCommit :: GetMergeCommitResponse -> TestTree
responseGetMergeCommit =
  res
    "GetMergeCommitResponse"
    "fixture/GetMergeCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMergeCommit)

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers =
  res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestRepositoryTriggers)

responseMergePullRequestBySquash :: MergePullRequestBySquashResponse -> TestTree
responseMergePullRequestBySquash =
  res
    "MergePullRequestBySquashResponse"
    "fixture/MergePullRequestBySquashResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergePullRequestBySquash)

responseUpdateComment :: UpdateCommentResponse -> TestTree
responseUpdateComment =
  res
    "UpdateCommentResponse"
    "fixture/UpdateCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateComment)

responsePostCommentForComparedCommit :: PostCommentForComparedCommitResponse -> TestTree
responsePostCommentForComparedCommit =
  res
    "PostCommentForComparedCommitResponse"
    "fixture/PostCommentForComparedCommitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PostCommentForComparedCommit)

responseMergeBranchesByFastForward :: MergeBranchesByFastForwardResponse -> TestTree
responseMergeBranchesByFastForward =
  res
    "MergeBranchesByFastForwardResponse"
    "fixture/MergeBranchesByFastForwardResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeBranchesByFastForward)

responseUpdatePullRequestTitle :: UpdatePullRequestTitleResponse -> TestTree
responseUpdatePullRequestTitle =
  res
    "UpdatePullRequestTitleResponse"
    "fixture/UpdatePullRequestTitleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestTitle)

responseBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> TestTree
responseBatchDisassociateApprovalRuleTemplateFromRepositories =
  res
    "BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateApprovalRuleTemplateFromRepositories)

responseUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContentResponse -> TestTree
responseUpdatePullRequestApprovalRuleContent =
  res
    "UpdatePullRequestApprovalRuleContentResponse"
    "fixture/UpdatePullRequestApprovalRuleContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestApprovalRuleContent)

responseGetBlob :: GetBlobResponse -> TestTree
responseGetBlob =
  res
    "GetBlobResponse"
    "fixture/GetBlobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlob)

responseAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepositoryResponse -> TestTree
responseAssociateApprovalRuleTemplateWithRepository =
  res
    "AssociateApprovalRuleTemplateWithRepositoryResponse"
    "fixture/AssociateApprovalRuleTemplateWithRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateApprovalRuleTemplateWithRepository)

responsePutRepositoryTriggers :: PutRepositoryTriggersResponse -> TestTree
responsePutRepositoryTriggers =
  res
    "PutRepositoryTriggersResponse"
    "fixture/PutRepositoryTriggersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRepositoryTriggers)

responseListApprovalRuleTemplates :: ListApprovalRuleTemplatesResponse -> TestTree
responseListApprovalRuleTemplates =
  res
    "ListApprovalRuleTemplatesResponse"
    "fixture/ListApprovalRuleTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApprovalRuleTemplates)

responseDescribeMergeConflicts :: DescribeMergeConflictsResponse -> TestTree
responseDescribeMergeConflicts =
  res
    "DescribeMergeConflictsResponse"
    "fixture/DescribeMergeConflictsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMergeConflicts)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseMergeBranchesByThreeWay :: MergeBranchesByThreeWayResponse -> TestTree
responseMergeBranchesByThreeWay =
  res
    "MergeBranchesByThreeWayResponse"
    "fixture/MergeBranchesByThreeWayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeBranchesByThreeWay)

responseGetFile :: GetFileResponse -> TestTree
responseGetFile =
  res
    "GetFileResponse"
    "fixture/GetFileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFile)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetMergeConflicts :: GetMergeConflictsResponse -> TestTree
responseGetMergeConflicts =
  res
    "GetMergeConflictsResponse"
    "fixture/GetMergeConflictsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMergeConflicts)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRepository)

responseDeleteCommentContent :: DeleteCommentContentResponse -> TestTree
responseDeleteCommentContent =
  res
    "DeleteCommentContentResponse"
    "fixture/DeleteCommentContentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCommentContent)

responseMergePullRequestByThreeWay :: MergePullRequestByThreeWayResponse -> TestTree
responseMergePullRequestByThreeWay =
  res
    "MergePullRequestByThreeWayResponse"
    "fixture/MergePullRequestByThreeWayResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergePullRequestByThreeWay)

responseDescribePullRequestEvents :: DescribePullRequestEventsResponse -> TestTree
responseDescribePullRequestEvents =
  res
    "DescribePullRequestEventsResponse"
    "fixture/DescribePullRequestEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePullRequestEvents)

responseBatchGetRepositories :: BatchGetRepositoriesResponse -> TestTree
responseBatchGetRepositories =
  res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetRepositories)

responseUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescriptionResponse -> TestTree
responseUpdateApprovalRuleTemplateDescription =
  res
    "UpdateApprovalRuleTemplateDescriptionResponse"
    "fixture/UpdateApprovalRuleTemplateDescriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApprovalRuleTemplateDescription)

responseGetPullRequestOverrideState :: GetPullRequestOverrideStateResponse -> TestTree
responseGetPullRequestOverrideState =
  res
    "GetPullRequestOverrideStateResponse"
    "fixture/GetPullRequestOverrideStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPullRequestOverrideState)

responseGetPullRequestApprovalStates :: GetPullRequestApprovalStatesResponse -> TestTree
responseGetPullRequestApprovalStates =
  res
    "GetPullRequestApprovalStatesResponse"
    "fixture/GetPullRequestApprovalStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPullRequestApprovalStates)

responseGetCommentsForPullRequest :: GetCommentsForPullRequestResponse -> TestTree
responseGetCommentsForPullRequest =
  res
    "GetCommentsForPullRequestResponse"
    "fixture/GetCommentsForPullRequestResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCommentsForPullRequest)

responseUpdatePullRequestStatus :: UpdatePullRequestStatusResponse -> TestTree
responseUpdatePullRequestStatus =
  res
    "UpdatePullRequestStatusResponse"
    "fixture/UpdatePullRequestStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePullRequestStatus)

responseListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> TestTree
responseListAssociatedApprovalRuleTemplatesForRepository =
  res
    "ListAssociatedApprovalRuleTemplatesForRepositoryResponse"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepositoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedApprovalRuleTemplatesForRepository)

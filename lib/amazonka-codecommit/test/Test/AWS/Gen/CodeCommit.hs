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
--             mkMergePullRequestByFastForward
--
--         , requestUpdateRepositoryName $
--             mkUpdateRepositoryName
--
--         , requestPostCommentForPullRequest $
--             mkPostCommentForPullRequest
--
--         , requestMergeBranchesBySquash $
--             mkMergeBranchesBySquash
--
--         , requestGetCommit $
--             mkGetCommit
--
--         , requestBatchAssociateApprovalRuleTemplateWithRepositories $
--             mkBatchAssociateApprovalRuleTemplateWithRepositories
--
--         , requestGetCommentReactions $
--             mkGetCommentReactions
--
--         , requestGetApprovalRuleTemplate $
--             mkGetApprovalRuleTemplate
--
--         , requestDisassociateApprovalRuleTemplateFromRepository $
--             mkDisassociateApprovalRuleTemplateFromRepository
--
--         , requestGetBranch $
--             mkGetBranch
--
--         , requestGetDifferences $
--             mkGetDifferences
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetPullRequest $
--             mkGetPullRequest
--
--         , requestOverridePullRequestApprovalRules $
--             mkOverridePullRequestApprovalRules
--
--         , requestListPullRequests $
--             mkListPullRequests
--
--         , requestCreateCommit $
--             mkCreateCommit
--
--         , requestUpdatePullRequestApprovalState $
--             mkUpdatePullRequestApprovalState
--
--         , requestEvaluatePullRequestApprovalRules $
--             mkEvaluatePullRequestApprovalRules
--
--         , requestGetComment $
--             mkGetComment
--
--         , requestCreateApprovalRuleTemplate $
--             mkCreateApprovalRuleTemplate
--
--         , requestDeleteBranch $
--             mkDeleteBranch
--
--         , requestUpdateRepositoryDescription $
--             mkUpdateRepositoryDescription
--
--         , requestCreateBranch $
--             mkCreateBranch
--
--         , requestGetFolder $
--             mkGetFolder
--
--         , requestCreatePullRequest $
--             mkCreatePullRequest
--
--         , requestDeleteApprovalRuleTemplate $
--             mkDeleteApprovalRuleTemplate
--
--         , requestListBranches $
--             mkListBranches
--
--         , requestBatchGetCommits $
--             mkBatchGetCommits
--
--         , requestPutCommentReaction $
--             mkPutCommentReaction
--
--         , requestUpdatePullRequestDescription $
--             mkUpdatePullRequestDescription
--
--         , requestListRepositories $
--             mkListRepositories
--
--         , requestCreateRepository $
--             mkCreateRepository
--
--         , requestUpdateDefaultBranch $
--             mkUpdateDefaultBranch
--
--         , requestGetMergeOptions $
--             mkGetMergeOptions
--
--         , requestCreatePullRequestApprovalRule $
--             mkCreatePullRequestApprovalRule
--
--         , requestPostCommentReply $
--             mkPostCommentReply
--
--         , requestUpdateApprovalRuleTemplateContent $
--             mkUpdateApprovalRuleTemplateContent
--
--         , requestCreateUnreferencedMergeCommit $
--             mkCreateUnreferencedMergeCommit
--
--         , requestListRepositoriesForApprovalRuleTemplate $
--             mkListRepositoriesForApprovalRuleTemplate
--
--         , requestGetRepository $
--             mkGetRepository
--
--         , requestBatchDescribeMergeConflicts $
--             mkBatchDescribeMergeConflicts
--
--         , requestDeletePullRequestApprovalRule $
--             mkDeletePullRequestApprovalRule
--
--         , requestGetRepositoryTriggers $
--             mkGetRepositoryTriggers
--
--         , requestUpdateApprovalRuleTemplateName $
--             mkUpdateApprovalRuleTemplateName
--
--         , requestPutFile $
--             mkPutFile
--
--         , requestDeleteFile $
--             mkDeleteFile
--
--         , requestGetCommentsForComparedCommit $
--             mkGetCommentsForComparedCommit
--
--         , requestGetMergeCommit $
--             mkGetMergeCommit
--
--         , requestTestRepositoryTriggers $
--             mkTestRepositoryTriggers
--
--         , requestMergePullRequestBySquash $
--             mkMergePullRequestBySquash
--
--         , requestUpdateComment $
--             mkUpdateComment
--
--         , requestPostCommentForComparedCommit $
--             mkPostCommentForComparedCommit
--
--         , requestMergeBranchesByFastForward $
--             mkMergeBranchesByFastForward
--
--         , requestUpdatePullRequestTitle $
--             mkUpdatePullRequestTitle
--
--         , requestBatchDisassociateApprovalRuleTemplateFromRepositories $
--             mkBatchDisassociateApprovalRuleTemplateFromRepositories
--
--         , requestUpdatePullRequestApprovalRuleContent $
--             mkUpdatePullRequestApprovalRuleContent
--
--         , requestGetBlob $
--             mkGetBlob
--
--         , requestAssociateApprovalRuleTemplateWithRepository $
--             mkAssociateApprovalRuleTemplateWithRepository
--
--         , requestPutRepositoryTriggers $
--             mkPutRepositoryTriggers
--
--         , requestListApprovalRuleTemplates $
--             mkListApprovalRuleTemplates
--
--         , requestDescribeMergeConflicts $
--             mkDescribeMergeConflicts
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestMergeBranchesByThreeWay $
--             mkMergeBranchesByThreeWay
--
--         , requestGetFile $
--             mkGetFile
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestGetMergeConflicts $
--             mkGetMergeConflicts
--
--         , requestDeleteRepository $
--             mkDeleteRepository
--
--         , requestDeleteCommentContent $
--             mkDeleteCommentContent
--
--         , requestMergePullRequestByThreeWay $
--             mkMergePullRequestByThreeWay
--
--         , requestDescribePullRequestEvents $
--             mkDescribePullRequestEvents
--
--         , requestBatchGetRepositories $
--             mkBatchGetRepositories
--
--         , requestUpdateApprovalRuleTemplateDescription $
--             mkUpdateApprovalRuleTemplateDescription
--
--         , requestGetPullRequestOverrideState $
--             mkGetPullRequestOverrideState
--
--         , requestGetPullRequestApprovalStates $
--             mkGetPullRequestApprovalStates
--
--         , requestGetCommentsForPullRequest $
--             mkGetCommentsForPullRequest
--
--         , requestUpdatePullRequestStatus $
--             mkUpdatePullRequestStatus
--
--         , requestListAssociatedApprovalRuleTemplatesForRepository $
--             mkListAssociatedApprovalRuleTemplatesForRepository
--
--           ]

--     , testGroup "response"
--         [ responseMergePullRequestByFastForward $
--             mkMergePullRequestByFastForwardResponse
--
--         , responseUpdateRepositoryName $
--             mkUpdateRepositoryNameResponse
--
--         , responsePostCommentForPullRequest $
--             mkPostCommentForPullRequestResponse
--
--         , responseMergeBranchesBySquash $
--             mkMergeBranchesBySquashResponse
--
--         , responseGetCommit $
--             mkGetCommitResponse
--
--         , responseBatchAssociateApprovalRuleTemplateWithRepositories $
--             mkBatchAssociateApprovalRuleTemplateWithRepositoriesResponse
--
--         , responseGetCommentReactions $
--             mkGetCommentReactionsResponse
--
--         , responseGetApprovalRuleTemplate $
--             mkGetApprovalRuleTemplateResponse
--
--         , responseDisassociateApprovalRuleTemplateFromRepository $
--             mkDisassociateApprovalRuleTemplateFromRepositoryResponse
--
--         , responseGetBranch $
--             mkGetBranchResponse
--
--         , responseGetDifferences $
--             mkGetDifferencesResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetPullRequest $
--             mkGetPullRequestResponse
--
--         , responseOverridePullRequestApprovalRules $
--             mkOverridePullRequestApprovalRulesResponse
--
--         , responseListPullRequests $
--             mkListPullRequestsResponse
--
--         , responseCreateCommit $
--             mkCreateCommitResponse
--
--         , responseUpdatePullRequestApprovalState $
--             mkUpdatePullRequestApprovalStateResponse
--
--         , responseEvaluatePullRequestApprovalRules $
--             mkEvaluatePullRequestApprovalRulesResponse
--
--         , responseGetComment $
--             mkGetCommentResponse
--
--         , responseCreateApprovalRuleTemplate $
--             mkCreateApprovalRuleTemplateResponse
--
--         , responseDeleteBranch $
--             mkDeleteBranchResponse
--
--         , responseUpdateRepositoryDescription $
--             mkUpdateRepositoryDescriptionResponse
--
--         , responseCreateBranch $
--             mkCreateBranchResponse
--
--         , responseGetFolder $
--             mkGetFolderResponse
--
--         , responseCreatePullRequest $
--             mkCreatePullRequestResponse
--
--         , responseDeleteApprovalRuleTemplate $
--             mkDeleteApprovalRuleTemplateResponse
--
--         , responseListBranches $
--             mkListBranchesResponse
--
--         , responseBatchGetCommits $
--             mkBatchGetCommitsResponse
--
--         , responsePutCommentReaction $
--             mkPutCommentReactionResponse
--
--         , responseUpdatePullRequestDescription $
--             mkUpdatePullRequestDescriptionResponse
--
--         , responseListRepositories $
--             mkListRepositoriesResponse
--
--         , responseCreateRepository $
--             mkCreateRepositoryResponse
--
--         , responseUpdateDefaultBranch $
--             mkUpdateDefaultBranchResponse
--
--         , responseGetMergeOptions $
--             mkGetMergeOptionsResponse
--
--         , responseCreatePullRequestApprovalRule $
--             mkCreatePullRequestApprovalRuleResponse
--
--         , responsePostCommentReply $
--             mkPostCommentReplyResponse
--
--         , responseUpdateApprovalRuleTemplateContent $
--             mkUpdateApprovalRuleTemplateContentResponse
--
--         , responseCreateUnreferencedMergeCommit $
--             mkCreateUnreferencedMergeCommitResponse
--
--         , responseListRepositoriesForApprovalRuleTemplate $
--             mkListRepositoriesForApprovalRuleTemplateResponse
--
--         , responseGetRepository $
--             mkGetRepositoryResponse
--
--         , responseBatchDescribeMergeConflicts $
--             mkBatchDescribeMergeConflictsResponse
--
--         , responseDeletePullRequestApprovalRule $
--             mkDeletePullRequestApprovalRuleResponse
--
--         , responseGetRepositoryTriggers $
--             mkGetRepositoryTriggersResponse
--
--         , responseUpdateApprovalRuleTemplateName $
--             mkUpdateApprovalRuleTemplateNameResponse
--
--         , responsePutFile $
--             mkPutFileResponse
--
--         , responseDeleteFile $
--             mkDeleteFileResponse
--
--         , responseGetCommentsForComparedCommit $
--             mkGetCommentsForComparedCommitResponse
--
--         , responseGetMergeCommit $
--             mkGetMergeCommitResponse
--
--         , responseTestRepositoryTriggers $
--             mkTestRepositoryTriggersResponse
--
--         , responseMergePullRequestBySquash $
--             mkMergePullRequestBySquashResponse
--
--         , responseUpdateComment $
--             mkUpdateCommentResponse
--
--         , responsePostCommentForComparedCommit $
--             mkPostCommentForComparedCommitResponse
--
--         , responseMergeBranchesByFastForward $
--             mkMergeBranchesByFastForwardResponse
--
--         , responseUpdatePullRequestTitle $
--             mkUpdatePullRequestTitleResponse
--
--         , responseBatchDisassociateApprovalRuleTemplateFromRepositories $
--             mkBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse
--
--         , responseUpdatePullRequestApprovalRuleContent $
--             mkUpdatePullRequestApprovalRuleContentResponse
--
--         , responseGetBlob $
--             mkGetBlobResponse
--
--         , responseAssociateApprovalRuleTemplateWithRepository $
--             mkAssociateApprovalRuleTemplateWithRepositoryResponse
--
--         , responsePutRepositoryTriggers $
--             mkPutRepositoryTriggersResponse
--
--         , responseListApprovalRuleTemplates $
--             mkListApprovalRuleTemplatesResponse
--
--         , responseDescribeMergeConflicts $
--             mkDescribeMergeConflictsResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseMergeBranchesByThreeWay $
--             mkMergeBranchesByThreeWayResponse
--
--         , responseGetFile $
--             mkGetFileResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseGetMergeConflicts $
--             mkGetMergeConflictsResponse
--
--         , responseDeleteRepository $
--             mkDeleteRepositoryResponse
--
--         , responseDeleteCommentContent $
--             mkDeleteCommentContentResponse
--
--         , responseMergePullRequestByThreeWay $
--             mkMergePullRequestByThreeWayResponse
--
--         , responseDescribePullRequestEvents $
--             mkDescribePullRequestEventsResponse
--
--         , responseBatchGetRepositories $
--             mkBatchGetRepositoriesResponse
--
--         , responseUpdateApprovalRuleTemplateDescription $
--             mkUpdateApprovalRuleTemplateDescriptionResponse
--
--         , responseGetPullRequestOverrideState $
--             mkGetPullRequestOverrideStateResponse
--
--         , responseGetPullRequestApprovalStates $
--             mkGetPullRequestApprovalStatesResponse
--
--         , responseGetCommentsForPullRequest $
--             mkGetCommentsForPullRequestResponse
--
--         , responseUpdatePullRequestStatus $
--             mkUpdatePullRequestStatusResponse
--
--         , responseListAssociatedApprovalRuleTemplatesForRepository $
--             mkListAssociatedApprovalRuleTemplatesForRepositoryResponse
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
    mkServiceConfig
    (Proxy :: Proxy MergePullRequestByFastForward)

responseUpdateRepositoryName :: UpdateRepositoryNameResponse -> TestTree
responseUpdateRepositoryName =
  res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRepositoryName)

responsePostCommentForPullRequest :: PostCommentForPullRequestResponse -> TestTree
responsePostCommentForPullRequest =
  res
    "PostCommentForPullRequestResponse"
    "fixture/PostCommentForPullRequestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PostCommentForPullRequest)

responseMergeBranchesBySquash :: MergeBranchesBySquashResponse -> TestTree
responseMergeBranchesBySquash =
  res
    "MergeBranchesBySquashResponse"
    "fixture/MergeBranchesBySquashResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MergeBranchesBySquash)

responseGetCommit :: GetCommitResponse -> TestTree
responseGetCommit =
  res
    "GetCommitResponse"
    "fixture/GetCommitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCommit)

responseBatchAssociateApprovalRuleTemplateWithRepositories :: BatchAssociateApprovalRuleTemplateWithRepositoriesResponse -> TestTree
responseBatchAssociateApprovalRuleTemplateWithRepositories =
  res
    "BatchAssociateApprovalRuleTemplateWithRepositoriesResponse"
    "fixture/BatchAssociateApprovalRuleTemplateWithRepositoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchAssociateApprovalRuleTemplateWithRepositories)

responseGetCommentReactions :: GetCommentReactionsResponse -> TestTree
responseGetCommentReactions =
  res
    "GetCommentReactionsResponse"
    "fixture/GetCommentReactionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCommentReactions)

responseGetApprovalRuleTemplate :: GetApprovalRuleTemplateResponse -> TestTree
responseGetApprovalRuleTemplate =
  res
    "GetApprovalRuleTemplateResponse"
    "fixture/GetApprovalRuleTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetApprovalRuleTemplate)

responseDisassociateApprovalRuleTemplateFromRepository :: DisassociateApprovalRuleTemplateFromRepositoryResponse -> TestTree
responseDisassociateApprovalRuleTemplateFromRepository =
  res
    "DisassociateApprovalRuleTemplateFromRepositoryResponse"
    "fixture/DisassociateApprovalRuleTemplateFromRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateApprovalRuleTemplateFromRepository)

responseGetBranch :: GetBranchResponse -> TestTree
responseGetBranch =
  res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBranch)

responseGetDifferences :: GetDifferencesResponse -> TestTree
responseGetDifferences =
  res
    "GetDifferencesResponse"
    "fixture/GetDifferencesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDifferences)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseGetPullRequest :: GetPullRequestResponse -> TestTree
responseGetPullRequest =
  res
    "GetPullRequestResponse"
    "fixture/GetPullRequestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPullRequest)

responseOverridePullRequestApprovalRules :: OverridePullRequestApprovalRulesResponse -> TestTree
responseOverridePullRequestApprovalRules =
  res
    "OverridePullRequestApprovalRulesResponse"
    "fixture/OverridePullRequestApprovalRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy OverridePullRequestApprovalRules)

responseListPullRequests :: ListPullRequestsResponse -> TestTree
responseListPullRequests =
  res
    "ListPullRequestsResponse"
    "fixture/ListPullRequestsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPullRequests)

responseCreateCommit :: CreateCommitResponse -> TestTree
responseCreateCommit =
  res
    "CreateCommitResponse"
    "fixture/CreateCommitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCommit)

responseUpdatePullRequestApprovalState :: UpdatePullRequestApprovalStateResponse -> TestTree
responseUpdatePullRequestApprovalState =
  res
    "UpdatePullRequestApprovalStateResponse"
    "fixture/UpdatePullRequestApprovalStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePullRequestApprovalState)

responseEvaluatePullRequestApprovalRules :: EvaluatePullRequestApprovalRulesResponse -> TestTree
responseEvaluatePullRequestApprovalRules =
  res
    "EvaluatePullRequestApprovalRulesResponse"
    "fixture/EvaluatePullRequestApprovalRulesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EvaluatePullRequestApprovalRules)

responseGetComment :: GetCommentResponse -> TestTree
responseGetComment =
  res
    "GetCommentResponse"
    "fixture/GetCommentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetComment)

responseCreateApprovalRuleTemplate :: CreateApprovalRuleTemplateResponse -> TestTree
responseCreateApprovalRuleTemplate =
  res
    "CreateApprovalRuleTemplateResponse"
    "fixture/CreateApprovalRuleTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateApprovalRuleTemplate)

responseDeleteBranch :: DeleteBranchResponse -> TestTree
responseDeleteBranch =
  res
    "DeleteBranchResponse"
    "fixture/DeleteBranchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBranch)

responseUpdateRepositoryDescription :: UpdateRepositoryDescriptionResponse -> TestTree
responseUpdateRepositoryDescription =
  res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRepositoryDescription)

responseCreateBranch :: CreateBranchResponse -> TestTree
responseCreateBranch =
  res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBranch)

responseGetFolder :: GetFolderResponse -> TestTree
responseGetFolder =
  res
    "GetFolderResponse"
    "fixture/GetFolderResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFolder)

responseCreatePullRequest :: CreatePullRequestResponse -> TestTree
responseCreatePullRequest =
  res
    "CreatePullRequestResponse"
    "fixture/CreatePullRequestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePullRequest)

responseDeleteApprovalRuleTemplate :: DeleteApprovalRuleTemplateResponse -> TestTree
responseDeleteApprovalRuleTemplate =
  res
    "DeleteApprovalRuleTemplateResponse"
    "fixture/DeleteApprovalRuleTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteApprovalRuleTemplate)

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches =
  res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListBranches)

responseBatchGetCommits :: BatchGetCommitsResponse -> TestTree
responseBatchGetCommits =
  res
    "BatchGetCommitsResponse"
    "fixture/BatchGetCommitsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetCommits)

responsePutCommentReaction :: PutCommentReactionResponse -> TestTree
responsePutCommentReaction =
  res
    "PutCommentReactionResponse"
    "fixture/PutCommentReactionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutCommentReaction)

responseUpdatePullRequestDescription :: UpdatePullRequestDescriptionResponse -> TestTree
responseUpdatePullRequestDescription =
  res
    "UpdatePullRequestDescriptionResponse"
    "fixture/UpdatePullRequestDescriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePullRequestDescription)

responseListRepositories :: ListRepositoriesResponse -> TestTree
responseListRepositories =
  res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRepositories)

responseCreateRepository :: CreateRepositoryResponse -> TestTree
responseCreateRepository =
  res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRepository)

responseUpdateDefaultBranch :: UpdateDefaultBranchResponse -> TestTree
responseUpdateDefaultBranch =
  res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDefaultBranch)

responseGetMergeOptions :: GetMergeOptionsResponse -> TestTree
responseGetMergeOptions =
  res
    "GetMergeOptionsResponse"
    "fixture/GetMergeOptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMergeOptions)

responseCreatePullRequestApprovalRule :: CreatePullRequestApprovalRuleResponse -> TestTree
responseCreatePullRequestApprovalRule =
  res
    "CreatePullRequestApprovalRuleResponse"
    "fixture/CreatePullRequestApprovalRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePullRequestApprovalRule)

responsePostCommentReply :: PostCommentReplyResponse -> TestTree
responsePostCommentReply =
  res
    "PostCommentReplyResponse"
    "fixture/PostCommentReplyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PostCommentReply)

responseUpdateApprovalRuleTemplateContent :: UpdateApprovalRuleTemplateContentResponse -> TestTree
responseUpdateApprovalRuleTemplateContent =
  res
    "UpdateApprovalRuleTemplateContentResponse"
    "fixture/UpdateApprovalRuleTemplateContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApprovalRuleTemplateContent)

responseCreateUnreferencedMergeCommit :: CreateUnreferencedMergeCommitResponse -> TestTree
responseCreateUnreferencedMergeCommit =
  res
    "CreateUnreferencedMergeCommitResponse"
    "fixture/CreateUnreferencedMergeCommitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateUnreferencedMergeCommit)

responseListRepositoriesForApprovalRuleTemplate :: ListRepositoriesForApprovalRuleTemplateResponse -> TestTree
responseListRepositoriesForApprovalRuleTemplate =
  res
    "ListRepositoriesForApprovalRuleTemplateResponse"
    "fixture/ListRepositoriesForApprovalRuleTemplateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRepositoriesForApprovalRuleTemplate)

responseGetRepository :: GetRepositoryResponse -> TestTree
responseGetRepository =
  res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRepository)

responseBatchDescribeMergeConflicts :: BatchDescribeMergeConflictsResponse -> TestTree
responseBatchDescribeMergeConflicts =
  res
    "BatchDescribeMergeConflictsResponse"
    "fixture/BatchDescribeMergeConflictsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDescribeMergeConflicts)

responseDeletePullRequestApprovalRule :: DeletePullRequestApprovalRuleResponse -> TestTree
responseDeletePullRequestApprovalRule =
  res
    "DeletePullRequestApprovalRuleResponse"
    "fixture/DeletePullRequestApprovalRuleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePullRequestApprovalRule)

responseGetRepositoryTriggers :: GetRepositoryTriggersResponse -> TestTree
responseGetRepositoryTriggers =
  res
    "GetRepositoryTriggersResponse"
    "fixture/GetRepositoryTriggersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRepositoryTriggers)

responseUpdateApprovalRuleTemplateName :: UpdateApprovalRuleTemplateNameResponse -> TestTree
responseUpdateApprovalRuleTemplateName =
  res
    "UpdateApprovalRuleTemplateNameResponse"
    "fixture/UpdateApprovalRuleTemplateNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApprovalRuleTemplateName)

responsePutFile :: PutFileResponse -> TestTree
responsePutFile =
  res
    "PutFileResponse"
    "fixture/PutFileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutFile)

responseDeleteFile :: DeleteFileResponse -> TestTree
responseDeleteFile =
  res
    "DeleteFileResponse"
    "fixture/DeleteFileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteFile)

responseGetCommentsForComparedCommit :: GetCommentsForComparedCommitResponse -> TestTree
responseGetCommentsForComparedCommit =
  res
    "GetCommentsForComparedCommitResponse"
    "fixture/GetCommentsForComparedCommitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCommentsForComparedCommit)

responseGetMergeCommit :: GetMergeCommitResponse -> TestTree
responseGetMergeCommit =
  res
    "GetMergeCommitResponse"
    "fixture/GetMergeCommitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMergeCommit)

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers =
  res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestRepositoryTriggers)

responseMergePullRequestBySquash :: MergePullRequestBySquashResponse -> TestTree
responseMergePullRequestBySquash =
  res
    "MergePullRequestBySquashResponse"
    "fixture/MergePullRequestBySquashResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MergePullRequestBySquash)

responseUpdateComment :: UpdateCommentResponse -> TestTree
responseUpdateComment =
  res
    "UpdateCommentResponse"
    "fixture/UpdateCommentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateComment)

responsePostCommentForComparedCommit :: PostCommentForComparedCommitResponse -> TestTree
responsePostCommentForComparedCommit =
  res
    "PostCommentForComparedCommitResponse"
    "fixture/PostCommentForComparedCommitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PostCommentForComparedCommit)

responseMergeBranchesByFastForward :: MergeBranchesByFastForwardResponse -> TestTree
responseMergeBranchesByFastForward =
  res
    "MergeBranchesByFastForwardResponse"
    "fixture/MergeBranchesByFastForwardResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MergeBranchesByFastForward)

responseUpdatePullRequestTitle :: UpdatePullRequestTitleResponse -> TestTree
responseUpdatePullRequestTitle =
  res
    "UpdatePullRequestTitleResponse"
    "fixture/UpdatePullRequestTitleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePullRequestTitle)

responseBatchDisassociateApprovalRuleTemplateFromRepositories :: BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse -> TestTree
responseBatchDisassociateApprovalRuleTemplateFromRepositories =
  res
    "BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse"
    "fixture/BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchDisassociateApprovalRuleTemplateFromRepositories)

responseUpdatePullRequestApprovalRuleContent :: UpdatePullRequestApprovalRuleContentResponse -> TestTree
responseUpdatePullRequestApprovalRuleContent =
  res
    "UpdatePullRequestApprovalRuleContentResponse"
    "fixture/UpdatePullRequestApprovalRuleContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePullRequestApprovalRuleContent)

responseGetBlob :: GetBlobResponse -> TestTree
responseGetBlob =
  res
    "GetBlobResponse"
    "fixture/GetBlobResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBlob)

responseAssociateApprovalRuleTemplateWithRepository :: AssociateApprovalRuleTemplateWithRepositoryResponse -> TestTree
responseAssociateApprovalRuleTemplateWithRepository =
  res
    "AssociateApprovalRuleTemplateWithRepositoryResponse"
    "fixture/AssociateApprovalRuleTemplateWithRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateApprovalRuleTemplateWithRepository)

responsePutRepositoryTriggers :: PutRepositoryTriggersResponse -> TestTree
responsePutRepositoryTriggers =
  res
    "PutRepositoryTriggersResponse"
    "fixture/PutRepositoryTriggersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutRepositoryTriggers)

responseListApprovalRuleTemplates :: ListApprovalRuleTemplatesResponse -> TestTree
responseListApprovalRuleTemplates =
  res
    "ListApprovalRuleTemplatesResponse"
    "fixture/ListApprovalRuleTemplatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListApprovalRuleTemplates)

responseDescribeMergeConflicts :: DescribeMergeConflictsResponse -> TestTree
responseDescribeMergeConflicts =
  res
    "DescribeMergeConflictsResponse"
    "fixture/DescribeMergeConflictsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMergeConflicts)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseMergeBranchesByThreeWay :: MergeBranchesByThreeWayResponse -> TestTree
responseMergeBranchesByThreeWay =
  res
    "MergeBranchesByThreeWayResponse"
    "fixture/MergeBranchesByThreeWayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MergeBranchesByThreeWay)

responseGetFile :: GetFileResponse -> TestTree
responseGetFile =
  res
    "GetFileResponse"
    "fixture/GetFileResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFile)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseGetMergeConflicts :: GetMergeConflictsResponse -> TestTree
responseGetMergeConflicts =
  res
    "GetMergeConflictsResponse"
    "fixture/GetMergeConflictsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetMergeConflicts)

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository =
  res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRepository)

responseDeleteCommentContent :: DeleteCommentContentResponse -> TestTree
responseDeleteCommentContent =
  res
    "DeleteCommentContentResponse"
    "fixture/DeleteCommentContentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCommentContent)

responseMergePullRequestByThreeWay :: MergePullRequestByThreeWayResponse -> TestTree
responseMergePullRequestByThreeWay =
  res
    "MergePullRequestByThreeWayResponse"
    "fixture/MergePullRequestByThreeWayResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MergePullRequestByThreeWay)

responseDescribePullRequestEvents :: DescribePullRequestEventsResponse -> TestTree
responseDescribePullRequestEvents =
  res
    "DescribePullRequestEventsResponse"
    "fixture/DescribePullRequestEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePullRequestEvents)

responseBatchGetRepositories :: BatchGetRepositoriesResponse -> TestTree
responseBatchGetRepositories =
  res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BatchGetRepositories)

responseUpdateApprovalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescriptionResponse -> TestTree
responseUpdateApprovalRuleTemplateDescription =
  res
    "UpdateApprovalRuleTemplateDescriptionResponse"
    "fixture/UpdateApprovalRuleTemplateDescriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateApprovalRuleTemplateDescription)

responseGetPullRequestOverrideState :: GetPullRequestOverrideStateResponse -> TestTree
responseGetPullRequestOverrideState =
  res
    "GetPullRequestOverrideStateResponse"
    "fixture/GetPullRequestOverrideStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPullRequestOverrideState)

responseGetPullRequestApprovalStates :: GetPullRequestApprovalStatesResponse -> TestTree
responseGetPullRequestApprovalStates =
  res
    "GetPullRequestApprovalStatesResponse"
    "fixture/GetPullRequestApprovalStatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPullRequestApprovalStates)

responseGetCommentsForPullRequest :: GetCommentsForPullRequestResponse -> TestTree
responseGetCommentsForPullRequest =
  res
    "GetCommentsForPullRequestResponse"
    "fixture/GetCommentsForPullRequestResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCommentsForPullRequest)

responseUpdatePullRequestStatus :: UpdatePullRequestStatusResponse -> TestTree
responseUpdatePullRequestStatus =
  res
    "UpdatePullRequestStatusResponse"
    "fixture/UpdatePullRequestStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePullRequestStatus)

responseListAssociatedApprovalRuleTemplatesForRepository :: ListAssociatedApprovalRuleTemplatesForRepositoryResponse -> TestTree
responseListAssociatedApprovalRuleTemplatesForRepository =
  res
    "ListAssociatedApprovalRuleTemplatesForRepositoryResponse"
    "fixture/ListAssociatedApprovalRuleTemplatesForRepositoryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAssociatedApprovalRuleTemplatesForRepository)

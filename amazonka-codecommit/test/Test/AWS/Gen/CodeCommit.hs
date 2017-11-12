{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeCommit
-- Copyright   : (c) 2013-2017 Brendan Hay
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
--         [ requestUpdateRepositoryName $
--             updateRepositoryName
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
--         , requestDeleteBranch $
--             deleteBranch
--
--         , requestUpdateRepositoryDescription $
--             updateRepositoryDescription
--
--         , requestCreateBranch $
--             createBranch
--
--         , requestListBranches $
--             listBranches
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
--         , requestGetRepository $
--             getRepository
--
--         , requestGetRepositoryTriggers $
--             getRepositoryTriggers
--
--         , requestTestRepositoryTriggers $
--             testRepositoryTriggers
--
--         , requestGetBlob $
--             getBlob
--
--         , requestPutRepositoryTriggers $
--             putRepositoryTriggers
--
--         , requestDeleteRepository $
--             deleteRepository
--
--         , requestBatchGetRepositories $
--             batchGetRepositories
--
--           ]

--     , testGroup "response"
--         [ responseUpdateRepositoryName $
--             updateRepositoryNameResponse
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
--         , responseDeleteBranch $
--             deleteBranchResponse
--
--         , responseUpdateRepositoryDescription $
--             updateRepositoryDescriptionResponse
--
--         , responseCreateBranch $
--             createBranchResponse
--
--         , responseListBranches $
--             listBranchesResponse
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
--         , responseGetRepository $
--             getRepositoryResponse
--
--         , responseGetRepositoryTriggers $
--             getRepositoryTriggersResponse
--
--         , responseTestRepositoryTriggers $
--             testRepositoryTriggersResponse
--
--         , responseGetBlob $
--             getBlobResponse
--
--         , responsePutRepositoryTriggers $
--             putRepositoryTriggersResponse
--
--         , responseDeleteRepository $
--             deleteRepositoryResponse
--
--         , responseBatchGetRepositories $
--             batchGetRepositoriesResponse
--
--           ]
--     ]

-- Requests

requestUpdateRepositoryName :: UpdateRepositoryName -> TestTree
requestUpdateRepositoryName = req
    "UpdateRepositoryName"
    "fixture/UpdateRepositoryName.yaml"

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

requestListBranches :: ListBranches -> TestTree
requestListBranches = req
    "ListBranches"
    "fixture/ListBranches.yaml"

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

requestGetRepository :: GetRepository -> TestTree
requestGetRepository = req
    "GetRepository"
    "fixture/GetRepository.yaml"

requestGetRepositoryTriggers :: GetRepositoryTriggers -> TestTree
requestGetRepositoryTriggers = req
    "GetRepositoryTriggers"
    "fixture/GetRepositoryTriggers.yaml"

requestTestRepositoryTriggers :: TestRepositoryTriggers -> TestTree
requestTestRepositoryTriggers = req
    "TestRepositoryTriggers"
    "fixture/TestRepositoryTriggers.yaml"

requestGetBlob :: GetBlob -> TestTree
requestGetBlob = req
    "GetBlob"
    "fixture/GetBlob.yaml"

requestPutRepositoryTriggers :: PutRepositoryTriggers -> TestTree
requestPutRepositoryTriggers = req
    "PutRepositoryTriggers"
    "fixture/PutRepositoryTriggers.yaml"

requestDeleteRepository :: DeleteRepository -> TestTree
requestDeleteRepository = req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

requestBatchGetRepositories :: BatchGetRepositories -> TestTree
requestBatchGetRepositories = req
    "BatchGetRepositories"
    "fixture/BatchGetRepositories.yaml"

-- Responses

responseUpdateRepositoryName :: UpdateRepositoryNameResponse -> TestTree
responseUpdateRepositoryName = res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateRepositoryName)

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

responseListBranches :: ListBranchesResponse -> TestTree
responseListBranches = res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListBranches)

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

responseTestRepositoryTriggers :: TestRepositoryTriggersResponse -> TestTree
responseTestRepositoryTriggers = res
    "TestRepositoryTriggersResponse"
    "fixture/TestRepositoryTriggersResponse.proto"
    codeCommit
    (Proxy :: Proxy TestRepositoryTriggers)

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

responseDeleteRepository :: DeleteRepositoryResponse -> TestTree
responseDeleteRepository = res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteRepository)

responseBatchGetRepositories :: BatchGetRepositoriesResponse -> TestTree
responseBatchGetRepositories = res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchGetRepositories)

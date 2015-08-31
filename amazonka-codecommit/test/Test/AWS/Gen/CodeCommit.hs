{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeCommit
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CodeCommit where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CodeCommit
import Test.AWS.CodeCommit.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testUpdateRepositoryName $
--             updateRepositoryName
--
--         , testGetBranch $
--             getBranch
--
--         , testUpdateRepositoryDescription $
--             updateRepositoryDescription
--
--         , testCreateBranch $
--             createBranch
--
--         , testListBranches $
--             listBranches
--
--         , testListRepositories $
--             listRepositories
--
--         , testCreateRepository $
--             createRepository
--
--         , testUpdateDefaultBranch $
--             updateDefaultBranch
--
--         , testGetRepository $
--             getRepository
--
--         , testDeleteRepository $
--             deleteRepository
--
--         , testBatchGetRepositories $
--             batchGetRepositories
--
--           ]

--     , testGroup "response"
--         [ testUpdateRepositoryNameResponse $
--             updateRepositoryNameResponse
--
--         , testGetBranchResponse $
--             getBranchResponse
--
--         , testUpdateRepositoryDescriptionResponse $
--             updateRepositoryDescriptionResponse
--
--         , testCreateBranchResponse $
--             createBranchResponse
--
--         , testListBranchesResponse $
--             listBranchesResponse
--
--         , testListRepositoriesResponse $
--             listRepositoriesResponse
--
--         , testCreateRepositoryResponse $
--             createRepositoryResponse
--
--         , testUpdateDefaultBranchResponse $
--             updateDefaultBranchResponse
--
--         , testGetRepositoryResponse $
--             getRepositoryResponse
--
--         , testDeleteRepositoryResponse $
--             deleteRepositoryResponse
--
--         , testBatchGetRepositoriesResponse $
--             batchGetRepositoriesResponse
--
--           ]
--     ]

-- Requests

testUpdateRepositoryName :: UpdateRepositoryName -> TestTree
testUpdateRepositoryName = req
    "UpdateRepositoryName"
    "fixture/UpdateRepositoryName.yaml"

testGetBranch :: GetBranch -> TestTree
testGetBranch = req
    "GetBranch"
    "fixture/GetBranch.yaml"

testUpdateRepositoryDescription :: UpdateRepositoryDescription -> TestTree
testUpdateRepositoryDescription = req
    "UpdateRepositoryDescription"
    "fixture/UpdateRepositoryDescription.yaml"

testCreateBranch :: CreateBranch -> TestTree
testCreateBranch = req
    "CreateBranch"
    "fixture/CreateBranch.yaml"

testListBranches :: ListBranches -> TestTree
testListBranches = req
    "ListBranches"
    "fixture/ListBranches.yaml"

testListRepositories :: ListRepositories -> TestTree
testListRepositories = req
    "ListRepositories"
    "fixture/ListRepositories.yaml"

testCreateRepository :: CreateRepository -> TestTree
testCreateRepository = req
    "CreateRepository"
    "fixture/CreateRepository.yaml"

testUpdateDefaultBranch :: UpdateDefaultBranch -> TestTree
testUpdateDefaultBranch = req
    "UpdateDefaultBranch"
    "fixture/UpdateDefaultBranch.yaml"

testGetRepository :: GetRepository -> TestTree
testGetRepository = req
    "GetRepository"
    "fixture/GetRepository.yaml"

testDeleteRepository :: DeleteRepository -> TestTree
testDeleteRepository = req
    "DeleteRepository"
    "fixture/DeleteRepository.yaml"

testBatchGetRepositories :: BatchGetRepositories -> TestTree
testBatchGetRepositories = req
    "BatchGetRepositories"
    "fixture/BatchGetRepositories.yaml"

-- Responses

testUpdateRepositoryNameResponse :: UpdateRepositoryNameResponse -> TestTree
testUpdateRepositoryNameResponse = res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateRepositoryName)

testGetBranchResponse :: GetBranchResponse -> TestTree
testGetBranchResponse = res
    "GetBranchResponse"
    "fixture/GetBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy GetBranch)

testUpdateRepositoryDescriptionResponse :: UpdateRepositoryDescriptionResponse -> TestTree
testUpdateRepositoryDescriptionResponse = res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateRepositoryDescription)

testCreateBranchResponse :: CreateBranchResponse -> TestTree
testCreateBranchResponse = res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateBranch)

testListBranchesResponse :: ListBranchesResponse -> TestTree
testListBranchesResponse = res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListBranches)

testListRepositoriesResponse :: ListRepositoriesResponse -> TestTree
testListRepositoriesResponse = res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy ListRepositories)

testCreateRepositoryResponse :: CreateRepositoryResponse -> TestTree
testCreateRepositoryResponse = res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy CreateRepository)

testUpdateDefaultBranchResponse :: UpdateDefaultBranchResponse -> TestTree
testUpdateDefaultBranchResponse = res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse.proto"
    codeCommit
    (Proxy :: Proxy UpdateDefaultBranch)

testGetRepositoryResponse :: GetRepositoryResponse -> TestTree
testGetRepositoryResponse = res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy GetRepository)

testDeleteRepositoryResponse :: DeleteRepositoryResponse -> TestTree
testDeleteRepositoryResponse = res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse.proto"
    codeCommit
    (Proxy :: Proxy DeleteRepository)

testBatchGetRepositoriesResponse :: BatchGetRepositoriesResponse -> TestTree
testBatchGetRepositoriesResponse = res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse.proto"
    codeCommit
    (Proxy :: Proxy BatchGetRepositories)

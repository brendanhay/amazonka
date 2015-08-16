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
--         , testListBranches $
--             listBranches
--
--         , testCreateBranch $
--             createBranch
--
--         , testUpdateDefaultBranch $
--             updateDefaultBranch
--
--         , testCreateRepository $
--             createRepository
--
--         , testListRepositories $
--             listRepositories
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
--         , testListBranchesResponse $
--             listBranchesResponse
--
--         , testCreateBranchResponse $
--             createBranchResponse
--
--         , testUpdateDefaultBranchResponse $
--             updateDefaultBranchResponse
--
--         , testCreateRepositoryResponse $
--             createRepositoryResponse
--
--         , testListRepositoriesResponse $
--             listRepositoriesResponse
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
    "fixture/UpdateRepositoryName"

testGetBranch :: GetBranch -> TestTree
testGetBranch = req
    "GetBranch"
    "fixture/GetBranch"

testUpdateRepositoryDescription :: UpdateRepositoryDescription -> TestTree
testUpdateRepositoryDescription = req
    "UpdateRepositoryDescription"
    "fixture/UpdateRepositoryDescription"

testListBranches :: ListBranches -> TestTree
testListBranches = req
    "ListBranches"
    "fixture/ListBranches"

testCreateBranch :: CreateBranch -> TestTree
testCreateBranch = req
    "CreateBranch"
    "fixture/CreateBranch"

testUpdateDefaultBranch :: UpdateDefaultBranch -> TestTree
testUpdateDefaultBranch = req
    "UpdateDefaultBranch"
    "fixture/UpdateDefaultBranch"

testCreateRepository :: CreateRepository -> TestTree
testCreateRepository = req
    "CreateRepository"
    "fixture/CreateRepository"

testListRepositories :: ListRepositories -> TestTree
testListRepositories = req
    "ListRepositories"
    "fixture/ListRepositories"

testGetRepository :: GetRepository -> TestTree
testGetRepository = req
    "GetRepository"
    "fixture/GetRepository"

testDeleteRepository :: DeleteRepository -> TestTree
testDeleteRepository = req
    "DeleteRepository"
    "fixture/DeleteRepository"

testBatchGetRepositories :: BatchGetRepositories -> TestTree
testBatchGetRepositories = req
    "BatchGetRepositories"
    "fixture/BatchGetRepositories"

-- Responses

testUpdateRepositoryNameResponse :: UpdateRepositoryNameResponse -> TestTree
testUpdateRepositoryNameResponse = res
    "UpdateRepositoryNameResponse"
    "fixture/UpdateRepositoryNameResponse"
    (Proxy :: Proxy UpdateRepositoryName)

testGetBranchResponse :: GetBranchResponse -> TestTree
testGetBranchResponse = res
    "GetBranchResponse"
    "fixture/GetBranchResponse"
    (Proxy :: Proxy GetBranch)

testUpdateRepositoryDescriptionResponse :: UpdateRepositoryDescriptionResponse -> TestTree
testUpdateRepositoryDescriptionResponse = res
    "UpdateRepositoryDescriptionResponse"
    "fixture/UpdateRepositoryDescriptionResponse"
    (Proxy :: Proxy UpdateRepositoryDescription)

testListBranchesResponse :: ListBranchesResponse -> TestTree
testListBranchesResponse = res
    "ListBranchesResponse"
    "fixture/ListBranchesResponse"
    (Proxy :: Proxy ListBranches)

testCreateBranchResponse :: CreateBranchResponse -> TestTree
testCreateBranchResponse = res
    "CreateBranchResponse"
    "fixture/CreateBranchResponse"
    (Proxy :: Proxy CreateBranch)

testUpdateDefaultBranchResponse :: UpdateDefaultBranchResponse -> TestTree
testUpdateDefaultBranchResponse = res
    "UpdateDefaultBranchResponse"
    "fixture/UpdateDefaultBranchResponse"
    (Proxy :: Proxy UpdateDefaultBranch)

testCreateRepositoryResponse :: CreateRepositoryResponse -> TestTree
testCreateRepositoryResponse = res
    "CreateRepositoryResponse"
    "fixture/CreateRepositoryResponse"
    (Proxy :: Proxy CreateRepository)

testListRepositoriesResponse :: ListRepositoriesResponse -> TestTree
testListRepositoriesResponse = res
    "ListRepositoriesResponse"
    "fixture/ListRepositoriesResponse"
    (Proxy :: Proxy ListRepositories)

testGetRepositoryResponse :: GetRepositoryResponse -> TestTree
testGetRepositoryResponse = res
    "GetRepositoryResponse"
    "fixture/GetRepositoryResponse"
    (Proxy :: Proxy GetRepository)

testDeleteRepositoryResponse :: DeleteRepositoryResponse -> TestTree
testDeleteRepositoryResponse = res
    "DeleteRepositoryResponse"
    "fixture/DeleteRepositoryResponse"
    (Proxy :: Proxy DeleteRepository)

testBatchGetRepositoriesResponse :: BatchGetRepositoriesResponse -> TestTree
testBatchGetRepositoriesResponse = res
    "BatchGetRepositoriesResponse"
    "fixture/BatchGetRepositoriesResponse"
    (Proxy :: Proxy BatchGetRepositories)

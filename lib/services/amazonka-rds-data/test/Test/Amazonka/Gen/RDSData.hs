{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RDSData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RDSData where

import Amazonka.RDSData
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RDSData.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestBeginTransaction $
--             newBeginTransaction
--
--         , requestCommitTransaction $
--             newCommitTransaction
--
--         , requestExecuteStatement $
--             newExecuteStatement
--
--         , requestRollbackTransaction $
--             newRollbackTransaction
--
--           ]

--     , testGroup "response"
--         [ responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseBeginTransaction $
--             newBeginTransactionResponse
--
--         , responseCommitTransaction $
--             newCommitTransactionResponse
--
--         , responseExecuteStatement $
--             newExecuteStatementResponse
--
--         , responseRollbackTransaction $
--             newRollbackTransactionResponse
--
--           ]
--     ]

-- Requests

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestBeginTransaction :: BeginTransaction -> TestTree
requestBeginTransaction =
  req
    "BeginTransaction"
    "fixture/BeginTransaction.yaml"

requestCommitTransaction :: CommitTransaction -> TestTree
requestCommitTransaction =
  req
    "CommitTransaction"
    "fixture/CommitTransaction.yaml"

requestExecuteStatement :: ExecuteStatement -> TestTree
requestExecuteStatement =
  req
    "ExecuteStatement"
    "fixture/ExecuteStatement.yaml"

requestRollbackTransaction :: RollbackTransaction -> TestTree
requestRollbackTransaction =
  req
    "RollbackTransaction"
    "fixture/RollbackTransaction.yaml"

-- Responses

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchExecuteStatement)

responseBeginTransaction :: BeginTransactionResponse -> TestTree
responseBeginTransaction =
  res
    "BeginTransactionResponse"
    "fixture/BeginTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BeginTransaction)

responseCommitTransaction :: CommitTransactionResponse -> TestTree
responseCommitTransaction =
  res
    "CommitTransactionResponse"
    "fixture/CommitTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CommitTransaction)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteStatement)

responseRollbackTransaction :: RollbackTransactionResponse -> TestTree
responseRollbackTransaction =
  res
    "RollbackTransactionResponse"
    "fixture/RollbackTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RollbackTransaction)

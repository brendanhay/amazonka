{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RDSData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.RDSData where

import Amazonka.RDSData
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.RDSData.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestRollbackTransaction $
--             newRollbackTransaction
--
--         , requestBeginTransaction $
--             newBeginTransaction
--
--         , requestBatchExecuteStatement $
--             newBatchExecuteStatement
--
--         , requestExecuteStatement $
--             newExecuteStatement
--
--         , requestCommitTransaction $
--             newCommitTransaction
--
--           ]

--     , testGroup "response"
--         [ responseRollbackTransaction $
--             newRollbackTransactionResponse
--
--         , responseBeginTransaction $
--             newBeginTransactionResponse
--
--         , responseBatchExecuteStatement $
--             newBatchExecuteStatementResponse
--
--         , responseExecuteStatement $
--             newExecuteStatementResponse
--
--         , responseCommitTransaction $
--             newCommitTransactionResponse
--
--           ]
--     ]

-- Requests

requestRollbackTransaction :: RollbackTransaction -> TestTree
requestRollbackTransaction =
  req
    "RollbackTransaction"
    "fixture/RollbackTransaction.yaml"

requestBeginTransaction :: BeginTransaction -> TestTree
requestBeginTransaction =
  req
    "BeginTransaction"
    "fixture/BeginTransaction.yaml"

requestBatchExecuteStatement :: BatchExecuteStatement -> TestTree
requestBatchExecuteStatement =
  req
    "BatchExecuteStatement"
    "fixture/BatchExecuteStatement.yaml"

requestExecuteStatement :: ExecuteStatement -> TestTree
requestExecuteStatement =
  req
    "ExecuteStatement"
    "fixture/ExecuteStatement.yaml"

requestCommitTransaction :: CommitTransaction -> TestTree
requestCommitTransaction =
  req
    "CommitTransaction"
    "fixture/CommitTransaction.yaml"

-- Responses

responseRollbackTransaction :: RollbackTransactionResponse -> TestTree
responseRollbackTransaction =
  res
    "RollbackTransactionResponse"
    "fixture/RollbackTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RollbackTransaction)

responseBeginTransaction :: BeginTransactionResponse -> TestTree
responseBeginTransaction =
  res
    "BeginTransactionResponse"
    "fixture/BeginTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BeginTransaction)

responseBatchExecuteStatement :: BatchExecuteStatementResponse -> TestTree
responseBatchExecuteStatement =
  res
    "BatchExecuteStatementResponse"
    "fixture/BatchExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchExecuteStatement)

responseExecuteStatement :: ExecuteStatementResponse -> TestTree
responseExecuteStatement =
  res
    "ExecuteStatementResponse"
    "fixture/ExecuteStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteStatement)

responseCommitTransaction :: CommitTransactionResponse -> TestTree
responseCommitTransaction =
  res
    "CommitTransactionResponse"
    "fixture/CommitTransactionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CommitTransaction)

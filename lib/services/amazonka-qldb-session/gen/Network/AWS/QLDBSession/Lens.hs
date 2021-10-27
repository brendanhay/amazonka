{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDBSession.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDBSession.Lens
  ( -- * Operations

    -- ** SendCommand
    sendCommand_fetchPage,
    sendCommand_sessionToken,
    sendCommand_abortTransaction,
    sendCommand_endSession,
    sendCommand_startTransaction,
    sendCommand_executeStatement,
    sendCommand_commitTransaction,
    sendCommand_startSession,
    sendCommandResponse_fetchPage,
    sendCommandResponse_abortTransaction,
    sendCommandResponse_endSession,
    sendCommandResponse_startTransaction,
    sendCommandResponse_executeStatement,
    sendCommandResponse_commitTransaction,
    sendCommandResponse_startSession,
    sendCommandResponse_httpStatus,

    -- * Types

    -- ** AbortTransactionRequest

    -- ** AbortTransactionResult
    abortTransactionResult_timingInformation,

    -- ** CommitTransactionRequest
    commitTransactionRequest_transactionId,
    commitTransactionRequest_commitDigest,

    -- ** CommitTransactionResult
    commitTransactionResult_timingInformation,
    commitTransactionResult_consumedIOs,
    commitTransactionResult_commitDigest,
    commitTransactionResult_transactionId,

    -- ** EndSessionRequest

    -- ** EndSessionResult
    endSessionResult_timingInformation,

    -- ** ExecuteStatementRequest
    executeStatementRequest_parameters,
    executeStatementRequest_transactionId,
    executeStatementRequest_statement,

    -- ** ExecuteStatementResult
    executeStatementResult_timingInformation,
    executeStatementResult_consumedIOs,
    executeStatementResult_firstPage,

    -- ** FetchPageRequest
    fetchPageRequest_transactionId,
    fetchPageRequest_nextPageToken,

    -- ** FetchPageResult
    fetchPageResult_timingInformation,
    fetchPageResult_consumedIOs,
    fetchPageResult_page,

    -- ** IOUsage
    iOUsage_readIOs,
    iOUsage_writeIOs,

    -- ** Page
    page_nextPageToken,
    page_values,

    -- ** StartSessionRequest
    startSessionRequest_ledgerName,

    -- ** StartSessionResult
    startSessionResult_timingInformation,
    startSessionResult_sessionToken,

    -- ** StartTransactionRequest

    -- ** StartTransactionResult
    startTransactionResult_timingInformation,
    startTransactionResult_transactionId,

    -- ** TimingInformation
    timingInformation_processingTimeMilliseconds,

    -- ** ValueHolder
    valueHolder_ionText,
    valueHolder_ionBinary,
  )
where

import Network.AWS.QLDBSession.SendCommand
import Network.AWS.QLDBSession.Types.AbortTransactionRequest
import Network.AWS.QLDBSession.Types.AbortTransactionResult
import Network.AWS.QLDBSession.Types.CommitTransactionRequest
import Network.AWS.QLDBSession.Types.CommitTransactionResult
import Network.AWS.QLDBSession.Types.EndSessionRequest
import Network.AWS.QLDBSession.Types.EndSessionResult
import Network.AWS.QLDBSession.Types.ExecuteStatementRequest
import Network.AWS.QLDBSession.Types.ExecuteStatementResult
import Network.AWS.QLDBSession.Types.FetchPageRequest
import Network.AWS.QLDBSession.Types.FetchPageResult
import Network.AWS.QLDBSession.Types.IOUsage
import Network.AWS.QLDBSession.Types.Page
import Network.AWS.QLDBSession.Types.StartSessionRequest
import Network.AWS.QLDBSession.Types.StartSessionResult
import Network.AWS.QLDBSession.Types.StartTransactionRequest
import Network.AWS.QLDBSession.Types.StartTransactionResult
import Network.AWS.QLDBSession.Types.TimingInformation
import Network.AWS.QLDBSession.Types.ValueHolder

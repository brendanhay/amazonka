{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.QLDBSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-07-11@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The transactional data APIs for Amazon QLDB
--
-- Instead of interacting directly with this API, we recommend using the
-- QLDB driver or the QLDB shell to execute data transactions on a ledger.
--
-- -   If you are working with an AWS SDK, use the QLDB driver. The driver
--     provides a high-level abstraction layer above this /QLDB Session/
--     data plane and manages @SendCommand@ API calls for you. For
--     information and a list of supported programming languages, see
--     <https://docs.aws.amazon.com/qldb/latest/developerguide/getting-started-driver.html Getting started with the driver>
--     in the /Amazon QLDB Developer Guide/.
--
-- -   If you are working with the AWS Command Line Interface (AWS CLI),
--     use the QLDB shell. The shell is a command line interface that uses
--     the QLDB driver to interact with a ledger. For information, see
--     <https://docs.aws.amazon.com/qldb/latest/developerguide/data-shell.html Accessing Amazon QLDB using the QLDB shell>.
module Amazonka.QLDBSession
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** CapacityExceededException
    _CapacityExceededException,

    -- ** InvalidSessionException
    _InvalidSessionException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** OccConflictException
    _OccConflictException,

    -- ** RateExceededException
    _RateExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SendCommand
    SendCommand (SendCommand'),
    newSendCommand,
    SendCommandResponse (SendCommandResponse'),
    newSendCommandResponse,

    -- * Types

    -- ** AbortTransactionRequest
    AbortTransactionRequest (AbortTransactionRequest'),
    newAbortTransactionRequest,

    -- ** AbortTransactionResult
    AbortTransactionResult (AbortTransactionResult'),
    newAbortTransactionResult,

    -- ** CommitTransactionRequest
    CommitTransactionRequest (CommitTransactionRequest'),
    newCommitTransactionRequest,

    -- ** CommitTransactionResult
    CommitTransactionResult (CommitTransactionResult'),
    newCommitTransactionResult,

    -- ** EndSessionRequest
    EndSessionRequest (EndSessionRequest'),
    newEndSessionRequest,

    -- ** EndSessionResult
    EndSessionResult (EndSessionResult'),
    newEndSessionResult,

    -- ** ExecuteStatementRequest
    ExecuteStatementRequest (ExecuteStatementRequest'),
    newExecuteStatementRequest,

    -- ** ExecuteStatementResult
    ExecuteStatementResult (ExecuteStatementResult'),
    newExecuteStatementResult,

    -- ** FetchPageRequest
    FetchPageRequest (FetchPageRequest'),
    newFetchPageRequest,

    -- ** FetchPageResult
    FetchPageResult (FetchPageResult'),
    newFetchPageResult,

    -- ** IOUsage
    IOUsage (IOUsage'),
    newIOUsage,

    -- ** Page
    Page (Page'),
    newPage,

    -- ** StartSessionRequest
    StartSessionRequest (StartSessionRequest'),
    newStartSessionRequest,

    -- ** StartSessionResult
    StartSessionResult (StartSessionResult'),
    newStartSessionResult,

    -- ** StartTransactionRequest
    StartTransactionRequest (StartTransactionRequest'),
    newStartTransactionRequest,

    -- ** StartTransactionResult
    StartTransactionResult (StartTransactionResult'),
    newStartTransactionResult,

    -- ** TimingInformation
    TimingInformation (TimingInformation'),
    newTimingInformation,

    -- ** ValueHolder
    ValueHolder (ValueHolder'),
    newValueHolder,
  )
where

import Amazonka.QLDBSession.Lens
import Amazonka.QLDBSession.SendCommand
import Amazonka.QLDBSession.Types
import Amazonka.QLDBSession.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'QLDBSession'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

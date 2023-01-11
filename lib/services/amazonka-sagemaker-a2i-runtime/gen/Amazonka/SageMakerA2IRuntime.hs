{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SageMakerA2IRuntime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-11-07@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Augmented AI (Amazon A2I) adds the benefit of human judgment to
-- any machine learning application. When an AI application can\'t evaluate
-- data with a high degree of confidence, human reviewers can take over.
-- This human review is called a human review workflow. To create and start
-- a human review workflow, you need three resources: a /worker task
-- template/, a /flow definition/, and a /human loop/.
--
-- For information about these resources and prerequisites for using Amazon
-- A2I, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-getting-started.html Get Started with Amazon Augmented AI>
-- in the Amazon SageMaker Developer Guide.
--
-- This API reference includes information about API actions and data types
-- that you can use to interact with Amazon A2I programmatically. Use this
-- guide to:
--
-- -   Start a human loop with the @StartHumanLoop@ operation when using
--     Amazon A2I with a /custom task type/. To learn more about the
--     difference between custom and built-in task types, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-task-types-general.html Use Task Types>
--     . To learn how to start a human loop using this API, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-start-human-loop.html#a2i-instructions-starthumanloop Create and Start a Human Loop for a Custom Task Type>
--     in the Amazon SageMaker Developer Guide.
--
-- -   Manage your human loops. You can list all human loops that you have
--     created, describe individual human loops, and stop and delete human
--     loops. To learn more, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-monitor-humanloop-results.html Monitor and Manage Your Human Loop>
--     in the Amazon SageMaker Developer Guide.
--
-- Amazon A2I integrates APIs from various AWS services to create and start
-- human review workflows for those services. To learn how Amazon A2I uses
-- these APIs, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-api-references.html Use APIs in Amazon A2I>
-- in the Amazon SageMaker Developer Guide.
module Amazonka.SageMakerA2IRuntime
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteHumanLoop
    DeleteHumanLoop (DeleteHumanLoop'),
    newDeleteHumanLoop,
    DeleteHumanLoopResponse (DeleteHumanLoopResponse'),
    newDeleteHumanLoopResponse,

    -- ** DescribeHumanLoop
    DescribeHumanLoop (DescribeHumanLoop'),
    newDescribeHumanLoop,
    DescribeHumanLoopResponse (DescribeHumanLoopResponse'),
    newDescribeHumanLoopResponse,

    -- ** ListHumanLoops (Paginated)
    ListHumanLoops (ListHumanLoops'),
    newListHumanLoops,
    ListHumanLoopsResponse (ListHumanLoopsResponse'),
    newListHumanLoopsResponse,

    -- ** StartHumanLoop
    StartHumanLoop (StartHumanLoop'),
    newStartHumanLoop,
    StartHumanLoopResponse (StartHumanLoopResponse'),
    newStartHumanLoopResponse,

    -- ** StopHumanLoop
    StopHumanLoop (StopHumanLoop'),
    newStopHumanLoop,
    StopHumanLoopResponse (StopHumanLoopResponse'),
    newStopHumanLoopResponse,

    -- * Types

    -- ** ContentClassifier
    ContentClassifier (..),

    -- ** HumanLoopStatus
    HumanLoopStatus (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** HumanLoopDataAttributes
    HumanLoopDataAttributes (HumanLoopDataAttributes'),
    newHumanLoopDataAttributes,

    -- ** HumanLoopInput
    HumanLoopInput (HumanLoopInput'),
    newHumanLoopInput,

    -- ** HumanLoopOutput
    HumanLoopOutput (HumanLoopOutput'),
    newHumanLoopOutput,

    -- ** HumanLoopSummary
    HumanLoopSummary (HumanLoopSummary'),
    newHumanLoopSummary,
  )
where

import Amazonka.SageMakerA2IRuntime.DeleteHumanLoop
import Amazonka.SageMakerA2IRuntime.DescribeHumanLoop
import Amazonka.SageMakerA2IRuntime.Lens
import Amazonka.SageMakerA2IRuntime.ListHumanLoops
import Amazonka.SageMakerA2IRuntime.StartHumanLoop
import Amazonka.SageMakerA2IRuntime.StopHumanLoop
import Amazonka.SageMakerA2IRuntime.Types
import Amazonka.SageMakerA2IRuntime.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SageMakerA2IRuntime'.

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

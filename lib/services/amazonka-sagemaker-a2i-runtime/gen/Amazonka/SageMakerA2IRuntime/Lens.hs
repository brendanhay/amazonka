{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerA2IRuntime.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerA2IRuntime.Lens
  ( -- * Operations

    -- ** DeleteHumanLoop
    deleteHumanLoop_humanLoopName,
    deleteHumanLoopResponse_httpStatus,

    -- ** DescribeHumanLoop
    describeHumanLoop_humanLoopName,
    describeHumanLoopResponse_failureCode,
    describeHumanLoopResponse_failureReason,
    describeHumanLoopResponse_humanLoopOutput,
    describeHumanLoopResponse_httpStatus,
    describeHumanLoopResponse_creationTime,
    describeHumanLoopResponse_humanLoopStatus,
    describeHumanLoopResponse_humanLoopName,
    describeHumanLoopResponse_humanLoopArn,
    describeHumanLoopResponse_flowDefinitionArn,

    -- ** ListHumanLoops
    listHumanLoops_creationTimeAfter,
    listHumanLoops_creationTimeBefore,
    listHumanLoops_maxResults,
    listHumanLoops_nextToken,
    listHumanLoops_sortOrder,
    listHumanLoops_flowDefinitionArn,
    listHumanLoopsResponse_nextToken,
    listHumanLoopsResponse_httpStatus,
    listHumanLoopsResponse_humanLoopSummaries,

    -- ** StartHumanLoop
    startHumanLoop_dataAttributes,
    startHumanLoop_humanLoopName,
    startHumanLoop_flowDefinitionArn,
    startHumanLoop_humanLoopInput,
    startHumanLoopResponse_humanLoopArn,
    startHumanLoopResponse_httpStatus,

    -- ** StopHumanLoop
    stopHumanLoop_humanLoopName,
    stopHumanLoopResponse_httpStatus,

    -- * Types

    -- ** HumanLoopDataAttributes
    humanLoopDataAttributes_contentClassifiers,

    -- ** HumanLoopInput
    humanLoopInput_inputContent,

    -- ** HumanLoopOutput
    humanLoopOutput_outputS3Uri,

    -- ** HumanLoopSummary
    humanLoopSummary_creationTime,
    humanLoopSummary_failureReason,
    humanLoopSummary_flowDefinitionArn,
    humanLoopSummary_humanLoopName,
    humanLoopSummary_humanLoopStatus,
  )
where

import Amazonka.SageMakerA2IRuntime.DeleteHumanLoop
import Amazonka.SageMakerA2IRuntime.DescribeHumanLoop
import Amazonka.SageMakerA2IRuntime.ListHumanLoops
import Amazonka.SageMakerA2IRuntime.StartHumanLoop
import Amazonka.SageMakerA2IRuntime.StopHumanLoop
import Amazonka.SageMakerA2IRuntime.Types.HumanLoopDataAttributes
import Amazonka.SageMakerA2IRuntime.Types.HumanLoopInput
import Amazonka.SageMakerA2IRuntime.Types.HumanLoopOutput
import Amazonka.SageMakerA2IRuntime.Types.HumanLoopSummary

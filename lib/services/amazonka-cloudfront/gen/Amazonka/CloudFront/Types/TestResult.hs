{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFront.Types.TestResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.TestResult where

import Amazonka.CloudFront.Types.FunctionSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the result of testing a CloudFront function with
-- @TestFunction@.
--
-- /See:/ 'newTestResult' smart constructor.
data TestResult = TestResult'
  { -- | The amount of time that the function took to run as a percentage of the
    -- maximum allowed time. For example, a compute utilization of 35 means
    -- that the function completed in 35% of the maximum allowed time.
    computeUtilization :: Prelude.Maybe Prelude.Text,
    -- | Contains the log lines that the function wrote (if any) when running the
    -- test.
    functionExecutionLogs :: Prelude.Maybe [Prelude.Text],
    -- | The event object returned by the function. For more information about
    -- the structure of the event object, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/functions-event-structure.html Event object structure>
    -- in the /Amazon CloudFront Developer Guide/.
    functionOutput :: Prelude.Maybe Prelude.Text,
    -- | Contains configuration information and metadata about the CloudFront
    -- function that was tested.
    functionSummary :: Prelude.Maybe FunctionSummary,
    -- | If the result of testing the function was an error, this field contains
    -- the error message.
    functionErrorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeUtilization', 'testResult_computeUtilization' - The amount of time that the function took to run as a percentage of the
-- maximum allowed time. For example, a compute utilization of 35 means
-- that the function completed in 35% of the maximum allowed time.
--
-- 'functionExecutionLogs', 'testResult_functionExecutionLogs' - Contains the log lines that the function wrote (if any) when running the
-- test.
--
-- 'functionOutput', 'testResult_functionOutput' - The event object returned by the function. For more information about
-- the structure of the event object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/functions-event-structure.html Event object structure>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'functionSummary', 'testResult_functionSummary' - Contains configuration information and metadata about the CloudFront
-- function that was tested.
--
-- 'functionErrorMessage', 'testResult_functionErrorMessage' - If the result of testing the function was an error, this field contains
-- the error message.
newTestResult ::
  TestResult
newTestResult =
  TestResult'
    { computeUtilization = Prelude.Nothing,
      functionExecutionLogs = Prelude.Nothing,
      functionOutput = Prelude.Nothing,
      functionSummary = Prelude.Nothing,
      functionErrorMessage = Prelude.Nothing
    }

-- | The amount of time that the function took to run as a percentage of the
-- maximum allowed time. For example, a compute utilization of 35 means
-- that the function completed in 35% of the maximum allowed time.
testResult_computeUtilization :: Lens.Lens' TestResult (Prelude.Maybe Prelude.Text)
testResult_computeUtilization = Lens.lens (\TestResult' {computeUtilization} -> computeUtilization) (\s@TestResult' {} a -> s {computeUtilization = a} :: TestResult)

-- | Contains the log lines that the function wrote (if any) when running the
-- test.
testResult_functionExecutionLogs :: Lens.Lens' TestResult (Prelude.Maybe [Prelude.Text])
testResult_functionExecutionLogs = Lens.lens (\TestResult' {functionExecutionLogs} -> functionExecutionLogs) (\s@TestResult' {} a -> s {functionExecutionLogs = a} :: TestResult) Prelude.. Lens.mapping Lens.coerced

-- | The event object returned by the function. For more information about
-- the structure of the event object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/functions-event-structure.html Event object structure>
-- in the /Amazon CloudFront Developer Guide/.
testResult_functionOutput :: Lens.Lens' TestResult (Prelude.Maybe Prelude.Text)
testResult_functionOutput = Lens.lens (\TestResult' {functionOutput} -> functionOutput) (\s@TestResult' {} a -> s {functionOutput = a} :: TestResult)

-- | Contains configuration information and metadata about the CloudFront
-- function that was tested.
testResult_functionSummary :: Lens.Lens' TestResult (Prelude.Maybe FunctionSummary)
testResult_functionSummary = Lens.lens (\TestResult' {functionSummary} -> functionSummary) (\s@TestResult' {} a -> s {functionSummary = a} :: TestResult)

-- | If the result of testing the function was an error, this field contains
-- the error message.
testResult_functionErrorMessage :: Lens.Lens' TestResult (Prelude.Maybe Prelude.Text)
testResult_functionErrorMessage = Lens.lens (\TestResult' {functionErrorMessage} -> functionErrorMessage) (\s@TestResult' {} a -> s {functionErrorMessage = a} :: TestResult)

instance Core.FromXML TestResult where
  parseXML x =
    TestResult'
      Prelude.<$> (x Core..@? "ComputeUtilization")
      Prelude.<*> ( x Core..@? "FunctionExecutionLogs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "FunctionOutput")
      Prelude.<*> (x Core..@? "FunctionSummary")
      Prelude.<*> (x Core..@? "FunctionErrorMessage")

instance Prelude.Hashable TestResult where
  hashWithSalt _salt TestResult' {..} =
    _salt `Prelude.hashWithSalt` computeUtilization
      `Prelude.hashWithSalt` functionExecutionLogs
      `Prelude.hashWithSalt` functionOutput
      `Prelude.hashWithSalt` functionSummary
      `Prelude.hashWithSalt` functionErrorMessage

instance Prelude.NFData TestResult where
  rnf TestResult' {..} =
    Prelude.rnf computeUtilization
      `Prelude.seq` Prelude.rnf functionExecutionLogs
      `Prelude.seq` Prelude.rnf functionOutput
      `Prelude.seq` Prelude.rnf functionSummary
      `Prelude.seq` Prelude.rnf functionErrorMessage

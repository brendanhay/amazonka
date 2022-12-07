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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.TestResult where

import Amazonka.CloudFront.Types.FunctionSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the result of testing a CloudFront function with
-- @TestFunction@.
--
-- /See:/ 'newTestResult' smart constructor.
data TestResult = TestResult'
  { -- | Contains configuration information and metadata about the CloudFront
    -- function that was tested.
    functionSummary :: Prelude.Maybe FunctionSummary,
    -- | The amount of time that the function took to run as a percentage of the
    -- maximum allowed time. For example, a compute utilization of 35 means
    -- that the function completed in 35% of the maximum allowed time.
    computeUtilization :: Prelude.Maybe Prelude.Text,
    -- | The event object returned by the function. For more information about
    -- the structure of the event object, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/functions-event-structure.html Event object structure>
    -- in the /Amazon CloudFront Developer Guide/.
    functionOutput :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | If the result of testing the function was an error, this field contains
    -- the error message.
    functionErrorMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Contains the log lines that the function wrote (if any) when running the
    -- test.
    functionExecutionLogs :: Prelude.Maybe (Data.Sensitive [Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionSummary', 'testResult_functionSummary' - Contains configuration information and metadata about the CloudFront
-- function that was tested.
--
-- 'computeUtilization', 'testResult_computeUtilization' - The amount of time that the function took to run as a percentage of the
-- maximum allowed time. For example, a compute utilization of 35 means
-- that the function completed in 35% of the maximum allowed time.
--
-- 'functionOutput', 'testResult_functionOutput' - The event object returned by the function. For more information about
-- the structure of the event object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/functions-event-structure.html Event object structure>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'functionErrorMessage', 'testResult_functionErrorMessage' - If the result of testing the function was an error, this field contains
-- the error message.
--
-- 'functionExecutionLogs', 'testResult_functionExecutionLogs' - Contains the log lines that the function wrote (if any) when running the
-- test.
newTestResult ::
  TestResult
newTestResult =
  TestResult'
    { functionSummary = Prelude.Nothing,
      computeUtilization = Prelude.Nothing,
      functionOutput = Prelude.Nothing,
      functionErrorMessage = Prelude.Nothing,
      functionExecutionLogs = Prelude.Nothing
    }

-- | Contains configuration information and metadata about the CloudFront
-- function that was tested.
testResult_functionSummary :: Lens.Lens' TestResult (Prelude.Maybe FunctionSummary)
testResult_functionSummary = Lens.lens (\TestResult' {functionSummary} -> functionSummary) (\s@TestResult' {} a -> s {functionSummary = a} :: TestResult)

-- | The amount of time that the function took to run as a percentage of the
-- maximum allowed time. For example, a compute utilization of 35 means
-- that the function completed in 35% of the maximum allowed time.
testResult_computeUtilization :: Lens.Lens' TestResult (Prelude.Maybe Prelude.Text)
testResult_computeUtilization = Lens.lens (\TestResult' {computeUtilization} -> computeUtilization) (\s@TestResult' {} a -> s {computeUtilization = a} :: TestResult)

-- | The event object returned by the function. For more information about
-- the structure of the event object, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/functions-event-structure.html Event object structure>
-- in the /Amazon CloudFront Developer Guide/.
testResult_functionOutput :: Lens.Lens' TestResult (Prelude.Maybe Prelude.Text)
testResult_functionOutput = Lens.lens (\TestResult' {functionOutput} -> functionOutput) (\s@TestResult' {} a -> s {functionOutput = a} :: TestResult) Prelude.. Lens.mapping Data._Sensitive

-- | If the result of testing the function was an error, this field contains
-- the error message.
testResult_functionErrorMessage :: Lens.Lens' TestResult (Prelude.Maybe Prelude.Text)
testResult_functionErrorMessage = Lens.lens (\TestResult' {functionErrorMessage} -> functionErrorMessage) (\s@TestResult' {} a -> s {functionErrorMessage = a} :: TestResult) Prelude.. Lens.mapping Data._Sensitive

-- | Contains the log lines that the function wrote (if any) when running the
-- test.
testResult_functionExecutionLogs :: Lens.Lens' TestResult (Prelude.Maybe [Prelude.Text])
testResult_functionExecutionLogs = Lens.lens (\TestResult' {functionExecutionLogs} -> functionExecutionLogs) (\s@TestResult' {} a -> s {functionExecutionLogs = a} :: TestResult) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromXML TestResult where
  parseXML x =
    TestResult'
      Prelude.<$> (x Data..@? "FunctionSummary")
      Prelude.<*> (x Data..@? "ComputeUtilization")
      Prelude.<*> (x Data..@? "FunctionOutput")
      Prelude.<*> (x Data..@? "FunctionErrorMessage")
      Prelude.<*> ( x Data..@? "FunctionExecutionLogs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Prelude.fmap
                            (Prelude.fmap Data.Sensitive)
                            (Data.parseXMLList "member")
                        )
                  )

instance Prelude.Hashable TestResult where
  hashWithSalt _salt TestResult' {..} =
    _salt `Prelude.hashWithSalt` functionSummary
      `Prelude.hashWithSalt` computeUtilization
      `Prelude.hashWithSalt` functionOutput
      `Prelude.hashWithSalt` functionErrorMessage
      `Prelude.hashWithSalt` functionExecutionLogs

instance Prelude.NFData TestResult where
  rnf TestResult' {..} =
    Prelude.rnf functionSummary
      `Prelude.seq` Prelude.rnf computeUtilization
      `Prelude.seq` Prelude.rnf functionOutput
      `Prelude.seq` Prelude.rnf functionErrorMessage
      `Prelude.seq` Prelude.rnf functionExecutionLogs

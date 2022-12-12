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
-- Module      : Amazonka.CodePipeline.Types.ExecutionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ExecutionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the actions taken and results produced on an artifact as
-- it passes through stages in the pipeline.
--
-- /See:/ 'newExecutionDetails' smart constructor.
data ExecutionDetails = ExecutionDetails'
  { -- | The system-generated unique ID of this action used to identify this job
    -- worker in any external systems, such as AWS CodeDeploy.
    externalExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The percentage of work completed on the action, represented on a scale
    -- of 0 to 100 percent.
    percentComplete :: Prelude.Maybe Prelude.Natural,
    -- | The summary of the current status of the actions.
    summary :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalExecutionId', 'executionDetails_externalExecutionId' - The system-generated unique ID of this action used to identify this job
-- worker in any external systems, such as AWS CodeDeploy.
--
-- 'percentComplete', 'executionDetails_percentComplete' - The percentage of work completed on the action, represented on a scale
-- of 0 to 100 percent.
--
-- 'summary', 'executionDetails_summary' - The summary of the current status of the actions.
newExecutionDetails ::
  ExecutionDetails
newExecutionDetails =
  ExecutionDetails'
    { externalExecutionId =
        Prelude.Nothing,
      percentComplete = Prelude.Nothing,
      summary = Prelude.Nothing
    }

-- | The system-generated unique ID of this action used to identify this job
-- worker in any external systems, such as AWS CodeDeploy.
executionDetails_externalExecutionId :: Lens.Lens' ExecutionDetails (Prelude.Maybe Prelude.Text)
executionDetails_externalExecutionId = Lens.lens (\ExecutionDetails' {externalExecutionId} -> externalExecutionId) (\s@ExecutionDetails' {} a -> s {externalExecutionId = a} :: ExecutionDetails)

-- | The percentage of work completed on the action, represented on a scale
-- of 0 to 100 percent.
executionDetails_percentComplete :: Lens.Lens' ExecutionDetails (Prelude.Maybe Prelude.Natural)
executionDetails_percentComplete = Lens.lens (\ExecutionDetails' {percentComplete} -> percentComplete) (\s@ExecutionDetails' {} a -> s {percentComplete = a} :: ExecutionDetails)

-- | The summary of the current status of the actions.
executionDetails_summary :: Lens.Lens' ExecutionDetails (Prelude.Maybe Prelude.Text)
executionDetails_summary = Lens.lens (\ExecutionDetails' {summary} -> summary) (\s@ExecutionDetails' {} a -> s {summary = a} :: ExecutionDetails)

instance Prelude.Hashable ExecutionDetails where
  hashWithSalt _salt ExecutionDetails' {..} =
    _salt `Prelude.hashWithSalt` externalExecutionId
      `Prelude.hashWithSalt` percentComplete
      `Prelude.hashWithSalt` summary

instance Prelude.NFData ExecutionDetails where
  rnf ExecutionDetails' {..} =
    Prelude.rnf externalExecutionId
      `Prelude.seq` Prelude.rnf percentComplete
      `Prelude.seq` Prelude.rnf summary

instance Data.ToJSON ExecutionDetails where
  toJSON ExecutionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("externalExecutionId" Data..=)
              Prelude.<$> externalExecutionId,
            ("percentComplete" Data..=)
              Prelude.<$> percentComplete,
            ("summary" Data..=) Prelude.<$> summary
          ]
      )

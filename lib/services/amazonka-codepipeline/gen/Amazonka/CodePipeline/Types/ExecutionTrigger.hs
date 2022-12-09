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
-- Module      : Amazonka.CodePipeline.Types.ExecutionTrigger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ExecutionTrigger where

import Amazonka.CodePipeline.Types.TriggerType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The interaction or event that started a pipeline execution.
--
-- /See:/ 'newExecutionTrigger' smart constructor.
data ExecutionTrigger = ExecutionTrigger'
  { -- | Detail related to the event that started a pipeline execution, such as
    -- the webhook ARN of the webhook that triggered the pipeline execution or
    -- the user ARN for a user-initiated @start-pipeline-execution@ CLI
    -- command.
    triggerDetail :: Prelude.Maybe Prelude.Text,
    -- | The type of change-detection method, command, or user interaction that
    -- started a pipeline execution.
    triggerType :: Prelude.Maybe TriggerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggerDetail', 'executionTrigger_triggerDetail' - Detail related to the event that started a pipeline execution, such as
-- the webhook ARN of the webhook that triggered the pipeline execution or
-- the user ARN for a user-initiated @start-pipeline-execution@ CLI
-- command.
--
-- 'triggerType', 'executionTrigger_triggerType' - The type of change-detection method, command, or user interaction that
-- started a pipeline execution.
newExecutionTrigger ::
  ExecutionTrigger
newExecutionTrigger =
  ExecutionTrigger'
    { triggerDetail = Prelude.Nothing,
      triggerType = Prelude.Nothing
    }

-- | Detail related to the event that started a pipeline execution, such as
-- the webhook ARN of the webhook that triggered the pipeline execution or
-- the user ARN for a user-initiated @start-pipeline-execution@ CLI
-- command.
executionTrigger_triggerDetail :: Lens.Lens' ExecutionTrigger (Prelude.Maybe Prelude.Text)
executionTrigger_triggerDetail = Lens.lens (\ExecutionTrigger' {triggerDetail} -> triggerDetail) (\s@ExecutionTrigger' {} a -> s {triggerDetail = a} :: ExecutionTrigger)

-- | The type of change-detection method, command, or user interaction that
-- started a pipeline execution.
executionTrigger_triggerType :: Lens.Lens' ExecutionTrigger (Prelude.Maybe TriggerType)
executionTrigger_triggerType = Lens.lens (\ExecutionTrigger' {triggerType} -> triggerType) (\s@ExecutionTrigger' {} a -> s {triggerType = a} :: ExecutionTrigger)

instance Data.FromJSON ExecutionTrigger where
  parseJSON =
    Data.withObject
      "ExecutionTrigger"
      ( \x ->
          ExecutionTrigger'
            Prelude.<$> (x Data..:? "triggerDetail")
            Prelude.<*> (x Data..:? "triggerType")
      )

instance Prelude.Hashable ExecutionTrigger where
  hashWithSalt _salt ExecutionTrigger' {..} =
    _salt `Prelude.hashWithSalt` triggerDetail
      `Prelude.hashWithSalt` triggerType

instance Prelude.NFData ExecutionTrigger where
  rnf ExecutionTrigger' {..} =
    Prelude.rnf triggerDetail
      `Prelude.seq` Prelude.rnf triggerType

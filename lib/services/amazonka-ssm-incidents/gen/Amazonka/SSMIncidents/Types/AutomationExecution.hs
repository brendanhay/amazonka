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
-- Module      : Amazonka.SSMIncidents.Types.AutomationExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.AutomationExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Systems Manager automation document process to start as the runbook
-- at the beginning of the incident.
--
-- /See:/ 'newAutomationExecution' smart constructor.
data AutomationExecution = AutomationExecution'
  { -- | The Amazon Resource Name (ARN) of the automation process.
    ssmExecutionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomationExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssmExecutionArn', 'automationExecution_ssmExecutionArn' - The Amazon Resource Name (ARN) of the automation process.
newAutomationExecution ::
  AutomationExecution
newAutomationExecution =
  AutomationExecution'
    { ssmExecutionArn =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the automation process.
automationExecution_ssmExecutionArn :: Lens.Lens' AutomationExecution (Prelude.Maybe Prelude.Text)
automationExecution_ssmExecutionArn = Lens.lens (\AutomationExecution' {ssmExecutionArn} -> ssmExecutionArn) (\s@AutomationExecution' {} a -> s {ssmExecutionArn = a} :: AutomationExecution)

instance Data.FromJSON AutomationExecution where
  parseJSON =
    Data.withObject
      "AutomationExecution"
      ( \x ->
          AutomationExecution'
            Prelude.<$> (x Data..:? "ssmExecutionArn")
      )

instance Prelude.Hashable AutomationExecution where
  hashWithSalt _salt AutomationExecution' {..} =
    _salt `Prelude.hashWithSalt` ssmExecutionArn

instance Prelude.NFData AutomationExecution where
  rnf AutomationExecution' {..} =
    Prelude.rnf ssmExecutionArn

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
-- Module      : Amazonka.SSM.Types.TargetLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.TargetLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.AlarmConfiguration

-- | The combination of Amazon Web Services Regions and Amazon Web Services
-- accounts targeted by the current Automation execution.
--
-- /See:/ 'newTargetLocation' smart constructor.
data TargetLocation = TargetLocation'
  { targetLocationAlarmConfiguration :: Prelude.Maybe AlarmConfiguration,
    -- | The Amazon Web Services Regions targeted by the current Automation
    -- execution.
    regions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of Amazon Web Services Regions and Amazon Web
    -- Services accounts allowed to run the Automation concurrently.
    targetLocationMaxConcurrency :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services accounts targeted by the current Automation
    -- execution.
    accounts :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Automation execution role used by the currently running Automation.
    -- If not specified, the default value is
    -- @AWS-SystemsManager-AutomationExecutionRole@.
    executionRoleName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of errors allowed before the system stops queueing
    -- additional Automation executions for the currently running Automation.
    targetLocationMaxErrors :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetLocationAlarmConfiguration', 'targetLocation_targetLocationAlarmConfiguration' - Undocumented member.
--
-- 'regions', 'targetLocation_regions' - The Amazon Web Services Regions targeted by the current Automation
-- execution.
--
-- 'targetLocationMaxConcurrency', 'targetLocation_targetLocationMaxConcurrency' - The maximum number of Amazon Web Services Regions and Amazon Web
-- Services accounts allowed to run the Automation concurrently.
--
-- 'accounts', 'targetLocation_accounts' - The Amazon Web Services accounts targeted by the current Automation
-- execution.
--
-- 'executionRoleName', 'targetLocation_executionRoleName' - The Automation execution role used by the currently running Automation.
-- If not specified, the default value is
-- @AWS-SystemsManager-AutomationExecutionRole@.
--
-- 'targetLocationMaxErrors', 'targetLocation_targetLocationMaxErrors' - The maximum number of errors allowed before the system stops queueing
-- additional Automation executions for the currently running Automation.
newTargetLocation ::
  TargetLocation
newTargetLocation =
  TargetLocation'
    { targetLocationAlarmConfiguration =
        Prelude.Nothing,
      regions = Prelude.Nothing,
      targetLocationMaxConcurrency = Prelude.Nothing,
      accounts = Prelude.Nothing,
      executionRoleName = Prelude.Nothing,
      targetLocationMaxErrors = Prelude.Nothing
    }

-- | Undocumented member.
targetLocation_targetLocationAlarmConfiguration :: Lens.Lens' TargetLocation (Prelude.Maybe AlarmConfiguration)
targetLocation_targetLocationAlarmConfiguration = Lens.lens (\TargetLocation' {targetLocationAlarmConfiguration} -> targetLocationAlarmConfiguration) (\s@TargetLocation' {} a -> s {targetLocationAlarmConfiguration = a} :: TargetLocation)

-- | The Amazon Web Services Regions targeted by the current Automation
-- execution.
targetLocation_regions :: Lens.Lens' TargetLocation (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
targetLocation_regions = Lens.lens (\TargetLocation' {regions} -> regions) (\s@TargetLocation' {} a -> s {regions = a} :: TargetLocation) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of Amazon Web Services Regions and Amazon Web
-- Services accounts allowed to run the Automation concurrently.
targetLocation_targetLocationMaxConcurrency :: Lens.Lens' TargetLocation (Prelude.Maybe Prelude.Text)
targetLocation_targetLocationMaxConcurrency = Lens.lens (\TargetLocation' {targetLocationMaxConcurrency} -> targetLocationMaxConcurrency) (\s@TargetLocation' {} a -> s {targetLocationMaxConcurrency = a} :: TargetLocation)

-- | The Amazon Web Services accounts targeted by the current Automation
-- execution.
targetLocation_accounts :: Lens.Lens' TargetLocation (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
targetLocation_accounts = Lens.lens (\TargetLocation' {accounts} -> accounts) (\s@TargetLocation' {} a -> s {accounts = a} :: TargetLocation) Prelude.. Lens.mapping Lens.coerced

-- | The Automation execution role used by the currently running Automation.
-- If not specified, the default value is
-- @AWS-SystemsManager-AutomationExecutionRole@.
targetLocation_executionRoleName :: Lens.Lens' TargetLocation (Prelude.Maybe Prelude.Text)
targetLocation_executionRoleName = Lens.lens (\TargetLocation' {executionRoleName} -> executionRoleName) (\s@TargetLocation' {} a -> s {executionRoleName = a} :: TargetLocation)

-- | The maximum number of errors allowed before the system stops queueing
-- additional Automation executions for the currently running Automation.
targetLocation_targetLocationMaxErrors :: Lens.Lens' TargetLocation (Prelude.Maybe Prelude.Text)
targetLocation_targetLocationMaxErrors = Lens.lens (\TargetLocation' {targetLocationMaxErrors} -> targetLocationMaxErrors) (\s@TargetLocation' {} a -> s {targetLocationMaxErrors = a} :: TargetLocation)

instance Core.FromJSON TargetLocation where
  parseJSON =
    Core.withObject
      "TargetLocation"
      ( \x ->
          TargetLocation'
            Prelude.<$> (x Core..:? "TargetLocationAlarmConfiguration")
            Prelude.<*> (x Core..:? "Regions")
            Prelude.<*> (x Core..:? "TargetLocationMaxConcurrency")
            Prelude.<*> (x Core..:? "Accounts")
            Prelude.<*> (x Core..:? "ExecutionRoleName")
            Prelude.<*> (x Core..:? "TargetLocationMaxErrors")
      )

instance Prelude.Hashable TargetLocation where
  hashWithSalt _salt TargetLocation' {..} =
    _salt
      `Prelude.hashWithSalt` targetLocationAlarmConfiguration
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` targetLocationMaxConcurrency
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` executionRoleName
      `Prelude.hashWithSalt` targetLocationMaxErrors

instance Prelude.NFData TargetLocation where
  rnf TargetLocation' {..} =
    Prelude.rnf targetLocationAlarmConfiguration
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf targetLocationMaxConcurrency
      `Prelude.seq` Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf executionRoleName
      `Prelude.seq` Prelude.rnf targetLocationMaxErrors

instance Core.ToJSON TargetLocation where
  toJSON TargetLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetLocationAlarmConfiguration" Core..=)
              Prelude.<$> targetLocationAlarmConfiguration,
            ("Regions" Core..=) Prelude.<$> regions,
            ("TargetLocationMaxConcurrency" Core..=)
              Prelude.<$> targetLocationMaxConcurrency,
            ("Accounts" Core..=) Prelude.<$> accounts,
            ("ExecutionRoleName" Core..=)
              Prelude.<$> executionRoleName,
            ("TargetLocationMaxErrors" Core..=)
              Prelude.<$> targetLocationMaxErrors
          ]
      )

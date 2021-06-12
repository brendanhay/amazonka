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
-- Module      : Network.AWS.SSM.Types.TargetLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.TargetLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The combination of AWS Regions and accounts targeted by the current
-- Automation execution.
--
-- /See:/ 'newTargetLocation' smart constructor.
data TargetLocation = TargetLocation'
  { -- | The Automation execution role used by the currently running Automation.
    -- If not specified, the default value is
    -- @AWS-SystemsManager-AutomationExecutionRole@.
    executionRoleName :: Core.Maybe Core.Text,
    -- | The AWS accounts targeted by the current Automation execution.
    accounts :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The AWS Regions targeted by the current Automation execution.
    regions :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The maximum number of errors allowed before the system stops queueing
    -- additional Automation executions for the currently running Automation.
    targetLocationMaxErrors :: Core.Maybe Core.Text,
    -- | The maximum number of AWS accounts and AWS regions allowed to run the
    -- Automation concurrently.
    targetLocationMaxConcurrency :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionRoleName', 'targetLocation_executionRoleName' - The Automation execution role used by the currently running Automation.
-- If not specified, the default value is
-- @AWS-SystemsManager-AutomationExecutionRole@.
--
-- 'accounts', 'targetLocation_accounts' - The AWS accounts targeted by the current Automation execution.
--
-- 'regions', 'targetLocation_regions' - The AWS Regions targeted by the current Automation execution.
--
-- 'targetLocationMaxErrors', 'targetLocation_targetLocationMaxErrors' - The maximum number of errors allowed before the system stops queueing
-- additional Automation executions for the currently running Automation.
--
-- 'targetLocationMaxConcurrency', 'targetLocation_targetLocationMaxConcurrency' - The maximum number of AWS accounts and AWS regions allowed to run the
-- Automation concurrently.
newTargetLocation ::
  TargetLocation
newTargetLocation =
  TargetLocation'
    { executionRoleName = Core.Nothing,
      accounts = Core.Nothing,
      regions = Core.Nothing,
      targetLocationMaxErrors = Core.Nothing,
      targetLocationMaxConcurrency = Core.Nothing
    }

-- | The Automation execution role used by the currently running Automation.
-- If not specified, the default value is
-- @AWS-SystemsManager-AutomationExecutionRole@.
targetLocation_executionRoleName :: Lens.Lens' TargetLocation (Core.Maybe Core.Text)
targetLocation_executionRoleName = Lens.lens (\TargetLocation' {executionRoleName} -> executionRoleName) (\s@TargetLocation' {} a -> s {executionRoleName = a} :: TargetLocation)

-- | The AWS accounts targeted by the current Automation execution.
targetLocation_accounts :: Lens.Lens' TargetLocation (Core.Maybe (Core.NonEmpty Core.Text))
targetLocation_accounts = Lens.lens (\TargetLocation' {accounts} -> accounts) (\s@TargetLocation' {} a -> s {accounts = a} :: TargetLocation) Core.. Lens.mapping Lens._Coerce

-- | The AWS Regions targeted by the current Automation execution.
targetLocation_regions :: Lens.Lens' TargetLocation (Core.Maybe (Core.NonEmpty Core.Text))
targetLocation_regions = Lens.lens (\TargetLocation' {regions} -> regions) (\s@TargetLocation' {} a -> s {regions = a} :: TargetLocation) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of errors allowed before the system stops queueing
-- additional Automation executions for the currently running Automation.
targetLocation_targetLocationMaxErrors :: Lens.Lens' TargetLocation (Core.Maybe Core.Text)
targetLocation_targetLocationMaxErrors = Lens.lens (\TargetLocation' {targetLocationMaxErrors} -> targetLocationMaxErrors) (\s@TargetLocation' {} a -> s {targetLocationMaxErrors = a} :: TargetLocation)

-- | The maximum number of AWS accounts and AWS regions allowed to run the
-- Automation concurrently.
targetLocation_targetLocationMaxConcurrency :: Lens.Lens' TargetLocation (Core.Maybe Core.Text)
targetLocation_targetLocationMaxConcurrency = Lens.lens (\TargetLocation' {targetLocationMaxConcurrency} -> targetLocationMaxConcurrency) (\s@TargetLocation' {} a -> s {targetLocationMaxConcurrency = a} :: TargetLocation)

instance Core.FromJSON TargetLocation where
  parseJSON =
    Core.withObject
      "TargetLocation"
      ( \x ->
          TargetLocation'
            Core.<$> (x Core..:? "ExecutionRoleName")
            Core.<*> (x Core..:? "Accounts")
            Core.<*> (x Core..:? "Regions")
            Core.<*> (x Core..:? "TargetLocationMaxErrors")
            Core.<*> (x Core..:? "TargetLocationMaxConcurrency")
      )

instance Core.Hashable TargetLocation

instance Core.NFData TargetLocation

instance Core.ToJSON TargetLocation where
  toJSON TargetLocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExecutionRoleName" Core..=)
              Core.<$> executionRoleName,
            ("Accounts" Core..=) Core.<$> accounts,
            ("Regions" Core..=) Core.<$> regions,
            ("TargetLocationMaxErrors" Core..=)
              Core.<$> targetLocationMaxErrors,
            ("TargetLocationMaxConcurrency" Core..=)
              Core.<$> targetLocationMaxConcurrency
          ]
      )

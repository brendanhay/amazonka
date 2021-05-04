{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The combination of AWS Regions and accounts targeted by the current
-- Automation execution.
--
-- /See:/ 'newTargetLocation' smart constructor.
data TargetLocation = TargetLocation'
  { -- | The Automation execution role used by the currently running Automation.
    -- If not specified, the default value is
    -- @AWS-SystemsManager-AutomationExecutionRole@.
    executionRoleName :: Prelude.Maybe Prelude.Text,
    -- | The AWS accounts targeted by the current Automation execution.
    accounts :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The AWS Regions targeted by the current Automation execution.
    regions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of errors allowed before the system stops queueing
    -- additional Automation executions for the currently running Automation.
    targetLocationMaxErrors :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of AWS accounts and AWS regions allowed to run the
    -- Automation concurrently.
    targetLocationMaxConcurrency :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { executionRoleName =
        Prelude.Nothing,
      accounts = Prelude.Nothing,
      regions = Prelude.Nothing,
      targetLocationMaxErrors = Prelude.Nothing,
      targetLocationMaxConcurrency = Prelude.Nothing
    }

-- | The Automation execution role used by the currently running Automation.
-- If not specified, the default value is
-- @AWS-SystemsManager-AutomationExecutionRole@.
targetLocation_executionRoleName :: Lens.Lens' TargetLocation (Prelude.Maybe Prelude.Text)
targetLocation_executionRoleName = Lens.lens (\TargetLocation' {executionRoleName} -> executionRoleName) (\s@TargetLocation' {} a -> s {executionRoleName = a} :: TargetLocation)

-- | The AWS accounts targeted by the current Automation execution.
targetLocation_accounts :: Lens.Lens' TargetLocation (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
targetLocation_accounts = Lens.lens (\TargetLocation' {accounts} -> accounts) (\s@TargetLocation' {} a -> s {accounts = a} :: TargetLocation) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS Regions targeted by the current Automation execution.
targetLocation_regions :: Lens.Lens' TargetLocation (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
targetLocation_regions = Lens.lens (\TargetLocation' {regions} -> regions) (\s@TargetLocation' {} a -> s {regions = a} :: TargetLocation) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum number of errors allowed before the system stops queueing
-- additional Automation executions for the currently running Automation.
targetLocation_targetLocationMaxErrors :: Lens.Lens' TargetLocation (Prelude.Maybe Prelude.Text)
targetLocation_targetLocationMaxErrors = Lens.lens (\TargetLocation' {targetLocationMaxErrors} -> targetLocationMaxErrors) (\s@TargetLocation' {} a -> s {targetLocationMaxErrors = a} :: TargetLocation)

-- | The maximum number of AWS accounts and AWS regions allowed to run the
-- Automation concurrently.
targetLocation_targetLocationMaxConcurrency :: Lens.Lens' TargetLocation (Prelude.Maybe Prelude.Text)
targetLocation_targetLocationMaxConcurrency = Lens.lens (\TargetLocation' {targetLocationMaxConcurrency} -> targetLocationMaxConcurrency) (\s@TargetLocation' {} a -> s {targetLocationMaxConcurrency = a} :: TargetLocation)

instance Prelude.FromJSON TargetLocation where
  parseJSON =
    Prelude.withObject
      "TargetLocation"
      ( \x ->
          TargetLocation'
            Prelude.<$> (x Prelude..:? "ExecutionRoleName")
            Prelude.<*> (x Prelude..:? "Accounts")
            Prelude.<*> (x Prelude..:? "Regions")
            Prelude.<*> (x Prelude..:? "TargetLocationMaxErrors")
            Prelude.<*> (x Prelude..:? "TargetLocationMaxConcurrency")
      )

instance Prelude.Hashable TargetLocation

instance Prelude.NFData TargetLocation

instance Prelude.ToJSON TargetLocation where
  toJSON TargetLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExecutionRoleName" Prelude..=)
              Prelude.<$> executionRoleName,
            ("Accounts" Prelude..=) Prelude.<$> accounts,
            ("Regions" Prelude..=) Prelude.<$> regions,
            ("TargetLocationMaxErrors" Prelude..=)
              Prelude.<$> targetLocationMaxErrors,
            ("TargetLocationMaxConcurrency" Prelude..=)
              Prelude.<$> targetLocationMaxConcurrency
          ]
      )

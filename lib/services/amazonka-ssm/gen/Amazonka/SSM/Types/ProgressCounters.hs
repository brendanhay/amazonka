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
-- Module      : Amazonka.SSM.Types.ProgressCounters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ProgressCounters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An aggregate of step execution statuses displayed in the Amazon Web
-- Services Systems Manager console for a multi-Region and multi-account
-- Automation execution.
--
-- /See:/ 'newProgressCounters' smart constructor.
data ProgressCounters = ProgressCounters'
  { -- | The total number of steps that the system cancelled in all specified
    -- Amazon Web Services Regions and Amazon Web Services accounts for the
    -- current Automation execution.
    cancelledSteps :: Prelude.Maybe Prelude.Int,
    -- | The total number of steps that failed to run in all specified Amazon Web
    -- Services Regions and Amazon Web Services accounts for the current
    -- Automation execution.
    failedSteps :: Prelude.Maybe Prelude.Int,
    -- | The total number of steps that successfully completed in all specified
    -- Amazon Web Services Regions and Amazon Web Services accounts for the
    -- current Automation execution.
    successSteps :: Prelude.Maybe Prelude.Int,
    -- | The total number of steps that timed out in all specified Amazon Web
    -- Services Regions and Amazon Web Services accounts for the current
    -- Automation execution.
    timedOutSteps :: Prelude.Maybe Prelude.Int,
    -- | The total number of steps run in all specified Amazon Web Services
    -- Regions and Amazon Web Services accounts for the current Automation
    -- execution.
    totalSteps :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProgressCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelledSteps', 'progressCounters_cancelledSteps' - The total number of steps that the system cancelled in all specified
-- Amazon Web Services Regions and Amazon Web Services accounts for the
-- current Automation execution.
--
-- 'failedSteps', 'progressCounters_failedSteps' - The total number of steps that failed to run in all specified Amazon Web
-- Services Regions and Amazon Web Services accounts for the current
-- Automation execution.
--
-- 'successSteps', 'progressCounters_successSteps' - The total number of steps that successfully completed in all specified
-- Amazon Web Services Regions and Amazon Web Services accounts for the
-- current Automation execution.
--
-- 'timedOutSteps', 'progressCounters_timedOutSteps' - The total number of steps that timed out in all specified Amazon Web
-- Services Regions and Amazon Web Services accounts for the current
-- Automation execution.
--
-- 'totalSteps', 'progressCounters_totalSteps' - The total number of steps run in all specified Amazon Web Services
-- Regions and Amazon Web Services accounts for the current Automation
-- execution.
newProgressCounters ::
  ProgressCounters
newProgressCounters =
  ProgressCounters'
    { cancelledSteps = Prelude.Nothing,
      failedSteps = Prelude.Nothing,
      successSteps = Prelude.Nothing,
      timedOutSteps = Prelude.Nothing,
      totalSteps = Prelude.Nothing
    }

-- | The total number of steps that the system cancelled in all specified
-- Amazon Web Services Regions and Amazon Web Services accounts for the
-- current Automation execution.
progressCounters_cancelledSteps :: Lens.Lens' ProgressCounters (Prelude.Maybe Prelude.Int)
progressCounters_cancelledSteps = Lens.lens (\ProgressCounters' {cancelledSteps} -> cancelledSteps) (\s@ProgressCounters' {} a -> s {cancelledSteps = a} :: ProgressCounters)

-- | The total number of steps that failed to run in all specified Amazon Web
-- Services Regions and Amazon Web Services accounts for the current
-- Automation execution.
progressCounters_failedSteps :: Lens.Lens' ProgressCounters (Prelude.Maybe Prelude.Int)
progressCounters_failedSteps = Lens.lens (\ProgressCounters' {failedSteps} -> failedSteps) (\s@ProgressCounters' {} a -> s {failedSteps = a} :: ProgressCounters)

-- | The total number of steps that successfully completed in all specified
-- Amazon Web Services Regions and Amazon Web Services accounts for the
-- current Automation execution.
progressCounters_successSteps :: Lens.Lens' ProgressCounters (Prelude.Maybe Prelude.Int)
progressCounters_successSteps = Lens.lens (\ProgressCounters' {successSteps} -> successSteps) (\s@ProgressCounters' {} a -> s {successSteps = a} :: ProgressCounters)

-- | The total number of steps that timed out in all specified Amazon Web
-- Services Regions and Amazon Web Services accounts for the current
-- Automation execution.
progressCounters_timedOutSteps :: Lens.Lens' ProgressCounters (Prelude.Maybe Prelude.Int)
progressCounters_timedOutSteps = Lens.lens (\ProgressCounters' {timedOutSteps} -> timedOutSteps) (\s@ProgressCounters' {} a -> s {timedOutSteps = a} :: ProgressCounters)

-- | The total number of steps run in all specified Amazon Web Services
-- Regions and Amazon Web Services accounts for the current Automation
-- execution.
progressCounters_totalSteps :: Lens.Lens' ProgressCounters (Prelude.Maybe Prelude.Int)
progressCounters_totalSteps = Lens.lens (\ProgressCounters' {totalSteps} -> totalSteps) (\s@ProgressCounters' {} a -> s {totalSteps = a} :: ProgressCounters)

instance Data.FromJSON ProgressCounters where
  parseJSON =
    Data.withObject
      "ProgressCounters"
      ( \x ->
          ProgressCounters'
            Prelude.<$> (x Data..:? "CancelledSteps")
            Prelude.<*> (x Data..:? "FailedSteps")
            Prelude.<*> (x Data..:? "SuccessSteps")
            Prelude.<*> (x Data..:? "TimedOutSteps")
            Prelude.<*> (x Data..:? "TotalSteps")
      )

instance Prelude.Hashable ProgressCounters where
  hashWithSalt _salt ProgressCounters' {..} =
    _salt
      `Prelude.hashWithSalt` cancelledSteps
      `Prelude.hashWithSalt` failedSteps
      `Prelude.hashWithSalt` successSteps
      `Prelude.hashWithSalt` timedOutSteps
      `Prelude.hashWithSalt` totalSteps

instance Prelude.NFData ProgressCounters where
  rnf ProgressCounters' {..} =
    Prelude.rnf cancelledSteps `Prelude.seq`
      Prelude.rnf failedSteps `Prelude.seq`
        Prelude.rnf successSteps `Prelude.seq`
          Prelude.rnf timedOutSteps `Prelude.seq`
            Prelude.rnf totalSteps

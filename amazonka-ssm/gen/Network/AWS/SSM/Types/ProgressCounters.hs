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
-- Module      : Network.AWS.SSM.Types.ProgressCounters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ProgressCounters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An aggregate of step execution statuses displayed in the AWS Console for
-- a multi-Region and multi-account Automation execution.
--
-- /See:/ 'newProgressCounters' smart constructor.
data ProgressCounters = ProgressCounters'
  { -- | The total number of steps that the system cancelled in all specified AWS
    -- Regions and accounts for the current Automation execution.
    cancelledSteps :: Core.Maybe Core.Int,
    -- | The total number of steps that timed out in all specified AWS Regions
    -- and accounts for the current Automation execution.
    timedOutSteps :: Core.Maybe Core.Int,
    -- | The total number of steps run in all specified AWS Regions and accounts
    -- for the current Automation execution.
    totalSteps :: Core.Maybe Core.Int,
    -- | The total number of steps that successfully completed in all specified
    -- AWS Regions and accounts for the current Automation execution.
    successSteps :: Core.Maybe Core.Int,
    -- | The total number of steps that failed to run in all specified AWS
    -- Regions and accounts for the current Automation execution.
    failedSteps :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProgressCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelledSteps', 'progressCounters_cancelledSteps' - The total number of steps that the system cancelled in all specified AWS
-- Regions and accounts for the current Automation execution.
--
-- 'timedOutSteps', 'progressCounters_timedOutSteps' - The total number of steps that timed out in all specified AWS Regions
-- and accounts for the current Automation execution.
--
-- 'totalSteps', 'progressCounters_totalSteps' - The total number of steps run in all specified AWS Regions and accounts
-- for the current Automation execution.
--
-- 'successSteps', 'progressCounters_successSteps' - The total number of steps that successfully completed in all specified
-- AWS Regions and accounts for the current Automation execution.
--
-- 'failedSteps', 'progressCounters_failedSteps' - The total number of steps that failed to run in all specified AWS
-- Regions and accounts for the current Automation execution.
newProgressCounters ::
  ProgressCounters
newProgressCounters =
  ProgressCounters'
    { cancelledSteps = Core.Nothing,
      timedOutSteps = Core.Nothing,
      totalSteps = Core.Nothing,
      successSteps = Core.Nothing,
      failedSteps = Core.Nothing
    }

-- | The total number of steps that the system cancelled in all specified AWS
-- Regions and accounts for the current Automation execution.
progressCounters_cancelledSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
progressCounters_cancelledSteps = Lens.lens (\ProgressCounters' {cancelledSteps} -> cancelledSteps) (\s@ProgressCounters' {} a -> s {cancelledSteps = a} :: ProgressCounters)

-- | The total number of steps that timed out in all specified AWS Regions
-- and accounts for the current Automation execution.
progressCounters_timedOutSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
progressCounters_timedOutSteps = Lens.lens (\ProgressCounters' {timedOutSteps} -> timedOutSteps) (\s@ProgressCounters' {} a -> s {timedOutSteps = a} :: ProgressCounters)

-- | The total number of steps run in all specified AWS Regions and accounts
-- for the current Automation execution.
progressCounters_totalSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
progressCounters_totalSteps = Lens.lens (\ProgressCounters' {totalSteps} -> totalSteps) (\s@ProgressCounters' {} a -> s {totalSteps = a} :: ProgressCounters)

-- | The total number of steps that successfully completed in all specified
-- AWS Regions and accounts for the current Automation execution.
progressCounters_successSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
progressCounters_successSteps = Lens.lens (\ProgressCounters' {successSteps} -> successSteps) (\s@ProgressCounters' {} a -> s {successSteps = a} :: ProgressCounters)

-- | The total number of steps that failed to run in all specified AWS
-- Regions and accounts for the current Automation execution.
progressCounters_failedSteps :: Lens.Lens' ProgressCounters (Core.Maybe Core.Int)
progressCounters_failedSteps = Lens.lens (\ProgressCounters' {failedSteps} -> failedSteps) (\s@ProgressCounters' {} a -> s {failedSteps = a} :: ProgressCounters)

instance Core.FromJSON ProgressCounters where
  parseJSON =
    Core.withObject
      "ProgressCounters"
      ( \x ->
          ProgressCounters'
            Core.<$> (x Core..:? "CancelledSteps")
            Core.<*> (x Core..:? "TimedOutSteps")
            Core.<*> (x Core..:? "TotalSteps")
            Core.<*> (x Core..:? "SuccessSteps")
            Core.<*> (x Core..:? "FailedSteps")
      )

instance Core.Hashable ProgressCounters

instance Core.NFData ProgressCounters

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
-- Module      : Network.AWS.MediaConvert.Types.HopDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HopDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Optional. Configuration for a destination queue to which the job can hop
-- once a customer-defined minimum wait time has passed.
--
-- /See:/ 'newHopDestination' smart constructor.
data HopDestination = HopDestination'
  { -- | Optional. When you set up a job to use queue hopping, you can specify a
    -- different relative priority for the job in the destination queue. If you
    -- don\'t specify, the relative priority will remain the same as in the
    -- previous queue.
    priority :: Core.Maybe Core.Int,
    -- | Optional unless the job is submitted on the default queue. When you set
    -- up a job to use queue hopping, you can specify a destination queue. This
    -- queue cannot be the original queue to which the job is submitted. If the
    -- original queue isn\'t the default queue and you don\'t specify the
    -- destination queue, the job will move to the default queue.
    queue :: Core.Maybe Core.Text,
    -- | Required for setting up a job to use queue hopping. Minimum wait time in
    -- minutes until the job can hop to the destination queue. Valid range is 1
    -- to 1440 minutes, inclusive.
    waitMinutes :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HopDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'hopDestination_priority' - Optional. When you set up a job to use queue hopping, you can specify a
-- different relative priority for the job in the destination queue. If you
-- don\'t specify, the relative priority will remain the same as in the
-- previous queue.
--
-- 'queue', 'hopDestination_queue' - Optional unless the job is submitted on the default queue. When you set
-- up a job to use queue hopping, you can specify a destination queue. This
-- queue cannot be the original queue to which the job is submitted. If the
-- original queue isn\'t the default queue and you don\'t specify the
-- destination queue, the job will move to the default queue.
--
-- 'waitMinutes', 'hopDestination_waitMinutes' - Required for setting up a job to use queue hopping. Minimum wait time in
-- minutes until the job can hop to the destination queue. Valid range is 1
-- to 1440 minutes, inclusive.
newHopDestination ::
  HopDestination
newHopDestination =
  HopDestination'
    { priority = Core.Nothing,
      queue = Core.Nothing,
      waitMinutes = Core.Nothing
    }

-- | Optional. When you set up a job to use queue hopping, you can specify a
-- different relative priority for the job in the destination queue. If you
-- don\'t specify, the relative priority will remain the same as in the
-- previous queue.
hopDestination_priority :: Lens.Lens' HopDestination (Core.Maybe Core.Int)
hopDestination_priority = Lens.lens (\HopDestination' {priority} -> priority) (\s@HopDestination' {} a -> s {priority = a} :: HopDestination)

-- | Optional unless the job is submitted on the default queue. When you set
-- up a job to use queue hopping, you can specify a destination queue. This
-- queue cannot be the original queue to which the job is submitted. If the
-- original queue isn\'t the default queue and you don\'t specify the
-- destination queue, the job will move to the default queue.
hopDestination_queue :: Lens.Lens' HopDestination (Core.Maybe Core.Text)
hopDestination_queue = Lens.lens (\HopDestination' {queue} -> queue) (\s@HopDestination' {} a -> s {queue = a} :: HopDestination)

-- | Required for setting up a job to use queue hopping. Minimum wait time in
-- minutes until the job can hop to the destination queue. Valid range is 1
-- to 1440 minutes, inclusive.
hopDestination_waitMinutes :: Lens.Lens' HopDestination (Core.Maybe Core.Int)
hopDestination_waitMinutes = Lens.lens (\HopDestination' {waitMinutes} -> waitMinutes) (\s@HopDestination' {} a -> s {waitMinutes = a} :: HopDestination)

instance Core.FromJSON HopDestination where
  parseJSON =
    Core.withObject
      "HopDestination"
      ( \x ->
          HopDestination'
            Core.<$> (x Core..:? "priority")
            Core.<*> (x Core..:? "queue")
            Core.<*> (x Core..:? "waitMinutes")
      )

instance Core.Hashable HopDestination

instance Core.NFData HopDestination

instance Core.ToJSON HopDestination where
  toJSON HopDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("priority" Core..=) Core.<$> priority,
            ("queue" Core..=) Core.<$> queue,
            ("waitMinutes" Core..=) Core.<$> waitMinutes
          ]
      )

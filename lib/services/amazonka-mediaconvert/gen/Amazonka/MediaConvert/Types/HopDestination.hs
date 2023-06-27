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
-- Module      : Amazonka.MediaConvert.Types.HopDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HopDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optional. Configuration for a destination queue to which the job can hop
-- once a customer-defined minimum wait time has passed.
--
-- /See:/ 'newHopDestination' smart constructor.
data HopDestination = HopDestination'
  { -- | Optional. When you set up a job to use queue hopping, you can specify a
    -- different relative priority for the job in the destination queue. If you
    -- don\'t specify, the relative priority will remain the same as in the
    -- previous queue.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Optional unless the job is submitted on the default queue. When you set
    -- up a job to use queue hopping, you can specify a destination queue. This
    -- queue cannot be the original queue to which the job is submitted. If the
    -- original queue isn\'t the default queue and you don\'t specify the
    -- destination queue, the job will move to the default queue.
    queue :: Prelude.Maybe Prelude.Text,
    -- | Required for setting up a job to use queue hopping. Minimum wait time in
    -- minutes until the job can hop to the destination queue. Valid range is 1
    -- to 4320 minutes, inclusive.
    waitMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- to 4320 minutes, inclusive.
newHopDestination ::
  HopDestination
newHopDestination =
  HopDestination'
    { priority = Prelude.Nothing,
      queue = Prelude.Nothing,
      waitMinutes = Prelude.Nothing
    }

-- | Optional. When you set up a job to use queue hopping, you can specify a
-- different relative priority for the job in the destination queue. If you
-- don\'t specify, the relative priority will remain the same as in the
-- previous queue.
hopDestination_priority :: Lens.Lens' HopDestination (Prelude.Maybe Prelude.Int)
hopDestination_priority = Lens.lens (\HopDestination' {priority} -> priority) (\s@HopDestination' {} a -> s {priority = a} :: HopDestination)

-- | Optional unless the job is submitted on the default queue. When you set
-- up a job to use queue hopping, you can specify a destination queue. This
-- queue cannot be the original queue to which the job is submitted. If the
-- original queue isn\'t the default queue and you don\'t specify the
-- destination queue, the job will move to the default queue.
hopDestination_queue :: Lens.Lens' HopDestination (Prelude.Maybe Prelude.Text)
hopDestination_queue = Lens.lens (\HopDestination' {queue} -> queue) (\s@HopDestination' {} a -> s {queue = a} :: HopDestination)

-- | Required for setting up a job to use queue hopping. Minimum wait time in
-- minutes until the job can hop to the destination queue. Valid range is 1
-- to 4320 minutes, inclusive.
hopDestination_waitMinutes :: Lens.Lens' HopDestination (Prelude.Maybe Prelude.Int)
hopDestination_waitMinutes = Lens.lens (\HopDestination' {waitMinutes} -> waitMinutes) (\s@HopDestination' {} a -> s {waitMinutes = a} :: HopDestination)

instance Data.FromJSON HopDestination where
  parseJSON =
    Data.withObject
      "HopDestination"
      ( \x ->
          HopDestination'
            Prelude.<$> (x Data..:? "priority")
            Prelude.<*> (x Data..:? "queue")
            Prelude.<*> (x Data..:? "waitMinutes")
      )

instance Prelude.Hashable HopDestination where
  hashWithSalt _salt HopDestination' {..} =
    _salt
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` queue
      `Prelude.hashWithSalt` waitMinutes

instance Prelude.NFData HopDestination where
  rnf HopDestination' {..} =
    Prelude.rnf priority
      `Prelude.seq` Prelude.rnf queue
      `Prelude.seq` Prelude.rnf waitMinutes

instance Data.ToJSON HopDestination where
  toJSON HopDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("priority" Data..=) Prelude.<$> priority,
            ("queue" Data..=) Prelude.<$> queue,
            ("waitMinutes" Data..=) Prelude.<$> waitMinutes
          ]
      )

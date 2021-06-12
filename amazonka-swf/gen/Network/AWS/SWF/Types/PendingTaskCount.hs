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
-- Module      : Network.AWS.SWF.Types.PendingTaskCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.PendingTaskCount where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the count of tasks in a task list.
--
-- /See:/ 'newPendingTaskCount' smart constructor.
data PendingTaskCount = PendingTaskCount'
  { -- | If set to true, indicates that the actual count was more than the
    -- maximum supported by this API and the count returned is the truncated
    -- value.
    truncated :: Core.Maybe Core.Bool,
    -- | The number of tasks in the task list.
    count :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PendingTaskCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'truncated', 'pendingTaskCount_truncated' - If set to true, indicates that the actual count was more than the
-- maximum supported by this API and the count returned is the truncated
-- value.
--
-- 'count', 'pendingTaskCount_count' - The number of tasks in the task list.
newPendingTaskCount ::
  -- | 'count'
  Core.Natural ->
  PendingTaskCount
newPendingTaskCount pCount_ =
  PendingTaskCount'
    { truncated = Core.Nothing,
      count = pCount_
    }

-- | If set to true, indicates that the actual count was more than the
-- maximum supported by this API and the count returned is the truncated
-- value.
pendingTaskCount_truncated :: Lens.Lens' PendingTaskCount (Core.Maybe Core.Bool)
pendingTaskCount_truncated = Lens.lens (\PendingTaskCount' {truncated} -> truncated) (\s@PendingTaskCount' {} a -> s {truncated = a} :: PendingTaskCount)

-- | The number of tasks in the task list.
pendingTaskCount_count :: Lens.Lens' PendingTaskCount Core.Natural
pendingTaskCount_count = Lens.lens (\PendingTaskCount' {count} -> count) (\s@PendingTaskCount' {} a -> s {count = a} :: PendingTaskCount)

instance Core.FromJSON PendingTaskCount where
  parseJSON =
    Core.withObject
      "PendingTaskCount"
      ( \x ->
          PendingTaskCount'
            Core.<$> (x Core..:? "truncated")
            Core.<*> (x Core..: "count")
      )

instance Core.Hashable PendingTaskCount

instance Core.NFData PendingTaskCount

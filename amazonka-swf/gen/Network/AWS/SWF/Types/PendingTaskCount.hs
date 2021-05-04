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
-- Module      : Network.AWS.SWF.Types.PendingTaskCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.PendingTaskCount where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the count of tasks in a task list.
--
-- /See:/ 'newPendingTaskCount' smart constructor.
data PendingTaskCount = PendingTaskCount'
  { -- | If set to true, indicates that the actual count was more than the
    -- maximum supported by this API and the count returned is the truncated
    -- value.
    truncated :: Prelude.Maybe Prelude.Bool,
    -- | The number of tasks in the task list.
    count :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Natural ->
  PendingTaskCount
newPendingTaskCount pCount_ =
  PendingTaskCount'
    { truncated = Prelude.Nothing,
      count = pCount_
    }

-- | If set to true, indicates that the actual count was more than the
-- maximum supported by this API and the count returned is the truncated
-- value.
pendingTaskCount_truncated :: Lens.Lens' PendingTaskCount (Prelude.Maybe Prelude.Bool)
pendingTaskCount_truncated = Lens.lens (\PendingTaskCount' {truncated} -> truncated) (\s@PendingTaskCount' {} a -> s {truncated = a} :: PendingTaskCount)

-- | The number of tasks in the task list.
pendingTaskCount_count :: Lens.Lens' PendingTaskCount Prelude.Natural
pendingTaskCount_count = Lens.lens (\PendingTaskCount' {count} -> count) (\s@PendingTaskCount' {} a -> s {count = a} :: PendingTaskCount)

instance Prelude.FromJSON PendingTaskCount where
  parseJSON =
    Prelude.withObject
      "PendingTaskCount"
      ( \x ->
          PendingTaskCount'
            Prelude.<$> (x Prelude..:? "truncated")
            Prelude.<*> (x Prelude..: "count")
      )

instance Prelude.Hashable PendingTaskCount

instance Prelude.NFData PendingTaskCount

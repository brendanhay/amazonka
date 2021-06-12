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
-- Module      : Network.AWS.RDS.Types.RestoreWindow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.RestoreWindow where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Earliest and latest time an instance can be restored to:
--
-- /See:/ 'newRestoreWindow' smart constructor.
data RestoreWindow = RestoreWindow'
  { -- | The earliest time you can restore an instance to.
    earliestTime :: Core.Maybe Core.ISO8601,
    -- | The latest time you can restore an instance to.
    latestTime :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'earliestTime', 'restoreWindow_earliestTime' - The earliest time you can restore an instance to.
--
-- 'latestTime', 'restoreWindow_latestTime' - The latest time you can restore an instance to.
newRestoreWindow ::
  RestoreWindow
newRestoreWindow =
  RestoreWindow'
    { earliestTime = Core.Nothing,
      latestTime = Core.Nothing
    }

-- | The earliest time you can restore an instance to.
restoreWindow_earliestTime :: Lens.Lens' RestoreWindow (Core.Maybe Core.UTCTime)
restoreWindow_earliestTime = Lens.lens (\RestoreWindow' {earliestTime} -> earliestTime) (\s@RestoreWindow' {} a -> s {earliestTime = a} :: RestoreWindow) Core.. Lens.mapping Core._Time

-- | The latest time you can restore an instance to.
restoreWindow_latestTime :: Lens.Lens' RestoreWindow (Core.Maybe Core.UTCTime)
restoreWindow_latestTime = Lens.lens (\RestoreWindow' {latestTime} -> latestTime) (\s@RestoreWindow' {} a -> s {latestTime = a} :: RestoreWindow) Core.. Lens.mapping Core._Time

instance Core.FromXML RestoreWindow where
  parseXML x =
    RestoreWindow'
      Core.<$> (x Core..@? "EarliestTime")
      Core.<*> (x Core..@? "LatestTime")

instance Core.Hashable RestoreWindow

instance Core.NFData RestoreWindow

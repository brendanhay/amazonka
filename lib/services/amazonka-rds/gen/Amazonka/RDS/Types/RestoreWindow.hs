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
-- Module      : Amazonka.RDS.Types.RestoreWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.RestoreWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Earliest and latest time an instance can be restored to:
--
-- /See:/ 'newRestoreWindow' smart constructor.
data RestoreWindow = RestoreWindow'
  { -- | The earliest time you can restore an instance to.
    earliestTime :: Prelude.Maybe Data.ISO8601,
    -- | The latest time you can restore an instance to.
    latestTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { earliestTime = Prelude.Nothing,
      latestTime = Prelude.Nothing
    }

-- | The earliest time you can restore an instance to.
restoreWindow_earliestTime :: Lens.Lens' RestoreWindow (Prelude.Maybe Prelude.UTCTime)
restoreWindow_earliestTime = Lens.lens (\RestoreWindow' {earliestTime} -> earliestTime) (\s@RestoreWindow' {} a -> s {earliestTime = a} :: RestoreWindow) Prelude.. Lens.mapping Data._Time

-- | The latest time you can restore an instance to.
restoreWindow_latestTime :: Lens.Lens' RestoreWindow (Prelude.Maybe Prelude.UTCTime)
restoreWindow_latestTime = Lens.lens (\RestoreWindow' {latestTime} -> latestTime) (\s@RestoreWindow' {} a -> s {latestTime = a} :: RestoreWindow) Prelude.. Lens.mapping Data._Time

instance Data.FromXML RestoreWindow where
  parseXML x =
    RestoreWindow'
      Prelude.<$> (x Data..@? "EarliestTime")
      Prelude.<*> (x Data..@? "LatestTime")

instance Prelude.Hashable RestoreWindow where
  hashWithSalt _salt RestoreWindow' {..} =
    _salt `Prelude.hashWithSalt` earliestTime
      `Prelude.hashWithSalt` latestTime

instance Prelude.NFData RestoreWindow where
  rnf RestoreWindow' {..} =
    Prelude.rnf earliestTime
      `Prelude.seq` Prelude.rnf latestTime

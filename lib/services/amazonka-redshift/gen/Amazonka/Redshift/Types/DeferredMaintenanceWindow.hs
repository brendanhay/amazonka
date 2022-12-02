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
-- Module      : Amazonka.Redshift.Types.DeferredMaintenanceWindow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DeferredMaintenanceWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a deferred maintenance window
--
-- /See:/ 'newDeferredMaintenanceWindow' smart constructor.
data DeferredMaintenanceWindow = DeferredMaintenanceWindow'
  { -- | A unique identifier for the maintenance window.
    deferMaintenanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A timestamp for the end of the time period when we defer maintenance.
    deferMaintenanceEndTime :: Prelude.Maybe Data.ISO8601,
    -- | A timestamp for the beginning of the time period when we defer
    -- maintenance.
    deferMaintenanceStartTime :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeferredMaintenanceWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deferMaintenanceIdentifier', 'deferredMaintenanceWindow_deferMaintenanceIdentifier' - A unique identifier for the maintenance window.
--
-- 'deferMaintenanceEndTime', 'deferredMaintenanceWindow_deferMaintenanceEndTime' - A timestamp for the end of the time period when we defer maintenance.
--
-- 'deferMaintenanceStartTime', 'deferredMaintenanceWindow_deferMaintenanceStartTime' - A timestamp for the beginning of the time period when we defer
-- maintenance.
newDeferredMaintenanceWindow ::
  DeferredMaintenanceWindow
newDeferredMaintenanceWindow =
  DeferredMaintenanceWindow'
    { deferMaintenanceIdentifier =
        Prelude.Nothing,
      deferMaintenanceEndTime = Prelude.Nothing,
      deferMaintenanceStartTime = Prelude.Nothing
    }

-- | A unique identifier for the maintenance window.
deferredMaintenanceWindow_deferMaintenanceIdentifier :: Lens.Lens' DeferredMaintenanceWindow (Prelude.Maybe Prelude.Text)
deferredMaintenanceWindow_deferMaintenanceIdentifier = Lens.lens (\DeferredMaintenanceWindow' {deferMaintenanceIdentifier} -> deferMaintenanceIdentifier) (\s@DeferredMaintenanceWindow' {} a -> s {deferMaintenanceIdentifier = a} :: DeferredMaintenanceWindow)

-- | A timestamp for the end of the time period when we defer maintenance.
deferredMaintenanceWindow_deferMaintenanceEndTime :: Lens.Lens' DeferredMaintenanceWindow (Prelude.Maybe Prelude.UTCTime)
deferredMaintenanceWindow_deferMaintenanceEndTime = Lens.lens (\DeferredMaintenanceWindow' {deferMaintenanceEndTime} -> deferMaintenanceEndTime) (\s@DeferredMaintenanceWindow' {} a -> s {deferMaintenanceEndTime = a} :: DeferredMaintenanceWindow) Prelude.. Lens.mapping Data._Time

-- | A timestamp for the beginning of the time period when we defer
-- maintenance.
deferredMaintenanceWindow_deferMaintenanceStartTime :: Lens.Lens' DeferredMaintenanceWindow (Prelude.Maybe Prelude.UTCTime)
deferredMaintenanceWindow_deferMaintenanceStartTime = Lens.lens (\DeferredMaintenanceWindow' {deferMaintenanceStartTime} -> deferMaintenanceStartTime) (\s@DeferredMaintenanceWindow' {} a -> s {deferMaintenanceStartTime = a} :: DeferredMaintenanceWindow) Prelude.. Lens.mapping Data._Time

instance Data.FromXML DeferredMaintenanceWindow where
  parseXML x =
    DeferredMaintenanceWindow'
      Prelude.<$> (x Data..@? "DeferMaintenanceIdentifier")
      Prelude.<*> (x Data..@? "DeferMaintenanceEndTime")
      Prelude.<*> (x Data..@? "DeferMaintenanceStartTime")

instance Prelude.Hashable DeferredMaintenanceWindow where
  hashWithSalt _salt DeferredMaintenanceWindow' {..} =
    _salt
      `Prelude.hashWithSalt` deferMaintenanceIdentifier
      `Prelude.hashWithSalt` deferMaintenanceEndTime
      `Prelude.hashWithSalt` deferMaintenanceStartTime

instance Prelude.NFData DeferredMaintenanceWindow where
  rnf DeferredMaintenanceWindow' {..} =
    Prelude.rnf deferMaintenanceIdentifier
      `Prelude.seq` Prelude.rnf deferMaintenanceEndTime
      `Prelude.seq` Prelude.rnf deferMaintenanceStartTime

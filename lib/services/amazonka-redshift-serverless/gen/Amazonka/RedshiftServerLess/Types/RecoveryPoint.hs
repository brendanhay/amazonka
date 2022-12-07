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
-- Module      : Amazonka.RedshiftServerLess.Types.RecoveryPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.RecoveryPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The automatically created recovery point of a namespace. Recovery points
-- are created every 30 minutes and kept for 24 hours.
--
-- /See:/ 'newRecoveryPoint' smart constructor.
data RecoveryPoint = RecoveryPoint'
  { -- | The name of the namespace the recovery point is associated with.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the recovery point.
    recoveryPointId :: Prelude.Maybe Prelude.Text,
    -- | The total size of the data in the recovery point in megabytes.
    totalSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The name of the workgroup the recovery point is associated with.
    workgroupName :: Prelude.Maybe Prelude.Text,
    -- | The time the recovery point is created.
    recoveryPointCreateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceName', 'recoveryPoint_namespaceName' - The name of the namespace the recovery point is associated with.
--
-- 'recoveryPointId', 'recoveryPoint_recoveryPointId' - The unique identifier of the recovery point.
--
-- 'totalSizeInMegaBytes', 'recoveryPoint_totalSizeInMegaBytes' - The total size of the data in the recovery point in megabytes.
--
-- 'workgroupName', 'recoveryPoint_workgroupName' - The name of the workgroup the recovery point is associated with.
--
-- 'recoveryPointCreateTime', 'recoveryPoint_recoveryPointCreateTime' - The time the recovery point is created.
newRecoveryPoint ::
  RecoveryPoint
newRecoveryPoint =
  RecoveryPoint'
    { namespaceName = Prelude.Nothing,
      recoveryPointId = Prelude.Nothing,
      totalSizeInMegaBytes = Prelude.Nothing,
      workgroupName = Prelude.Nothing,
      recoveryPointCreateTime = Prelude.Nothing
    }

-- | The name of the namespace the recovery point is associated with.
recoveryPoint_namespaceName :: Lens.Lens' RecoveryPoint (Prelude.Maybe Prelude.Text)
recoveryPoint_namespaceName = Lens.lens (\RecoveryPoint' {namespaceName} -> namespaceName) (\s@RecoveryPoint' {} a -> s {namespaceName = a} :: RecoveryPoint)

-- | The unique identifier of the recovery point.
recoveryPoint_recoveryPointId :: Lens.Lens' RecoveryPoint (Prelude.Maybe Prelude.Text)
recoveryPoint_recoveryPointId = Lens.lens (\RecoveryPoint' {recoveryPointId} -> recoveryPointId) (\s@RecoveryPoint' {} a -> s {recoveryPointId = a} :: RecoveryPoint)

-- | The total size of the data in the recovery point in megabytes.
recoveryPoint_totalSizeInMegaBytes :: Lens.Lens' RecoveryPoint (Prelude.Maybe Prelude.Double)
recoveryPoint_totalSizeInMegaBytes = Lens.lens (\RecoveryPoint' {totalSizeInMegaBytes} -> totalSizeInMegaBytes) (\s@RecoveryPoint' {} a -> s {totalSizeInMegaBytes = a} :: RecoveryPoint)

-- | The name of the workgroup the recovery point is associated with.
recoveryPoint_workgroupName :: Lens.Lens' RecoveryPoint (Prelude.Maybe Prelude.Text)
recoveryPoint_workgroupName = Lens.lens (\RecoveryPoint' {workgroupName} -> workgroupName) (\s@RecoveryPoint' {} a -> s {workgroupName = a} :: RecoveryPoint)

-- | The time the recovery point is created.
recoveryPoint_recoveryPointCreateTime :: Lens.Lens' RecoveryPoint (Prelude.Maybe Prelude.UTCTime)
recoveryPoint_recoveryPointCreateTime = Lens.lens (\RecoveryPoint' {recoveryPointCreateTime} -> recoveryPointCreateTime) (\s@RecoveryPoint' {} a -> s {recoveryPointCreateTime = a} :: RecoveryPoint) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON RecoveryPoint where
  parseJSON =
    Data.withObject
      "RecoveryPoint"
      ( \x ->
          RecoveryPoint'
            Prelude.<$> (x Data..:? "namespaceName")
            Prelude.<*> (x Data..:? "recoveryPointId")
            Prelude.<*> (x Data..:? "totalSizeInMegaBytes")
            Prelude.<*> (x Data..:? "workgroupName")
            Prelude.<*> (x Data..:? "recoveryPointCreateTime")
      )

instance Prelude.Hashable RecoveryPoint where
  hashWithSalt _salt RecoveryPoint' {..} =
    _salt `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` recoveryPointId
      `Prelude.hashWithSalt` totalSizeInMegaBytes
      `Prelude.hashWithSalt` workgroupName
      `Prelude.hashWithSalt` recoveryPointCreateTime

instance Prelude.NFData RecoveryPoint where
  rnf RecoveryPoint' {..} =
    Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf recoveryPointId
      `Prelude.seq` Prelude.rnf totalSizeInMegaBytes
      `Prelude.seq` Prelude.rnf workgroupName
      `Prelude.seq` Prelude.rnf recoveryPointCreateTime

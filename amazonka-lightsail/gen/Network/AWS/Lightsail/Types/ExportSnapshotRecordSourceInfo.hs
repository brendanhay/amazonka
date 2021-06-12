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
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo

-- | Describes the source of an export snapshot record.
--
-- /See:/ 'newExportSnapshotRecordSourceInfo' smart constructor.
data ExportSnapshotRecordSourceInfo = ExportSnapshotRecordSourceInfo'
  { -- | A list of objects describing a disk snapshot.
    diskSnapshotInfo :: Core.Maybe DiskSnapshotInfo,
    -- | The date when the source instance or disk snapshot was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the source instance or disk snapshot.
    arn :: Core.Maybe Core.Text,
    -- | The Lightsail resource type (e.g., @InstanceSnapshot@ or
    -- @DiskSnapshot@).
    resourceType :: Core.Maybe ExportSnapshotRecordSourceType,
    -- | The name of the source instance or disk snapshot.
    name :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the snapshot\'s source instance or
    -- disk.
    fromResourceArn :: Core.Maybe Core.Text,
    -- | A list of objects describing an instance snapshot.
    instanceSnapshotInfo :: Core.Maybe InstanceSnapshotInfo,
    -- | The name of the snapshot\'s source instance or disk.
    fromResourceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportSnapshotRecordSourceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskSnapshotInfo', 'exportSnapshotRecordSourceInfo_diskSnapshotInfo' - A list of objects describing a disk snapshot.
--
-- 'createdAt', 'exportSnapshotRecordSourceInfo_createdAt' - The date when the source instance or disk snapshot was created.
--
-- 'arn', 'exportSnapshotRecordSourceInfo_arn' - The Amazon Resource Name (ARN) of the source instance or disk snapshot.
--
-- 'resourceType', 'exportSnapshotRecordSourceInfo_resourceType' - The Lightsail resource type (e.g., @InstanceSnapshot@ or
-- @DiskSnapshot@).
--
-- 'name', 'exportSnapshotRecordSourceInfo_name' - The name of the source instance or disk snapshot.
--
-- 'fromResourceArn', 'exportSnapshotRecordSourceInfo_fromResourceArn' - The Amazon Resource Name (ARN) of the snapshot\'s source instance or
-- disk.
--
-- 'instanceSnapshotInfo', 'exportSnapshotRecordSourceInfo_instanceSnapshotInfo' - A list of objects describing an instance snapshot.
--
-- 'fromResourceName', 'exportSnapshotRecordSourceInfo_fromResourceName' - The name of the snapshot\'s source instance or disk.
newExportSnapshotRecordSourceInfo ::
  ExportSnapshotRecordSourceInfo
newExportSnapshotRecordSourceInfo =
  ExportSnapshotRecordSourceInfo'
    { diskSnapshotInfo =
        Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      resourceType = Core.Nothing,
      name = Core.Nothing,
      fromResourceArn = Core.Nothing,
      instanceSnapshotInfo = Core.Nothing,
      fromResourceName = Core.Nothing
    }

-- | A list of objects describing a disk snapshot.
exportSnapshotRecordSourceInfo_diskSnapshotInfo :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe DiskSnapshotInfo)
exportSnapshotRecordSourceInfo_diskSnapshotInfo = Lens.lens (\ExportSnapshotRecordSourceInfo' {diskSnapshotInfo} -> diskSnapshotInfo) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {diskSnapshotInfo = a} :: ExportSnapshotRecordSourceInfo)

-- | The date when the source instance or disk snapshot was created.
exportSnapshotRecordSourceInfo_createdAt :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Core.UTCTime)
exportSnapshotRecordSourceInfo_createdAt = Lens.lens (\ExportSnapshotRecordSourceInfo' {createdAt} -> createdAt) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {createdAt = a} :: ExportSnapshotRecordSourceInfo) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the source instance or disk snapshot.
exportSnapshotRecordSourceInfo_arn :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Core.Text)
exportSnapshotRecordSourceInfo_arn = Lens.lens (\ExportSnapshotRecordSourceInfo' {arn} -> arn) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {arn = a} :: ExportSnapshotRecordSourceInfo)

-- | The Lightsail resource type (e.g., @InstanceSnapshot@ or
-- @DiskSnapshot@).
exportSnapshotRecordSourceInfo_resourceType :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe ExportSnapshotRecordSourceType)
exportSnapshotRecordSourceInfo_resourceType = Lens.lens (\ExportSnapshotRecordSourceInfo' {resourceType} -> resourceType) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {resourceType = a} :: ExportSnapshotRecordSourceInfo)

-- | The name of the source instance or disk snapshot.
exportSnapshotRecordSourceInfo_name :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Core.Text)
exportSnapshotRecordSourceInfo_name = Lens.lens (\ExportSnapshotRecordSourceInfo' {name} -> name) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {name = a} :: ExportSnapshotRecordSourceInfo)

-- | The Amazon Resource Name (ARN) of the snapshot\'s source instance or
-- disk.
exportSnapshotRecordSourceInfo_fromResourceArn :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Core.Text)
exportSnapshotRecordSourceInfo_fromResourceArn = Lens.lens (\ExportSnapshotRecordSourceInfo' {fromResourceArn} -> fromResourceArn) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {fromResourceArn = a} :: ExportSnapshotRecordSourceInfo)

-- | A list of objects describing an instance snapshot.
exportSnapshotRecordSourceInfo_instanceSnapshotInfo :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe InstanceSnapshotInfo)
exportSnapshotRecordSourceInfo_instanceSnapshotInfo = Lens.lens (\ExportSnapshotRecordSourceInfo' {instanceSnapshotInfo} -> instanceSnapshotInfo) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {instanceSnapshotInfo = a} :: ExportSnapshotRecordSourceInfo)

-- | The name of the snapshot\'s source instance or disk.
exportSnapshotRecordSourceInfo_fromResourceName :: Lens.Lens' ExportSnapshotRecordSourceInfo (Core.Maybe Core.Text)
exportSnapshotRecordSourceInfo_fromResourceName = Lens.lens (\ExportSnapshotRecordSourceInfo' {fromResourceName} -> fromResourceName) (\s@ExportSnapshotRecordSourceInfo' {} a -> s {fromResourceName = a} :: ExportSnapshotRecordSourceInfo)

instance Core.FromJSON ExportSnapshotRecordSourceInfo where
  parseJSON =
    Core.withObject
      "ExportSnapshotRecordSourceInfo"
      ( \x ->
          ExportSnapshotRecordSourceInfo'
            Core.<$> (x Core..:? "diskSnapshotInfo")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "fromResourceArn")
            Core.<*> (x Core..:? "instanceSnapshotInfo")
            Core.<*> (x Core..:? "fromResourceName")
      )

instance Core.Hashable ExportSnapshotRecordSourceInfo

instance Core.NFData ExportSnapshotRecordSourceInfo

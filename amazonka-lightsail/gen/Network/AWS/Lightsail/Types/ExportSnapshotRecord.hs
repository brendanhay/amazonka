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
-- Module      : Network.AWS.Lightsail.Types.ExportSnapshotRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ExportSnapshotRecord where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Prelude

-- | Describes an export snapshot record.
--
-- /See:/ 'newExportSnapshotRecord' smart constructor.
data ExportSnapshotRecord = ExportSnapshotRecord'
  { -- | The date when the export snapshot record was created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the export snapshot record.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type (e.g., @ExportSnapshotRecord@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The state of the export snapshot record.
    state :: Prelude.Maybe RecordState,
    -- | The export snapshot record name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of objects describing the source of the export snapshot record.
    sourceInfo :: Prelude.Maybe ExportSnapshotRecordSourceInfo,
    -- | The AWS Region and Availability Zone where the export snapshot record is
    -- located.
    location :: Prelude.Maybe ResourceLocation,
    -- | A list of objects describing the destination of the export snapshot
    -- record.
    destinationInfo :: Prelude.Maybe DestinationInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportSnapshotRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'exportSnapshotRecord_createdAt' - The date when the export snapshot record was created.
--
-- 'arn', 'exportSnapshotRecord_arn' - The Amazon Resource Name (ARN) of the export snapshot record.
--
-- 'resourceType', 'exportSnapshotRecord_resourceType' - The Lightsail resource type (e.g., @ExportSnapshotRecord@).
--
-- 'state', 'exportSnapshotRecord_state' - The state of the export snapshot record.
--
-- 'name', 'exportSnapshotRecord_name' - The export snapshot record name.
--
-- 'sourceInfo', 'exportSnapshotRecord_sourceInfo' - A list of objects describing the source of the export snapshot record.
--
-- 'location', 'exportSnapshotRecord_location' - The AWS Region and Availability Zone where the export snapshot record is
-- located.
--
-- 'destinationInfo', 'exportSnapshotRecord_destinationInfo' - A list of objects describing the destination of the export snapshot
-- record.
newExportSnapshotRecord ::
  ExportSnapshotRecord
newExportSnapshotRecord =
  ExportSnapshotRecord'
    { createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceInfo = Prelude.Nothing,
      location = Prelude.Nothing,
      destinationInfo = Prelude.Nothing
    }

-- | The date when the export snapshot record was created.
exportSnapshotRecord_createdAt :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe Prelude.UTCTime)
exportSnapshotRecord_createdAt = Lens.lens (\ExportSnapshotRecord' {createdAt} -> createdAt) (\s@ExportSnapshotRecord' {} a -> s {createdAt = a} :: ExportSnapshotRecord) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the export snapshot record.
exportSnapshotRecord_arn :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe Prelude.Text)
exportSnapshotRecord_arn = Lens.lens (\ExportSnapshotRecord' {arn} -> arn) (\s@ExportSnapshotRecord' {} a -> s {arn = a} :: ExportSnapshotRecord)

-- | The Lightsail resource type (e.g., @ExportSnapshotRecord@).
exportSnapshotRecord_resourceType :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe ResourceType)
exportSnapshotRecord_resourceType = Lens.lens (\ExportSnapshotRecord' {resourceType} -> resourceType) (\s@ExportSnapshotRecord' {} a -> s {resourceType = a} :: ExportSnapshotRecord)

-- | The state of the export snapshot record.
exportSnapshotRecord_state :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe RecordState)
exportSnapshotRecord_state = Lens.lens (\ExportSnapshotRecord' {state} -> state) (\s@ExportSnapshotRecord' {} a -> s {state = a} :: ExportSnapshotRecord)

-- | The export snapshot record name.
exportSnapshotRecord_name :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe Prelude.Text)
exportSnapshotRecord_name = Lens.lens (\ExportSnapshotRecord' {name} -> name) (\s@ExportSnapshotRecord' {} a -> s {name = a} :: ExportSnapshotRecord)

-- | A list of objects describing the source of the export snapshot record.
exportSnapshotRecord_sourceInfo :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe ExportSnapshotRecordSourceInfo)
exportSnapshotRecord_sourceInfo = Lens.lens (\ExportSnapshotRecord' {sourceInfo} -> sourceInfo) (\s@ExportSnapshotRecord' {} a -> s {sourceInfo = a} :: ExportSnapshotRecord)

-- | The AWS Region and Availability Zone where the export snapshot record is
-- located.
exportSnapshotRecord_location :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe ResourceLocation)
exportSnapshotRecord_location = Lens.lens (\ExportSnapshotRecord' {location} -> location) (\s@ExportSnapshotRecord' {} a -> s {location = a} :: ExportSnapshotRecord)

-- | A list of objects describing the destination of the export snapshot
-- record.
exportSnapshotRecord_destinationInfo :: Lens.Lens' ExportSnapshotRecord (Prelude.Maybe DestinationInfo)
exportSnapshotRecord_destinationInfo = Lens.lens (\ExportSnapshotRecord' {destinationInfo} -> destinationInfo) (\s@ExportSnapshotRecord' {} a -> s {destinationInfo = a} :: ExportSnapshotRecord)

instance Prelude.FromJSON ExportSnapshotRecord where
  parseJSON =
    Prelude.withObject
      "ExportSnapshotRecord"
      ( \x ->
          ExportSnapshotRecord'
            Prelude.<$> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "sourceInfo")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "destinationInfo")
      )

instance Prelude.Hashable ExportSnapshotRecord

instance Prelude.NFData ExportSnapshotRecord

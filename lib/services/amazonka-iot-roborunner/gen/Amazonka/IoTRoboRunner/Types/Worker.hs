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
-- Module      : Amazonka.IoTRoboRunner.Types.Worker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.Worker where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types.Orientation
import Amazonka.IoTRoboRunner.Types.PositionCoordinates
import Amazonka.IoTRoboRunner.Types.VendorProperties
import qualified Amazonka.Prelude as Prelude

-- | A unit capable of performing tasks.
--
-- /See:/ 'newWorker' smart constructor.
data Worker = Worker'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    additionalTransientProperties :: Prelude.Maybe Prelude.Text,
    orientation :: Prelude.Maybe Orientation,
    position :: Prelude.Maybe PositionCoordinates,
    vendorProperties :: Prelude.Maybe VendorProperties,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    fleet :: Prelude.Text,
    createdAt :: Data.POSIX,
    updatedAt :: Data.POSIX,
    name :: Prelude.Text,
    site :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Worker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'worker_additionalFixedProperties' - Undocumented member.
--
-- 'additionalTransientProperties', 'worker_additionalTransientProperties' - Undocumented member.
--
-- 'orientation', 'worker_orientation' - Undocumented member.
--
-- 'position', 'worker_position' - Undocumented member.
--
-- 'vendorProperties', 'worker_vendorProperties' - Undocumented member.
--
-- 'arn', 'worker_arn' - Undocumented member.
--
-- 'id', 'worker_id' - Undocumented member.
--
-- 'fleet', 'worker_fleet' - Undocumented member.
--
-- 'createdAt', 'worker_createdAt' - Undocumented member.
--
-- 'updatedAt', 'worker_updatedAt' - Undocumented member.
--
-- 'name', 'worker_name' - Undocumented member.
--
-- 'site', 'worker_site' - Undocumented member.
newWorker ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'fleet'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'site'
  Prelude.Text ->
  Worker
newWorker
  pArn_
  pId_
  pFleet_
  pCreatedAt_
  pUpdatedAt_
  pName_
  pSite_ =
    Worker'
      { additionalFixedProperties =
          Prelude.Nothing,
        additionalTransientProperties = Prelude.Nothing,
        orientation = Prelude.Nothing,
        position = Prelude.Nothing,
        vendorProperties = Prelude.Nothing,
        arn = pArn_,
        id = pId_,
        fleet = pFleet_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        name = pName_,
        site = pSite_
      }

-- | Undocumented member.
worker_additionalFixedProperties :: Lens.Lens' Worker (Prelude.Maybe Prelude.Text)
worker_additionalFixedProperties = Lens.lens (\Worker' {additionalFixedProperties} -> additionalFixedProperties) (\s@Worker' {} a -> s {additionalFixedProperties = a} :: Worker)

-- | Undocumented member.
worker_additionalTransientProperties :: Lens.Lens' Worker (Prelude.Maybe Prelude.Text)
worker_additionalTransientProperties = Lens.lens (\Worker' {additionalTransientProperties} -> additionalTransientProperties) (\s@Worker' {} a -> s {additionalTransientProperties = a} :: Worker)

-- | Undocumented member.
worker_orientation :: Lens.Lens' Worker (Prelude.Maybe Orientation)
worker_orientation = Lens.lens (\Worker' {orientation} -> orientation) (\s@Worker' {} a -> s {orientation = a} :: Worker)

-- | Undocumented member.
worker_position :: Lens.Lens' Worker (Prelude.Maybe PositionCoordinates)
worker_position = Lens.lens (\Worker' {position} -> position) (\s@Worker' {} a -> s {position = a} :: Worker)

-- | Undocumented member.
worker_vendorProperties :: Lens.Lens' Worker (Prelude.Maybe VendorProperties)
worker_vendorProperties = Lens.lens (\Worker' {vendorProperties} -> vendorProperties) (\s@Worker' {} a -> s {vendorProperties = a} :: Worker)

-- | Undocumented member.
worker_arn :: Lens.Lens' Worker Prelude.Text
worker_arn = Lens.lens (\Worker' {arn} -> arn) (\s@Worker' {} a -> s {arn = a} :: Worker)

-- | Undocumented member.
worker_id :: Lens.Lens' Worker Prelude.Text
worker_id = Lens.lens (\Worker' {id} -> id) (\s@Worker' {} a -> s {id = a} :: Worker)

-- | Undocumented member.
worker_fleet :: Lens.Lens' Worker Prelude.Text
worker_fleet = Lens.lens (\Worker' {fleet} -> fleet) (\s@Worker' {} a -> s {fleet = a} :: Worker)

-- | Undocumented member.
worker_createdAt :: Lens.Lens' Worker Prelude.UTCTime
worker_createdAt = Lens.lens (\Worker' {createdAt} -> createdAt) (\s@Worker' {} a -> s {createdAt = a} :: Worker) Prelude.. Data._Time

-- | Undocumented member.
worker_updatedAt :: Lens.Lens' Worker Prelude.UTCTime
worker_updatedAt = Lens.lens (\Worker' {updatedAt} -> updatedAt) (\s@Worker' {} a -> s {updatedAt = a} :: Worker) Prelude.. Data._Time

-- | Undocumented member.
worker_name :: Lens.Lens' Worker Prelude.Text
worker_name = Lens.lens (\Worker' {name} -> name) (\s@Worker' {} a -> s {name = a} :: Worker)

-- | Undocumented member.
worker_site :: Lens.Lens' Worker Prelude.Text
worker_site = Lens.lens (\Worker' {site} -> site) (\s@Worker' {} a -> s {site = a} :: Worker)

instance Data.FromJSON Worker where
  parseJSON =
    Data.withObject
      "Worker"
      ( \x ->
          Worker'
            Prelude.<$> (x Data..:? "additionalFixedProperties")
            Prelude.<*> (x Data..:? "additionalTransientProperties")
            Prelude.<*> (x Data..:? "orientation")
            Prelude.<*> (x Data..:? "position")
            Prelude.<*> (x Data..:? "vendorProperties")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "fleet")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "updatedAt")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "site")
      )

instance Prelude.Hashable Worker where
  hashWithSalt _salt Worker' {..} =
    _salt
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` additionalTransientProperties
      `Prelude.hashWithSalt` orientation
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` vendorProperties
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` fleet
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` site

instance Prelude.NFData Worker where
  rnf Worker' {..} =
    Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf additionalTransientProperties
      `Prelude.seq` Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf vendorProperties
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf site

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
-- Module      : Amazonka.GroundStation.Types.EphemerisItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.EphemerisStatus
import Amazonka.GroundStation.Types.S3Object
import qualified Amazonka.Prelude as Prelude

-- | Ephemeris item.
--
-- /See:/ 'newEphemerisItem' smart constructor.
data EphemerisItem = EphemerisItem'
  { -- | The time the ephemeris was uploaded in UTC.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Whether or not the ephemeris is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The AWS Ground Station ephemeris ID.
    ephemerisId :: Prelude.Maybe Prelude.Text,
    -- | A name string associated with the ephemeris. Used as a human-readable
    -- identifier for the ephemeris.
    name :: Prelude.Maybe Prelude.Text,
    -- | Customer-provided priority score to establish the order in which
    -- overlapping ephemerides should be used.
    --
    -- The default for customer-provided ephemeris priority is 1, and higher
    -- numbers take precedence.
    --
    -- Priority must be 1 or greater
    priority :: Prelude.Maybe Prelude.Natural,
    -- | Source S3 object used for the ephemeris.
    sourceS3Object :: Prelude.Maybe S3Object,
    -- | The status of the ephemeris.
    status :: Prelude.Maybe EphemerisStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EphemerisItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'ephemerisItem_creationTime' - The time the ephemeris was uploaded in UTC.
--
-- 'enabled', 'ephemerisItem_enabled' - Whether or not the ephemeris is enabled.
--
-- 'ephemerisId', 'ephemerisItem_ephemerisId' - The AWS Ground Station ephemeris ID.
--
-- 'name', 'ephemerisItem_name' - A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
--
-- 'priority', 'ephemerisItem_priority' - Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
--
-- 'sourceS3Object', 'ephemerisItem_sourceS3Object' - Source S3 object used for the ephemeris.
--
-- 'status', 'ephemerisItem_status' - The status of the ephemeris.
newEphemerisItem ::
  EphemerisItem
newEphemerisItem =
  EphemerisItem'
    { creationTime = Prelude.Nothing,
      enabled = Prelude.Nothing,
      ephemerisId = Prelude.Nothing,
      name = Prelude.Nothing,
      priority = Prelude.Nothing,
      sourceS3Object = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The time the ephemeris was uploaded in UTC.
ephemerisItem_creationTime :: Lens.Lens' EphemerisItem (Prelude.Maybe Prelude.UTCTime)
ephemerisItem_creationTime = Lens.lens (\EphemerisItem' {creationTime} -> creationTime) (\s@EphemerisItem' {} a -> s {creationTime = a} :: EphemerisItem) Prelude.. Lens.mapping Data._Time

-- | Whether or not the ephemeris is enabled.
ephemerisItem_enabled :: Lens.Lens' EphemerisItem (Prelude.Maybe Prelude.Bool)
ephemerisItem_enabled = Lens.lens (\EphemerisItem' {enabled} -> enabled) (\s@EphemerisItem' {} a -> s {enabled = a} :: EphemerisItem)

-- | The AWS Ground Station ephemeris ID.
ephemerisItem_ephemerisId :: Lens.Lens' EphemerisItem (Prelude.Maybe Prelude.Text)
ephemerisItem_ephemerisId = Lens.lens (\EphemerisItem' {ephemerisId} -> ephemerisId) (\s@EphemerisItem' {} a -> s {ephemerisId = a} :: EphemerisItem)

-- | A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
ephemerisItem_name :: Lens.Lens' EphemerisItem (Prelude.Maybe Prelude.Text)
ephemerisItem_name = Lens.lens (\EphemerisItem' {name} -> name) (\s@EphemerisItem' {} a -> s {name = a} :: EphemerisItem)

-- | Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
ephemerisItem_priority :: Lens.Lens' EphemerisItem (Prelude.Maybe Prelude.Natural)
ephemerisItem_priority = Lens.lens (\EphemerisItem' {priority} -> priority) (\s@EphemerisItem' {} a -> s {priority = a} :: EphemerisItem)

-- | Source S3 object used for the ephemeris.
ephemerisItem_sourceS3Object :: Lens.Lens' EphemerisItem (Prelude.Maybe S3Object)
ephemerisItem_sourceS3Object = Lens.lens (\EphemerisItem' {sourceS3Object} -> sourceS3Object) (\s@EphemerisItem' {} a -> s {sourceS3Object = a} :: EphemerisItem)

-- | The status of the ephemeris.
ephemerisItem_status :: Lens.Lens' EphemerisItem (Prelude.Maybe EphemerisStatus)
ephemerisItem_status = Lens.lens (\EphemerisItem' {status} -> status) (\s@EphemerisItem' {} a -> s {status = a} :: EphemerisItem)

instance Data.FromJSON EphemerisItem where
  parseJSON =
    Data.withObject
      "EphemerisItem"
      ( \x ->
          EphemerisItem'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "enabled")
            Prelude.<*> (x Data..:? "ephemerisId")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "priority")
            Prelude.<*> (x Data..:? "sourceS3Object")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable EphemerisItem where
  hashWithSalt _salt EphemerisItem' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` ephemerisId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` sourceS3Object
      `Prelude.hashWithSalt` status

instance Prelude.NFData EphemerisItem where
  rnf EphemerisItem' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf enabled `Prelude.seq`
        Prelude.rnf ephemerisId `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf priority `Prelude.seq`
              Prelude.rnf sourceS3Object `Prelude.seq`
                Prelude.rnf status

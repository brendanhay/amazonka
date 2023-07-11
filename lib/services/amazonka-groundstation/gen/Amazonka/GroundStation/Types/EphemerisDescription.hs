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
-- Module      : Amazonka.GroundStation.Types.EphemerisDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.S3Object
import qualified Amazonka.Prelude as Prelude

-- | Description of ephemeris.
--
-- /See:/ 'newEphemerisDescription' smart constructor.
data EphemerisDescription = EphemerisDescription'
  { -- | Supplied ephemeris data.
    ephemerisData :: Prelude.Maybe Prelude.Text,
    -- | Source S3 object used for the ephemeris.
    sourceS3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EphemerisDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ephemerisData', 'ephemerisDescription_ephemerisData' - Supplied ephemeris data.
--
-- 'sourceS3Object', 'ephemerisDescription_sourceS3Object' - Source S3 object used for the ephemeris.
newEphemerisDescription ::
  EphemerisDescription
newEphemerisDescription =
  EphemerisDescription'
    { ephemerisData =
        Prelude.Nothing,
      sourceS3Object = Prelude.Nothing
    }

-- | Supplied ephemeris data.
ephemerisDescription_ephemerisData :: Lens.Lens' EphemerisDescription (Prelude.Maybe Prelude.Text)
ephemerisDescription_ephemerisData = Lens.lens (\EphemerisDescription' {ephemerisData} -> ephemerisData) (\s@EphemerisDescription' {} a -> s {ephemerisData = a} :: EphemerisDescription)

-- | Source S3 object used for the ephemeris.
ephemerisDescription_sourceS3Object :: Lens.Lens' EphemerisDescription (Prelude.Maybe S3Object)
ephemerisDescription_sourceS3Object = Lens.lens (\EphemerisDescription' {sourceS3Object} -> sourceS3Object) (\s@EphemerisDescription' {} a -> s {sourceS3Object = a} :: EphemerisDescription)

instance Data.FromJSON EphemerisDescription where
  parseJSON =
    Data.withObject
      "EphemerisDescription"
      ( \x ->
          EphemerisDescription'
            Prelude.<$> (x Data..:? "ephemerisData")
            Prelude.<*> (x Data..:? "sourceS3Object")
      )

instance Prelude.Hashable EphemerisDescription where
  hashWithSalt _salt EphemerisDescription' {..} =
    _salt
      `Prelude.hashWithSalt` ephemerisData
      `Prelude.hashWithSalt` sourceS3Object

instance Prelude.NFData EphemerisDescription where
  rnf EphemerisDescription' {..} =
    Prelude.rnf ephemerisData
      `Prelude.seq` Prelude.rnf sourceS3Object

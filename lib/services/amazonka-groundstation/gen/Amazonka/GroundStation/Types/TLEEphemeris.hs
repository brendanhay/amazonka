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
-- Module      : Amazonka.GroundStation.Types.TLEEphemeris
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.TLEEphemeris where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.S3Object
import Amazonka.GroundStation.Types.TLEData
import qualified Amazonka.Prelude as Prelude

-- | Two-line element set (TLE) ephemeris.
--
-- /See:/ 'newTLEEphemeris' smart constructor.
data TLEEphemeris = TLEEphemeris'
  { -- | The data for a TLE ephemeris, supplied directly in the request rather
    -- than through an S3 object.
    tleData :: Prelude.Maybe (Prelude.NonEmpty TLEData),
    -- | Identifies the S3 object to be used as the ephemeris.
    s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TLEEphemeris' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tleData', 'tLEEphemeris_tleData' - The data for a TLE ephemeris, supplied directly in the request rather
-- than through an S3 object.
--
-- 's3Object', 'tLEEphemeris_s3Object' - Identifies the S3 object to be used as the ephemeris.
newTLEEphemeris ::
  TLEEphemeris
newTLEEphemeris =
  TLEEphemeris'
    { tleData = Prelude.Nothing,
      s3Object = Prelude.Nothing
    }

-- | The data for a TLE ephemeris, supplied directly in the request rather
-- than through an S3 object.
tLEEphemeris_tleData :: Lens.Lens' TLEEphemeris (Prelude.Maybe (Prelude.NonEmpty TLEData))
tLEEphemeris_tleData = Lens.lens (\TLEEphemeris' {tleData} -> tleData) (\s@TLEEphemeris' {} a -> s {tleData = a} :: TLEEphemeris) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the S3 object to be used as the ephemeris.
tLEEphemeris_s3Object :: Lens.Lens' TLEEphemeris (Prelude.Maybe S3Object)
tLEEphemeris_s3Object = Lens.lens (\TLEEphemeris' {s3Object} -> s3Object) (\s@TLEEphemeris' {} a -> s {s3Object = a} :: TLEEphemeris)

instance Prelude.Hashable TLEEphemeris where
  hashWithSalt _salt TLEEphemeris' {..} =
    _salt `Prelude.hashWithSalt` tleData
      `Prelude.hashWithSalt` s3Object

instance Prelude.NFData TLEEphemeris where
  rnf TLEEphemeris' {..} =
    Prelude.rnf tleData
      `Prelude.seq` Prelude.rnf s3Object

instance Data.ToJSON TLEEphemeris where
  toJSON TLEEphemeris' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tleData" Data..=) Prelude.<$> tleData,
            ("s3Object" Data..=) Prelude.<$> s3Object
          ]
      )

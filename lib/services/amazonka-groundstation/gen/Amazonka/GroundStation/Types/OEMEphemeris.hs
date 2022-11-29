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
-- Module      : Amazonka.GroundStation.Types.OEMEphemeris
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.OEMEphemeris where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.S3Object
import qualified Amazonka.Prelude as Prelude

-- | Ephemeris data in Orbit Ephemeris Message (OEM) format.
--
-- /See:/ 'newOEMEphemeris' smart constructor.
data OEMEphemeris = OEMEphemeris'
  { -- | The data for an OEM ephemeris, supplied directly in the request rather
    -- than through an S3 object.
    oemData :: Prelude.Maybe Prelude.Text,
    -- | Identifies the S3 object to be used as the ephemeris.
    s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OEMEphemeris' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oemData', 'oEMEphemeris_oemData' - The data for an OEM ephemeris, supplied directly in the request rather
-- than through an S3 object.
--
-- 's3Object', 'oEMEphemeris_s3Object' - Identifies the S3 object to be used as the ephemeris.
newOEMEphemeris ::
  OEMEphemeris
newOEMEphemeris =
  OEMEphemeris'
    { oemData = Prelude.Nothing,
      s3Object = Prelude.Nothing
    }

-- | The data for an OEM ephemeris, supplied directly in the request rather
-- than through an S3 object.
oEMEphemeris_oemData :: Lens.Lens' OEMEphemeris (Prelude.Maybe Prelude.Text)
oEMEphemeris_oemData = Lens.lens (\OEMEphemeris' {oemData} -> oemData) (\s@OEMEphemeris' {} a -> s {oemData = a} :: OEMEphemeris)

-- | Identifies the S3 object to be used as the ephemeris.
oEMEphemeris_s3Object :: Lens.Lens' OEMEphemeris (Prelude.Maybe S3Object)
oEMEphemeris_s3Object = Lens.lens (\OEMEphemeris' {s3Object} -> s3Object) (\s@OEMEphemeris' {} a -> s {s3Object = a} :: OEMEphemeris)

instance Prelude.Hashable OEMEphemeris where
  hashWithSalt _salt OEMEphemeris' {..} =
    _salt `Prelude.hashWithSalt` oemData
      `Prelude.hashWithSalt` s3Object

instance Prelude.NFData OEMEphemeris where
  rnf OEMEphemeris' {..} =
    Prelude.rnf oemData
      `Prelude.seq` Prelude.rnf s3Object

instance Core.ToJSON OEMEphemeris where
  toJSON OEMEphemeris' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("oemData" Core..=) Prelude.<$> oemData,
            ("s3Object" Core..=) Prelude.<$> s3Object
          ]
      )

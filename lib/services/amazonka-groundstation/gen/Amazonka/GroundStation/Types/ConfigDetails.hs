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
-- Module      : Amazonka.GroundStation.Types.ConfigDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.AntennaDemodDecodeDetails
import Amazonka.GroundStation.Types.EndpointDetails
import Amazonka.GroundStation.Types.S3RecordingDetails
import qualified Amazonka.Prelude as Prelude

-- | Details for certain @Config@ object types in a contact.
--
-- /See:/ 'newConfigDetails' smart constructor.
data ConfigDetails = ConfigDetails'
  { endpointDetails :: Prelude.Maybe EndpointDetails,
    -- | Details for an S3 recording @Config@ in a contact.
    s3RecordingDetails :: Prelude.Maybe S3RecordingDetails,
    -- | Details for antenna demod decode @Config@ in a contact.
    antennaDemodDecodeDetails :: Prelude.Maybe AntennaDemodDecodeDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointDetails', 'configDetails_endpointDetails' - Undocumented member.
--
-- 's3RecordingDetails', 'configDetails_s3RecordingDetails' - Details for an S3 recording @Config@ in a contact.
--
-- 'antennaDemodDecodeDetails', 'configDetails_antennaDemodDecodeDetails' - Details for antenna demod decode @Config@ in a contact.
newConfigDetails ::
  ConfigDetails
newConfigDetails =
  ConfigDetails'
    { endpointDetails = Prelude.Nothing,
      s3RecordingDetails = Prelude.Nothing,
      antennaDemodDecodeDetails = Prelude.Nothing
    }

-- | Undocumented member.
configDetails_endpointDetails :: Lens.Lens' ConfigDetails (Prelude.Maybe EndpointDetails)
configDetails_endpointDetails = Lens.lens (\ConfigDetails' {endpointDetails} -> endpointDetails) (\s@ConfigDetails' {} a -> s {endpointDetails = a} :: ConfigDetails)

-- | Details for an S3 recording @Config@ in a contact.
configDetails_s3RecordingDetails :: Lens.Lens' ConfigDetails (Prelude.Maybe S3RecordingDetails)
configDetails_s3RecordingDetails = Lens.lens (\ConfigDetails' {s3RecordingDetails} -> s3RecordingDetails) (\s@ConfigDetails' {} a -> s {s3RecordingDetails = a} :: ConfigDetails)

-- | Details for antenna demod decode @Config@ in a contact.
configDetails_antennaDemodDecodeDetails :: Lens.Lens' ConfigDetails (Prelude.Maybe AntennaDemodDecodeDetails)
configDetails_antennaDemodDecodeDetails = Lens.lens (\ConfigDetails' {antennaDemodDecodeDetails} -> antennaDemodDecodeDetails) (\s@ConfigDetails' {} a -> s {antennaDemodDecodeDetails = a} :: ConfigDetails)

instance Core.FromJSON ConfigDetails where
  parseJSON =
    Core.withObject
      "ConfigDetails"
      ( \x ->
          ConfigDetails'
            Prelude.<$> (x Core..:? "endpointDetails")
            Prelude.<*> (x Core..:? "s3RecordingDetails")
            Prelude.<*> (x Core..:? "antennaDemodDecodeDetails")
      )

instance Prelude.Hashable ConfigDetails where
  hashWithSalt _salt ConfigDetails' {..} =
    _salt `Prelude.hashWithSalt` endpointDetails
      `Prelude.hashWithSalt` s3RecordingDetails
      `Prelude.hashWithSalt` antennaDemodDecodeDetails

instance Prelude.NFData ConfigDetails where
  rnf ConfigDetails' {..} =
    Prelude.rnf endpointDetails
      `Prelude.seq` Prelude.rnf s3RecordingDetails
      `Prelude.seq` Prelude.rnf antennaDemodDecodeDetails

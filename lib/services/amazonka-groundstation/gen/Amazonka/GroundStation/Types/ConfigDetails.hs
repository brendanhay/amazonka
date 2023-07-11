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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.AntennaDemodDecodeDetails
import Amazonka.GroundStation.Types.EndpointDetails
import Amazonka.GroundStation.Types.S3RecordingDetails
import qualified Amazonka.Prelude as Prelude

-- | Details for certain @Config@ object types in a contact.
--
-- /See:/ 'newConfigDetails' smart constructor.
data ConfigDetails = ConfigDetails'
  { -- | Details for antenna demod decode @Config@ in a contact.
    antennaDemodDecodeDetails :: Prelude.Maybe AntennaDemodDecodeDetails,
    endpointDetails :: Prelude.Maybe EndpointDetails,
    -- | Details for an S3 recording @Config@ in a contact.
    s3RecordingDetails :: Prelude.Maybe S3RecordingDetails
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
-- 'antennaDemodDecodeDetails', 'configDetails_antennaDemodDecodeDetails' - Details for antenna demod decode @Config@ in a contact.
--
-- 'endpointDetails', 'configDetails_endpointDetails' - Undocumented member.
--
-- 's3RecordingDetails', 'configDetails_s3RecordingDetails' - Details for an S3 recording @Config@ in a contact.
newConfigDetails ::
  ConfigDetails
newConfigDetails =
  ConfigDetails'
    { antennaDemodDecodeDetails =
        Prelude.Nothing,
      endpointDetails = Prelude.Nothing,
      s3RecordingDetails = Prelude.Nothing
    }

-- | Details for antenna demod decode @Config@ in a contact.
configDetails_antennaDemodDecodeDetails :: Lens.Lens' ConfigDetails (Prelude.Maybe AntennaDemodDecodeDetails)
configDetails_antennaDemodDecodeDetails = Lens.lens (\ConfigDetails' {antennaDemodDecodeDetails} -> antennaDemodDecodeDetails) (\s@ConfigDetails' {} a -> s {antennaDemodDecodeDetails = a} :: ConfigDetails)

-- | Undocumented member.
configDetails_endpointDetails :: Lens.Lens' ConfigDetails (Prelude.Maybe EndpointDetails)
configDetails_endpointDetails = Lens.lens (\ConfigDetails' {endpointDetails} -> endpointDetails) (\s@ConfigDetails' {} a -> s {endpointDetails = a} :: ConfigDetails)

-- | Details for an S3 recording @Config@ in a contact.
configDetails_s3RecordingDetails :: Lens.Lens' ConfigDetails (Prelude.Maybe S3RecordingDetails)
configDetails_s3RecordingDetails = Lens.lens (\ConfigDetails' {s3RecordingDetails} -> s3RecordingDetails) (\s@ConfigDetails' {} a -> s {s3RecordingDetails = a} :: ConfigDetails)

instance Data.FromJSON ConfigDetails where
  parseJSON =
    Data.withObject
      "ConfigDetails"
      ( \x ->
          ConfigDetails'
            Prelude.<$> (x Data..:? "antennaDemodDecodeDetails")
            Prelude.<*> (x Data..:? "endpointDetails")
            Prelude.<*> (x Data..:? "s3RecordingDetails")
      )

instance Prelude.Hashable ConfigDetails where
  hashWithSalt _salt ConfigDetails' {..} =
    _salt
      `Prelude.hashWithSalt` antennaDemodDecodeDetails
      `Prelude.hashWithSalt` endpointDetails
      `Prelude.hashWithSalt` s3RecordingDetails

instance Prelude.NFData ConfigDetails where
  rnf ConfigDetails' {..} =
    Prelude.rnf antennaDemodDecodeDetails
      `Prelude.seq` Prelude.rnf endpointDetails
      `Prelude.seq` Prelude.rnf s3RecordingDetails

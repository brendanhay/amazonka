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
-- Module      : Network.AWS.MediaConvert.Types.DvbSdtSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSdtSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.OutputSdt

-- | Inserts DVB Service Description Table (NIT) at the specified table
-- repetition interval.
--
-- /See:/ 'newDvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'
  { -- | Selects method of inserting SDT information into output stream. \"Follow
    -- input SDT\" copies SDT information from input stream to output stream.
    -- \"Follow input SDT if present\" copies SDT information from input stream
    -- to output stream if SDT information is present in the input, otherwise
    -- it will fall back on the user-defined values. Enter \"SDT Manually\"
    -- means user will enter the SDT information. \"No SDT\" means output
    -- stream will not contain SDT information.
    outputSdt :: Core.Maybe OutputSdt,
    -- | The service name placed in the service_descriptor in the Service
    -- Description Table. Maximum length is 256 characters.
    serviceName :: Core.Maybe Core.Text,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    sdtInterval :: Core.Maybe Core.Natural,
    -- | The service provider name placed in the service_descriptor in the
    -- Service Description Table. Maximum length is 256 characters.
    serviceProviderName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DvbSdtSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSdt', 'dvbSdtSettings_outputSdt' - Selects method of inserting SDT information into output stream. \"Follow
-- input SDT\" copies SDT information from input stream to output stream.
-- \"Follow input SDT if present\" copies SDT information from input stream
-- to output stream if SDT information is present in the input, otherwise
-- it will fall back on the user-defined values. Enter \"SDT Manually\"
-- means user will enter the SDT information. \"No SDT\" means output
-- stream will not contain SDT information.
--
-- 'serviceName', 'dvbSdtSettings_serviceName' - The service name placed in the service_descriptor in the Service
-- Description Table. Maximum length is 256 characters.
--
-- 'sdtInterval', 'dvbSdtSettings_sdtInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'serviceProviderName', 'dvbSdtSettings_serviceProviderName' - The service provider name placed in the service_descriptor in the
-- Service Description Table. Maximum length is 256 characters.
newDvbSdtSettings ::
  DvbSdtSettings
newDvbSdtSettings =
  DvbSdtSettings'
    { outputSdt = Core.Nothing,
      serviceName = Core.Nothing,
      sdtInterval = Core.Nothing,
      serviceProviderName = Core.Nothing
    }

-- | Selects method of inserting SDT information into output stream. \"Follow
-- input SDT\" copies SDT information from input stream to output stream.
-- \"Follow input SDT if present\" copies SDT information from input stream
-- to output stream if SDT information is present in the input, otherwise
-- it will fall back on the user-defined values. Enter \"SDT Manually\"
-- means user will enter the SDT information. \"No SDT\" means output
-- stream will not contain SDT information.
dvbSdtSettings_outputSdt :: Lens.Lens' DvbSdtSettings (Core.Maybe OutputSdt)
dvbSdtSettings_outputSdt = Lens.lens (\DvbSdtSettings' {outputSdt} -> outputSdt) (\s@DvbSdtSettings' {} a -> s {outputSdt = a} :: DvbSdtSettings)

-- | The service name placed in the service_descriptor in the Service
-- Description Table. Maximum length is 256 characters.
dvbSdtSettings_serviceName :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Text)
dvbSdtSettings_serviceName = Lens.lens (\DvbSdtSettings' {serviceName} -> serviceName) (\s@DvbSdtSettings' {} a -> s {serviceName = a} :: DvbSdtSettings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
dvbSdtSettings_sdtInterval :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Natural)
dvbSdtSettings_sdtInterval = Lens.lens (\DvbSdtSettings' {sdtInterval} -> sdtInterval) (\s@DvbSdtSettings' {} a -> s {sdtInterval = a} :: DvbSdtSettings)

-- | The service provider name placed in the service_descriptor in the
-- Service Description Table. Maximum length is 256 characters.
dvbSdtSettings_serviceProviderName :: Lens.Lens' DvbSdtSettings (Core.Maybe Core.Text)
dvbSdtSettings_serviceProviderName = Lens.lens (\DvbSdtSettings' {serviceProviderName} -> serviceProviderName) (\s@DvbSdtSettings' {} a -> s {serviceProviderName = a} :: DvbSdtSettings)

instance Core.FromJSON DvbSdtSettings where
  parseJSON =
    Core.withObject
      "DvbSdtSettings"
      ( \x ->
          DvbSdtSettings'
            Core.<$> (x Core..:? "outputSdt")
            Core.<*> (x Core..:? "serviceName")
            Core.<*> (x Core..:? "sdtInterval")
            Core.<*> (x Core..:? "serviceProviderName")
      )

instance Core.Hashable DvbSdtSettings

instance Core.NFData DvbSdtSettings

instance Core.ToJSON DvbSdtSettings where
  toJSON DvbSdtSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("outputSdt" Core..=) Core.<$> outputSdt,
            ("serviceName" Core..=) Core.<$> serviceName,
            ("sdtInterval" Core..=) Core.<$> sdtInterval,
            ("serviceProviderName" Core..=)
              Core.<$> serviceProviderName
          ]
      )

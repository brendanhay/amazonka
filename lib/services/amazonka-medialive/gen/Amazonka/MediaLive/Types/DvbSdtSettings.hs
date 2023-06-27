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
-- Module      : Amazonka.MediaLive.Types.DvbSdtSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSdtSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.DvbSdtOutputSdt
import qualified Amazonka.Prelude as Prelude

-- | DVB Service Description Table (SDT)
--
-- /See:/ 'newDvbSdtSettings' smart constructor.
data DvbSdtSettings = DvbSdtSettings'
  { -- | Selects method of inserting SDT information into output stream. The
    -- sdtFollow setting copies SDT information from input stream to output
    -- stream. The sdtFollowIfPresent setting copies SDT information from input
    -- stream to output stream if SDT information is present in the input,
    -- otherwise it will fall back on the user-defined values. The sdtManual
    -- setting means user will enter the SDT information. The sdtNone setting
    -- means output stream will not contain SDT information.
    outputSdt :: Prelude.Maybe DvbSdtOutputSdt,
    -- | The number of milliseconds between instances of this table in the output
    -- transport stream.
    repInterval :: Prelude.Maybe Prelude.Natural,
    -- | The service name placed in the serviceDescriptor in the Service
    -- Description Table. Maximum length is 256 characters.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The service provider name placed in the serviceDescriptor in the Service
    -- Description Table. Maximum length is 256 characters.
    serviceProviderName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DvbSdtSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSdt', 'dvbSdtSettings_outputSdt' - Selects method of inserting SDT information into output stream. The
-- sdtFollow setting copies SDT information from input stream to output
-- stream. The sdtFollowIfPresent setting copies SDT information from input
-- stream to output stream if SDT information is present in the input,
-- otherwise it will fall back on the user-defined values. The sdtManual
-- setting means user will enter the SDT information. The sdtNone setting
-- means output stream will not contain SDT information.
--
-- 'repInterval', 'dvbSdtSettings_repInterval' - The number of milliseconds between instances of this table in the output
-- transport stream.
--
-- 'serviceName', 'dvbSdtSettings_serviceName' - The service name placed in the serviceDescriptor in the Service
-- Description Table. Maximum length is 256 characters.
--
-- 'serviceProviderName', 'dvbSdtSettings_serviceProviderName' - The service provider name placed in the serviceDescriptor in the Service
-- Description Table. Maximum length is 256 characters.
newDvbSdtSettings ::
  DvbSdtSettings
newDvbSdtSettings =
  DvbSdtSettings'
    { outputSdt = Prelude.Nothing,
      repInterval = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      serviceProviderName = Prelude.Nothing
    }

-- | Selects method of inserting SDT information into output stream. The
-- sdtFollow setting copies SDT information from input stream to output
-- stream. The sdtFollowIfPresent setting copies SDT information from input
-- stream to output stream if SDT information is present in the input,
-- otherwise it will fall back on the user-defined values. The sdtManual
-- setting means user will enter the SDT information. The sdtNone setting
-- means output stream will not contain SDT information.
dvbSdtSettings_outputSdt :: Lens.Lens' DvbSdtSettings (Prelude.Maybe DvbSdtOutputSdt)
dvbSdtSettings_outputSdt = Lens.lens (\DvbSdtSettings' {outputSdt} -> outputSdt) (\s@DvbSdtSettings' {} a -> s {outputSdt = a} :: DvbSdtSettings)

-- | The number of milliseconds between instances of this table in the output
-- transport stream.
dvbSdtSettings_repInterval :: Lens.Lens' DvbSdtSettings (Prelude.Maybe Prelude.Natural)
dvbSdtSettings_repInterval = Lens.lens (\DvbSdtSettings' {repInterval} -> repInterval) (\s@DvbSdtSettings' {} a -> s {repInterval = a} :: DvbSdtSettings)

-- | The service name placed in the serviceDescriptor in the Service
-- Description Table. Maximum length is 256 characters.
dvbSdtSettings_serviceName :: Lens.Lens' DvbSdtSettings (Prelude.Maybe Prelude.Text)
dvbSdtSettings_serviceName = Lens.lens (\DvbSdtSettings' {serviceName} -> serviceName) (\s@DvbSdtSettings' {} a -> s {serviceName = a} :: DvbSdtSettings)

-- | The service provider name placed in the serviceDescriptor in the Service
-- Description Table. Maximum length is 256 characters.
dvbSdtSettings_serviceProviderName :: Lens.Lens' DvbSdtSettings (Prelude.Maybe Prelude.Text)
dvbSdtSettings_serviceProviderName = Lens.lens (\DvbSdtSettings' {serviceProviderName} -> serviceProviderName) (\s@DvbSdtSettings' {} a -> s {serviceProviderName = a} :: DvbSdtSettings)

instance Data.FromJSON DvbSdtSettings where
  parseJSON =
    Data.withObject
      "DvbSdtSettings"
      ( \x ->
          DvbSdtSettings'
            Prelude.<$> (x Data..:? "outputSdt")
            Prelude.<*> (x Data..:? "repInterval")
            Prelude.<*> (x Data..:? "serviceName")
            Prelude.<*> (x Data..:? "serviceProviderName")
      )

instance Prelude.Hashable DvbSdtSettings where
  hashWithSalt _salt DvbSdtSettings' {..} =
    _salt
      `Prelude.hashWithSalt` outputSdt
      `Prelude.hashWithSalt` repInterval
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceProviderName

instance Prelude.NFData DvbSdtSettings where
  rnf DvbSdtSettings' {..} =
    Prelude.rnf outputSdt
      `Prelude.seq` Prelude.rnf repInterval
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceProviderName

instance Data.ToJSON DvbSdtSettings where
  toJSON DvbSdtSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("outputSdt" Data..=) Prelude.<$> outputSdt,
            ("repInterval" Data..=) Prelude.<$> repInterval,
            ("serviceName" Data..=) Prelude.<$> serviceName,
            ("serviceProviderName" Data..=)
              Prelude.<$> serviceProviderName
          ]
      )

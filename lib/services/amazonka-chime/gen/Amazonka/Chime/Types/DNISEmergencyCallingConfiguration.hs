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
-- Module      : Amazonka.Chime.Types.DNISEmergencyCallingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.DNISEmergencyCallingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Dialed Number Identification Service (DNIS) emergency calling
-- configuration details associated with an Amazon Chime Voice Connector\'s
-- emergency calling configuration.
--
-- /See:/ 'newDNISEmergencyCallingConfiguration' smart constructor.
data DNISEmergencyCallingConfiguration = DNISEmergencyCallingConfiguration'
  { -- | The DNIS phone number to route test emergency calls to, in E.164 format.
    testPhoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The DNIS phone number to route emergency calls to, in E.164 format.
    emergencyPhoneNumber :: Core.Sensitive Prelude.Text,
    -- | The country from which emergency calls are allowed, in ISO 3166-1
    -- alpha-2 format.
    callingCountry :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DNISEmergencyCallingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testPhoneNumber', 'dNISEmergencyCallingConfiguration_testPhoneNumber' - The DNIS phone number to route test emergency calls to, in E.164 format.
--
-- 'emergencyPhoneNumber', 'dNISEmergencyCallingConfiguration_emergencyPhoneNumber' - The DNIS phone number to route emergency calls to, in E.164 format.
--
-- 'callingCountry', 'dNISEmergencyCallingConfiguration_callingCountry' - The country from which emergency calls are allowed, in ISO 3166-1
-- alpha-2 format.
newDNISEmergencyCallingConfiguration ::
  -- | 'emergencyPhoneNumber'
  Prelude.Text ->
  -- | 'callingCountry'
  Prelude.Text ->
  DNISEmergencyCallingConfiguration
newDNISEmergencyCallingConfiguration
  pEmergencyPhoneNumber_
  pCallingCountry_ =
    DNISEmergencyCallingConfiguration'
      { testPhoneNumber =
          Prelude.Nothing,
        emergencyPhoneNumber =
          Core._Sensitive
            Lens.# pEmergencyPhoneNumber_,
        callingCountry = pCallingCountry_
      }

-- | The DNIS phone number to route test emergency calls to, in E.164 format.
dNISEmergencyCallingConfiguration_testPhoneNumber :: Lens.Lens' DNISEmergencyCallingConfiguration (Prelude.Maybe Prelude.Text)
dNISEmergencyCallingConfiguration_testPhoneNumber = Lens.lens (\DNISEmergencyCallingConfiguration' {testPhoneNumber} -> testPhoneNumber) (\s@DNISEmergencyCallingConfiguration' {} a -> s {testPhoneNumber = a} :: DNISEmergencyCallingConfiguration) Prelude.. Lens.mapping Core._Sensitive

-- | The DNIS phone number to route emergency calls to, in E.164 format.
dNISEmergencyCallingConfiguration_emergencyPhoneNumber :: Lens.Lens' DNISEmergencyCallingConfiguration Prelude.Text
dNISEmergencyCallingConfiguration_emergencyPhoneNumber = Lens.lens (\DNISEmergencyCallingConfiguration' {emergencyPhoneNumber} -> emergencyPhoneNumber) (\s@DNISEmergencyCallingConfiguration' {} a -> s {emergencyPhoneNumber = a} :: DNISEmergencyCallingConfiguration) Prelude.. Core._Sensitive

-- | The country from which emergency calls are allowed, in ISO 3166-1
-- alpha-2 format.
dNISEmergencyCallingConfiguration_callingCountry :: Lens.Lens' DNISEmergencyCallingConfiguration Prelude.Text
dNISEmergencyCallingConfiguration_callingCountry = Lens.lens (\DNISEmergencyCallingConfiguration' {callingCountry} -> callingCountry) (\s@DNISEmergencyCallingConfiguration' {} a -> s {callingCountry = a} :: DNISEmergencyCallingConfiguration)

instance
  Core.FromJSON
    DNISEmergencyCallingConfiguration
  where
  parseJSON =
    Core.withObject
      "DNISEmergencyCallingConfiguration"
      ( \x ->
          DNISEmergencyCallingConfiguration'
            Prelude.<$> (x Core..:? "TestPhoneNumber")
            Prelude.<*> (x Core..: "EmergencyPhoneNumber")
            Prelude.<*> (x Core..: "CallingCountry")
      )

instance
  Prelude.Hashable
    DNISEmergencyCallingConfiguration
  where
  hashWithSalt
    _salt
    DNISEmergencyCallingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` testPhoneNumber
        `Prelude.hashWithSalt` emergencyPhoneNumber
        `Prelude.hashWithSalt` callingCountry

instance
  Prelude.NFData
    DNISEmergencyCallingConfiguration
  where
  rnf DNISEmergencyCallingConfiguration' {..} =
    Prelude.rnf testPhoneNumber
      `Prelude.seq` Prelude.rnf emergencyPhoneNumber
      `Prelude.seq` Prelude.rnf callingCountry

instance
  Core.ToJSON
    DNISEmergencyCallingConfiguration
  where
  toJSON DNISEmergencyCallingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TestPhoneNumber" Core..=)
              Prelude.<$> testPhoneNumber,
            Prelude.Just
              ( "EmergencyPhoneNumber"
                  Core..= emergencyPhoneNumber
              ),
            Prelude.Just
              ("CallingCountry" Core..= callingCountry)
          ]
      )

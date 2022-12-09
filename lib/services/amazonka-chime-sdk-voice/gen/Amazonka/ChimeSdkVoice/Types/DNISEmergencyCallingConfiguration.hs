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
-- Module      : Amazonka.ChimeSdkVoice.Types.DNISEmergencyCallingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.DNISEmergencyCallingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newDNISEmergencyCallingConfiguration' smart constructor.
data DNISEmergencyCallingConfiguration = DNISEmergencyCallingConfiguration'
  { testPhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    emergencyPhoneNumber :: Data.Sensitive Prelude.Text,
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
-- 'testPhoneNumber', 'dNISEmergencyCallingConfiguration_testPhoneNumber' - Undocumented member.
--
-- 'emergencyPhoneNumber', 'dNISEmergencyCallingConfiguration_emergencyPhoneNumber' - Undocumented member.
--
-- 'callingCountry', 'dNISEmergencyCallingConfiguration_callingCountry' - Undocumented member.
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
          Data._Sensitive
            Lens.# pEmergencyPhoneNumber_,
        callingCountry = pCallingCountry_
      }

-- | Undocumented member.
dNISEmergencyCallingConfiguration_testPhoneNumber :: Lens.Lens' DNISEmergencyCallingConfiguration (Prelude.Maybe Prelude.Text)
dNISEmergencyCallingConfiguration_testPhoneNumber = Lens.lens (\DNISEmergencyCallingConfiguration' {testPhoneNumber} -> testPhoneNumber) (\s@DNISEmergencyCallingConfiguration' {} a -> s {testPhoneNumber = a} :: DNISEmergencyCallingConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
dNISEmergencyCallingConfiguration_emergencyPhoneNumber :: Lens.Lens' DNISEmergencyCallingConfiguration Prelude.Text
dNISEmergencyCallingConfiguration_emergencyPhoneNumber = Lens.lens (\DNISEmergencyCallingConfiguration' {emergencyPhoneNumber} -> emergencyPhoneNumber) (\s@DNISEmergencyCallingConfiguration' {} a -> s {emergencyPhoneNumber = a} :: DNISEmergencyCallingConfiguration) Prelude.. Data._Sensitive

-- | Undocumented member.
dNISEmergencyCallingConfiguration_callingCountry :: Lens.Lens' DNISEmergencyCallingConfiguration Prelude.Text
dNISEmergencyCallingConfiguration_callingCountry = Lens.lens (\DNISEmergencyCallingConfiguration' {callingCountry} -> callingCountry) (\s@DNISEmergencyCallingConfiguration' {} a -> s {callingCountry = a} :: DNISEmergencyCallingConfiguration)

instance
  Data.FromJSON
    DNISEmergencyCallingConfiguration
  where
  parseJSON =
    Data.withObject
      "DNISEmergencyCallingConfiguration"
      ( \x ->
          DNISEmergencyCallingConfiguration'
            Prelude.<$> (x Data..:? "TestPhoneNumber")
            Prelude.<*> (x Data..: "EmergencyPhoneNumber")
            Prelude.<*> (x Data..: "CallingCountry")
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
  Data.ToJSON
    DNISEmergencyCallingConfiguration
  where
  toJSON DNISEmergencyCallingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TestPhoneNumber" Data..=)
              Prelude.<$> testPhoneNumber,
            Prelude.Just
              ( "EmergencyPhoneNumber"
                  Data..= emergencyPhoneNumber
              ),
            Prelude.Just
              ("CallingCountry" Data..= callingCountry)
          ]
      )

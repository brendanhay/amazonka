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
-- Module      : Amazonka.ChimeSdkVoice.Types.EmergencyCallingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.EmergencyCallingConfiguration where

import Amazonka.ChimeSdkVoice.Types.DNISEmergencyCallingConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The emergency calling configuration details associated with an Amazon
-- Chime SDK Voice Connector.
--
-- /See:/ 'newEmergencyCallingConfiguration' smart constructor.
data EmergencyCallingConfiguration = EmergencyCallingConfiguration'
  { -- | The Dialed Number Identification Service (DNIS) emergency calling
    -- configuration details.
    dnis :: Prelude.Maybe [DNISEmergencyCallingConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmergencyCallingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnis', 'emergencyCallingConfiguration_dnis' - The Dialed Number Identification Service (DNIS) emergency calling
-- configuration details.
newEmergencyCallingConfiguration ::
  EmergencyCallingConfiguration
newEmergencyCallingConfiguration =
  EmergencyCallingConfiguration'
    { dnis =
        Prelude.Nothing
    }

-- | The Dialed Number Identification Service (DNIS) emergency calling
-- configuration details.
emergencyCallingConfiguration_dnis :: Lens.Lens' EmergencyCallingConfiguration (Prelude.Maybe [DNISEmergencyCallingConfiguration])
emergencyCallingConfiguration_dnis = Lens.lens (\EmergencyCallingConfiguration' {dnis} -> dnis) (\s@EmergencyCallingConfiguration' {} a -> s {dnis = a} :: EmergencyCallingConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EmergencyCallingConfiguration where
  parseJSON =
    Data.withObject
      "EmergencyCallingConfiguration"
      ( \x ->
          EmergencyCallingConfiguration'
            Prelude.<$> (x Data..:? "DNIS" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    EmergencyCallingConfiguration
  where
  hashWithSalt _salt EmergencyCallingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` dnis

instance Prelude.NFData EmergencyCallingConfiguration where
  rnf EmergencyCallingConfiguration' {..} =
    Prelude.rnf dnis

instance Data.ToJSON EmergencyCallingConfiguration where
  toJSON EmergencyCallingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("DNIS" Data..=) Prelude.<$> dnis]
      )

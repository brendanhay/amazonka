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
-- Module      : Amazonka.IoTSiteWise.Types.ConfigurationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ConfigurationStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.ConfigurationErrorDetails
import Amazonka.IoTSiteWise.Types.ConfigurationState
import qualified Amazonka.Prelude as Prelude

-- | Contains current status information for the configuration.
--
-- /See:/ 'newConfigurationStatus' smart constructor.
data ConfigurationStatus = ConfigurationStatus'
  { -- | Contains associated error information, if any.
    error :: Prelude.Maybe ConfigurationErrorDetails,
    -- | The current state of the configuration.
    state :: ConfigurationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'configurationStatus_error' - Contains associated error information, if any.
--
-- 'state', 'configurationStatus_state' - The current state of the configuration.
newConfigurationStatus ::
  -- | 'state'
  ConfigurationState ->
  ConfigurationStatus
newConfigurationStatus pState_ =
  ConfigurationStatus'
    { error = Prelude.Nothing,
      state = pState_
    }

-- | Contains associated error information, if any.
configurationStatus_error :: Lens.Lens' ConfigurationStatus (Prelude.Maybe ConfigurationErrorDetails)
configurationStatus_error = Lens.lens (\ConfigurationStatus' {error} -> error) (\s@ConfigurationStatus' {} a -> s {error = a} :: ConfigurationStatus)

-- | The current state of the configuration.
configurationStatus_state :: Lens.Lens' ConfigurationStatus ConfigurationState
configurationStatus_state = Lens.lens (\ConfigurationStatus' {state} -> state) (\s@ConfigurationStatus' {} a -> s {state = a} :: ConfigurationStatus)

instance Data.FromJSON ConfigurationStatus where
  parseJSON =
    Data.withObject
      "ConfigurationStatus"
      ( \x ->
          ConfigurationStatus'
            Prelude.<$> (x Data..:? "error")
            Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable ConfigurationStatus where
  hashWithSalt _salt ConfigurationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` state

instance Prelude.NFData ConfigurationStatus where
  rnf ConfigurationStatus' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf state

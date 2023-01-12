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
-- Module      : Amazonka.IoTSiteWise.Types.PortalStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.PortalStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.MonitorErrorDetails
import Amazonka.IoTSiteWise.Types.PortalState
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the current status of a portal.
--
-- /See:/ 'newPortalStatus' smart constructor.
data PortalStatus = PortalStatus'
  { -- | Contains associated error information, if any.
    error :: Prelude.Maybe MonitorErrorDetails,
    -- | The current state of the portal.
    state :: PortalState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortalStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'portalStatus_error' - Contains associated error information, if any.
--
-- 'state', 'portalStatus_state' - The current state of the portal.
newPortalStatus ::
  -- | 'state'
  PortalState ->
  PortalStatus
newPortalStatus pState_ =
  PortalStatus'
    { error = Prelude.Nothing,
      state = pState_
    }

-- | Contains associated error information, if any.
portalStatus_error :: Lens.Lens' PortalStatus (Prelude.Maybe MonitorErrorDetails)
portalStatus_error = Lens.lens (\PortalStatus' {error} -> error) (\s@PortalStatus' {} a -> s {error = a} :: PortalStatus)

-- | The current state of the portal.
portalStatus_state :: Lens.Lens' PortalStatus PortalState
portalStatus_state = Lens.lens (\PortalStatus' {state} -> state) (\s@PortalStatus' {} a -> s {state = a} :: PortalStatus)

instance Data.FromJSON PortalStatus where
  parseJSON =
    Data.withObject
      "PortalStatus"
      ( \x ->
          PortalStatus'
            Prelude.<$> (x Data..:? "error") Prelude.<*> (x Data..: "state")
      )

instance Prelude.Hashable PortalStatus where
  hashWithSalt _salt PortalStatus' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` state

instance Prelude.NFData PortalStatus where
  rnf PortalStatus' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf state

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
-- Module      : Amazonka.IoTSiteWise.Types.GatewayCapabilitySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.GatewayCapabilitySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.CapabilitySyncStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of a gateway capability configuration.
--
-- /See:/ 'newGatewayCapabilitySummary' smart constructor.
data GatewayCapabilitySummary = GatewayCapabilitySummary'
  { -- | The namespace of the capability configuration. For example, if you
    -- configure OPC-UA sources from the IoT SiteWise console, your OPC-UA
    -- capability configuration has the namespace
    -- @iotsitewise:opcuacollector:version@, where @version@ is a number such
    -- as @1@.
    capabilityNamespace :: Prelude.Text,
    -- | The synchronization status of the capability configuration. The sync
    -- status can be one of the following:
    --
    -- -   @IN_SYNC@ – The gateway is running the capability configuration.
    --
    -- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
    --     configuration.
    --
    -- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
    capabilitySyncStatus :: CapabilitySyncStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayCapabilitySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilityNamespace', 'gatewayCapabilitySummary_capabilityNamespace' - The namespace of the capability configuration. For example, if you
-- configure OPC-UA sources from the IoT SiteWise console, your OPC-UA
-- capability configuration has the namespace
-- @iotsitewise:opcuacollector:version@, where @version@ is a number such
-- as @1@.
--
-- 'capabilitySyncStatus', 'gatewayCapabilitySummary_capabilitySyncStatus' - The synchronization status of the capability configuration. The sync
-- status can be one of the following:
--
-- -   @IN_SYNC@ – The gateway is running the capability configuration.
--
-- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
--     configuration.
--
-- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
newGatewayCapabilitySummary ::
  -- | 'capabilityNamespace'
  Prelude.Text ->
  -- | 'capabilitySyncStatus'
  CapabilitySyncStatus ->
  GatewayCapabilitySummary
newGatewayCapabilitySummary
  pCapabilityNamespace_
  pCapabilitySyncStatus_ =
    GatewayCapabilitySummary'
      { capabilityNamespace =
          pCapabilityNamespace_,
        capabilitySyncStatus = pCapabilitySyncStatus_
      }

-- | The namespace of the capability configuration. For example, if you
-- configure OPC-UA sources from the IoT SiteWise console, your OPC-UA
-- capability configuration has the namespace
-- @iotsitewise:opcuacollector:version@, where @version@ is a number such
-- as @1@.
gatewayCapabilitySummary_capabilityNamespace :: Lens.Lens' GatewayCapabilitySummary Prelude.Text
gatewayCapabilitySummary_capabilityNamespace = Lens.lens (\GatewayCapabilitySummary' {capabilityNamespace} -> capabilityNamespace) (\s@GatewayCapabilitySummary' {} a -> s {capabilityNamespace = a} :: GatewayCapabilitySummary)

-- | The synchronization status of the capability configuration. The sync
-- status can be one of the following:
--
-- -   @IN_SYNC@ – The gateway is running the capability configuration.
--
-- -   @OUT_OF_SYNC@ – The gateway hasn\'t received the capability
--     configuration.
--
-- -   @SYNC_FAILED@ – The gateway rejected the capability configuration.
gatewayCapabilitySummary_capabilitySyncStatus :: Lens.Lens' GatewayCapabilitySummary CapabilitySyncStatus
gatewayCapabilitySummary_capabilitySyncStatus = Lens.lens (\GatewayCapabilitySummary' {capabilitySyncStatus} -> capabilitySyncStatus) (\s@GatewayCapabilitySummary' {} a -> s {capabilitySyncStatus = a} :: GatewayCapabilitySummary)

instance Data.FromJSON GatewayCapabilitySummary where
  parseJSON =
    Data.withObject
      "GatewayCapabilitySummary"
      ( \x ->
          GatewayCapabilitySummary'
            Prelude.<$> (x Data..: "capabilityNamespace")
            Prelude.<*> (x Data..: "capabilitySyncStatus")
      )

instance Prelude.Hashable GatewayCapabilitySummary where
  hashWithSalt _salt GatewayCapabilitySummary' {..} =
    _salt `Prelude.hashWithSalt` capabilityNamespace
      `Prelude.hashWithSalt` capabilitySyncStatus

instance Prelude.NFData GatewayCapabilitySummary where
  rnf GatewayCapabilitySummary' {..} =
    Prelude.rnf capabilityNamespace
      `Prelude.seq` Prelude.rnf capabilitySyncStatus

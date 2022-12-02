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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteTarget where

import Amazonka.AppMesh.Types.GatewayRouteVirtualService
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a gateway route target.
--
-- /See:/ 'newGatewayRouteTarget' smart constructor.
data GatewayRouteTarget = GatewayRouteTarget'
  { -- | The port number of the gateway route target.
    port :: Prelude.Maybe Prelude.Natural,
    -- | An object that represents a virtual service gateway route target.
    virtualService :: GatewayRouteVirtualService
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayRouteTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'gatewayRouteTarget_port' - The port number of the gateway route target.
--
-- 'virtualService', 'gatewayRouteTarget_virtualService' - An object that represents a virtual service gateway route target.
newGatewayRouteTarget ::
  -- | 'virtualService'
  GatewayRouteVirtualService ->
  GatewayRouteTarget
newGatewayRouteTarget pVirtualService_ =
  GatewayRouteTarget'
    { port = Prelude.Nothing,
      virtualService = pVirtualService_
    }

-- | The port number of the gateway route target.
gatewayRouteTarget_port :: Lens.Lens' GatewayRouteTarget (Prelude.Maybe Prelude.Natural)
gatewayRouteTarget_port = Lens.lens (\GatewayRouteTarget' {port} -> port) (\s@GatewayRouteTarget' {} a -> s {port = a} :: GatewayRouteTarget)

-- | An object that represents a virtual service gateway route target.
gatewayRouteTarget_virtualService :: Lens.Lens' GatewayRouteTarget GatewayRouteVirtualService
gatewayRouteTarget_virtualService = Lens.lens (\GatewayRouteTarget' {virtualService} -> virtualService) (\s@GatewayRouteTarget' {} a -> s {virtualService = a} :: GatewayRouteTarget)

instance Data.FromJSON GatewayRouteTarget where
  parseJSON =
    Data.withObject
      "GatewayRouteTarget"
      ( \x ->
          GatewayRouteTarget'
            Prelude.<$> (x Data..:? "port")
            Prelude.<*> (x Data..: "virtualService")
      )

instance Prelude.Hashable GatewayRouteTarget where
  hashWithSalt _salt GatewayRouteTarget' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` virtualService

instance Prelude.NFData GatewayRouteTarget where
  rnf GatewayRouteTarget' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf virtualService

instance Data.ToJSON GatewayRouteTarget where
  toJSON GatewayRouteTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("port" Data..=) Prelude.<$> port,
            Prelude.Just
              ("virtualService" Data..= virtualService)
          ]
      )

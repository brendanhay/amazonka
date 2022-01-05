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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteTarget where

import Amazonka.AppMesh.Types.GatewayRouteVirtualService
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a gateway route target.
--
-- /See:/ 'newGatewayRouteTarget' smart constructor.
data GatewayRouteTarget = GatewayRouteTarget'
  { -- | An object that represents a virtual service gateway route target.
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
-- 'virtualService', 'gatewayRouteTarget_virtualService' - An object that represents a virtual service gateway route target.
newGatewayRouteTarget ::
  -- | 'virtualService'
  GatewayRouteVirtualService ->
  GatewayRouteTarget
newGatewayRouteTarget pVirtualService_ =
  GatewayRouteTarget'
    { virtualService =
        pVirtualService_
    }

-- | An object that represents a virtual service gateway route target.
gatewayRouteTarget_virtualService :: Lens.Lens' GatewayRouteTarget GatewayRouteVirtualService
gatewayRouteTarget_virtualService = Lens.lens (\GatewayRouteTarget' {virtualService} -> virtualService) (\s@GatewayRouteTarget' {} a -> s {virtualService = a} :: GatewayRouteTarget)

instance Core.FromJSON GatewayRouteTarget where
  parseJSON =
    Core.withObject
      "GatewayRouteTarget"
      ( \x ->
          GatewayRouteTarget'
            Prelude.<$> (x Core..: "virtualService")
      )

instance Prelude.Hashable GatewayRouteTarget where
  hashWithSalt _salt GatewayRouteTarget' {..} =
    _salt `Prelude.hashWithSalt` virtualService

instance Prelude.NFData GatewayRouteTarget where
  rnf GatewayRouteTarget' {..} =
    Prelude.rnf virtualService

instance Core.ToJSON GatewayRouteTarget where
  toJSON GatewayRouteTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("virtualService" Core..= virtualService)
          ]
      )

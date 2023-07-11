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
-- Module      : Amazonka.AppMesh.Types.GrpcGatewayRouteAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcGatewayRouteAction where

import Amazonka.AppMesh.Types.GatewayRouteTarget
import Amazonka.AppMesh.Types.GrpcGatewayRouteRewrite
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the action to take if a match is determined.
--
-- /See:/ 'newGrpcGatewayRouteAction' smart constructor.
data GrpcGatewayRouteAction = GrpcGatewayRouteAction'
  { -- | The gateway route action to rewrite.
    rewrite :: Prelude.Maybe GrpcGatewayRouteRewrite,
    -- | An object that represents the target that traffic is routed to when a
    -- request matches the gateway route.
    target :: GatewayRouteTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcGatewayRouteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rewrite', 'grpcGatewayRouteAction_rewrite' - The gateway route action to rewrite.
--
-- 'target', 'grpcGatewayRouteAction_target' - An object that represents the target that traffic is routed to when a
-- request matches the gateway route.
newGrpcGatewayRouteAction ::
  -- | 'target'
  GatewayRouteTarget ->
  GrpcGatewayRouteAction
newGrpcGatewayRouteAction pTarget_ =
  GrpcGatewayRouteAction'
    { rewrite = Prelude.Nothing,
      target = pTarget_
    }

-- | The gateway route action to rewrite.
grpcGatewayRouteAction_rewrite :: Lens.Lens' GrpcGatewayRouteAction (Prelude.Maybe GrpcGatewayRouteRewrite)
grpcGatewayRouteAction_rewrite = Lens.lens (\GrpcGatewayRouteAction' {rewrite} -> rewrite) (\s@GrpcGatewayRouteAction' {} a -> s {rewrite = a} :: GrpcGatewayRouteAction)

-- | An object that represents the target that traffic is routed to when a
-- request matches the gateway route.
grpcGatewayRouteAction_target :: Lens.Lens' GrpcGatewayRouteAction GatewayRouteTarget
grpcGatewayRouteAction_target = Lens.lens (\GrpcGatewayRouteAction' {target} -> target) (\s@GrpcGatewayRouteAction' {} a -> s {target = a} :: GrpcGatewayRouteAction)

instance Data.FromJSON GrpcGatewayRouteAction where
  parseJSON =
    Data.withObject
      "GrpcGatewayRouteAction"
      ( \x ->
          GrpcGatewayRouteAction'
            Prelude.<$> (x Data..:? "rewrite")
            Prelude.<*> (x Data..: "target")
      )

instance Prelude.Hashable GrpcGatewayRouteAction where
  hashWithSalt _salt GrpcGatewayRouteAction' {..} =
    _salt
      `Prelude.hashWithSalt` rewrite
      `Prelude.hashWithSalt` target

instance Prelude.NFData GrpcGatewayRouteAction where
  rnf GrpcGatewayRouteAction' {..} =
    Prelude.rnf rewrite
      `Prelude.seq` Prelude.rnf target

instance Data.ToJSON GrpcGatewayRouteAction where
  toJSON GrpcGatewayRouteAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("rewrite" Data..=) Prelude.<$> rewrite,
            Prelude.Just ("target" Data..= target)
          ]
      )

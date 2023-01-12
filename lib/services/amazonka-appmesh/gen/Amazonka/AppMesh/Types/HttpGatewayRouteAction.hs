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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRouteAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRouteAction where

import Amazonka.AppMesh.Types.GatewayRouteTarget
import Amazonka.AppMesh.Types.HttpGatewayRouteRewrite
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the action to take if a match is determined.
--
-- /See:/ 'newHttpGatewayRouteAction' smart constructor.
data HttpGatewayRouteAction = HttpGatewayRouteAction'
  { -- | The gateway route action to rewrite.
    rewrite :: Prelude.Maybe HttpGatewayRouteRewrite,
    -- | An object that represents the target that traffic is routed to when a
    -- request matches the gateway route.
    target :: GatewayRouteTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRouteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rewrite', 'httpGatewayRouteAction_rewrite' - The gateway route action to rewrite.
--
-- 'target', 'httpGatewayRouteAction_target' - An object that represents the target that traffic is routed to when a
-- request matches the gateway route.
newHttpGatewayRouteAction ::
  -- | 'target'
  GatewayRouteTarget ->
  HttpGatewayRouteAction
newHttpGatewayRouteAction pTarget_ =
  HttpGatewayRouteAction'
    { rewrite = Prelude.Nothing,
      target = pTarget_
    }

-- | The gateway route action to rewrite.
httpGatewayRouteAction_rewrite :: Lens.Lens' HttpGatewayRouteAction (Prelude.Maybe HttpGatewayRouteRewrite)
httpGatewayRouteAction_rewrite = Lens.lens (\HttpGatewayRouteAction' {rewrite} -> rewrite) (\s@HttpGatewayRouteAction' {} a -> s {rewrite = a} :: HttpGatewayRouteAction)

-- | An object that represents the target that traffic is routed to when a
-- request matches the gateway route.
httpGatewayRouteAction_target :: Lens.Lens' HttpGatewayRouteAction GatewayRouteTarget
httpGatewayRouteAction_target = Lens.lens (\HttpGatewayRouteAction' {target} -> target) (\s@HttpGatewayRouteAction' {} a -> s {target = a} :: HttpGatewayRouteAction)

instance Data.FromJSON HttpGatewayRouteAction where
  parseJSON =
    Data.withObject
      "HttpGatewayRouteAction"
      ( \x ->
          HttpGatewayRouteAction'
            Prelude.<$> (x Data..:? "rewrite")
            Prelude.<*> (x Data..: "target")
      )

instance Prelude.Hashable HttpGatewayRouteAction where
  hashWithSalt _salt HttpGatewayRouteAction' {..} =
    _salt `Prelude.hashWithSalt` rewrite
      `Prelude.hashWithSalt` target

instance Prelude.NFData HttpGatewayRouteAction where
  rnf HttpGatewayRouteAction' {..} =
    Prelude.rnf rewrite
      `Prelude.seq` Prelude.rnf target

instance Data.ToJSON HttpGatewayRouteAction where
  toJSON HttpGatewayRouteAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("rewrite" Data..=) Prelude.<$> rewrite,
            Prelude.Just ("target" Data..= target)
          ]
      )

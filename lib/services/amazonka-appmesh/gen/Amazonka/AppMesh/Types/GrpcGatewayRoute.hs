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
-- Module      : Amazonka.AppMesh.Types.GrpcGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcGatewayRoute where

import Amazonka.AppMesh.Types.GrpcGatewayRouteAction
import Amazonka.AppMesh.Types.GrpcGatewayRouteMatch
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a gRPC gateway route.
--
-- /See:/ 'newGrpcGatewayRoute' smart constructor.
data GrpcGatewayRoute = GrpcGatewayRoute'
  { -- | An object that represents the action to take if a match is determined.
    action :: GrpcGatewayRouteAction,
    -- | An object that represents the criteria for determining a request match.
    match :: GrpcGatewayRouteMatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'grpcGatewayRoute_action' - An object that represents the action to take if a match is determined.
--
-- 'match', 'grpcGatewayRoute_match' - An object that represents the criteria for determining a request match.
newGrpcGatewayRoute ::
  -- | 'action'
  GrpcGatewayRouteAction ->
  -- | 'match'
  GrpcGatewayRouteMatch ->
  GrpcGatewayRoute
newGrpcGatewayRoute pAction_ pMatch_ =
  GrpcGatewayRoute'
    { action = pAction_,
      match = pMatch_
    }

-- | An object that represents the action to take if a match is determined.
grpcGatewayRoute_action :: Lens.Lens' GrpcGatewayRoute GrpcGatewayRouteAction
grpcGatewayRoute_action = Lens.lens (\GrpcGatewayRoute' {action} -> action) (\s@GrpcGatewayRoute' {} a -> s {action = a} :: GrpcGatewayRoute)

-- | An object that represents the criteria for determining a request match.
grpcGatewayRoute_match :: Lens.Lens' GrpcGatewayRoute GrpcGatewayRouteMatch
grpcGatewayRoute_match = Lens.lens (\GrpcGatewayRoute' {match} -> match) (\s@GrpcGatewayRoute' {} a -> s {match = a} :: GrpcGatewayRoute)

instance Core.FromJSON GrpcGatewayRoute where
  parseJSON =
    Core.withObject
      "GrpcGatewayRoute"
      ( \x ->
          GrpcGatewayRoute'
            Prelude.<$> (x Core..: "action") Prelude.<*> (x Core..: "match")
      )

instance Prelude.Hashable GrpcGatewayRoute where
  hashWithSalt _salt GrpcGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` match

instance Prelude.NFData GrpcGatewayRoute where
  rnf GrpcGatewayRoute' {..} =
    Prelude.rnf action `Prelude.seq` Prelude.rnf match

instance Core.ToJSON GrpcGatewayRoute where
  toJSON GrpcGatewayRoute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("action" Core..= action),
            Prelude.Just ("match" Core..= match)
          ]
      )

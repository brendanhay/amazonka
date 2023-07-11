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
-- Module      : Amazonka.AppMesh.Types.GrpcRouteAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GrpcRouteAction where

import Amazonka.AppMesh.Types.WeightedTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the action to take if a match is determined.
--
-- /See:/ 'newGrpcRouteAction' smart constructor.
data GrpcRouteAction = GrpcRouteAction'
  { -- | An object that represents the targets that traffic is routed to when a
    -- request matches the route.
    weightedTargets :: Prelude.NonEmpty WeightedTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GrpcRouteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weightedTargets', 'grpcRouteAction_weightedTargets' - An object that represents the targets that traffic is routed to when a
-- request matches the route.
newGrpcRouteAction ::
  -- | 'weightedTargets'
  Prelude.NonEmpty WeightedTarget ->
  GrpcRouteAction
newGrpcRouteAction pWeightedTargets_ =
  GrpcRouteAction'
    { weightedTargets =
        Lens.coerced Lens.# pWeightedTargets_
    }

-- | An object that represents the targets that traffic is routed to when a
-- request matches the route.
grpcRouteAction_weightedTargets :: Lens.Lens' GrpcRouteAction (Prelude.NonEmpty WeightedTarget)
grpcRouteAction_weightedTargets = Lens.lens (\GrpcRouteAction' {weightedTargets} -> weightedTargets) (\s@GrpcRouteAction' {} a -> s {weightedTargets = a} :: GrpcRouteAction) Prelude.. Lens.coerced

instance Data.FromJSON GrpcRouteAction where
  parseJSON =
    Data.withObject
      "GrpcRouteAction"
      ( \x ->
          GrpcRouteAction'
            Prelude.<$> (x Data..: "weightedTargets")
      )

instance Prelude.Hashable GrpcRouteAction where
  hashWithSalt _salt GrpcRouteAction' {..} =
    _salt `Prelude.hashWithSalt` weightedTargets

instance Prelude.NFData GrpcRouteAction where
  rnf GrpcRouteAction' {..} =
    Prelude.rnf weightedTargets

instance Data.ToJSON GrpcRouteAction where
  toJSON GrpcRouteAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("weightedTargets" Data..= weightedTargets)
          ]
      )

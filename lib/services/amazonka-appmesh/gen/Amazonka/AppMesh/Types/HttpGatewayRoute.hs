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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRoute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRoute where

import Amazonka.AppMesh.Types.HttpGatewayRouteAction
import Amazonka.AppMesh.Types.HttpGatewayRouteMatch
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an HTTP gateway route.
--
-- /See:/ 'newHttpGatewayRoute' smart constructor.
data HttpGatewayRoute = HttpGatewayRoute'
  { -- | An object that represents the action to take if a match is determined.
    action :: HttpGatewayRouteAction,
    -- | An object that represents the criteria for determining a request match.
    match :: HttpGatewayRouteMatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'httpGatewayRoute_action' - An object that represents the action to take if a match is determined.
--
-- 'match', 'httpGatewayRoute_match' - An object that represents the criteria for determining a request match.
newHttpGatewayRoute ::
  -- | 'action'
  HttpGatewayRouteAction ->
  -- | 'match'
  HttpGatewayRouteMatch ->
  HttpGatewayRoute
newHttpGatewayRoute pAction_ pMatch_ =
  HttpGatewayRoute'
    { action = pAction_,
      match = pMatch_
    }

-- | An object that represents the action to take if a match is determined.
httpGatewayRoute_action :: Lens.Lens' HttpGatewayRoute HttpGatewayRouteAction
httpGatewayRoute_action = Lens.lens (\HttpGatewayRoute' {action} -> action) (\s@HttpGatewayRoute' {} a -> s {action = a} :: HttpGatewayRoute)

-- | An object that represents the criteria for determining a request match.
httpGatewayRoute_match :: Lens.Lens' HttpGatewayRoute HttpGatewayRouteMatch
httpGatewayRoute_match = Lens.lens (\HttpGatewayRoute' {match} -> match) (\s@HttpGatewayRoute' {} a -> s {match = a} :: HttpGatewayRoute)

instance Data.FromJSON HttpGatewayRoute where
  parseJSON =
    Data.withObject
      "HttpGatewayRoute"
      ( \x ->
          HttpGatewayRoute'
            Prelude.<$> (x Data..: "action") Prelude.<*> (x Data..: "match")
      )

instance Prelude.Hashable HttpGatewayRoute where
  hashWithSalt _salt HttpGatewayRoute' {..} =
    _salt `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` match

instance Prelude.NFData HttpGatewayRoute where
  rnf HttpGatewayRoute' {..} =
    Prelude.rnf action `Prelude.seq` Prelude.rnf match

instance Data.ToJSON HttpGatewayRoute where
  toJSON HttpGatewayRoute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("action" Data..= action),
            Prelude.Just ("match" Data..= match)
          ]
      )

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
-- Module      : Amazonka.NetworkManager.Types.RouteTableIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.RouteTableIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types.CoreNetworkSegmentEdgeIdentifier
import qualified Amazonka.Prelude as Prelude

-- | Describes a route table.
--
-- /See:/ 'newRouteTableIdentifier' smart constructor.
data RouteTableIdentifier = RouteTableIdentifier'
  { -- | The segment edge in a core network.
    coreNetworkSegmentEdge :: Prelude.Maybe CoreNetworkSegmentEdgeIdentifier,
    -- | The ARN of the transit gateway route table.
    transitGatewayRouteTableArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteTableIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreNetworkSegmentEdge', 'routeTableIdentifier_coreNetworkSegmentEdge' - The segment edge in a core network.
--
-- 'transitGatewayRouteTableArn', 'routeTableIdentifier_transitGatewayRouteTableArn' - The ARN of the transit gateway route table.
newRouteTableIdentifier ::
  RouteTableIdentifier
newRouteTableIdentifier =
  RouteTableIdentifier'
    { coreNetworkSegmentEdge =
        Prelude.Nothing,
      transitGatewayRouteTableArn = Prelude.Nothing
    }

-- | The segment edge in a core network.
routeTableIdentifier_coreNetworkSegmentEdge :: Lens.Lens' RouteTableIdentifier (Prelude.Maybe CoreNetworkSegmentEdgeIdentifier)
routeTableIdentifier_coreNetworkSegmentEdge = Lens.lens (\RouteTableIdentifier' {coreNetworkSegmentEdge} -> coreNetworkSegmentEdge) (\s@RouteTableIdentifier' {} a -> s {coreNetworkSegmentEdge = a} :: RouteTableIdentifier)

-- | The ARN of the transit gateway route table.
routeTableIdentifier_transitGatewayRouteTableArn :: Lens.Lens' RouteTableIdentifier (Prelude.Maybe Prelude.Text)
routeTableIdentifier_transitGatewayRouteTableArn = Lens.lens (\RouteTableIdentifier' {transitGatewayRouteTableArn} -> transitGatewayRouteTableArn) (\s@RouteTableIdentifier' {} a -> s {transitGatewayRouteTableArn = a} :: RouteTableIdentifier)

instance Prelude.Hashable RouteTableIdentifier where
  hashWithSalt _salt RouteTableIdentifier' {..} =
    _salt `Prelude.hashWithSalt` coreNetworkSegmentEdge
      `Prelude.hashWithSalt` transitGatewayRouteTableArn

instance Prelude.NFData RouteTableIdentifier where
  rnf RouteTableIdentifier' {..} =
    Prelude.rnf coreNetworkSegmentEdge
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableArn

instance Data.ToJSON RouteTableIdentifier where
  toJSON RouteTableIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CoreNetworkSegmentEdge" Data..=)
              Prelude.<$> coreNetworkSegmentEdge,
            ("TransitGatewayRouteTableArn" Data..=)
              Prelude.<$> transitGatewayRouteTableArn
          ]
      )

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
-- Module      : Network.AWS.DirectConnect.Types.RouteFilterPrefix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.RouteFilterPrefix where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a route filter prefix that a customer can advertise
-- through Border Gateway Protocol (BGP) over a public virtual interface.
--
-- /See:/ 'newRouteFilterPrefix' smart constructor.
data RouteFilterPrefix = RouteFilterPrefix'
  { -- | The CIDR block for the advertised route. Separate multiple routes using
    -- commas. An IPv6 CIDR must use \/64 or shorter.
    cidr :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RouteFilterPrefix' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'routeFilterPrefix_cidr' - The CIDR block for the advertised route. Separate multiple routes using
-- commas. An IPv6 CIDR must use \/64 or shorter.
newRouteFilterPrefix ::
  RouteFilterPrefix
newRouteFilterPrefix =
  RouteFilterPrefix' {cidr = Core.Nothing}

-- | The CIDR block for the advertised route. Separate multiple routes using
-- commas. An IPv6 CIDR must use \/64 or shorter.
routeFilterPrefix_cidr :: Lens.Lens' RouteFilterPrefix (Core.Maybe Core.Text)
routeFilterPrefix_cidr = Lens.lens (\RouteFilterPrefix' {cidr} -> cidr) (\s@RouteFilterPrefix' {} a -> s {cidr = a} :: RouteFilterPrefix)

instance Core.FromJSON RouteFilterPrefix where
  parseJSON =
    Core.withObject
      "RouteFilterPrefix"
      ( \x ->
          RouteFilterPrefix' Core.<$> (x Core..:? "cidr")
      )

instance Core.Hashable RouteFilterPrefix

instance Core.NFData RouteFilterPrefix

instance Core.ToJSON RouteFilterPrefix where
  toJSON RouteFilterPrefix' {..} =
    Core.object
      (Core.catMaybes [("cidr" Core..=) Core.<$> cidr])

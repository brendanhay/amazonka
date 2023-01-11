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
-- Module      : Amazonka.DirectConnect.Types.RouteFilterPrefix
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.RouteFilterPrefix where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a route filter prefix that a customer can advertise
-- through Border Gateway Protocol (BGP) over a public virtual interface.
--
-- /See:/ 'newRouteFilterPrefix' smart constructor.
data RouteFilterPrefix = RouteFilterPrefix'
  { -- | The CIDR block for the advertised route. Separate multiple routes using
    -- commas. An IPv6 CIDR must use \/64 or shorter.
    cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  RouteFilterPrefix' {cidr = Prelude.Nothing}

-- | The CIDR block for the advertised route. Separate multiple routes using
-- commas. An IPv6 CIDR must use \/64 or shorter.
routeFilterPrefix_cidr :: Lens.Lens' RouteFilterPrefix (Prelude.Maybe Prelude.Text)
routeFilterPrefix_cidr = Lens.lens (\RouteFilterPrefix' {cidr} -> cidr) (\s@RouteFilterPrefix' {} a -> s {cidr = a} :: RouteFilterPrefix)

instance Data.FromJSON RouteFilterPrefix where
  parseJSON =
    Data.withObject
      "RouteFilterPrefix"
      ( \x ->
          RouteFilterPrefix' Prelude.<$> (x Data..:? "cidr")
      )

instance Prelude.Hashable RouteFilterPrefix where
  hashWithSalt _salt RouteFilterPrefix' {..} =
    _salt `Prelude.hashWithSalt` cidr

instance Prelude.NFData RouteFilterPrefix where
  rnf RouteFilterPrefix' {..} = Prelude.rnf cidr

instance Data.ToJSON RouteFilterPrefix where
  toJSON RouteFilterPrefix' {..} =
    Data.object
      ( Prelude.catMaybes
          [("cidr" Data..=) Prelude.<$> cidr]
      )

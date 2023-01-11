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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteHostnameMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteHostnameMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the gateway route host name to match.
--
-- /See:/ 'newGatewayRouteHostnameMatch' smart constructor.
data GatewayRouteHostnameMatch = GatewayRouteHostnameMatch'
  { -- | The exact host name to match on.
    exact :: Prelude.Maybe Prelude.Text,
    -- | The specified ending characters of the host name to match on.
    suffix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayRouteHostnameMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exact', 'gatewayRouteHostnameMatch_exact' - The exact host name to match on.
--
-- 'suffix', 'gatewayRouteHostnameMatch_suffix' - The specified ending characters of the host name to match on.
newGatewayRouteHostnameMatch ::
  GatewayRouteHostnameMatch
newGatewayRouteHostnameMatch =
  GatewayRouteHostnameMatch'
    { exact = Prelude.Nothing,
      suffix = Prelude.Nothing
    }

-- | The exact host name to match on.
gatewayRouteHostnameMatch_exact :: Lens.Lens' GatewayRouteHostnameMatch (Prelude.Maybe Prelude.Text)
gatewayRouteHostnameMatch_exact = Lens.lens (\GatewayRouteHostnameMatch' {exact} -> exact) (\s@GatewayRouteHostnameMatch' {} a -> s {exact = a} :: GatewayRouteHostnameMatch)

-- | The specified ending characters of the host name to match on.
gatewayRouteHostnameMatch_suffix :: Lens.Lens' GatewayRouteHostnameMatch (Prelude.Maybe Prelude.Text)
gatewayRouteHostnameMatch_suffix = Lens.lens (\GatewayRouteHostnameMatch' {suffix} -> suffix) (\s@GatewayRouteHostnameMatch' {} a -> s {suffix = a} :: GatewayRouteHostnameMatch)

instance Data.FromJSON GatewayRouteHostnameMatch where
  parseJSON =
    Data.withObject
      "GatewayRouteHostnameMatch"
      ( \x ->
          GatewayRouteHostnameMatch'
            Prelude.<$> (x Data..:? "exact")
            Prelude.<*> (x Data..:? "suffix")
      )

instance Prelude.Hashable GatewayRouteHostnameMatch where
  hashWithSalt _salt GatewayRouteHostnameMatch' {..} =
    _salt `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` suffix

instance Prelude.NFData GatewayRouteHostnameMatch where
  rnf GatewayRouteHostnameMatch' {..} =
    Prelude.rnf exact `Prelude.seq` Prelude.rnf suffix

instance Data.ToJSON GatewayRouteHostnameMatch where
  toJSON GatewayRouteHostnameMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("exact" Data..=) Prelude.<$> exact,
            ("suffix" Data..=) Prelude.<$> suffix
          ]
      )

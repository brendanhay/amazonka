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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRouteRewrite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRouteRewrite where

import Amazonka.AppMesh.Types.GatewayRouteHostnameRewrite
import Amazonka.AppMesh.Types.HttpGatewayRoutePathRewrite
import Amazonka.AppMesh.Types.HttpGatewayRoutePrefixRewrite
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the gateway route to rewrite.
--
-- /See:/ 'newHttpGatewayRouteRewrite' smart constructor.
data HttpGatewayRouteRewrite = HttpGatewayRouteRewrite'
  { -- | The path to rewrite.
    path :: Prelude.Maybe HttpGatewayRoutePathRewrite,
    -- | The host name to rewrite.
    hostname :: Prelude.Maybe GatewayRouteHostnameRewrite,
    -- | The specified beginning characters to rewrite.
    prefix :: Prelude.Maybe HttpGatewayRoutePrefixRewrite
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRouteRewrite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'httpGatewayRouteRewrite_path' - The path to rewrite.
--
-- 'hostname', 'httpGatewayRouteRewrite_hostname' - The host name to rewrite.
--
-- 'prefix', 'httpGatewayRouteRewrite_prefix' - The specified beginning characters to rewrite.
newHttpGatewayRouteRewrite ::
  HttpGatewayRouteRewrite
newHttpGatewayRouteRewrite =
  HttpGatewayRouteRewrite'
    { path = Prelude.Nothing,
      hostname = Prelude.Nothing,
      prefix = Prelude.Nothing
    }

-- | The path to rewrite.
httpGatewayRouteRewrite_path :: Lens.Lens' HttpGatewayRouteRewrite (Prelude.Maybe HttpGatewayRoutePathRewrite)
httpGatewayRouteRewrite_path = Lens.lens (\HttpGatewayRouteRewrite' {path} -> path) (\s@HttpGatewayRouteRewrite' {} a -> s {path = a} :: HttpGatewayRouteRewrite)

-- | The host name to rewrite.
httpGatewayRouteRewrite_hostname :: Lens.Lens' HttpGatewayRouteRewrite (Prelude.Maybe GatewayRouteHostnameRewrite)
httpGatewayRouteRewrite_hostname = Lens.lens (\HttpGatewayRouteRewrite' {hostname} -> hostname) (\s@HttpGatewayRouteRewrite' {} a -> s {hostname = a} :: HttpGatewayRouteRewrite)

-- | The specified beginning characters to rewrite.
httpGatewayRouteRewrite_prefix :: Lens.Lens' HttpGatewayRouteRewrite (Prelude.Maybe HttpGatewayRoutePrefixRewrite)
httpGatewayRouteRewrite_prefix = Lens.lens (\HttpGatewayRouteRewrite' {prefix} -> prefix) (\s@HttpGatewayRouteRewrite' {} a -> s {prefix = a} :: HttpGatewayRouteRewrite)

instance Core.FromJSON HttpGatewayRouteRewrite where
  parseJSON =
    Core.withObject
      "HttpGatewayRouteRewrite"
      ( \x ->
          HttpGatewayRouteRewrite'
            Prelude.<$> (x Core..:? "path")
            Prelude.<*> (x Core..:? "hostname")
            Prelude.<*> (x Core..:? "prefix")
      )

instance Prelude.Hashable HttpGatewayRouteRewrite where
  hashWithSalt _salt HttpGatewayRouteRewrite' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData HttpGatewayRouteRewrite where
  rnf HttpGatewayRouteRewrite' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf prefix

instance Core.ToJSON HttpGatewayRouteRewrite where
  toJSON HttpGatewayRouteRewrite' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("path" Core..=) Prelude.<$> path,
            ("hostname" Core..=) Prelude.<$> hostname,
            ("prefix" Core..=) Prelude.<$> prefix
          ]
      )

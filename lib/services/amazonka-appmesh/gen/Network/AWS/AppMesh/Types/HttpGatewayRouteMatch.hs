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
-- Module      : Network.AWS.AppMesh.Types.HttpGatewayRouteMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.HttpGatewayRouteMatch where

import Network.AWS.AppMesh.Types.GatewayRouteHostnameMatch
import Network.AWS.AppMesh.Types.HttpGatewayRouteHeader
import Network.AWS.AppMesh.Types.HttpMethod
import Network.AWS.AppMesh.Types.HttpPathMatch
import Network.AWS.AppMesh.Types.HttpQueryParameter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the criteria for determining a request match.
--
-- /See:/ 'newHttpGatewayRouteMatch' smart constructor.
data HttpGatewayRouteMatch = HttpGatewayRouteMatch'
  { -- | The host name to match on.
    hostname :: Prelude.Maybe GatewayRouteHostnameMatch,
    -- | The path to match on.
    path :: Prelude.Maybe HttpPathMatch,
    -- | Specifies the path to match requests with. This parameter must always
    -- start with @\/@, which by itself matches all requests to the virtual
    -- service name. You can also match for path-based routing of requests. For
    -- example, if your virtual service name is @my-service.local@ and you want
    -- the route to match requests to @my-service.local\/metrics@, your prefix
    -- should be @\/metrics@.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The query parameter to match on.
    queryParameters :: Prelude.Maybe (Prelude.NonEmpty HttpQueryParameter),
    -- | The client request headers to match on.
    headers :: Prelude.Maybe (Prelude.NonEmpty HttpGatewayRouteHeader),
    -- | The method to match on.
    method :: Prelude.Maybe HttpMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRouteMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'httpGatewayRouteMatch_hostname' - The host name to match on.
--
-- 'path', 'httpGatewayRouteMatch_path' - The path to match on.
--
-- 'prefix', 'httpGatewayRouteMatch_prefix' - Specifies the path to match requests with. This parameter must always
-- start with @\/@, which by itself matches all requests to the virtual
-- service name. You can also match for path-based routing of requests. For
-- example, if your virtual service name is @my-service.local@ and you want
-- the route to match requests to @my-service.local\/metrics@, your prefix
-- should be @\/metrics@.
--
-- 'queryParameters', 'httpGatewayRouteMatch_queryParameters' - The query parameter to match on.
--
-- 'headers', 'httpGatewayRouteMatch_headers' - The client request headers to match on.
--
-- 'method', 'httpGatewayRouteMatch_method' - The method to match on.
newHttpGatewayRouteMatch ::
  HttpGatewayRouteMatch
newHttpGatewayRouteMatch =
  HttpGatewayRouteMatch'
    { hostname = Prelude.Nothing,
      path = Prelude.Nothing,
      prefix = Prelude.Nothing,
      queryParameters = Prelude.Nothing,
      headers = Prelude.Nothing,
      method = Prelude.Nothing
    }

-- | The host name to match on.
httpGatewayRouteMatch_hostname :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe GatewayRouteHostnameMatch)
httpGatewayRouteMatch_hostname = Lens.lens (\HttpGatewayRouteMatch' {hostname} -> hostname) (\s@HttpGatewayRouteMatch' {} a -> s {hostname = a} :: HttpGatewayRouteMatch)

-- | The path to match on.
httpGatewayRouteMatch_path :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe HttpPathMatch)
httpGatewayRouteMatch_path = Lens.lens (\HttpGatewayRouteMatch' {path} -> path) (\s@HttpGatewayRouteMatch' {} a -> s {path = a} :: HttpGatewayRouteMatch)

-- | Specifies the path to match requests with. This parameter must always
-- start with @\/@, which by itself matches all requests to the virtual
-- service name. You can also match for path-based routing of requests. For
-- example, if your virtual service name is @my-service.local@ and you want
-- the route to match requests to @my-service.local\/metrics@, your prefix
-- should be @\/metrics@.
httpGatewayRouteMatch_prefix :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe Prelude.Text)
httpGatewayRouteMatch_prefix = Lens.lens (\HttpGatewayRouteMatch' {prefix} -> prefix) (\s@HttpGatewayRouteMatch' {} a -> s {prefix = a} :: HttpGatewayRouteMatch)

-- | The query parameter to match on.
httpGatewayRouteMatch_queryParameters :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe (Prelude.NonEmpty HttpQueryParameter))
httpGatewayRouteMatch_queryParameters = Lens.lens (\HttpGatewayRouteMatch' {queryParameters} -> queryParameters) (\s@HttpGatewayRouteMatch' {} a -> s {queryParameters = a} :: HttpGatewayRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The client request headers to match on.
httpGatewayRouteMatch_headers :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe (Prelude.NonEmpty HttpGatewayRouteHeader))
httpGatewayRouteMatch_headers = Lens.lens (\HttpGatewayRouteMatch' {headers} -> headers) (\s@HttpGatewayRouteMatch' {} a -> s {headers = a} :: HttpGatewayRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The method to match on.
httpGatewayRouteMatch_method :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe HttpMethod)
httpGatewayRouteMatch_method = Lens.lens (\HttpGatewayRouteMatch' {method} -> method) (\s@HttpGatewayRouteMatch' {} a -> s {method = a} :: HttpGatewayRouteMatch)

instance Core.FromJSON HttpGatewayRouteMatch where
  parseJSON =
    Core.withObject
      "HttpGatewayRouteMatch"
      ( \x ->
          HttpGatewayRouteMatch'
            Prelude.<$> (x Core..:? "hostname")
            Prelude.<*> (x Core..:? "path")
            Prelude.<*> (x Core..:? "prefix")
            Prelude.<*> (x Core..:? "queryParameters")
            Prelude.<*> (x Core..:? "headers")
            Prelude.<*> (x Core..:? "method")
      )

instance Prelude.Hashable HttpGatewayRouteMatch

instance Prelude.NFData HttpGatewayRouteMatch

instance Core.ToJSON HttpGatewayRouteMatch where
  toJSON HttpGatewayRouteMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hostname" Core..=) Prelude.<$> hostname,
            ("path" Core..=) Prelude.<$> path,
            ("prefix" Core..=) Prelude.<$> prefix,
            ("queryParameters" Core..=)
              Prelude.<$> queryParameters,
            ("headers" Core..=) Prelude.<$> headers,
            ("method" Core..=) Prelude.<$> method
          ]
      )

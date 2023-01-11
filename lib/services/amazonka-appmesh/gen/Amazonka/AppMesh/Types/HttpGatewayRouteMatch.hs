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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRouteMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRouteMatch where

import Amazonka.AppMesh.Types.GatewayRouteHostnameMatch
import Amazonka.AppMesh.Types.HttpGatewayRouteHeader
import Amazonka.AppMesh.Types.HttpMethod
import Amazonka.AppMesh.Types.HttpPathMatch
import Amazonka.AppMesh.Types.HttpQueryParameter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the criteria for determining a request match.
--
-- /See:/ 'newHttpGatewayRouteMatch' smart constructor.
data HttpGatewayRouteMatch = HttpGatewayRouteMatch'
  { -- | The client request headers to match on.
    headers :: Prelude.Maybe (Prelude.NonEmpty HttpGatewayRouteHeader),
    -- | The host name to match on.
    hostname :: Prelude.Maybe GatewayRouteHostnameMatch,
    -- | The method to match on.
    method :: Prelude.Maybe HttpMethod,
    -- | The path to match on.
    path :: Prelude.Maybe HttpPathMatch,
    -- | The port number to match on.
    port :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the path to match requests with. This parameter must always
    -- start with @\/@, which by itself matches all requests to the virtual
    -- service name. You can also match for path-based routing of requests. For
    -- example, if your virtual service name is @my-service.local@ and you want
    -- the route to match requests to @my-service.local\/metrics@, your prefix
    -- should be @\/metrics@.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The query parameter to match on.
    queryParameters :: Prelude.Maybe (Prelude.NonEmpty HttpQueryParameter)
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
-- 'headers', 'httpGatewayRouteMatch_headers' - The client request headers to match on.
--
-- 'hostname', 'httpGatewayRouteMatch_hostname' - The host name to match on.
--
-- 'method', 'httpGatewayRouteMatch_method' - The method to match on.
--
-- 'path', 'httpGatewayRouteMatch_path' - The path to match on.
--
-- 'port', 'httpGatewayRouteMatch_port' - The port number to match on.
--
-- 'prefix', 'httpGatewayRouteMatch_prefix' - Specifies the path to match requests with. This parameter must always
-- start with @\/@, which by itself matches all requests to the virtual
-- service name. You can also match for path-based routing of requests. For
-- example, if your virtual service name is @my-service.local@ and you want
-- the route to match requests to @my-service.local\/metrics@, your prefix
-- should be @\/metrics@.
--
-- 'queryParameters', 'httpGatewayRouteMatch_queryParameters' - The query parameter to match on.
newHttpGatewayRouteMatch ::
  HttpGatewayRouteMatch
newHttpGatewayRouteMatch =
  HttpGatewayRouteMatch'
    { headers = Prelude.Nothing,
      hostname = Prelude.Nothing,
      method = Prelude.Nothing,
      path = Prelude.Nothing,
      port = Prelude.Nothing,
      prefix = Prelude.Nothing,
      queryParameters = Prelude.Nothing
    }

-- | The client request headers to match on.
httpGatewayRouteMatch_headers :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe (Prelude.NonEmpty HttpGatewayRouteHeader))
httpGatewayRouteMatch_headers = Lens.lens (\HttpGatewayRouteMatch' {headers} -> headers) (\s@HttpGatewayRouteMatch' {} a -> s {headers = a} :: HttpGatewayRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The host name to match on.
httpGatewayRouteMatch_hostname :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe GatewayRouteHostnameMatch)
httpGatewayRouteMatch_hostname = Lens.lens (\HttpGatewayRouteMatch' {hostname} -> hostname) (\s@HttpGatewayRouteMatch' {} a -> s {hostname = a} :: HttpGatewayRouteMatch)

-- | The method to match on.
httpGatewayRouteMatch_method :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe HttpMethod)
httpGatewayRouteMatch_method = Lens.lens (\HttpGatewayRouteMatch' {method} -> method) (\s@HttpGatewayRouteMatch' {} a -> s {method = a} :: HttpGatewayRouteMatch)

-- | The path to match on.
httpGatewayRouteMatch_path :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe HttpPathMatch)
httpGatewayRouteMatch_path = Lens.lens (\HttpGatewayRouteMatch' {path} -> path) (\s@HttpGatewayRouteMatch' {} a -> s {path = a} :: HttpGatewayRouteMatch)

-- | The port number to match on.
httpGatewayRouteMatch_port :: Lens.Lens' HttpGatewayRouteMatch (Prelude.Maybe Prelude.Natural)
httpGatewayRouteMatch_port = Lens.lens (\HttpGatewayRouteMatch' {port} -> port) (\s@HttpGatewayRouteMatch' {} a -> s {port = a} :: HttpGatewayRouteMatch)

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

instance Data.FromJSON HttpGatewayRouteMatch where
  parseJSON =
    Data.withObject
      "HttpGatewayRouteMatch"
      ( \x ->
          HttpGatewayRouteMatch'
            Prelude.<$> (x Data..:? "headers")
            Prelude.<*> (x Data..:? "hostname")
            Prelude.<*> (x Data..:? "method")
            Prelude.<*> (x Data..:? "path")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..:? "queryParameters")
      )

instance Prelude.Hashable HttpGatewayRouteMatch where
  hashWithSalt _salt HttpGatewayRouteMatch' {..} =
    _salt `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` queryParameters

instance Prelude.NFData HttpGatewayRouteMatch where
  rnf HttpGatewayRouteMatch' {..} =
    Prelude.rnf headers
      `Prelude.seq` Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf method
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf queryParameters

instance Data.ToJSON HttpGatewayRouteMatch where
  toJSON HttpGatewayRouteMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("headers" Data..=) Prelude.<$> headers,
            ("hostname" Data..=) Prelude.<$> hostname,
            ("method" Data..=) Prelude.<$> method,
            ("path" Data..=) Prelude.<$> path,
            ("port" Data..=) Prelude.<$> port,
            ("prefix" Data..=) Prelude.<$> prefix,
            ("queryParameters" Data..=)
              Prelude.<$> queryParameters
          ]
      )

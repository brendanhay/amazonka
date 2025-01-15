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
-- Module      : Amazonka.AppMesh.Types.HttpRouteMatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpRouteMatch where

import Amazonka.AppMesh.Types.HttpMethod
import Amazonka.AppMesh.Types.HttpPathMatch
import Amazonka.AppMesh.Types.HttpQueryParameter
import Amazonka.AppMesh.Types.HttpRouteHeader
import Amazonka.AppMesh.Types.HttpScheme
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the requirements for a route to match HTTP
-- requests for a virtual router.
--
-- /See:/ 'newHttpRouteMatch' smart constructor.
data HttpRouteMatch = HttpRouteMatch'
  { -- | The client request headers to match on.
    headers :: Prelude.Maybe (Prelude.NonEmpty HttpRouteHeader),
    -- | The client request method to match on. Specify only one.
    method :: Prelude.Maybe HttpMethod,
    -- | The client request path to match on.
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
    -- | The client request query parameters to match on.
    queryParameters :: Prelude.Maybe (Prelude.NonEmpty HttpQueryParameter),
    -- | The client request scheme to match on. Specify only one. Applicable only
    -- for HTTP2 routes.
    scheme :: Prelude.Maybe HttpScheme
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpRouteMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'httpRouteMatch_headers' - The client request headers to match on.
--
-- 'method', 'httpRouteMatch_method' - The client request method to match on. Specify only one.
--
-- 'path', 'httpRouteMatch_path' - The client request path to match on.
--
-- 'port', 'httpRouteMatch_port' - The port number to match on.
--
-- 'prefix', 'httpRouteMatch_prefix' - Specifies the path to match requests with. This parameter must always
-- start with @\/@, which by itself matches all requests to the virtual
-- service name. You can also match for path-based routing of requests. For
-- example, if your virtual service name is @my-service.local@ and you want
-- the route to match requests to @my-service.local\/metrics@, your prefix
-- should be @\/metrics@.
--
-- 'queryParameters', 'httpRouteMatch_queryParameters' - The client request query parameters to match on.
--
-- 'scheme', 'httpRouteMatch_scheme' - The client request scheme to match on. Specify only one. Applicable only
-- for HTTP2 routes.
newHttpRouteMatch ::
  HttpRouteMatch
newHttpRouteMatch =
  HttpRouteMatch'
    { headers = Prelude.Nothing,
      method = Prelude.Nothing,
      path = Prelude.Nothing,
      port = Prelude.Nothing,
      prefix = Prelude.Nothing,
      queryParameters = Prelude.Nothing,
      scheme = Prelude.Nothing
    }

-- | The client request headers to match on.
httpRouteMatch_headers :: Lens.Lens' HttpRouteMatch (Prelude.Maybe (Prelude.NonEmpty HttpRouteHeader))
httpRouteMatch_headers = Lens.lens (\HttpRouteMatch' {headers} -> headers) (\s@HttpRouteMatch' {} a -> s {headers = a} :: HttpRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The client request method to match on. Specify only one.
httpRouteMatch_method :: Lens.Lens' HttpRouteMatch (Prelude.Maybe HttpMethod)
httpRouteMatch_method = Lens.lens (\HttpRouteMatch' {method} -> method) (\s@HttpRouteMatch' {} a -> s {method = a} :: HttpRouteMatch)

-- | The client request path to match on.
httpRouteMatch_path :: Lens.Lens' HttpRouteMatch (Prelude.Maybe HttpPathMatch)
httpRouteMatch_path = Lens.lens (\HttpRouteMatch' {path} -> path) (\s@HttpRouteMatch' {} a -> s {path = a} :: HttpRouteMatch)

-- | The port number to match on.
httpRouteMatch_port :: Lens.Lens' HttpRouteMatch (Prelude.Maybe Prelude.Natural)
httpRouteMatch_port = Lens.lens (\HttpRouteMatch' {port} -> port) (\s@HttpRouteMatch' {} a -> s {port = a} :: HttpRouteMatch)

-- | Specifies the path to match requests with. This parameter must always
-- start with @\/@, which by itself matches all requests to the virtual
-- service name. You can also match for path-based routing of requests. For
-- example, if your virtual service name is @my-service.local@ and you want
-- the route to match requests to @my-service.local\/metrics@, your prefix
-- should be @\/metrics@.
httpRouteMatch_prefix :: Lens.Lens' HttpRouteMatch (Prelude.Maybe Prelude.Text)
httpRouteMatch_prefix = Lens.lens (\HttpRouteMatch' {prefix} -> prefix) (\s@HttpRouteMatch' {} a -> s {prefix = a} :: HttpRouteMatch)

-- | The client request query parameters to match on.
httpRouteMatch_queryParameters :: Lens.Lens' HttpRouteMatch (Prelude.Maybe (Prelude.NonEmpty HttpQueryParameter))
httpRouteMatch_queryParameters = Lens.lens (\HttpRouteMatch' {queryParameters} -> queryParameters) (\s@HttpRouteMatch' {} a -> s {queryParameters = a} :: HttpRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The client request scheme to match on. Specify only one. Applicable only
-- for HTTP2 routes.
httpRouteMatch_scheme :: Lens.Lens' HttpRouteMatch (Prelude.Maybe HttpScheme)
httpRouteMatch_scheme = Lens.lens (\HttpRouteMatch' {scheme} -> scheme) (\s@HttpRouteMatch' {} a -> s {scheme = a} :: HttpRouteMatch)

instance Data.FromJSON HttpRouteMatch where
  parseJSON =
    Data.withObject
      "HttpRouteMatch"
      ( \x ->
          HttpRouteMatch'
            Prelude.<$> (x Data..:? "headers")
            Prelude.<*> (x Data..:? "method")
            Prelude.<*> (x Data..:? "path")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "prefix")
            Prelude.<*> (x Data..:? "queryParameters")
            Prelude.<*> (x Data..:? "scheme")
      )

instance Prelude.Hashable HttpRouteMatch where
  hashWithSalt _salt HttpRouteMatch' {..} =
    _salt
      `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` queryParameters
      `Prelude.hashWithSalt` scheme

instance Prelude.NFData HttpRouteMatch where
  rnf HttpRouteMatch' {..} =
    Prelude.rnf headers `Prelude.seq`
      Prelude.rnf method `Prelude.seq`
        Prelude.rnf path `Prelude.seq`
          Prelude.rnf port `Prelude.seq`
            Prelude.rnf prefix `Prelude.seq`
              Prelude.rnf queryParameters `Prelude.seq`
                Prelude.rnf scheme

instance Data.ToJSON HttpRouteMatch where
  toJSON HttpRouteMatch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("headers" Data..=) Prelude.<$> headers,
            ("method" Data..=) Prelude.<$> method,
            ("path" Data..=) Prelude.<$> path,
            ("port" Data..=) Prelude.<$> port,
            ("prefix" Data..=) Prelude.<$> prefix,
            ("queryParameters" Data..=)
              Prelude.<$> queryParameters,
            ("scheme" Data..=) Prelude.<$> scheme
          ]
      )

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
-- Module      : Network.AWS.AppMesh.Types.HttpRouteMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.HttpRouteMatch where

import Network.AWS.AppMesh.Types.HttpMethod
import Network.AWS.AppMesh.Types.HttpPathMatch
import Network.AWS.AppMesh.Types.HttpQueryParameter
import Network.AWS.AppMesh.Types.HttpRouteHeader
import Network.AWS.AppMesh.Types.HttpScheme
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the requirements for a route to match HTTP
-- requests for a virtual router.
--
-- /See:/ 'newHttpRouteMatch' smart constructor.
data HttpRouteMatch = HttpRouteMatch'
  { -- | The client request path to match on.
    path :: Prelude.Maybe HttpPathMatch,
    -- | Specifies the path to match requests with. This parameter must always
    -- start with @\/@, which by itself matches all requests to the virtual
    -- service name. You can also match for path-based routing of requests. For
    -- example, if your virtual service name is @my-service.local@ and you want
    -- the route to match requests to @my-service.local\/metrics@, your prefix
    -- should be @\/metrics@.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | The client request query parameters to match on.
    queryParameters :: Prelude.Maybe (Prelude.NonEmpty HttpQueryParameter),
    -- | The client request headers to match on.
    headers :: Prelude.Maybe (Prelude.NonEmpty HttpRouteHeader),
    -- | The client request method to match on. Specify only one.
    method :: Prelude.Maybe HttpMethod,
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
-- 'path', 'httpRouteMatch_path' - The client request path to match on.
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
-- 'headers', 'httpRouteMatch_headers' - The client request headers to match on.
--
-- 'method', 'httpRouteMatch_method' - The client request method to match on. Specify only one.
--
-- 'scheme', 'httpRouteMatch_scheme' - The client request scheme to match on. Specify only one. Applicable only
-- for HTTP2 routes.
newHttpRouteMatch ::
  HttpRouteMatch
newHttpRouteMatch =
  HttpRouteMatch'
    { path = Prelude.Nothing,
      prefix = Prelude.Nothing,
      queryParameters = Prelude.Nothing,
      headers = Prelude.Nothing,
      method = Prelude.Nothing,
      scheme = Prelude.Nothing
    }

-- | The client request path to match on.
httpRouteMatch_path :: Lens.Lens' HttpRouteMatch (Prelude.Maybe HttpPathMatch)
httpRouteMatch_path = Lens.lens (\HttpRouteMatch' {path} -> path) (\s@HttpRouteMatch' {} a -> s {path = a} :: HttpRouteMatch)

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

-- | The client request headers to match on.
httpRouteMatch_headers :: Lens.Lens' HttpRouteMatch (Prelude.Maybe (Prelude.NonEmpty HttpRouteHeader))
httpRouteMatch_headers = Lens.lens (\HttpRouteMatch' {headers} -> headers) (\s@HttpRouteMatch' {} a -> s {headers = a} :: HttpRouteMatch) Prelude.. Lens.mapping Lens.coerced

-- | The client request method to match on. Specify only one.
httpRouteMatch_method :: Lens.Lens' HttpRouteMatch (Prelude.Maybe HttpMethod)
httpRouteMatch_method = Lens.lens (\HttpRouteMatch' {method} -> method) (\s@HttpRouteMatch' {} a -> s {method = a} :: HttpRouteMatch)

-- | The client request scheme to match on. Specify only one. Applicable only
-- for HTTP2 routes.
httpRouteMatch_scheme :: Lens.Lens' HttpRouteMatch (Prelude.Maybe HttpScheme)
httpRouteMatch_scheme = Lens.lens (\HttpRouteMatch' {scheme} -> scheme) (\s@HttpRouteMatch' {} a -> s {scheme = a} :: HttpRouteMatch)

instance Core.FromJSON HttpRouteMatch where
  parseJSON =
    Core.withObject
      "HttpRouteMatch"
      ( \x ->
          HttpRouteMatch'
            Prelude.<$> (x Core..:? "path")
            Prelude.<*> (x Core..:? "prefix")
            Prelude.<*> (x Core..:? "queryParameters")
            Prelude.<*> (x Core..:? "headers")
            Prelude.<*> (x Core..:? "method")
            Prelude.<*> (x Core..:? "scheme")
      )

instance Prelude.Hashable HttpRouteMatch

instance Prelude.NFData HttpRouteMatch

instance Core.ToJSON HttpRouteMatch where
  toJSON HttpRouteMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("path" Core..=) Prelude.<$> path,
            ("prefix" Core..=) Prelude.<$> prefix,
            ("queryParameters" Core..=)
              Prelude.<$> queryParameters,
            ("headers" Core..=) Prelude.<$> headers,
            ("method" Core..=) Prelude.<$> method,
            ("scheme" Core..=) Prelude.<$> scheme
          ]
      )

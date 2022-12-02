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
-- Module      : Amazonka.Lambda.Types.Cors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.Cors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your Lambda function URL. Use CORS to grant access to your
-- function URL from any origin. You can also use CORS to control access
-- for specific HTTP headers and methods in requests to your function URL.
--
-- /See:/ 'newCors' smart constructor.
data Cors = Cors'
  { -- | The HTTP headers that origins can include in requests to your function
    -- URL. For example: @Date@, @Keep-Alive@, @X-Custom-Header@.
    allowHeaders :: Prelude.Maybe [Prelude.Text],
    -- | The HTTP headers in your function response that you want to expose to
    -- origins that call your function URL. For example: @Date@, @Keep-Alive@,
    -- @X-Custom-Header@.
    exposeHeaders :: Prelude.Maybe [Prelude.Text],
    -- | Whether to allow cookies or other credentials in requests to your
    -- function URL. The default is @false@.
    allowCredentials :: Prelude.Maybe Prelude.Bool,
    -- | The HTTP methods that are allowed when calling your function URL. For
    -- example: @GET@, @POST@, @DELETE@, or the wildcard character (@*@).
    allowMethods :: Prelude.Maybe [Prelude.Text],
    -- | The origins that can access your function URL. You can list any number
    -- of specific origins, separated by a comma. For example:
    -- @https:\/\/www.example.com@, @http:\/\/localhost:60905@.
    --
    -- Alternatively, you can grant access to all origins using the wildcard
    -- character (@*@).
    allowOrigins :: Prelude.Maybe [Prelude.Text],
    -- | The maximum amount of time, in seconds, that web browsers can cache
    -- results of a preflight request. By default, this is set to @0@, which
    -- means that the browser doesn\'t cache results.
    maxAge :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowHeaders', 'cors_allowHeaders' - The HTTP headers that origins can include in requests to your function
-- URL. For example: @Date@, @Keep-Alive@, @X-Custom-Header@.
--
-- 'exposeHeaders', 'cors_exposeHeaders' - The HTTP headers in your function response that you want to expose to
-- origins that call your function URL. For example: @Date@, @Keep-Alive@,
-- @X-Custom-Header@.
--
-- 'allowCredentials', 'cors_allowCredentials' - Whether to allow cookies or other credentials in requests to your
-- function URL. The default is @false@.
--
-- 'allowMethods', 'cors_allowMethods' - The HTTP methods that are allowed when calling your function URL. For
-- example: @GET@, @POST@, @DELETE@, or the wildcard character (@*@).
--
-- 'allowOrigins', 'cors_allowOrigins' - The origins that can access your function URL. You can list any number
-- of specific origins, separated by a comma. For example:
-- @https:\/\/www.example.com@, @http:\/\/localhost:60905@.
--
-- Alternatively, you can grant access to all origins using the wildcard
-- character (@*@).
--
-- 'maxAge', 'cors_maxAge' - The maximum amount of time, in seconds, that web browsers can cache
-- results of a preflight request. By default, this is set to @0@, which
-- means that the browser doesn\'t cache results.
newCors ::
  Cors
newCors =
  Cors'
    { allowHeaders = Prelude.Nothing,
      exposeHeaders = Prelude.Nothing,
      allowCredentials = Prelude.Nothing,
      allowMethods = Prelude.Nothing,
      allowOrigins = Prelude.Nothing,
      maxAge = Prelude.Nothing
    }

-- | The HTTP headers that origins can include in requests to your function
-- URL. For example: @Date@, @Keep-Alive@, @X-Custom-Header@.
cors_allowHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowHeaders = Lens.lens (\Cors' {allowHeaders} -> allowHeaders) (\s@Cors' {} a -> s {allowHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP headers in your function response that you want to expose to
-- origins that call your function URL. For example: @Date@, @Keep-Alive@,
-- @X-Custom-Header@.
cors_exposeHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_exposeHeaders = Lens.lens (\Cors' {exposeHeaders} -> exposeHeaders) (\s@Cors' {} a -> s {exposeHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Whether to allow cookies or other credentials in requests to your
-- function URL. The default is @false@.
cors_allowCredentials :: Lens.Lens' Cors (Prelude.Maybe Prelude.Bool)
cors_allowCredentials = Lens.lens (\Cors' {allowCredentials} -> allowCredentials) (\s@Cors' {} a -> s {allowCredentials = a} :: Cors)

-- | The HTTP methods that are allowed when calling your function URL. For
-- example: @GET@, @POST@, @DELETE@, or the wildcard character (@*@).
cors_allowMethods :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowMethods = Lens.lens (\Cors' {allowMethods} -> allowMethods) (\s@Cors' {} a -> s {allowMethods = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | The origins that can access your function URL. You can list any number
-- of specific origins, separated by a comma. For example:
-- @https:\/\/www.example.com@, @http:\/\/localhost:60905@.
--
-- Alternatively, you can grant access to all origins using the wildcard
-- character (@*@).
cors_allowOrigins :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowOrigins = Lens.lens (\Cors' {allowOrigins} -> allowOrigins) (\s@Cors' {} a -> s {allowOrigins = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | The maximum amount of time, in seconds, that web browsers can cache
-- results of a preflight request. By default, this is set to @0@, which
-- means that the browser doesn\'t cache results.
cors_maxAge :: Lens.Lens' Cors (Prelude.Maybe Prelude.Natural)
cors_maxAge = Lens.lens (\Cors' {maxAge} -> maxAge) (\s@Cors' {} a -> s {maxAge = a} :: Cors)

instance Data.FromJSON Cors where
  parseJSON =
    Data.withObject
      "Cors"
      ( \x ->
          Cors'
            Prelude.<$> (x Data..:? "AllowHeaders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ExposeHeaders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AllowCredentials")
            Prelude.<*> (x Data..:? "AllowMethods" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AllowOrigins" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MaxAge")
      )

instance Prelude.Hashable Cors where
  hashWithSalt _salt Cors' {..} =
    _salt `Prelude.hashWithSalt` allowHeaders
      `Prelude.hashWithSalt` exposeHeaders
      `Prelude.hashWithSalt` allowCredentials
      `Prelude.hashWithSalt` allowMethods
      `Prelude.hashWithSalt` allowOrigins
      `Prelude.hashWithSalt` maxAge

instance Prelude.NFData Cors where
  rnf Cors' {..} =
    Prelude.rnf allowHeaders
      `Prelude.seq` Prelude.rnf exposeHeaders
      `Prelude.seq` Prelude.rnf allowCredentials
      `Prelude.seq` Prelude.rnf allowMethods
      `Prelude.seq` Prelude.rnf allowOrigins
      `Prelude.seq` Prelude.rnf maxAge

instance Data.ToJSON Cors where
  toJSON Cors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowHeaders" Data..=) Prelude.<$> allowHeaders,
            ("ExposeHeaders" Data..=) Prelude.<$> exposeHeaders,
            ("AllowCredentials" Data..=)
              Prelude.<$> allowCredentials,
            ("AllowMethods" Data..=) Prelude.<$> allowMethods,
            ("AllowOrigins" Data..=) Prelude.<$> allowOrigins,
            ("MaxAge" Data..=) Prelude.<$> maxAge
          ]
      )

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
-- Module      : Amazonka.ApiGatewayV2.Types.Cors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Cors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a CORS configuration. Supported only for HTTP APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-cors.html Configuring CORS>
-- for more information.
--
-- /See:/ 'newCors' smart constructor.
data Cors = Cors'
  { -- | Specifies whether credentials are included in the CORS request.
    -- Supported only for HTTP APIs.
    allowCredentials :: Prelude.Maybe Prelude.Bool,
    -- | Represents a collection of allowed headers. Supported only for HTTP
    -- APIs.
    allowHeaders :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of allowed HTTP methods. Supported only for HTTP
    -- APIs.
    allowMethods :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of allowed origins. Supported only for HTTP
    -- APIs.
    allowOrigins :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of exposed headers. Supported only for HTTP
    -- APIs.
    exposeHeaders :: Prelude.Maybe [Prelude.Text],
    -- | The number of seconds that the browser should cache preflight request
    -- results. Supported only for HTTP APIs.
    maxAge :: Prelude.Maybe Prelude.Int
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
-- 'allowCredentials', 'cors_allowCredentials' - Specifies whether credentials are included in the CORS request.
-- Supported only for HTTP APIs.
--
-- 'allowHeaders', 'cors_allowHeaders' - Represents a collection of allowed headers. Supported only for HTTP
-- APIs.
--
-- 'allowMethods', 'cors_allowMethods' - Represents a collection of allowed HTTP methods. Supported only for HTTP
-- APIs.
--
-- 'allowOrigins', 'cors_allowOrigins' - Represents a collection of allowed origins. Supported only for HTTP
-- APIs.
--
-- 'exposeHeaders', 'cors_exposeHeaders' - Represents a collection of exposed headers. Supported only for HTTP
-- APIs.
--
-- 'maxAge', 'cors_maxAge' - The number of seconds that the browser should cache preflight request
-- results. Supported only for HTTP APIs.
newCors ::
  Cors
newCors =
  Cors'
    { allowCredentials = Prelude.Nothing,
      allowHeaders = Prelude.Nothing,
      allowMethods = Prelude.Nothing,
      allowOrigins = Prelude.Nothing,
      exposeHeaders = Prelude.Nothing,
      maxAge = Prelude.Nothing
    }

-- | Specifies whether credentials are included in the CORS request.
-- Supported only for HTTP APIs.
cors_allowCredentials :: Lens.Lens' Cors (Prelude.Maybe Prelude.Bool)
cors_allowCredentials = Lens.lens (\Cors' {allowCredentials} -> allowCredentials) (\s@Cors' {} a -> s {allowCredentials = a} :: Cors)

-- | Represents a collection of allowed headers. Supported only for HTTP
-- APIs.
cors_allowHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowHeaders = Lens.lens (\Cors' {allowHeaders} -> allowHeaders) (\s@Cors' {} a -> s {allowHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of allowed HTTP methods. Supported only for HTTP
-- APIs.
cors_allowMethods :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowMethods = Lens.lens (\Cors' {allowMethods} -> allowMethods) (\s@Cors' {} a -> s {allowMethods = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of allowed origins. Supported only for HTTP
-- APIs.
cors_allowOrigins :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowOrigins = Lens.lens (\Cors' {allowOrigins} -> allowOrigins) (\s@Cors' {} a -> s {allowOrigins = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of exposed headers. Supported only for HTTP
-- APIs.
cors_exposeHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_exposeHeaders = Lens.lens (\Cors' {exposeHeaders} -> exposeHeaders) (\s@Cors' {} a -> s {exposeHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds that the browser should cache preflight request
-- results. Supported only for HTTP APIs.
cors_maxAge :: Lens.Lens' Cors (Prelude.Maybe Prelude.Int)
cors_maxAge = Lens.lens (\Cors' {maxAge} -> maxAge) (\s@Cors' {} a -> s {maxAge = a} :: Cors)

instance Data.FromJSON Cors where
  parseJSON =
    Data.withObject
      "Cors"
      ( \x ->
          Cors'
            Prelude.<$> (x Data..:? "allowCredentials")
            Prelude.<*> (x Data..:? "allowHeaders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "allowMethods" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "allowOrigins" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "exposeHeaders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "maxAge")
      )

instance Prelude.Hashable Cors where
  hashWithSalt _salt Cors' {..} =
    _salt
      `Prelude.hashWithSalt` allowCredentials
      `Prelude.hashWithSalt` allowHeaders
      `Prelude.hashWithSalt` allowMethods
      `Prelude.hashWithSalt` allowOrigins
      `Prelude.hashWithSalt` exposeHeaders
      `Prelude.hashWithSalt` maxAge

instance Prelude.NFData Cors where
  rnf Cors' {..} =
    Prelude.rnf allowCredentials `Prelude.seq`
      Prelude.rnf allowHeaders `Prelude.seq`
        Prelude.rnf allowMethods `Prelude.seq`
          Prelude.rnf allowOrigins `Prelude.seq`
            Prelude.rnf exposeHeaders `Prelude.seq`
              Prelude.rnf maxAge

instance Data.ToJSON Cors where
  toJSON Cors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowCredentials" Data..=)
              Prelude.<$> allowCredentials,
            ("allowHeaders" Data..=) Prelude.<$> allowHeaders,
            ("allowMethods" Data..=) Prelude.<$> allowMethods,
            ("allowOrigins" Data..=) Prelude.<$> allowOrigins,
            ("exposeHeaders" Data..=) Prelude.<$> exposeHeaders,
            ("maxAge" Data..=) Prelude.<$> maxAge
          ]
      )

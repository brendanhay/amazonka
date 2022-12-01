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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Cors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a CORS configuration. Supported only for HTTP APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-cors.html Configuring CORS>
-- for more information.
--
-- /See:/ 'newCors' smart constructor.
data Cors = Cors'
  { -- | Represents a collection of allowed headers. Supported only for HTTP
    -- APIs.
    allowHeaders :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of exposed headers. Supported only for HTTP
    -- APIs.
    exposeHeaders :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether credentials are included in the CORS request.
    -- Supported only for HTTP APIs.
    allowCredentials :: Prelude.Maybe Prelude.Bool,
    -- | Represents a collection of allowed HTTP methods. Supported only for HTTP
    -- APIs.
    allowMethods :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of allowed origins. Supported only for HTTP
    -- APIs.
    allowOrigins :: Prelude.Maybe [Prelude.Text],
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
-- 'allowHeaders', 'cors_allowHeaders' - Represents a collection of allowed headers. Supported only for HTTP
-- APIs.
--
-- 'exposeHeaders', 'cors_exposeHeaders' - Represents a collection of exposed headers. Supported only for HTTP
-- APIs.
--
-- 'allowCredentials', 'cors_allowCredentials' - Specifies whether credentials are included in the CORS request.
-- Supported only for HTTP APIs.
--
-- 'allowMethods', 'cors_allowMethods' - Represents a collection of allowed HTTP methods. Supported only for HTTP
-- APIs.
--
-- 'allowOrigins', 'cors_allowOrigins' - Represents a collection of allowed origins. Supported only for HTTP
-- APIs.
--
-- 'maxAge', 'cors_maxAge' - The number of seconds that the browser should cache preflight request
-- results. Supported only for HTTP APIs.
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

-- | Represents a collection of allowed headers. Supported only for HTTP
-- APIs.
cors_allowHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowHeaders = Lens.lens (\Cors' {allowHeaders} -> allowHeaders) (\s@Cors' {} a -> s {allowHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of exposed headers. Supported only for HTTP
-- APIs.
cors_exposeHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_exposeHeaders = Lens.lens (\Cors' {exposeHeaders} -> exposeHeaders) (\s@Cors' {} a -> s {exposeHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether credentials are included in the CORS request.
-- Supported only for HTTP APIs.
cors_allowCredentials :: Lens.Lens' Cors (Prelude.Maybe Prelude.Bool)
cors_allowCredentials = Lens.lens (\Cors' {allowCredentials} -> allowCredentials) (\s@Cors' {} a -> s {allowCredentials = a} :: Cors)

-- | Represents a collection of allowed HTTP methods. Supported only for HTTP
-- APIs.
cors_allowMethods :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowMethods = Lens.lens (\Cors' {allowMethods} -> allowMethods) (\s@Cors' {} a -> s {allowMethods = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of allowed origins. Supported only for HTTP
-- APIs.
cors_allowOrigins :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowOrigins = Lens.lens (\Cors' {allowOrigins} -> allowOrigins) (\s@Cors' {} a -> s {allowOrigins = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds that the browser should cache preflight request
-- results. Supported only for HTTP APIs.
cors_maxAge :: Lens.Lens' Cors (Prelude.Maybe Prelude.Int)
cors_maxAge = Lens.lens (\Cors' {maxAge} -> maxAge) (\s@Cors' {} a -> s {maxAge = a} :: Cors)

instance Core.FromJSON Cors where
  parseJSON =
    Core.withObject
      "Cors"
      ( \x ->
          Cors'
            Prelude.<$> (x Core..:? "allowHeaders" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "exposeHeaders" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "allowCredentials")
            Prelude.<*> (x Core..:? "allowMethods" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "allowOrigins" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "maxAge")
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

instance Core.ToJSON Cors where
  toJSON Cors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("allowHeaders" Core..=) Prelude.<$> allowHeaders,
            ("exposeHeaders" Core..=) Prelude.<$> exposeHeaders,
            ("allowCredentials" Core..=)
              Prelude.<$> allowCredentials,
            ("allowMethods" Core..=) Prelude.<$> allowMethods,
            ("allowOrigins" Core..=) Prelude.<$> allowOrigins,
            ("maxAge" Core..=) Prelude.<$> maxAge
          ]
      )

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
-- Module      : Network.AWS.ApiGatewayV2.Types.Cors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApiGatewayV2.Types.Cors where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a CORS configuration. Supported only for HTTP APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-cors.html Configuring CORS>
-- for more information.
--
-- /See:/ 'newCors' smart constructor.
data Cors = Cors'
  { -- | The number of seconds that the browser should cache preflight request
    -- results. Supported only for HTTP APIs.
    maxAge :: Prelude.Maybe Prelude.Int,
    -- | Represents a collection of allowed HTTP methods. Supported only for HTTP
    -- APIs.
    allowMethods :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of allowed headers. Supported only for HTTP
    -- APIs.
    allowHeaders :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of exposed headers. Supported only for HTTP
    -- APIs.
    exposeHeaders :: Prelude.Maybe [Prelude.Text],
    -- | Represents a collection of allowed origins. Supported only for HTTP
    -- APIs.
    allowOrigins :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether credentials are included in the CORS request.
    -- Supported only for HTTP APIs.
    allowCredentials :: Prelude.Maybe Prelude.Bool
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
-- 'maxAge', 'cors_maxAge' - The number of seconds that the browser should cache preflight request
-- results. Supported only for HTTP APIs.
--
-- 'allowMethods', 'cors_allowMethods' - Represents a collection of allowed HTTP methods. Supported only for HTTP
-- APIs.
--
-- 'allowHeaders', 'cors_allowHeaders' - Represents a collection of allowed headers. Supported only for HTTP
-- APIs.
--
-- 'exposeHeaders', 'cors_exposeHeaders' - Represents a collection of exposed headers. Supported only for HTTP
-- APIs.
--
-- 'allowOrigins', 'cors_allowOrigins' - Represents a collection of allowed origins. Supported only for HTTP
-- APIs.
--
-- 'allowCredentials', 'cors_allowCredentials' - Specifies whether credentials are included in the CORS request.
-- Supported only for HTTP APIs.
newCors ::
  Cors
newCors =
  Cors'
    { maxAge = Prelude.Nothing,
      allowMethods = Prelude.Nothing,
      allowHeaders = Prelude.Nothing,
      exposeHeaders = Prelude.Nothing,
      allowOrigins = Prelude.Nothing,
      allowCredentials = Prelude.Nothing
    }

-- | The number of seconds that the browser should cache preflight request
-- results. Supported only for HTTP APIs.
cors_maxAge :: Lens.Lens' Cors (Prelude.Maybe Prelude.Int)
cors_maxAge = Lens.lens (\Cors' {maxAge} -> maxAge) (\s@Cors' {} a -> s {maxAge = a} :: Cors)

-- | Represents a collection of allowed HTTP methods. Supported only for HTTP
-- APIs.
cors_allowMethods :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowMethods = Lens.lens (\Cors' {allowMethods} -> allowMethods) (\s@Cors' {} a -> s {allowMethods = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of allowed headers. Supported only for HTTP
-- APIs.
cors_allowHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowHeaders = Lens.lens (\Cors' {allowHeaders} -> allowHeaders) (\s@Cors' {} a -> s {allowHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of exposed headers. Supported only for HTTP
-- APIs.
cors_exposeHeaders :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_exposeHeaders = Lens.lens (\Cors' {exposeHeaders} -> exposeHeaders) (\s@Cors' {} a -> s {exposeHeaders = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Represents a collection of allowed origins. Supported only for HTTP
-- APIs.
cors_allowOrigins :: Lens.Lens' Cors (Prelude.Maybe [Prelude.Text])
cors_allowOrigins = Lens.lens (\Cors' {allowOrigins} -> allowOrigins) (\s@Cors' {} a -> s {allowOrigins = a} :: Cors) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether credentials are included in the CORS request.
-- Supported only for HTTP APIs.
cors_allowCredentials :: Lens.Lens' Cors (Prelude.Maybe Prelude.Bool)
cors_allowCredentials = Lens.lens (\Cors' {allowCredentials} -> allowCredentials) (\s@Cors' {} a -> s {allowCredentials = a} :: Cors)

instance Core.FromJSON Cors where
  parseJSON =
    Core.withObject
      "Cors"
      ( \x ->
          Cors'
            Prelude.<$> (x Core..:? "maxAge")
            Prelude.<*> (x Core..:? "allowMethods" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "allowHeaders" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "exposeHeaders" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "allowOrigins" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "allowCredentials")
      )

instance Prelude.Hashable Cors

instance Prelude.NFData Cors

instance Core.ToJSON Cors where
  toJSON Cors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxAge" Core..=) Prelude.<$> maxAge,
            ("allowMethods" Core..=) Prelude.<$> allowMethods,
            ("allowHeaders" Core..=) Prelude.<$> allowHeaders,
            ("exposeHeaders" Core..=) Prelude.<$> exposeHeaders,
            ("allowOrigins" Core..=) Prelude.<$> allowOrigins,
            ("allowCredentials" Core..=)
              Prelude.<$> allowCredentials
          ]
      )

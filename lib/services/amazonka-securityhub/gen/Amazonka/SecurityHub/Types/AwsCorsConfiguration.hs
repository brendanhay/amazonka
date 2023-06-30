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
-- Module      : Amazonka.SecurityHub.Types.AwsCorsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCorsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the cross-origin resource sharing (CORS) configuration for the
-- API. CORS is only supported for HTTP APIs.
--
-- /See:/ 'newAwsCorsConfiguration' smart constructor.
data AwsCorsConfiguration = AwsCorsConfiguration'
  { -- | Indicates whether the CORS request includes credentials.
    allowCredentials :: Prelude.Maybe Prelude.Bool,
    -- | The allowed headers for CORS requests.
    allowHeaders :: Prelude.Maybe [Prelude.Text],
    -- | The allowed methods for CORS requests.
    allowMethods :: Prelude.Maybe [Prelude.Text],
    -- | The allowed origins for CORS requests.
    allowOrigins :: Prelude.Maybe [Prelude.Text],
    -- | The exposed headers for CORS requests.
    exposeHeaders :: Prelude.Maybe [Prelude.Text],
    -- | The number of seconds for which the browser caches preflight request
    -- results.
    maxAge :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCorsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowCredentials', 'awsCorsConfiguration_allowCredentials' - Indicates whether the CORS request includes credentials.
--
-- 'allowHeaders', 'awsCorsConfiguration_allowHeaders' - The allowed headers for CORS requests.
--
-- 'allowMethods', 'awsCorsConfiguration_allowMethods' - The allowed methods for CORS requests.
--
-- 'allowOrigins', 'awsCorsConfiguration_allowOrigins' - The allowed origins for CORS requests.
--
-- 'exposeHeaders', 'awsCorsConfiguration_exposeHeaders' - The exposed headers for CORS requests.
--
-- 'maxAge', 'awsCorsConfiguration_maxAge' - The number of seconds for which the browser caches preflight request
-- results.
newAwsCorsConfiguration ::
  AwsCorsConfiguration
newAwsCorsConfiguration =
  AwsCorsConfiguration'
    { allowCredentials =
        Prelude.Nothing,
      allowHeaders = Prelude.Nothing,
      allowMethods = Prelude.Nothing,
      allowOrigins = Prelude.Nothing,
      exposeHeaders = Prelude.Nothing,
      maxAge = Prelude.Nothing
    }

-- | Indicates whether the CORS request includes credentials.
awsCorsConfiguration_allowCredentials :: Lens.Lens' AwsCorsConfiguration (Prelude.Maybe Prelude.Bool)
awsCorsConfiguration_allowCredentials = Lens.lens (\AwsCorsConfiguration' {allowCredentials} -> allowCredentials) (\s@AwsCorsConfiguration' {} a -> s {allowCredentials = a} :: AwsCorsConfiguration)

-- | The allowed headers for CORS requests.
awsCorsConfiguration_allowHeaders :: Lens.Lens' AwsCorsConfiguration (Prelude.Maybe [Prelude.Text])
awsCorsConfiguration_allowHeaders = Lens.lens (\AwsCorsConfiguration' {allowHeaders} -> allowHeaders) (\s@AwsCorsConfiguration' {} a -> s {allowHeaders = a} :: AwsCorsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The allowed methods for CORS requests.
awsCorsConfiguration_allowMethods :: Lens.Lens' AwsCorsConfiguration (Prelude.Maybe [Prelude.Text])
awsCorsConfiguration_allowMethods = Lens.lens (\AwsCorsConfiguration' {allowMethods} -> allowMethods) (\s@AwsCorsConfiguration' {} a -> s {allowMethods = a} :: AwsCorsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The allowed origins for CORS requests.
awsCorsConfiguration_allowOrigins :: Lens.Lens' AwsCorsConfiguration (Prelude.Maybe [Prelude.Text])
awsCorsConfiguration_allowOrigins = Lens.lens (\AwsCorsConfiguration' {allowOrigins} -> allowOrigins) (\s@AwsCorsConfiguration' {} a -> s {allowOrigins = a} :: AwsCorsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The exposed headers for CORS requests.
awsCorsConfiguration_exposeHeaders :: Lens.Lens' AwsCorsConfiguration (Prelude.Maybe [Prelude.Text])
awsCorsConfiguration_exposeHeaders = Lens.lens (\AwsCorsConfiguration' {exposeHeaders} -> exposeHeaders) (\s@AwsCorsConfiguration' {} a -> s {exposeHeaders = a} :: AwsCorsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds for which the browser caches preflight request
-- results.
awsCorsConfiguration_maxAge :: Lens.Lens' AwsCorsConfiguration (Prelude.Maybe Prelude.Int)
awsCorsConfiguration_maxAge = Lens.lens (\AwsCorsConfiguration' {maxAge} -> maxAge) (\s@AwsCorsConfiguration' {} a -> s {maxAge = a} :: AwsCorsConfiguration)

instance Data.FromJSON AwsCorsConfiguration where
  parseJSON =
    Data.withObject
      "AwsCorsConfiguration"
      ( \x ->
          AwsCorsConfiguration'
            Prelude.<$> (x Data..:? "AllowCredentials")
            Prelude.<*> (x Data..:? "AllowHeaders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AllowMethods" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AllowOrigins" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ExposeHeaders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MaxAge")
      )

instance Prelude.Hashable AwsCorsConfiguration where
  hashWithSalt _salt AwsCorsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` allowCredentials
      `Prelude.hashWithSalt` allowHeaders
      `Prelude.hashWithSalt` allowMethods
      `Prelude.hashWithSalt` allowOrigins
      `Prelude.hashWithSalt` exposeHeaders
      `Prelude.hashWithSalt` maxAge

instance Prelude.NFData AwsCorsConfiguration where
  rnf AwsCorsConfiguration' {..} =
    Prelude.rnf allowCredentials
      `Prelude.seq` Prelude.rnf allowHeaders
      `Prelude.seq` Prelude.rnf allowMethods
      `Prelude.seq` Prelude.rnf allowOrigins
      `Prelude.seq` Prelude.rnf exposeHeaders
      `Prelude.seq` Prelude.rnf maxAge

instance Data.ToJSON AwsCorsConfiguration where
  toJSON AwsCorsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowCredentials" Data..=)
              Prelude.<$> allowCredentials,
            ("AllowHeaders" Data..=) Prelude.<$> allowHeaders,
            ("AllowMethods" Data..=) Prelude.<$> allowMethods,
            ("AllowOrigins" Data..=) Prelude.<$> allowOrigins,
            ("ExposeHeaders" Data..=) Prelude.<$> exposeHeaders,
            ("MaxAge" Data..=) Prelude.<$> maxAge
          ]
      )

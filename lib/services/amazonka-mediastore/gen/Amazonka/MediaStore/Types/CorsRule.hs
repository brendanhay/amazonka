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
-- Module      : Amazonka.MediaStore.Types.CorsRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaStore.Types.CorsRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaStore.Types.MethodName
import qualified Amazonka.Prelude as Prelude

-- | A rule for a CORS policy. You can add up to 100 rules to a CORS policy.
-- If more than one rule applies, the service uses the first applicable
-- rule listed.
--
-- /See:/ 'newCorsRule' smart constructor.
data CorsRule = CorsRule'
  { -- | Identifies an HTTP method that the origin that is specified in the rule
    -- is allowed to execute.
    --
    -- Each CORS rule must contain at least one @AllowedMethods@ and one
    -- @AllowedOrigins@ element.
    allowedMethods :: Prelude.Maybe (Prelude.NonEmpty MethodName),
    -- | One or more headers in the response that you want users to be able to
    -- access from their applications (for example, from a JavaScript
    -- @XMLHttpRequest@ object).
    --
    -- This element is optional for each rule.
    exposeHeaders :: Prelude.Maybe [Prelude.Text],
    -- | The time in seconds that your browser caches the preflight response for
    -- the specified resource.
    --
    -- A CORS rule can have only one @MaxAgeSeconds@ element.
    maxAgeSeconds :: Prelude.Maybe Prelude.Natural,
    -- | One or more response headers that you want users to be able to access
    -- from their applications (for example, from a JavaScript @XMLHttpRequest@
    -- object).
    --
    -- Each CORS rule must have at least one @AllowedOrigins@ element. The
    -- string value can include only one wildcard character (*), for example,
    -- http:\/\/*.example.com. Additionally, you can specify only one wildcard
    -- character to allow cross-origin access for all origins.
    allowedOrigins :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies which headers are allowed in a preflight @OPTIONS@ request
    -- through the @Access-Control-Request-Headers@ header. Each header name
    -- that is specified in @Access-Control-Request-Headers@ must have a
    -- corresponding entry in the rule. Only the headers that were requested
    -- are sent back.
    --
    -- This element can contain only one wildcard character (*).
    allowedHeaders :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CorsRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedMethods', 'corsRule_allowedMethods' - Identifies an HTTP method that the origin that is specified in the rule
-- is allowed to execute.
--
-- Each CORS rule must contain at least one @AllowedMethods@ and one
-- @AllowedOrigins@ element.
--
-- 'exposeHeaders', 'corsRule_exposeHeaders' - One or more headers in the response that you want users to be able to
-- access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
--
-- 'maxAgeSeconds', 'corsRule_maxAgeSeconds' - The time in seconds that your browser caches the preflight response for
-- the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
--
-- 'allowedOrigins', 'corsRule_allowedOrigins' - One or more response headers that you want users to be able to access
-- from their applications (for example, from a JavaScript @XMLHttpRequest@
-- object).
--
-- Each CORS rule must have at least one @AllowedOrigins@ element. The
-- string value can include only one wildcard character (*), for example,
-- http:\/\/*.example.com. Additionally, you can specify only one wildcard
-- character to allow cross-origin access for all origins.
--
-- 'allowedHeaders', 'corsRule_allowedHeaders' - Specifies which headers are allowed in a preflight @OPTIONS@ request
-- through the @Access-Control-Request-Headers@ header. Each header name
-- that is specified in @Access-Control-Request-Headers@ must have a
-- corresponding entry in the rule. Only the headers that were requested
-- are sent back.
--
-- This element can contain only one wildcard character (*).
newCorsRule ::
  -- | 'allowedOrigins'
  Prelude.NonEmpty Prelude.Text ->
  CorsRule
newCorsRule pAllowedOrigins_ =
  CorsRule'
    { allowedMethods = Prelude.Nothing,
      exposeHeaders = Prelude.Nothing,
      maxAgeSeconds = Prelude.Nothing,
      allowedOrigins =
        Lens.coerced Lens.# pAllowedOrigins_,
      allowedHeaders = Prelude.mempty
    }

-- | Identifies an HTTP method that the origin that is specified in the rule
-- is allowed to execute.
--
-- Each CORS rule must contain at least one @AllowedMethods@ and one
-- @AllowedOrigins@ element.
corsRule_allowedMethods :: Lens.Lens' CorsRule (Prelude.Maybe (Prelude.NonEmpty MethodName))
corsRule_allowedMethods = Lens.lens (\CorsRule' {allowedMethods} -> allowedMethods) (\s@CorsRule' {} a -> s {allowedMethods = a} :: CorsRule) Prelude.. Lens.mapping Lens.coerced

-- | One or more headers in the response that you want users to be able to
-- access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
corsRule_exposeHeaders :: Lens.Lens' CorsRule (Prelude.Maybe [Prelude.Text])
corsRule_exposeHeaders = Lens.lens (\CorsRule' {exposeHeaders} -> exposeHeaders) (\s@CorsRule' {} a -> s {exposeHeaders = a} :: CorsRule) Prelude.. Lens.mapping Lens.coerced

-- | The time in seconds that your browser caches the preflight response for
-- the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
corsRule_maxAgeSeconds :: Lens.Lens' CorsRule (Prelude.Maybe Prelude.Natural)
corsRule_maxAgeSeconds = Lens.lens (\CorsRule' {maxAgeSeconds} -> maxAgeSeconds) (\s@CorsRule' {} a -> s {maxAgeSeconds = a} :: CorsRule)

-- | One or more response headers that you want users to be able to access
-- from their applications (for example, from a JavaScript @XMLHttpRequest@
-- object).
--
-- Each CORS rule must have at least one @AllowedOrigins@ element. The
-- string value can include only one wildcard character (*), for example,
-- http:\/\/*.example.com. Additionally, you can specify only one wildcard
-- character to allow cross-origin access for all origins.
corsRule_allowedOrigins :: Lens.Lens' CorsRule (Prelude.NonEmpty Prelude.Text)
corsRule_allowedOrigins = Lens.lens (\CorsRule' {allowedOrigins} -> allowedOrigins) (\s@CorsRule' {} a -> s {allowedOrigins = a} :: CorsRule) Prelude.. Lens.coerced

-- | Specifies which headers are allowed in a preflight @OPTIONS@ request
-- through the @Access-Control-Request-Headers@ header. Each header name
-- that is specified in @Access-Control-Request-Headers@ must have a
-- corresponding entry in the rule. Only the headers that were requested
-- are sent back.
--
-- This element can contain only one wildcard character (*).
corsRule_allowedHeaders :: Lens.Lens' CorsRule [Prelude.Text]
corsRule_allowedHeaders = Lens.lens (\CorsRule' {allowedHeaders} -> allowedHeaders) (\s@CorsRule' {} a -> s {allowedHeaders = a} :: CorsRule) Prelude.. Lens.coerced

instance Core.FromJSON CorsRule where
  parseJSON =
    Core.withObject
      "CorsRule"
      ( \x ->
          CorsRule'
            Prelude.<$> (x Core..:? "AllowedMethods")
            Prelude.<*> (x Core..:? "ExposeHeaders" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MaxAgeSeconds")
            Prelude.<*> (x Core..: "AllowedOrigins")
            Prelude.<*> ( x Core..:? "AllowedHeaders"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CorsRule where
  hashWithSalt _salt CorsRule' {..} =
    _salt `Prelude.hashWithSalt` allowedMethods
      `Prelude.hashWithSalt` exposeHeaders
      `Prelude.hashWithSalt` maxAgeSeconds
      `Prelude.hashWithSalt` allowedOrigins
      `Prelude.hashWithSalt` allowedHeaders

instance Prelude.NFData CorsRule where
  rnf CorsRule' {..} =
    Prelude.rnf allowedMethods
      `Prelude.seq` Prelude.rnf exposeHeaders
      `Prelude.seq` Prelude.rnf maxAgeSeconds
      `Prelude.seq` Prelude.rnf allowedOrigins
      `Prelude.seq` Prelude.rnf allowedHeaders

instance Core.ToJSON CorsRule where
  toJSON CorsRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AllowedMethods" Core..=)
              Prelude.<$> allowedMethods,
            ("ExposeHeaders" Core..=) Prelude.<$> exposeHeaders,
            ("MaxAgeSeconds" Core..=) Prelude.<$> maxAgeSeconds,
            Prelude.Just
              ("AllowedOrigins" Core..= allowedOrigins),
            Prelude.Just
              ("AllowedHeaders" Core..= allowedHeaders)
          ]
      )

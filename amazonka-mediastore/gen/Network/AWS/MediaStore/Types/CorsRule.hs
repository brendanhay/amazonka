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
-- Module      : Network.AWS.MediaStore.Types.CorsRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.CorsRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types.MethodName

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
    allowedMethods :: Core.Maybe (Core.NonEmpty MethodName),
    -- | The time in seconds that your browser caches the preflight response for
    -- the specified resource.
    --
    -- A CORS rule can have only one @MaxAgeSeconds@ element.
    maxAgeSeconds :: Core.Maybe Core.Natural,
    -- | One or more headers in the response that you want users to be able to
    -- access from their applications (for example, from a JavaScript
    -- @XMLHttpRequest@ object).
    --
    -- This element is optional for each rule.
    exposeHeaders :: Core.Maybe [Core.Text],
    -- | One or more response headers that you want users to be able to access
    -- from their applications (for example, from a JavaScript @XMLHttpRequest@
    -- object).
    --
    -- Each CORS rule must have at least one @AllowedOrigins@ element. The
    -- string value can include only one wildcard character (*), for example,
    -- http:\/\/*.example.com. Additionally, you can specify only one wildcard
    -- character to allow cross-origin access for all origins.
    allowedOrigins :: Core.NonEmpty Core.Text,
    -- | Specifies which headers are allowed in a preflight @OPTIONS@ request
    -- through the @Access-Control-Request-Headers@ header. Each header name
    -- that is specified in @Access-Control-Request-Headers@ must have a
    -- corresponding entry in the rule. Only the headers that were requested
    -- are sent back.
    --
    -- This element can contain only one wildcard character (*).
    allowedHeaders :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxAgeSeconds', 'corsRule_maxAgeSeconds' - The time in seconds that your browser caches the preflight response for
-- the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
--
-- 'exposeHeaders', 'corsRule_exposeHeaders' - One or more headers in the response that you want users to be able to
-- access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
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
  Core.NonEmpty Core.Text ->
  CorsRule
newCorsRule pAllowedOrigins_ =
  CorsRule'
    { allowedMethods = Core.Nothing,
      maxAgeSeconds = Core.Nothing,
      exposeHeaders = Core.Nothing,
      allowedOrigins =
        Lens._Coerce Lens.# pAllowedOrigins_,
      allowedHeaders = Core.mempty
    }

-- | Identifies an HTTP method that the origin that is specified in the rule
-- is allowed to execute.
--
-- Each CORS rule must contain at least one @AllowedMethods@ and one
-- @AllowedOrigins@ element.
corsRule_allowedMethods :: Lens.Lens' CorsRule (Core.Maybe (Core.NonEmpty MethodName))
corsRule_allowedMethods = Lens.lens (\CorsRule' {allowedMethods} -> allowedMethods) (\s@CorsRule' {} a -> s {allowedMethods = a} :: CorsRule) Core.. Lens.mapping Lens._Coerce

-- | The time in seconds that your browser caches the preflight response for
-- the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
corsRule_maxAgeSeconds :: Lens.Lens' CorsRule (Core.Maybe Core.Natural)
corsRule_maxAgeSeconds = Lens.lens (\CorsRule' {maxAgeSeconds} -> maxAgeSeconds) (\s@CorsRule' {} a -> s {maxAgeSeconds = a} :: CorsRule)

-- | One or more headers in the response that you want users to be able to
-- access from their applications (for example, from a JavaScript
-- @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
corsRule_exposeHeaders :: Lens.Lens' CorsRule (Core.Maybe [Core.Text])
corsRule_exposeHeaders = Lens.lens (\CorsRule' {exposeHeaders} -> exposeHeaders) (\s@CorsRule' {} a -> s {exposeHeaders = a} :: CorsRule) Core.. Lens.mapping Lens._Coerce

-- | One or more response headers that you want users to be able to access
-- from their applications (for example, from a JavaScript @XMLHttpRequest@
-- object).
--
-- Each CORS rule must have at least one @AllowedOrigins@ element. The
-- string value can include only one wildcard character (*), for example,
-- http:\/\/*.example.com. Additionally, you can specify only one wildcard
-- character to allow cross-origin access for all origins.
corsRule_allowedOrigins :: Lens.Lens' CorsRule (Core.NonEmpty Core.Text)
corsRule_allowedOrigins = Lens.lens (\CorsRule' {allowedOrigins} -> allowedOrigins) (\s@CorsRule' {} a -> s {allowedOrigins = a} :: CorsRule) Core.. Lens._Coerce

-- | Specifies which headers are allowed in a preflight @OPTIONS@ request
-- through the @Access-Control-Request-Headers@ header. Each header name
-- that is specified in @Access-Control-Request-Headers@ must have a
-- corresponding entry in the rule. Only the headers that were requested
-- are sent back.
--
-- This element can contain only one wildcard character (*).
corsRule_allowedHeaders :: Lens.Lens' CorsRule [Core.Text]
corsRule_allowedHeaders = Lens.lens (\CorsRule' {allowedHeaders} -> allowedHeaders) (\s@CorsRule' {} a -> s {allowedHeaders = a} :: CorsRule) Core.. Lens._Coerce

instance Core.FromJSON CorsRule where
  parseJSON =
    Core.withObject
      "CorsRule"
      ( \x ->
          CorsRule'
            Core.<$> (x Core..:? "AllowedMethods")
            Core.<*> (x Core..:? "MaxAgeSeconds")
            Core.<*> (x Core..:? "ExposeHeaders" Core..!= Core.mempty)
            Core.<*> (x Core..: "AllowedOrigins")
            Core.<*> (x Core..:? "AllowedHeaders" Core..!= Core.mempty)
      )

instance Core.Hashable CorsRule

instance Core.NFData CorsRule

instance Core.ToJSON CorsRule where
  toJSON CorsRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AllowedMethods" Core..=) Core.<$> allowedMethods,
            ("MaxAgeSeconds" Core..=) Core.<$> maxAgeSeconds,
            ("ExposeHeaders" Core..=) Core.<$> exposeHeaders,
            Core.Just ("AllowedOrigins" Core..= allowedOrigins),
            Core.Just ("AllowedHeaders" Core..= allowedHeaders)
          ]
      )

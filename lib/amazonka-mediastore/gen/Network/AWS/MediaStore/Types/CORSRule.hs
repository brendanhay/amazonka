{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.CORSRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.CORSRule
  ( CORSRule (..),

    -- * Smart constructor
    mkCORSRule,

    -- * Lenses
    crAllowedMethods,
    crMaxAgeSeconds,
    crAllowedHeaders,
    crAllowedOrigins,
    crExposeHeaders,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types.MethodName
import qualified Network.AWS.Prelude as Lude

-- | A rule for a CORS policy. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.
--
-- /See:/ 'mkCORSRule' smart constructor.
data CORSRule = CORSRule'
  { -- | Identifies an HTTP method that the origin that is specified in the rule is allowed to execute.
    --
    -- Each CORS rule must contain at least one @AllowedMethods@ and one @AllowedOrigins@ element.
    allowedMethods :: Lude.Maybe (Lude.NonEmpty MethodName),
    -- | The time in seconds that your browser caches the preflight response for the specified resource.
    --
    -- A CORS rule can have only one @MaxAgeSeconds@ element.
    maxAgeSeconds :: Lude.Maybe Lude.Natural,
    -- | Specifies which headers are allowed in a preflight @OPTIONS@ request through the @Access-Control-Request-Headers@ header. Each header name that is specified in @Access-Control-Request-Headers@ must have a corresponding entry in the rule. Only the headers that were requested are sent back.
    --
    -- This element can contain only one wildcard character (*).
    allowedHeaders :: [Lude.Text],
    -- | One or more response headers that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
    --
    -- Each CORS rule must have at least one @AllowedOrigins@ element. The string value can include only one wildcard character (*), for example, http://*.example.com. Additionally, you can specify only one wildcard character to allow cross-origin access for all origins.
    allowedOrigins :: Lude.NonEmpty Lude.Text,
    -- | One or more headers in the response that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
    --
    -- This element is optional for each rule.
    exposeHeaders :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CORSRule' with the minimum fields required to make a request.
--
-- * 'allowedMethods' - Identifies an HTTP method that the origin that is specified in the rule is allowed to execute.
--
-- Each CORS rule must contain at least one @AllowedMethods@ and one @AllowedOrigins@ element.
-- * 'maxAgeSeconds' - The time in seconds that your browser caches the preflight response for the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
-- * 'allowedHeaders' - Specifies which headers are allowed in a preflight @OPTIONS@ request through the @Access-Control-Request-Headers@ header. Each header name that is specified in @Access-Control-Request-Headers@ must have a corresponding entry in the rule. Only the headers that were requested are sent back.
--
-- This element can contain only one wildcard character (*).
-- * 'allowedOrigins' - One or more response headers that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- Each CORS rule must have at least one @AllowedOrigins@ element. The string value can include only one wildcard character (*), for example, http://*.example.com. Additionally, you can specify only one wildcard character to allow cross-origin access for all origins.
-- * 'exposeHeaders' - One or more headers in the response that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
mkCORSRule ::
  -- | 'allowedOrigins'
  Lude.NonEmpty Lude.Text ->
  CORSRule
mkCORSRule pAllowedOrigins_ =
  CORSRule'
    { allowedMethods = Lude.Nothing,
      maxAgeSeconds = Lude.Nothing,
      allowedHeaders = Lude.mempty,
      allowedOrigins = pAllowedOrigins_,
      exposeHeaders = Lude.Nothing
    }

-- | Identifies an HTTP method that the origin that is specified in the rule is allowed to execute.
--
-- Each CORS rule must contain at least one @AllowedMethods@ and one @AllowedOrigins@ element.
--
-- /Note:/ Consider using 'allowedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedMethods :: Lens.Lens' CORSRule (Lude.Maybe (Lude.NonEmpty MethodName))
crAllowedMethods = Lens.lens (allowedMethods :: CORSRule -> Lude.Maybe (Lude.NonEmpty MethodName)) (\s a -> s {allowedMethods = a} :: CORSRule)
{-# DEPRECATED crAllowedMethods "Use generic-lens or generic-optics with 'allowedMethods' instead." #-}

-- | The time in seconds that your browser caches the preflight response for the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
--
-- /Note:/ Consider using 'maxAgeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaxAgeSeconds :: Lens.Lens' CORSRule (Lude.Maybe Lude.Natural)
crMaxAgeSeconds = Lens.lens (maxAgeSeconds :: CORSRule -> Lude.Maybe Lude.Natural) (\s a -> s {maxAgeSeconds = a} :: CORSRule)
{-# DEPRECATED crMaxAgeSeconds "Use generic-lens or generic-optics with 'maxAgeSeconds' instead." #-}

-- | Specifies which headers are allowed in a preflight @OPTIONS@ request through the @Access-Control-Request-Headers@ header. Each header name that is specified in @Access-Control-Request-Headers@ must have a corresponding entry in the rule. Only the headers that were requested are sent back.
--
-- This element can contain only one wildcard character (*).
--
-- /Note:/ Consider using 'allowedHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedHeaders :: Lens.Lens' CORSRule [Lude.Text]
crAllowedHeaders = Lens.lens (allowedHeaders :: CORSRule -> [Lude.Text]) (\s a -> s {allowedHeaders = a} :: CORSRule)
{-# DEPRECATED crAllowedHeaders "Use generic-lens or generic-optics with 'allowedHeaders' instead." #-}

-- | One or more response headers that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- Each CORS rule must have at least one @AllowedOrigins@ element. The string value can include only one wildcard character (*), for example, http://*.example.com. Additionally, you can specify only one wildcard character to allow cross-origin access for all origins.
--
-- /Note:/ Consider using 'allowedOrigins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedOrigins :: Lens.Lens' CORSRule (Lude.NonEmpty Lude.Text)
crAllowedOrigins = Lens.lens (allowedOrigins :: CORSRule -> Lude.NonEmpty Lude.Text) (\s a -> s {allowedOrigins = a} :: CORSRule)
{-# DEPRECATED crAllowedOrigins "Use generic-lens or generic-optics with 'allowedOrigins' instead." #-}

-- | One or more headers in the response that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
--
-- /Note:/ Consider using 'exposeHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crExposeHeaders :: Lens.Lens' CORSRule (Lude.Maybe [Lude.Text])
crExposeHeaders = Lens.lens (exposeHeaders :: CORSRule -> Lude.Maybe [Lude.Text]) (\s a -> s {exposeHeaders = a} :: CORSRule)
{-# DEPRECATED crExposeHeaders "Use generic-lens or generic-optics with 'exposeHeaders' instead." #-}

instance Lude.FromJSON CORSRule where
  parseJSON =
    Lude.withObject
      "CORSRule"
      ( \x ->
          CORSRule'
            Lude.<$> (x Lude..:? "AllowedMethods")
            Lude.<*> (x Lude..:? "MaxAgeSeconds")
            Lude.<*> (x Lude..:? "AllowedHeaders" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "AllowedOrigins")
            Lude.<*> (x Lude..:? "ExposeHeaders" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON CORSRule where
  toJSON CORSRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AllowedMethods" Lude..=) Lude.<$> allowedMethods,
            ("MaxAgeSeconds" Lude..=) Lude.<$> maxAgeSeconds,
            Lude.Just ("AllowedHeaders" Lude..= allowedHeaders),
            Lude.Just ("AllowedOrigins" Lude..= allowedOrigins),
            ("ExposeHeaders" Lude..=) Lude.<$> exposeHeaders
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.CorsRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types.CorsRule
  ( CorsRule (..)
  -- * Smart constructor
  , mkCorsRule
  -- * Lenses
  , crAllowedOrigins
  , crAllowedHeaders
  , crAllowedMethods
  , crExposeHeaders
  , crMaxAgeSeconds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types.Header as Types
import qualified Network.AWS.MediaStore.Types.MethodName as Types
import qualified Network.AWS.MediaStore.Types.Origin as Types
import qualified Network.AWS.Prelude as Core

-- | A rule for a CORS policy. You can add up to 100 rules to a CORS policy. If more than one rule applies, the service uses the first applicable rule listed.
--
-- /See:/ 'mkCorsRule' smart constructor.
data CorsRule = CorsRule'
  { allowedOrigins :: Core.NonEmpty Types.Origin
    -- ^ One or more response headers that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- Each CORS rule must have at least one @AllowedOrigins@ element. The string value can include only one wildcard character (*), for example, http://*.example.com. Additionally, you can specify only one wildcard character to allow cross-origin access for all origins.
  , allowedHeaders :: [Types.Header]
    -- ^ Specifies which headers are allowed in a preflight @OPTIONS@ request through the @Access-Control-Request-Headers@ header. Each header name that is specified in @Access-Control-Request-Headers@ must have a corresponding entry in the rule. Only the headers that were requested are sent back. 
--
-- This element can contain only one wildcard character (*).
  , allowedMethods :: Core.Maybe (Core.NonEmpty Types.MethodName)
    -- ^ Identifies an HTTP method that the origin that is specified in the rule is allowed to execute.
--
-- Each CORS rule must contain at least one @AllowedMethods@ and one @AllowedOrigins@ element.
  , exposeHeaders :: Core.Maybe [Types.Header]
    -- ^ One or more headers in the response that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
  , maxAgeSeconds :: Core.Maybe Core.Natural
    -- ^ The time in seconds that your browser caches the preflight response for the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CorsRule' value with any optional fields omitted.
mkCorsRule
    :: Core.NonEmpty Types.Origin -- ^ 'allowedOrigins'
    -> CorsRule
mkCorsRule allowedOrigins
  = CorsRule'{allowedOrigins, allowedHeaders = Core.mempty,
              allowedMethods = Core.Nothing, exposeHeaders = Core.Nothing,
              maxAgeSeconds = Core.Nothing}

-- | One or more response headers that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- Each CORS rule must have at least one @AllowedOrigins@ element. The string value can include only one wildcard character (*), for example, http://*.example.com. Additionally, you can specify only one wildcard character to allow cross-origin access for all origins.
--
-- /Note:/ Consider using 'allowedOrigins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedOrigins :: Lens.Lens' CorsRule (Core.NonEmpty Types.Origin)
crAllowedOrigins = Lens.field @"allowedOrigins"
{-# INLINEABLE crAllowedOrigins #-}
{-# DEPRECATED allowedOrigins "Use generic-lens or generic-optics with 'allowedOrigins' instead"  #-}

-- | Specifies which headers are allowed in a preflight @OPTIONS@ request through the @Access-Control-Request-Headers@ header. Each header name that is specified in @Access-Control-Request-Headers@ must have a corresponding entry in the rule. Only the headers that were requested are sent back. 
--
-- This element can contain only one wildcard character (*).
--
-- /Note:/ Consider using 'allowedHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedHeaders :: Lens.Lens' CorsRule [Types.Header]
crAllowedHeaders = Lens.field @"allowedHeaders"
{-# INLINEABLE crAllowedHeaders #-}
{-# DEPRECATED allowedHeaders "Use generic-lens or generic-optics with 'allowedHeaders' instead"  #-}

-- | Identifies an HTTP method that the origin that is specified in the rule is allowed to execute.
--
-- Each CORS rule must contain at least one @AllowedMethods@ and one @AllowedOrigins@ element.
--
-- /Note:/ Consider using 'allowedMethods' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crAllowedMethods :: Lens.Lens' CorsRule (Core.Maybe (Core.NonEmpty Types.MethodName))
crAllowedMethods = Lens.field @"allowedMethods"
{-# INLINEABLE crAllowedMethods #-}
{-# DEPRECATED allowedMethods "Use generic-lens or generic-optics with 'allowedMethods' instead"  #-}

-- | One or more headers in the response that you want users to be able to access from their applications (for example, from a JavaScript @XMLHttpRequest@ object).
--
-- This element is optional for each rule.
--
-- /Note:/ Consider using 'exposeHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crExposeHeaders :: Lens.Lens' CorsRule (Core.Maybe [Types.Header])
crExposeHeaders = Lens.field @"exposeHeaders"
{-# INLINEABLE crExposeHeaders #-}
{-# DEPRECATED exposeHeaders "Use generic-lens or generic-optics with 'exposeHeaders' instead"  #-}

-- | The time in seconds that your browser caches the preflight response for the specified resource.
--
-- A CORS rule can have only one @MaxAgeSeconds@ element.
--
-- /Note:/ Consider using 'maxAgeSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crMaxAgeSeconds :: Lens.Lens' CorsRule (Core.Maybe Core.Natural)
crMaxAgeSeconds = Lens.field @"maxAgeSeconds"
{-# INLINEABLE crMaxAgeSeconds #-}
{-# DEPRECATED maxAgeSeconds "Use generic-lens or generic-optics with 'maxAgeSeconds' instead"  #-}

instance Core.FromJSON CorsRule where
        toJSON CorsRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AllowedOrigins" Core..= allowedOrigins),
                  Core.Just ("AllowedHeaders" Core..= allowedHeaders),
                  ("AllowedMethods" Core..=) Core.<$> allowedMethods,
                  ("ExposeHeaders" Core..=) Core.<$> exposeHeaders,
                  ("MaxAgeSeconds" Core..=) Core.<$> maxAgeSeconds])

instance Core.FromJSON CorsRule where
        parseJSON
          = Core.withObject "CorsRule" Core.$
              \ x ->
                CorsRule' Core.<$>
                  (x Core..: "AllowedOrigins") Core.<*>
                    x Core..:? "AllowedHeaders" Core..!= Core.mempty
                    Core.<*> x Core..:? "AllowedMethods"
                    Core.<*> x Core..:? "ExposeHeaders"
                    Core.<*> x Core..:? "MaxAgeSeconds"

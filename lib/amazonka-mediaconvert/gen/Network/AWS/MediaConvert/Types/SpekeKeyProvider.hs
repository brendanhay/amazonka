{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.SpekeKeyProvider
  ( SpekeKeyProvider (..)
  -- * Smart constructor
  , mkSpekeKeyProvider
  -- * Lenses
  , sCertificateArn
  , sResourceId
  , sSystemIds
  , sUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /See:/ 'mkSpekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { certificateArn :: Core.Maybe Core.Text
    -- ^ If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
  , resourceId :: Core.Maybe Core.Text
    -- ^ Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
  , systemIds :: Core.Maybe [Core.Text]
    -- ^ Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id. See
--
--  https://dashif.org/identifiers/content_protection/ for more details.
  , url :: Core.Maybe Core.Text
    -- ^ Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpekeKeyProvider' value with any optional fields omitted.
mkSpekeKeyProvider
    :: SpekeKeyProvider
mkSpekeKeyProvider
  = SpekeKeyProvider'{certificateArn = Core.Nothing,
                      resourceId = Core.Nothing, systemIds = Core.Nothing,
                      url = Core.Nothing}

-- | If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCertificateArn :: Lens.Lens' SpekeKeyProvider (Core.Maybe Core.Text)
sCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE sCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResourceId :: Lens.Lens' SpekeKeyProvider (Core.Maybe Core.Text)
sResourceId = Lens.field @"resourceId"
{-# INLINEABLE sResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id. See
--
--  https://dashif.org/identifiers/content_protection/ for more details.
--
-- /Note:/ Consider using 'systemIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSystemIds :: Lens.Lens' SpekeKeyProvider (Core.Maybe [Core.Text])
sSystemIds = Lens.field @"systemIds"
{-# INLINEABLE sSystemIds #-}
{-# DEPRECATED systemIds "Use generic-lens or generic-optics with 'systemIds' instead"  #-}

-- | Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUrl :: Lens.Lens' SpekeKeyProvider (Core.Maybe Core.Text)
sUrl = Lens.field @"url"
{-# INLINEABLE sUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON SpekeKeyProvider where
        toJSON SpekeKeyProvider{..}
          = Core.object
              (Core.catMaybes
                 [("certificateArn" Core..=) Core.<$> certificateArn,
                  ("resourceId" Core..=) Core.<$> resourceId,
                  ("systemIds" Core..=) Core.<$> systemIds,
                  ("url" Core..=) Core.<$> url])

instance Core.FromJSON SpekeKeyProvider where
        parseJSON
          = Core.withObject "SpekeKeyProvider" Core.$
              \ x ->
                SpekeKeyProvider' Core.<$>
                  (x Core..:? "certificateArn") Core.<*> x Core..:? "resourceId"
                    Core.<*> x Core..:? "systemIds"
                    Core.<*> x Core..:? "url"

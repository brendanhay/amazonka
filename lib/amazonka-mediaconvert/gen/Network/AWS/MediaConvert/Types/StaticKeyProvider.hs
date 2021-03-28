{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StaticKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.StaticKeyProvider
  ( StaticKeyProvider (..)
  -- * Smart constructor
  , mkStaticKeyProvider
  -- * Lenses
  , skpKeyFormat
  , skpKeyFormatVersions
  , skpStaticKeyValue
  , skpUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use these settings to set up encryption with a static key provider.
--
-- /See:/ 'mkStaticKeyProvider' smart constructor.
data StaticKeyProvider = StaticKeyProvider'
  { keyFormat :: Core.Maybe Core.Text
    -- ^ Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
  , keyFormatVersions :: Core.Maybe Core.Text
    -- ^ Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
  , staticKeyValue :: Core.Maybe Core.Text
    -- ^ Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
  , url :: Core.Maybe Core.Text
    -- ^ Relates to DRM implementation. The location of the license server used for protecting content.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StaticKeyProvider' value with any optional fields omitted.
mkStaticKeyProvider
    :: StaticKeyProvider
mkStaticKeyProvider
  = StaticKeyProvider'{keyFormat = Core.Nothing,
                       keyFormatVersions = Core.Nothing, staticKeyValue = Core.Nothing,
                       url = Core.Nothing}

-- | Relates to DRM implementation. Sets the value of the KEYFORMAT attribute. Must be 'identity' or a reverse DNS string. May be omitted to indicate an implicit value of 'identity'.
--
-- /Note:/ Consider using 'keyFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpKeyFormat :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
skpKeyFormat = Lens.field @"keyFormat"
{-# INLINEABLE skpKeyFormat #-}
{-# DEPRECATED keyFormat "Use generic-lens or generic-optics with 'keyFormat' instead"  #-}

-- | Relates to DRM implementation. Either a single positive integer version value or a slash delimited list of version values (1/2/3).
--
-- /Note:/ Consider using 'keyFormatVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpKeyFormatVersions :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
skpKeyFormatVersions = Lens.field @"keyFormatVersions"
{-# INLINEABLE skpKeyFormatVersions #-}
{-# DEPRECATED keyFormatVersions "Use generic-lens or generic-optics with 'keyFormatVersions' instead"  #-}

-- | Relates to DRM implementation. Use a 32-character hexidecimal string to specify Key Value (StaticKeyValue).
--
-- /Note:/ Consider using 'staticKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpStaticKeyValue :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
skpStaticKeyValue = Lens.field @"staticKeyValue"
{-# INLINEABLE skpStaticKeyValue #-}
{-# DEPRECATED staticKeyValue "Use generic-lens or generic-optics with 'staticKeyValue' instead"  #-}

-- | Relates to DRM implementation. The location of the license server used for protecting content.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
skpUrl :: Lens.Lens' StaticKeyProvider (Core.Maybe Core.Text)
skpUrl = Lens.field @"url"
{-# INLINEABLE skpUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON StaticKeyProvider where
        toJSON StaticKeyProvider{..}
          = Core.object
              (Core.catMaybes
                 [("keyFormat" Core..=) Core.<$> keyFormat,
                  ("keyFormatVersions" Core..=) Core.<$> keyFormatVersions,
                  ("staticKeyValue" Core..=) Core.<$> staticKeyValue,
                  ("url" Core..=) Core.<$> url])

instance Core.FromJSON StaticKeyProvider where
        parseJSON
          = Core.withObject "StaticKeyProvider" Core.$
              \ x ->
                StaticKeyProvider' Core.<$>
                  (x Core..:? "keyFormat") Core.<*> x Core..:? "keyFormatVersions"
                    Core.<*> x Core..:? "staticKeyValue"
                    Core.<*> x Core..:? "url"

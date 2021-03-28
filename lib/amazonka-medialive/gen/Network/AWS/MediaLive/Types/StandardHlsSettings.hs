{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.StandardHlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.StandardHlsSettings
  ( StandardHlsSettings (..)
  -- * Smart constructor
  , mkStandardHlsSettings
  -- * Lenses
  , shsM3u8Settings
  , shsAudioRenditionSets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.M3u8Settings as Types
import qualified Network.AWS.Prelude as Core

-- | Standard Hls Settings
--
-- /See:/ 'mkStandardHlsSettings' smart constructor.
data StandardHlsSettings = StandardHlsSettings'
  { m3u8Settings :: Types.M3u8Settings
  , audioRenditionSets :: Core.Maybe Core.Text
    -- ^ List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StandardHlsSettings' value with any optional fields omitted.
mkStandardHlsSettings
    :: Types.M3u8Settings -- ^ 'm3u8Settings'
    -> StandardHlsSettings
mkStandardHlsSettings m3u8Settings
  = StandardHlsSettings'{m3u8Settings,
                         audioRenditionSets = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'm3u8Settings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shsM3u8Settings :: Lens.Lens' StandardHlsSettings Types.M3u8Settings
shsM3u8Settings = Lens.field @"m3u8Settings"
{-# INLINEABLE shsM3u8Settings #-}
{-# DEPRECATED m3u8Settings "Use generic-lens or generic-optics with 'm3u8Settings' instead"  #-}

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- /Note:/ Consider using 'audioRenditionSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shsAudioRenditionSets :: Lens.Lens' StandardHlsSettings (Core.Maybe Core.Text)
shsAudioRenditionSets = Lens.field @"audioRenditionSets"
{-# INLINEABLE shsAudioRenditionSets #-}
{-# DEPRECATED audioRenditionSets "Use generic-lens or generic-optics with 'audioRenditionSets' instead"  #-}

instance Core.FromJSON StandardHlsSettings where
        toJSON StandardHlsSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("m3u8Settings" Core..= m3u8Settings),
                  ("audioRenditionSets" Core..=) Core.<$> audioRenditionSets])

instance Core.FromJSON StandardHlsSettings where
        parseJSON
          = Core.withObject "StandardHlsSettings" Core.$
              \ x ->
                StandardHlsSettings' Core.<$>
                  (x Core..: "m3u8Settings") Core.<*> x Core..:? "audioRenditionSets"

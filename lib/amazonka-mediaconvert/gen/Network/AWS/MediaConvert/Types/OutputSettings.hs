{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.OutputSettings
  ( OutputSettings (..)
  -- * Smart constructor
  , mkOutputSettings
  -- * Lenses
  , osHlsSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.HlsSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Specific settings for this type of output.
--
-- /See:/ 'mkOutputSettings' smart constructor.
newtype OutputSettings = OutputSettings'
  { hlsSettings :: Core.Maybe Types.HlsSettings
    -- ^ Settings for HLS output groups
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputSettings' value with any optional fields omitted.
mkOutputSettings
    :: OutputSettings
mkOutputSettings = OutputSettings'{hlsSettings = Core.Nothing}

-- | Settings for HLS output groups
--
-- /Note:/ Consider using 'hlsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osHlsSettings :: Lens.Lens' OutputSettings (Core.Maybe Types.HlsSettings)
osHlsSettings = Lens.field @"hlsSettings"
{-# INLINEABLE osHlsSettings #-}
{-# DEPRECATED hlsSettings "Use generic-lens or generic-optics with 'hlsSettings' instead"  #-}

instance Core.FromJSON OutputSettings where
        toJSON OutputSettings{..}
          = Core.object
              (Core.catMaybes [("hlsSettings" Core..=) Core.<$> hlsSettings])

instance Core.FromJSON OutputSettings where
        parseJSON
          = Core.withObject "OutputSettings" Core.$
              \ x -> OutputSettings' Core.<$> (x Core..:? "hlsSettings")

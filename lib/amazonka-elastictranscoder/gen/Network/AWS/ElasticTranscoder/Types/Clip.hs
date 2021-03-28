{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Clip
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Clip
  ( Clip (..)
  -- * Smart constructor
  , mkClip
  -- * Lenses
  , cTimeSpan
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.TimeSpan as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for one clip in a composition. All jobs in a playlist must have the same clip settings.
--
-- /See:/ 'mkClip' smart constructor.
newtype Clip = Clip'
  { timeSpan :: Core.Maybe Types.TimeSpan
    -- ^ Settings that determine when a clip begins and how long it lasts.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Clip' value with any optional fields omitted.
mkClip
    :: Clip
mkClip = Clip'{timeSpan = Core.Nothing}

-- | Settings that determine when a clip begins and how long it lasts.
--
-- /Note:/ Consider using 'timeSpan' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTimeSpan :: Lens.Lens' Clip (Core.Maybe Types.TimeSpan)
cTimeSpan = Lens.field @"timeSpan"
{-# INLINEABLE cTimeSpan #-}
{-# DEPRECATED timeSpan "Use generic-lens or generic-optics with 'timeSpan' instead"  #-}

instance Core.FromJSON Clip where
        toJSON Clip{..}
          = Core.object
              (Core.catMaybes [("TimeSpan" Core..=) Core.<$> timeSpan])

instance Core.FromJSON Clip where
        parseJSON
          = Core.withObject "Clip" Core.$
              \ x -> Clip' Core.<$> (x Core..:? "TimeSpan")

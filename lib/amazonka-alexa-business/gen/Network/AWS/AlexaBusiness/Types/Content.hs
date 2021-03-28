{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Content
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.Content
  ( Content (..)
  -- * Smart constructor
  , mkContent
  -- * Lenses
  , cAudioList
  , cSsmlList
  , cTextList
  ) where

import qualified Network.AWS.AlexaBusiness.Types.Audio as Types
import qualified Network.AWS.AlexaBusiness.Types.Ssml as Types
import qualified Network.AWS.AlexaBusiness.Types.TextMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The content definition. This can contain only one text, SSML, or audio list object.
--
-- /See:/ 'mkContent' smart constructor.
data Content = Content'
  { audioList :: Core.Maybe [Types.Audio]
    -- ^ The list of audio messages.
  , ssmlList :: Core.Maybe [Types.Ssml]
    -- ^ The list of SSML messages.
  , textList :: Core.Maybe [Types.TextMessage]
    -- ^ The list of text messages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Content' value with any optional fields omitted.
mkContent
    :: Content
mkContent
  = Content'{audioList = Core.Nothing, ssmlList = Core.Nothing,
             textList = Core.Nothing}

-- | The list of audio messages.
--
-- /Note:/ Consider using 'audioList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAudioList :: Lens.Lens' Content (Core.Maybe [Types.Audio])
cAudioList = Lens.field @"audioList"
{-# INLINEABLE cAudioList #-}
{-# DEPRECATED audioList "Use generic-lens or generic-optics with 'audioList' instead"  #-}

-- | The list of SSML messages.
--
-- /Note:/ Consider using 'ssmlList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSsmlList :: Lens.Lens' Content (Core.Maybe [Types.Ssml])
cSsmlList = Lens.field @"ssmlList"
{-# INLINEABLE cSsmlList #-}
{-# DEPRECATED ssmlList "Use generic-lens or generic-optics with 'ssmlList' instead"  #-}

-- | The list of text messages.
--
-- /Note:/ Consider using 'textList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTextList :: Lens.Lens' Content (Core.Maybe [Types.TextMessage])
cTextList = Lens.field @"textList"
{-# INLINEABLE cTextList #-}
{-# DEPRECATED textList "Use generic-lens or generic-optics with 'textList' instead"  #-}

instance Core.FromJSON Content where
        toJSON Content{..}
          = Core.object
              (Core.catMaybes
                 [("AudioList" Core..=) Core.<$> audioList,
                  ("SsmlList" Core..=) Core.<$> ssmlList,
                  ("TextList" Core..=) Core.<$> textList])

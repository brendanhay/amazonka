{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReactionValueFormats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReactionValueFormats
  ( ReactionValueFormats (..),

    -- * Smart constructor
    mkReactionValueFormats,

    -- * Lenses
    rvfEmoji,
    rvfShortCode,
    rvfUnicode,
  )
where

import qualified Network.AWS.CodeCommit.Types.Emoji as Types
import qualified Network.AWS.CodeCommit.Types.ShortCode as Types
import qualified Network.AWS.CodeCommit.Types.Unicode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the values for reactions to a comment. AWS CodeCommit supports a limited set of reactions.
--
-- /See:/ 'mkReactionValueFormats' smart constructor.
data ReactionValueFormats = ReactionValueFormats'
  { -- | The Emoji Version 1.0 graphic of the reaction. These graphics are interpreted slightly differently on different operating systems.
    emoji :: Core.Maybe Types.Emoji,
    -- | The emoji short code for the reaction. Short codes are interpreted slightly differently on different operating systems.
    shortCode :: Core.Maybe Types.ShortCode,
    -- | The Unicode codepoint for the reaction.
    unicode :: Core.Maybe Types.Unicode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReactionValueFormats' value with any optional fields omitted.
mkReactionValueFormats ::
  ReactionValueFormats
mkReactionValueFormats =
  ReactionValueFormats'
    { emoji = Core.Nothing,
      shortCode = Core.Nothing,
      unicode = Core.Nothing
    }

-- | The Emoji Version 1.0 graphic of the reaction. These graphics are interpreted slightly differently on different operating systems.
--
-- /Note:/ Consider using 'emoji' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvfEmoji :: Lens.Lens' ReactionValueFormats (Core.Maybe Types.Emoji)
rvfEmoji = Lens.field @"emoji"
{-# DEPRECATED rvfEmoji "Use generic-lens or generic-optics with 'emoji' instead." #-}

-- | The emoji short code for the reaction. Short codes are interpreted slightly differently on different operating systems.
--
-- /Note:/ Consider using 'shortCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvfShortCode :: Lens.Lens' ReactionValueFormats (Core.Maybe Types.ShortCode)
rvfShortCode = Lens.field @"shortCode"
{-# DEPRECATED rvfShortCode "Use generic-lens or generic-optics with 'shortCode' instead." #-}

-- | The Unicode codepoint for the reaction.
--
-- /Note:/ Consider using 'unicode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvfUnicode :: Lens.Lens' ReactionValueFormats (Core.Maybe Types.Unicode)
rvfUnicode = Lens.field @"unicode"
{-# DEPRECATED rvfUnicode "Use generic-lens or generic-optics with 'unicode' instead." #-}

instance Core.FromJSON ReactionValueFormats where
  parseJSON =
    Core.withObject "ReactionValueFormats" Core.$
      \x ->
        ReactionValueFormats'
          Core.<$> (x Core..:? "emoji")
          Core.<*> (x Core..:? "shortCode")
          Core.<*> (x Core..:? "unicode")

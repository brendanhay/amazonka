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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the values for reactions to a comment. AWS CodeCommit supports a limited set of reactions.
--
-- /See:/ 'mkReactionValueFormats' smart constructor.
data ReactionValueFormats = ReactionValueFormats'
  { -- | The Emoji Version 1.0 graphic of the reaction. These graphics are interpreted slightly differently on different operating systems.
    emoji :: Lude.Maybe Lude.Text,
    -- | The emoji short code for the reaction. Short codes are interpreted slightly differently on different operating systems.
    shortCode :: Lude.Maybe Lude.Text,
    -- | The Unicode codepoint for the reaction.
    unicode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReactionValueFormats' with the minimum fields required to make a request.
--
-- * 'emoji' - The Emoji Version 1.0 graphic of the reaction. These graphics are interpreted slightly differently on different operating systems.
-- * 'shortCode' - The emoji short code for the reaction. Short codes are interpreted slightly differently on different operating systems.
-- * 'unicode' - The Unicode codepoint for the reaction.
mkReactionValueFormats ::
  ReactionValueFormats
mkReactionValueFormats =
  ReactionValueFormats'
    { emoji = Lude.Nothing,
      shortCode = Lude.Nothing,
      unicode = Lude.Nothing
    }

-- | The Emoji Version 1.0 graphic of the reaction. These graphics are interpreted slightly differently on different operating systems.
--
-- /Note:/ Consider using 'emoji' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvfEmoji :: Lens.Lens' ReactionValueFormats (Lude.Maybe Lude.Text)
rvfEmoji = Lens.lens (emoji :: ReactionValueFormats -> Lude.Maybe Lude.Text) (\s a -> s {emoji = a} :: ReactionValueFormats)
{-# DEPRECATED rvfEmoji "Use generic-lens or generic-optics with 'emoji' instead." #-}

-- | The emoji short code for the reaction. Short codes are interpreted slightly differently on different operating systems.
--
-- /Note:/ Consider using 'shortCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvfShortCode :: Lens.Lens' ReactionValueFormats (Lude.Maybe Lude.Text)
rvfShortCode = Lens.lens (shortCode :: ReactionValueFormats -> Lude.Maybe Lude.Text) (\s a -> s {shortCode = a} :: ReactionValueFormats)
{-# DEPRECATED rvfShortCode "Use generic-lens or generic-optics with 'shortCode' instead." #-}

-- | The Unicode codepoint for the reaction.
--
-- /Note:/ Consider using 'unicode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvfUnicode :: Lens.Lens' ReactionValueFormats (Lude.Maybe Lude.Text)
rvfUnicode = Lens.lens (unicode :: ReactionValueFormats -> Lude.Maybe Lude.Text) (\s a -> s {unicode = a} :: ReactionValueFormats)
{-# DEPRECATED rvfUnicode "Use generic-lens or generic-optics with 'unicode' instead." #-}

instance Lude.FromJSON ReactionValueFormats where
  parseJSON =
    Lude.withObject
      "ReactionValueFormats"
      ( \x ->
          ReactionValueFormats'
            Lude.<$> (x Lude..:? "emoji")
            Lude.<*> (x Lude..:? "shortCode")
            Lude.<*> (x Lude..:? "unicode")
      )

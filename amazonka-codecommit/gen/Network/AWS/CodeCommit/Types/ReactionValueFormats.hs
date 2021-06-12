{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReactionValueFormats
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReactionValueFormats where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the values for reactions to a comment. AWS CodeCommit
-- supports a limited set of reactions.
--
-- /See:/ 'newReactionValueFormats' smart constructor.
data ReactionValueFormats = ReactionValueFormats'
  { -- | The Unicode codepoint for the reaction.
    unicode :: Core.Maybe Core.Text,
    -- | The emoji short code for the reaction. Short codes are interpreted
    -- slightly differently on different operating systems.
    shortCode :: Core.Maybe Core.Text,
    -- | The Emoji Version 1.0 graphic of the reaction. These graphics are
    -- interpreted slightly differently on different operating systems.
    emoji :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReactionValueFormats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unicode', 'reactionValueFormats_unicode' - The Unicode codepoint for the reaction.
--
-- 'shortCode', 'reactionValueFormats_shortCode' - The emoji short code for the reaction. Short codes are interpreted
-- slightly differently on different operating systems.
--
-- 'emoji', 'reactionValueFormats_emoji' - The Emoji Version 1.0 graphic of the reaction. These graphics are
-- interpreted slightly differently on different operating systems.
newReactionValueFormats ::
  ReactionValueFormats
newReactionValueFormats =
  ReactionValueFormats'
    { unicode = Core.Nothing,
      shortCode = Core.Nothing,
      emoji = Core.Nothing
    }

-- | The Unicode codepoint for the reaction.
reactionValueFormats_unicode :: Lens.Lens' ReactionValueFormats (Core.Maybe Core.Text)
reactionValueFormats_unicode = Lens.lens (\ReactionValueFormats' {unicode} -> unicode) (\s@ReactionValueFormats' {} a -> s {unicode = a} :: ReactionValueFormats)

-- | The emoji short code for the reaction. Short codes are interpreted
-- slightly differently on different operating systems.
reactionValueFormats_shortCode :: Lens.Lens' ReactionValueFormats (Core.Maybe Core.Text)
reactionValueFormats_shortCode = Lens.lens (\ReactionValueFormats' {shortCode} -> shortCode) (\s@ReactionValueFormats' {} a -> s {shortCode = a} :: ReactionValueFormats)

-- | The Emoji Version 1.0 graphic of the reaction. These graphics are
-- interpreted slightly differently on different operating systems.
reactionValueFormats_emoji :: Lens.Lens' ReactionValueFormats (Core.Maybe Core.Text)
reactionValueFormats_emoji = Lens.lens (\ReactionValueFormats' {emoji} -> emoji) (\s@ReactionValueFormats' {} a -> s {emoji = a} :: ReactionValueFormats)

instance Core.FromJSON ReactionValueFormats where
  parseJSON =
    Core.withObject
      "ReactionValueFormats"
      ( \x ->
          ReactionValueFormats'
            Core.<$> (x Core..:? "unicode")
            Core.<*> (x Core..:? "shortCode")
            Core.<*> (x Core..:? "emoji")
      )

instance Core.Hashable ReactionValueFormats

instance Core.NFData ReactionValueFormats

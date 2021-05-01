{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the values for reactions to a comment. AWS CodeCommit
-- supports a limited set of reactions.
--
-- /See:/ 'newReactionValueFormats' smart constructor.
data ReactionValueFormats = ReactionValueFormats'
  { -- | The Unicode codepoint for the reaction.
    unicode :: Prelude.Maybe Prelude.Text,
    -- | The emoji short code for the reaction. Short codes are interpreted
    -- slightly differently on different operating systems.
    shortCode :: Prelude.Maybe Prelude.Text,
    -- | The Emoji Version 1.0 graphic of the reaction. These graphics are
    -- interpreted slightly differently on different operating systems.
    emoji :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { unicode = Prelude.Nothing,
      shortCode = Prelude.Nothing,
      emoji = Prelude.Nothing
    }

-- | The Unicode codepoint for the reaction.
reactionValueFormats_unicode :: Lens.Lens' ReactionValueFormats (Prelude.Maybe Prelude.Text)
reactionValueFormats_unicode = Lens.lens (\ReactionValueFormats' {unicode} -> unicode) (\s@ReactionValueFormats' {} a -> s {unicode = a} :: ReactionValueFormats)

-- | The emoji short code for the reaction. Short codes are interpreted
-- slightly differently on different operating systems.
reactionValueFormats_shortCode :: Lens.Lens' ReactionValueFormats (Prelude.Maybe Prelude.Text)
reactionValueFormats_shortCode = Lens.lens (\ReactionValueFormats' {shortCode} -> shortCode) (\s@ReactionValueFormats' {} a -> s {shortCode = a} :: ReactionValueFormats)

-- | The Emoji Version 1.0 graphic of the reaction. These graphics are
-- interpreted slightly differently on different operating systems.
reactionValueFormats_emoji :: Lens.Lens' ReactionValueFormats (Prelude.Maybe Prelude.Text)
reactionValueFormats_emoji = Lens.lens (\ReactionValueFormats' {emoji} -> emoji) (\s@ReactionValueFormats' {} a -> s {emoji = a} :: ReactionValueFormats)

instance Prelude.FromJSON ReactionValueFormats where
  parseJSON =
    Prelude.withObject
      "ReactionValueFormats"
      ( \x ->
          ReactionValueFormats'
            Prelude.<$> (x Prelude..:? "unicode")
            Prelude.<*> (x Prelude..:? "shortCode")
            Prelude.<*> (x Prelude..:? "emoji")
      )

instance Prelude.Hashable ReactionValueFormats

instance Prelude.NFData ReactionValueFormats

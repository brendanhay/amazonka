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
-- Module      : Amazonka.CodeCommit.Types.ReactionValueFormats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ReactionValueFormats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the values for reactions to a comment. AWS CodeCommit
-- supports a limited set of reactions.
--
-- /See:/ 'newReactionValueFormats' smart constructor.
data ReactionValueFormats = ReactionValueFormats'
  { -- | The Emoji Version 1.0 graphic of the reaction. These graphics are
    -- interpreted slightly differently on different operating systems.
    emoji :: Prelude.Maybe Prelude.Text,
    -- | The emoji short code for the reaction. Short codes are interpreted
    -- slightly differently on different operating systems.
    shortCode :: Prelude.Maybe Prelude.Text,
    -- | The Unicode codepoint for the reaction.
    unicode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReactionValueFormats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emoji', 'reactionValueFormats_emoji' - The Emoji Version 1.0 graphic of the reaction. These graphics are
-- interpreted slightly differently on different operating systems.
--
-- 'shortCode', 'reactionValueFormats_shortCode' - The emoji short code for the reaction. Short codes are interpreted
-- slightly differently on different operating systems.
--
-- 'unicode', 'reactionValueFormats_unicode' - The Unicode codepoint for the reaction.
newReactionValueFormats ::
  ReactionValueFormats
newReactionValueFormats =
  ReactionValueFormats'
    { emoji = Prelude.Nothing,
      shortCode = Prelude.Nothing,
      unicode = Prelude.Nothing
    }

-- | The Emoji Version 1.0 graphic of the reaction. These graphics are
-- interpreted slightly differently on different operating systems.
reactionValueFormats_emoji :: Lens.Lens' ReactionValueFormats (Prelude.Maybe Prelude.Text)
reactionValueFormats_emoji = Lens.lens (\ReactionValueFormats' {emoji} -> emoji) (\s@ReactionValueFormats' {} a -> s {emoji = a} :: ReactionValueFormats)

-- | The emoji short code for the reaction. Short codes are interpreted
-- slightly differently on different operating systems.
reactionValueFormats_shortCode :: Lens.Lens' ReactionValueFormats (Prelude.Maybe Prelude.Text)
reactionValueFormats_shortCode = Lens.lens (\ReactionValueFormats' {shortCode} -> shortCode) (\s@ReactionValueFormats' {} a -> s {shortCode = a} :: ReactionValueFormats)

-- | The Unicode codepoint for the reaction.
reactionValueFormats_unicode :: Lens.Lens' ReactionValueFormats (Prelude.Maybe Prelude.Text)
reactionValueFormats_unicode = Lens.lens (\ReactionValueFormats' {unicode} -> unicode) (\s@ReactionValueFormats' {} a -> s {unicode = a} :: ReactionValueFormats)

instance Data.FromJSON ReactionValueFormats where
  parseJSON =
    Data.withObject
      "ReactionValueFormats"
      ( \x ->
          ReactionValueFormats'
            Prelude.<$> (x Data..:? "emoji")
            Prelude.<*> (x Data..:? "shortCode")
            Prelude.<*> (x Data..:? "unicode")
      )

instance Prelude.Hashable ReactionValueFormats where
  hashWithSalt _salt ReactionValueFormats' {..} =
    _salt
      `Prelude.hashWithSalt` emoji
      `Prelude.hashWithSalt` shortCode
      `Prelude.hashWithSalt` unicode

instance Prelude.NFData ReactionValueFormats where
  rnf ReactionValueFormats' {..} =
    Prelude.rnf emoji `Prelude.seq`
      Prelude.rnf shortCode `Prelude.seq`
        Prelude.rnf unicode

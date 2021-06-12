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
-- Module      : Network.AWS.Connect.Types.LexBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.LexBot where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configuration information of an Amazon Lex bot.
--
-- /See:/ 'newLexBot' smart constructor.
data LexBot = LexBot'
  { -- | The name of the Amazon Lex bot.
    name :: Core.Maybe Core.Text,
    -- | The Region that the Amazon Lex bot was created in.
    lexRegion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LexBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'lexBot_name' - The name of the Amazon Lex bot.
--
-- 'lexRegion', 'lexBot_lexRegion' - The Region that the Amazon Lex bot was created in.
newLexBot ::
  LexBot
newLexBot =
  LexBot'
    { name = Core.Nothing,
      lexRegion = Core.Nothing
    }

-- | The name of the Amazon Lex bot.
lexBot_name :: Lens.Lens' LexBot (Core.Maybe Core.Text)
lexBot_name = Lens.lens (\LexBot' {name} -> name) (\s@LexBot' {} a -> s {name = a} :: LexBot)

-- | The Region that the Amazon Lex bot was created in.
lexBot_lexRegion :: Lens.Lens' LexBot (Core.Maybe Core.Text)
lexBot_lexRegion = Lens.lens (\LexBot' {lexRegion} -> lexRegion) (\s@LexBot' {} a -> s {lexRegion = a} :: LexBot)

instance Core.FromJSON LexBot where
  parseJSON =
    Core.withObject
      "LexBot"
      ( \x ->
          LexBot'
            Core.<$> (x Core..:? "Name")
            Core.<*> (x Core..:? "LexRegion")
      )

instance Core.Hashable LexBot

instance Core.NFData LexBot

instance Core.ToJSON LexBot where
  toJSON LexBot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("LexRegion" Core..=) Core.<$> lexRegion
          ]
      )

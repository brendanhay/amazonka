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
-- Module      : Network.AWS.Connect.Types.LexBot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.LexBot where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information of an Amazon Lex bot.
--
-- /See:/ 'newLexBot' smart constructor.
data LexBot = LexBot'
  { -- | The name of the Amazon Lex bot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Region that the Amazon Lex bot was created in.
    lexRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { name = Prelude.Nothing,
      lexRegion = Prelude.Nothing
    }

-- | The name of the Amazon Lex bot.
lexBot_name :: Lens.Lens' LexBot (Prelude.Maybe Prelude.Text)
lexBot_name = Lens.lens (\LexBot' {name} -> name) (\s@LexBot' {} a -> s {name = a} :: LexBot)

-- | The Region that the Amazon Lex bot was created in.
lexBot_lexRegion :: Lens.Lens' LexBot (Prelude.Maybe Prelude.Text)
lexBot_lexRegion = Lens.lens (\LexBot' {lexRegion} -> lexRegion) (\s@LexBot' {} a -> s {lexRegion = a} :: LexBot)

instance Prelude.FromJSON LexBot where
  parseJSON =
    Prelude.withObject
      "LexBot"
      ( \x ->
          LexBot'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "LexRegion")
      )

instance Prelude.Hashable LexBot

instance Prelude.NFData LexBot

instance Prelude.ToJSON LexBot where
  toJSON LexBot' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("LexRegion" Prelude..=) Prelude.<$> lexRegion
          ]
      )

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
-- Module      : Amazonka.Connect.Types.LexBot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.LexBot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information of an Amazon Lex bot.
--
-- /See:/ 'newLexBot' smart constructor.
data LexBot = LexBot'
  { -- | The name of the Amazon Lex bot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the Amazon Lex bot was created.
    lexRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lexRegion', 'lexBot_lexRegion' - The Amazon Web Services Region where the Amazon Lex bot was created.
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

-- | The Amazon Web Services Region where the Amazon Lex bot was created.
lexBot_lexRegion :: Lens.Lens' LexBot (Prelude.Maybe Prelude.Text)
lexBot_lexRegion = Lens.lens (\LexBot' {lexRegion} -> lexRegion) (\s@LexBot' {} a -> s {lexRegion = a} :: LexBot)

instance Data.FromJSON LexBot where
  parseJSON =
    Data.withObject
      "LexBot"
      ( \x ->
          LexBot'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "LexRegion")
      )

instance Prelude.Hashable LexBot where
  hashWithSalt _salt LexBot' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lexRegion

instance Prelude.NFData LexBot where
  rnf LexBot' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lexRegion

instance Data.ToJSON LexBot where
  toJSON LexBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("LexRegion" Data..=) Prelude.<$> lexRegion
          ]
      )

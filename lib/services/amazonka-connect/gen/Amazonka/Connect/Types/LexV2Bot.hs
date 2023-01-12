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
-- Module      : Amazonka.Connect.Types.LexV2Bot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.LexV2Bot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information of an Amazon Lex V2 bot.
--
-- /See:/ 'newLexV2Bot' smart constructor.
data LexV2Bot = LexV2Bot'
  { -- | The Amazon Resource Name (ARN) of the Amazon Lex V2 bot.
    aliasArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LexV2Bot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasArn', 'lexV2Bot_aliasArn' - The Amazon Resource Name (ARN) of the Amazon Lex V2 bot.
newLexV2Bot ::
  LexV2Bot
newLexV2Bot = LexV2Bot' {aliasArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the Amazon Lex V2 bot.
lexV2Bot_aliasArn :: Lens.Lens' LexV2Bot (Prelude.Maybe Prelude.Text)
lexV2Bot_aliasArn = Lens.lens (\LexV2Bot' {aliasArn} -> aliasArn) (\s@LexV2Bot' {} a -> s {aliasArn = a} :: LexV2Bot)

instance Data.FromJSON LexV2Bot where
  parseJSON =
    Data.withObject
      "LexV2Bot"
      ( \x ->
          LexV2Bot' Prelude.<$> (x Data..:? "AliasArn")
      )

instance Prelude.Hashable LexV2Bot where
  hashWithSalt _salt LexV2Bot' {..} =
    _salt `Prelude.hashWithSalt` aliasArn

instance Prelude.NFData LexV2Bot where
  rnf LexV2Bot' {..} = Prelude.rnf aliasArn

instance Data.ToJSON LexV2Bot where
  toJSON LexV2Bot' {..} =
    Data.object
      ( Prelude.catMaybes
          [("AliasArn" Data..=) Prelude.<$> aliasArn]
      )

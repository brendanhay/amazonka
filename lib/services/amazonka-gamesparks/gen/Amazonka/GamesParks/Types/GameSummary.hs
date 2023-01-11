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
-- Module      : Amazonka.GamesParks.Types.GameSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.GameSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types.GameState
import qualified Amazonka.Prelude as Prelude

-- | The summary of the properties of a game.
--
-- /See:/ 'newGameSummary' smart constructor.
data GameSummary = GameSummary'
  { -- | The description of the game.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the game.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the game.
    state :: Prelude.Maybe GameState,
    -- | The tags associated with the game.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'gameSummary_description' - The description of the game.
--
-- 'name', 'gameSummary_name' - The name of the game.
--
-- 'state', 'gameSummary_state' - The state of the game.
--
-- 'tags', 'gameSummary_tags' - The tags associated with the game.
newGameSummary ::
  GameSummary
newGameSummary =
  GameSummary'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The description of the game.
gameSummary_description :: Lens.Lens' GameSummary (Prelude.Maybe Prelude.Text)
gameSummary_description = Lens.lens (\GameSummary' {description} -> description) (\s@GameSummary' {} a -> s {description = a} :: GameSummary)

-- | The name of the game.
gameSummary_name :: Lens.Lens' GameSummary (Prelude.Maybe Prelude.Text)
gameSummary_name = Lens.lens (\GameSummary' {name} -> name) (\s@GameSummary' {} a -> s {name = a} :: GameSummary)

-- | The state of the game.
gameSummary_state :: Lens.Lens' GameSummary (Prelude.Maybe GameState)
gameSummary_state = Lens.lens (\GameSummary' {state} -> state) (\s@GameSummary' {} a -> s {state = a} :: GameSummary)

-- | The tags associated with the game.
gameSummary_tags :: Lens.Lens' GameSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
gameSummary_tags = Lens.lens (\GameSummary' {tags} -> tags) (\s@GameSummary' {} a -> s {tags = a} :: GameSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GameSummary where
  parseJSON =
    Data.withObject
      "GameSummary"
      ( \x ->
          GameSummary'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable GameSummary where
  hashWithSalt _salt GameSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData GameSummary where
  rnf GameSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags

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
-- Module      : Amazonka.GamesParks.Types.GameDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.GameDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types.GameState
import qualified Amazonka.Prelude as Prelude

-- | Details about a game.
--
-- /See:/ 'newGameDetails' smart constructor.
data GameDetails = GameDetails'
  { -- | The tags associated with the game.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the game.
    name :: Prelude.Maybe Prelude.Text,
    -- | Determines if the game can be deleted.
    enableTerminationProtection :: Prelude.Maybe Prelude.Bool,
    -- | The date when the game was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of this game.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the game.
    state :: Prelude.Maybe GameState,
    -- | The description of the game.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date when the game was last modified.
    lastUpdated :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'gameDetails_tags' - The tags associated with the game.
--
-- 'name', 'gameDetails_name' - The name of the game.
--
-- 'enableTerminationProtection', 'gameDetails_enableTerminationProtection' - Determines if the game can be deleted.
--
-- 'created', 'gameDetails_created' - The date when the game was created.
--
-- 'arn', 'gameDetails_arn' - The Amazon Resource Name (ARN) of this game.
--
-- 'state', 'gameDetails_state' - The state of the game.
--
-- 'description', 'gameDetails_description' - The description of the game.
--
-- 'lastUpdated', 'gameDetails_lastUpdated' - The date when the game was last modified.
newGameDetails ::
  GameDetails
newGameDetails =
  GameDetails'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      enableTerminationProtection = Prelude.Nothing,
      created = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdated = Prelude.Nothing
    }

-- | The tags associated with the game.
gameDetails_tags :: Lens.Lens' GameDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
gameDetails_tags = Lens.lens (\GameDetails' {tags} -> tags) (\s@GameDetails' {} a -> s {tags = a} :: GameDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the game.
gameDetails_name :: Lens.Lens' GameDetails (Prelude.Maybe Prelude.Text)
gameDetails_name = Lens.lens (\GameDetails' {name} -> name) (\s@GameDetails' {} a -> s {name = a} :: GameDetails)

-- | Determines if the game can be deleted.
gameDetails_enableTerminationProtection :: Lens.Lens' GameDetails (Prelude.Maybe Prelude.Bool)
gameDetails_enableTerminationProtection = Lens.lens (\GameDetails' {enableTerminationProtection} -> enableTerminationProtection) (\s@GameDetails' {} a -> s {enableTerminationProtection = a} :: GameDetails)

-- | The date when the game was created.
gameDetails_created :: Lens.Lens' GameDetails (Prelude.Maybe Prelude.UTCTime)
gameDetails_created = Lens.lens (\GameDetails' {created} -> created) (\s@GameDetails' {} a -> s {created = a} :: GameDetails) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of this game.
gameDetails_arn :: Lens.Lens' GameDetails (Prelude.Maybe Prelude.Text)
gameDetails_arn = Lens.lens (\GameDetails' {arn} -> arn) (\s@GameDetails' {} a -> s {arn = a} :: GameDetails)

-- | The state of the game.
gameDetails_state :: Lens.Lens' GameDetails (Prelude.Maybe GameState)
gameDetails_state = Lens.lens (\GameDetails' {state} -> state) (\s@GameDetails' {} a -> s {state = a} :: GameDetails)

-- | The description of the game.
gameDetails_description :: Lens.Lens' GameDetails (Prelude.Maybe Prelude.Text)
gameDetails_description = Lens.lens (\GameDetails' {description} -> description) (\s@GameDetails' {} a -> s {description = a} :: GameDetails)

-- | The date when the game was last modified.
gameDetails_lastUpdated :: Lens.Lens' GameDetails (Prelude.Maybe Prelude.UTCTime)
gameDetails_lastUpdated = Lens.lens (\GameDetails' {lastUpdated} -> lastUpdated) (\s@GameDetails' {} a -> s {lastUpdated = a} :: GameDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON GameDetails where
  parseJSON =
    Data.withObject
      "GameDetails"
      ( \x ->
          GameDetails'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "EnableTerminationProtection")
            Prelude.<*> (x Data..:? "Created")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastUpdated")
      )

instance Prelude.Hashable GameDetails where
  hashWithSalt _salt GameDetails' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` enableTerminationProtection
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdated

instance Prelude.NFData GameDetails where
  rnf GameDetails' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf enableTerminationProtection
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdated

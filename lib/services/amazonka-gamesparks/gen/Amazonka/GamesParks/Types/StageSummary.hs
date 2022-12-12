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
-- Module      : Amazonka.GamesParks.Types.StageSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.StageSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types.StageState
import qualified Amazonka.Prelude as Prelude

-- | The summary of the properties of a stage.
--
-- /See:/ 'newStageSummary' smart constructor.
data StageSummary = StageSummary'
  { -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The game key associated with the stage.
    --
    -- The game key is a unique identifier that the game client uses to connect
    -- to the GameSparks backend.
    gameKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the stage.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the stage.
    state :: Prelude.Maybe StageState,
    -- | The tags associated with the stage.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'stageSummary_description' - The description of the stage.
--
-- 'gameKey', 'stageSummary_gameKey' - The game key associated with the stage.
--
-- The game key is a unique identifier that the game client uses to connect
-- to the GameSparks backend.
--
-- 'name', 'stageSummary_name' - The name of the stage.
--
-- 'state', 'stageSummary_state' - The state of the stage.
--
-- 'tags', 'stageSummary_tags' - The tags associated with the stage.
newStageSummary ::
  StageSummary
newStageSummary =
  StageSummary'
    { description = Prelude.Nothing,
      gameKey = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The description of the stage.
stageSummary_description :: Lens.Lens' StageSummary (Prelude.Maybe Prelude.Text)
stageSummary_description = Lens.lens (\StageSummary' {description} -> description) (\s@StageSummary' {} a -> s {description = a} :: StageSummary)

-- | The game key associated with the stage.
--
-- The game key is a unique identifier that the game client uses to connect
-- to the GameSparks backend.
stageSummary_gameKey :: Lens.Lens' StageSummary (Prelude.Maybe Prelude.Text)
stageSummary_gameKey = Lens.lens (\StageSummary' {gameKey} -> gameKey) (\s@StageSummary' {} a -> s {gameKey = a} :: StageSummary)

-- | The name of the stage.
stageSummary_name :: Lens.Lens' StageSummary (Prelude.Maybe Prelude.Text)
stageSummary_name = Lens.lens (\StageSummary' {name} -> name) (\s@StageSummary' {} a -> s {name = a} :: StageSummary)

-- | The state of the stage.
stageSummary_state :: Lens.Lens' StageSummary (Prelude.Maybe StageState)
stageSummary_state = Lens.lens (\StageSummary' {state} -> state) (\s@StageSummary' {} a -> s {state = a} :: StageSummary)

-- | The tags associated with the stage.
stageSummary_tags :: Lens.Lens' StageSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stageSummary_tags = Lens.lens (\StageSummary' {tags} -> tags) (\s@StageSummary' {} a -> s {tags = a} :: StageSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StageSummary where
  parseJSON =
    Data.withObject
      "StageSummary"
      ( \x ->
          StageSummary'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "GameKey")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StageSummary where
  hashWithSalt _salt StageSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` gameKey
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags

instance Prelude.NFData StageSummary where
  rnf StageSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf gameKey
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags

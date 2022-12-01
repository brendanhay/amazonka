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
-- Module      : Amazonka.GamesParks.Types.StageDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.StageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types.StageState
import qualified Amazonka.Prelude as Prelude

-- | Properties that provide details of a stage.
--
-- /See:/ 'newStageDetails' smart constructor.
data StageDetails = StageDetails'
  { -- | The tags associated with the stage.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the stage.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon CloudWatch log group for game runtimes deployed to the stage.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the stage was created.
    created :: Prelude.Maybe Core.POSIX,
    -- | The game key associated with the stage.
    --
    -- The game key is a unique identifier that the game client uses to connect
    -- to the GameSparks backend.
    gameKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stage.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the stage.
    state :: Prelude.Maybe StageState,
    -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the stage was last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the role used to run the game runtimes
    -- deployed to the stage.
    role' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'stageDetails_tags' - The tags associated with the stage.
--
-- 'name', 'stageDetails_name' - The name of the stage.
--
-- 'logGroup', 'stageDetails_logGroup' - The Amazon CloudWatch log group for game runtimes deployed to the stage.
--
-- 'created', 'stageDetails_created' - The timestamp of when the stage was created.
--
-- 'gameKey', 'stageDetails_gameKey' - The game key associated with the stage.
--
-- The game key is a unique identifier that the game client uses to connect
-- to the GameSparks backend.
--
-- 'arn', 'stageDetails_arn' - The Amazon Resource Name (ARN) of the stage.
--
-- 'state', 'stageDetails_state' - The state of the stage.
--
-- 'description', 'stageDetails_description' - The description of the stage.
--
-- 'lastUpdated', 'stageDetails_lastUpdated' - The timestamp of when the stage was last updated.
--
-- 'role'', 'stageDetails_role' - The Amazon Resource Name (ARN) of the role used to run the game runtimes
-- deployed to the stage.
newStageDetails ::
  StageDetails
newStageDetails =
  StageDetails'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      logGroup = Prelude.Nothing,
      created = Prelude.Nothing,
      gameKey = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      role' = Prelude.Nothing
    }

-- | The tags associated with the stage.
stageDetails_tags :: Lens.Lens' StageDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
stageDetails_tags = Lens.lens (\StageDetails' {tags} -> tags) (\s@StageDetails' {} a -> s {tags = a} :: StageDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stage.
stageDetails_name :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.Text)
stageDetails_name = Lens.lens (\StageDetails' {name} -> name) (\s@StageDetails' {} a -> s {name = a} :: StageDetails)

-- | The Amazon CloudWatch log group for game runtimes deployed to the stage.
stageDetails_logGroup :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.Text)
stageDetails_logGroup = Lens.lens (\StageDetails' {logGroup} -> logGroup) (\s@StageDetails' {} a -> s {logGroup = a} :: StageDetails)

-- | The timestamp of when the stage was created.
stageDetails_created :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.UTCTime)
stageDetails_created = Lens.lens (\StageDetails' {created} -> created) (\s@StageDetails' {} a -> s {created = a} :: StageDetails) Prelude.. Lens.mapping Core._Time

-- | The game key associated with the stage.
--
-- The game key is a unique identifier that the game client uses to connect
-- to the GameSparks backend.
stageDetails_gameKey :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.Text)
stageDetails_gameKey = Lens.lens (\StageDetails' {gameKey} -> gameKey) (\s@StageDetails' {} a -> s {gameKey = a} :: StageDetails)

-- | The Amazon Resource Name (ARN) of the stage.
stageDetails_arn :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.Text)
stageDetails_arn = Lens.lens (\StageDetails' {arn} -> arn) (\s@StageDetails' {} a -> s {arn = a} :: StageDetails)

-- | The state of the stage.
stageDetails_state :: Lens.Lens' StageDetails (Prelude.Maybe StageState)
stageDetails_state = Lens.lens (\StageDetails' {state} -> state) (\s@StageDetails' {} a -> s {state = a} :: StageDetails)

-- | The description of the stage.
stageDetails_description :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.Text)
stageDetails_description = Lens.lens (\StageDetails' {description} -> description) (\s@StageDetails' {} a -> s {description = a} :: StageDetails)

-- | The timestamp of when the stage was last updated.
stageDetails_lastUpdated :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.UTCTime)
stageDetails_lastUpdated = Lens.lens (\StageDetails' {lastUpdated} -> lastUpdated) (\s@StageDetails' {} a -> s {lastUpdated = a} :: StageDetails) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the role used to run the game runtimes
-- deployed to the stage.
stageDetails_role :: Lens.Lens' StageDetails (Prelude.Maybe Prelude.Text)
stageDetails_role = Lens.lens (\StageDetails' {role'} -> role') (\s@StageDetails' {} a -> s {role' = a} :: StageDetails)

instance Core.FromJSON StageDetails where
  parseJSON =
    Core.withObject
      "StageDetails"
      ( \x ->
          StageDetails'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LogGroup")
            Prelude.<*> (x Core..:? "Created")
            Prelude.<*> (x Core..:? "GameKey")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LastUpdated")
            Prelude.<*> (x Core..:? "Role")
      )

instance Prelude.Hashable StageDetails where
  hashWithSalt _salt StageDetails' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` logGroup
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` gameKey
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` role'

instance Prelude.NFData StageDetails where
  rnf StageDetails' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf logGroup
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf gameKey
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf role'

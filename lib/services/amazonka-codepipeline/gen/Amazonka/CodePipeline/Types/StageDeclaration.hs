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
-- Module      : Amazonka.CodePipeline.Types.StageDeclaration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.StageDeclaration where

import Amazonka.CodePipeline.Types.ActionDeclaration
import Amazonka.CodePipeline.Types.BlockerDeclaration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about a stage and its definition.
--
-- /See:/ 'newStageDeclaration' smart constructor.
data StageDeclaration = StageDeclaration'
  { -- | Reserved for future use.
    blockers :: Prelude.Maybe [BlockerDeclaration],
    -- | The name of the stage.
    name :: Prelude.Text,
    -- | The actions included in a stage.
    actions :: [ActionDeclaration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StageDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockers', 'stageDeclaration_blockers' - Reserved for future use.
--
-- 'name', 'stageDeclaration_name' - The name of the stage.
--
-- 'actions', 'stageDeclaration_actions' - The actions included in a stage.
newStageDeclaration ::
  -- | 'name'
  Prelude.Text ->
  StageDeclaration
newStageDeclaration pName_ =
  StageDeclaration'
    { blockers = Prelude.Nothing,
      name = pName_,
      actions = Prelude.mempty
    }

-- | Reserved for future use.
stageDeclaration_blockers :: Lens.Lens' StageDeclaration (Prelude.Maybe [BlockerDeclaration])
stageDeclaration_blockers = Lens.lens (\StageDeclaration' {blockers} -> blockers) (\s@StageDeclaration' {} a -> s {blockers = a} :: StageDeclaration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the stage.
stageDeclaration_name :: Lens.Lens' StageDeclaration Prelude.Text
stageDeclaration_name = Lens.lens (\StageDeclaration' {name} -> name) (\s@StageDeclaration' {} a -> s {name = a} :: StageDeclaration)

-- | The actions included in a stage.
stageDeclaration_actions :: Lens.Lens' StageDeclaration [ActionDeclaration]
stageDeclaration_actions = Lens.lens (\StageDeclaration' {actions} -> actions) (\s@StageDeclaration' {} a -> s {actions = a} :: StageDeclaration) Prelude.. Lens.coerced

instance Data.FromJSON StageDeclaration where
  parseJSON =
    Data.withObject
      "StageDeclaration"
      ( \x ->
          StageDeclaration'
            Prelude.<$> (x Data..:? "blockers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..:? "actions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StageDeclaration where
  hashWithSalt _salt StageDeclaration' {..} =
    _salt `Prelude.hashWithSalt` blockers
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` actions

instance Prelude.NFData StageDeclaration where
  rnf StageDeclaration' {..} =
    Prelude.rnf blockers
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf actions

instance Data.ToJSON StageDeclaration where
  toJSON StageDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("blockers" Data..=) Prelude.<$> blockers,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("actions" Data..= actions)
          ]
      )

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
-- Module      : Network.AWS.CodePipeline.Types.StageDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageDeclaration where

import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about a stage and its definition.
--
-- /See:/ 'newStageDeclaration' smart constructor.
data StageDeclaration = StageDeclaration'
  { -- | Reserved for future use.
    blockers :: Core.Maybe [BlockerDeclaration],
    -- | The name of the stage.
    name :: Core.Text,
    -- | The actions included in a stage.
    actions :: [ActionDeclaration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StageDeclaration
newStageDeclaration pName_ =
  StageDeclaration'
    { blockers = Core.Nothing,
      name = pName_,
      actions = Core.mempty
    }

-- | Reserved for future use.
stageDeclaration_blockers :: Lens.Lens' StageDeclaration (Core.Maybe [BlockerDeclaration])
stageDeclaration_blockers = Lens.lens (\StageDeclaration' {blockers} -> blockers) (\s@StageDeclaration' {} a -> s {blockers = a} :: StageDeclaration) Core.. Lens.mapping Lens._Coerce

-- | The name of the stage.
stageDeclaration_name :: Lens.Lens' StageDeclaration Core.Text
stageDeclaration_name = Lens.lens (\StageDeclaration' {name} -> name) (\s@StageDeclaration' {} a -> s {name = a} :: StageDeclaration)

-- | The actions included in a stage.
stageDeclaration_actions :: Lens.Lens' StageDeclaration [ActionDeclaration]
stageDeclaration_actions = Lens.lens (\StageDeclaration' {actions} -> actions) (\s@StageDeclaration' {} a -> s {actions = a} :: StageDeclaration) Core.. Lens._Coerce

instance Core.FromJSON StageDeclaration where
  parseJSON =
    Core.withObject
      "StageDeclaration"
      ( \x ->
          StageDeclaration'
            Core.<$> (x Core..:? "blockers" Core..!= Core.mempty)
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..:? "actions" Core..!= Core.mempty)
      )

instance Core.Hashable StageDeclaration

instance Core.NFData StageDeclaration

instance Core.ToJSON StageDeclaration where
  toJSON StageDeclaration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("blockers" Core..=) Core.<$> blockers,
            Core.Just ("name" Core..= name),
            Core.Just ("actions" Core..= actions)
          ]
      )

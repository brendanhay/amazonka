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
-- Module      : Network.AWS.CodePipeline.Types.StageDeclaration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageDeclaration where

import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
stageDeclaration_blockers = Lens.lens (\StageDeclaration' {blockers} -> blockers) (\s@StageDeclaration' {} a -> s {blockers = a} :: StageDeclaration) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the stage.
stageDeclaration_name :: Lens.Lens' StageDeclaration Prelude.Text
stageDeclaration_name = Lens.lens (\StageDeclaration' {name} -> name) (\s@StageDeclaration' {} a -> s {name = a} :: StageDeclaration)

-- | The actions included in a stage.
stageDeclaration_actions :: Lens.Lens' StageDeclaration [ActionDeclaration]
stageDeclaration_actions = Lens.lens (\StageDeclaration' {actions} -> actions) (\s@StageDeclaration' {} a -> s {actions = a} :: StageDeclaration) Prelude.. Prelude._Coerce

instance Prelude.FromJSON StageDeclaration where
  parseJSON =
    Prelude.withObject
      "StageDeclaration"
      ( \x ->
          StageDeclaration'
            Prelude.<$> (x Prelude..:? "blockers" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..:? "actions" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable StageDeclaration

instance Prelude.NFData StageDeclaration

instance Prelude.ToJSON StageDeclaration where
  toJSON StageDeclaration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("blockers" Prelude..=) Prelude.<$> blockers,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("actions" Prelude..= actions)
          ]
      )

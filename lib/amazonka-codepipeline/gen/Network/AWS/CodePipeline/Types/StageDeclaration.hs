{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageDeclaration
  ( StageDeclaration (..),

    -- * Smart constructor
    mkStageDeclaration,

    -- * Lenses
    sdActions,
    sdBlockers,
    sdName,
  )
where

import Network.AWS.CodePipeline.Types.ActionDeclaration
import Network.AWS.CodePipeline.Types.BlockerDeclaration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about a stage and its definition.
--
-- /See:/ 'mkStageDeclaration' smart constructor.
data StageDeclaration = StageDeclaration'
  { -- | The actions included in a stage.
    actions :: [ActionDeclaration],
    -- | Reserved for future use.
    blockers :: Lude.Maybe [BlockerDeclaration],
    -- | The name of the stage.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StageDeclaration' with the minimum fields required to make a request.
--
-- * 'actions' - The actions included in a stage.
-- * 'blockers' - Reserved for future use.
-- * 'name' - The name of the stage.
mkStageDeclaration ::
  -- | 'name'
  Lude.Text ->
  StageDeclaration
mkStageDeclaration pName_ =
  StageDeclaration'
    { actions = Lude.mempty,
      blockers = Lude.Nothing,
      name = pName_
    }

-- | The actions included in a stage.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdActions :: Lens.Lens' StageDeclaration [ActionDeclaration]
sdActions = Lens.lens (actions :: StageDeclaration -> [ActionDeclaration]) (\s a -> s {actions = a} :: StageDeclaration)
{-# DEPRECATED sdActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'blockers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBlockers :: Lens.Lens' StageDeclaration (Lude.Maybe [BlockerDeclaration])
sdBlockers = Lens.lens (blockers :: StageDeclaration -> Lude.Maybe [BlockerDeclaration]) (\s a -> s {blockers = a} :: StageDeclaration)
{-# DEPRECATED sdBlockers "Use generic-lens or generic-optics with 'blockers' instead." #-}

-- | The name of the stage.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdName :: Lens.Lens' StageDeclaration Lude.Text
sdName = Lens.lens (name :: StageDeclaration -> Lude.Text) (\s a -> s {name = a} :: StageDeclaration)
{-# DEPRECATED sdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON StageDeclaration where
  parseJSON =
    Lude.withObject
      "StageDeclaration"
      ( \x ->
          StageDeclaration'
            Lude.<$> (x Lude..:? "actions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "blockers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON StageDeclaration where
  toJSON StageDeclaration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("actions" Lude..= actions),
            ("blockers" Lude..=) Lude.<$> blockers,
            Lude.Just ("name" Lude..= name)
          ]
      )

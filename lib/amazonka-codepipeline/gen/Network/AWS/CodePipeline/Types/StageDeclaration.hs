{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageDeclaration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.StageDeclaration
  ( StageDeclaration (..)
  -- * Smart constructor
  , mkStageDeclaration
  -- * Lenses
  , sdName
  , sdActions
  , sdBlockers
  ) where

import qualified Network.AWS.CodePipeline.Types.ActionDeclaration as Types
import qualified Network.AWS.CodePipeline.Types.BlockerDeclaration as Types
import qualified Network.AWS.CodePipeline.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about a stage and its definition.
--
-- /See:/ 'mkStageDeclaration' smart constructor.
data StageDeclaration = StageDeclaration'
  { name :: Types.Name
    -- ^ The name of the stage.
  , actions :: [Types.ActionDeclaration]
    -- ^ The actions included in a stage.
  , blockers :: Core.Maybe [Types.BlockerDeclaration]
    -- ^ Reserved for future use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StageDeclaration' value with any optional fields omitted.
mkStageDeclaration
    :: Types.Name -- ^ 'name'
    -> StageDeclaration
mkStageDeclaration name
  = StageDeclaration'{name, actions = Core.mempty,
                      blockers = Core.Nothing}

-- | The name of the stage.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdName :: Lens.Lens' StageDeclaration Types.Name
sdName = Lens.field @"name"
{-# INLINEABLE sdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The actions included in a stage.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdActions :: Lens.Lens' StageDeclaration [Types.ActionDeclaration]
sdActions = Lens.field @"actions"
{-# INLINEABLE sdActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'blockers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBlockers :: Lens.Lens' StageDeclaration (Core.Maybe [Types.BlockerDeclaration])
sdBlockers = Lens.field @"blockers"
{-# INLINEABLE sdBlockers #-}
{-# DEPRECATED blockers "Use generic-lens or generic-optics with 'blockers' instead"  #-}

instance Core.FromJSON StageDeclaration where
        toJSON StageDeclaration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("actions" Core..= actions),
                  ("blockers" Core..=) Core.<$> blockers])

instance Core.FromJSON StageDeclaration where
        parseJSON
          = Core.withObject "StageDeclaration" Core.$
              \ x ->
                StageDeclaration' Core.<$>
                  (x Core..: "name") Core.<*>
                    x Core..:? "actions" Core..!= Core.mempty
                    Core.<*> x Core..:? "blockers"

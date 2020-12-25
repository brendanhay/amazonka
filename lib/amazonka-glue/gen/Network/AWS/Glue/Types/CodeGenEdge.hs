{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenEdge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenEdge
  ( CodeGenEdge (..),

    -- * Smart constructor
    mkCodeGenEdge,

    -- * Lenses
    cgeSource,
    cgeTarget,
    cgeTargetParameter,
  )
where

import qualified Network.AWS.Glue.Types.CodeGenArgName as Types
import qualified Network.AWS.Glue.Types.Source as Types
import qualified Network.AWS.Glue.Types.Target as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a directional edge in a directed acyclic graph (DAG).
--
-- /See:/ 'mkCodeGenEdge' smart constructor.
data CodeGenEdge = CodeGenEdge'
  { -- | The ID of the node at which the edge starts.
    source :: Types.Source,
    -- | The ID of the node at which the edge ends.
    target :: Types.Target,
    -- | The target of the edge.
    targetParameter :: Core.Maybe Types.CodeGenArgName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeGenEdge' value with any optional fields omitted.
mkCodeGenEdge ::
  -- | 'source'
  Types.Source ->
  -- | 'target'
  Types.Target ->
  CodeGenEdge
mkCodeGenEdge source target =
  CodeGenEdge' {source, target, targetParameter = Core.Nothing}

-- | The ID of the node at which the edge starts.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgeSource :: Lens.Lens' CodeGenEdge Types.Source
cgeSource = Lens.field @"source"
{-# DEPRECATED cgeSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The ID of the node at which the edge ends.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgeTarget :: Lens.Lens' CodeGenEdge Types.Target
cgeTarget = Lens.field @"target"
{-# DEPRECATED cgeTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | The target of the edge.
--
-- /Note:/ Consider using 'targetParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgeTargetParameter :: Lens.Lens' CodeGenEdge (Core.Maybe Types.CodeGenArgName)
cgeTargetParameter = Lens.field @"targetParameter"
{-# DEPRECATED cgeTargetParameter "Use generic-lens or generic-optics with 'targetParameter' instead." #-}

instance Core.FromJSON CodeGenEdge where
  toJSON CodeGenEdge {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Source" Core..= source),
            Core.Just ("Target" Core..= target),
            ("TargetParameter" Core..=) Core.<$> targetParameter
          ]
      )

instance Core.FromJSON CodeGenEdge where
  parseJSON =
    Core.withObject "CodeGenEdge" Core.$
      \x ->
        CodeGenEdge'
          Core.<$> (x Core..: "Source")
          Core.<*> (x Core..: "Target")
          Core.<*> (x Core..:? "TargetParameter")

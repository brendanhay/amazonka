{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenNode
  ( CodeGenNode (..),

    -- * Smart constructor
    mkCodeGenNode,

    -- * Lenses
    cgnId,
    cgnNodeType,
    cgnArgs,
    cgnLineNumber,
  )
where

import qualified Network.AWS.Glue.Types.CodeGenIdentifier as Types
import qualified Network.AWS.Glue.Types.CodeGenNodeArg as Types
import qualified Network.AWS.Glue.Types.CodeGenNodeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a node in a directed acyclic graph (DAG)
--
-- /See:/ 'mkCodeGenNode' smart constructor.
data CodeGenNode = CodeGenNode'
  { -- | A node identifier that is unique within the node's graph.
    id :: Types.CodeGenIdentifier,
    -- | The type of node that this is.
    nodeType :: Types.CodeGenNodeType,
    -- | Properties of the node, in the form of name-value pairs.
    args :: [Types.CodeGenNodeArg],
    -- | The line number of the node.
    lineNumber :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeGenNode' value with any optional fields omitted.
mkCodeGenNode ::
  -- | 'id'
  Types.CodeGenIdentifier ->
  -- | 'nodeType'
  Types.CodeGenNodeType ->
  CodeGenNode
mkCodeGenNode id nodeType =
  CodeGenNode'
    { id,
      nodeType,
      args = Core.mempty,
      lineNumber = Core.Nothing
    }

-- | A node identifier that is unique within the node's graph.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnId :: Lens.Lens' CodeGenNode Types.CodeGenIdentifier
cgnId = Lens.field @"id"
{-# DEPRECATED cgnId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of node that this is.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnNodeType :: Lens.Lens' CodeGenNode Types.CodeGenNodeType
cgnNodeType = Lens.field @"nodeType"
{-# DEPRECATED cgnNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | Properties of the node, in the form of name-value pairs.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnArgs :: Lens.Lens' CodeGenNode [Types.CodeGenNodeArg]
cgnArgs = Lens.field @"args"
{-# DEPRECATED cgnArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The line number of the node.
--
-- /Note:/ Consider using 'lineNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnLineNumber :: Lens.Lens' CodeGenNode (Core.Maybe Core.Int)
cgnLineNumber = Lens.field @"lineNumber"
{-# DEPRECATED cgnLineNumber "Use generic-lens or generic-optics with 'lineNumber' instead." #-}

instance Core.FromJSON CodeGenNode where
  toJSON CodeGenNode {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Id" Core..= id),
            Core.Just ("NodeType" Core..= nodeType),
            Core.Just ("Args" Core..= args),
            ("LineNumber" Core..=) Core.<$> lineNumber
          ]
      )

instance Core.FromJSON CodeGenNode where
  parseJSON =
    Core.withObject "CodeGenNode" Core.$
      \x ->
        CodeGenNode'
          Core.<$> (x Core..: "Id")
          Core.<*> (x Core..: "NodeType")
          Core.<*> (x Core..:? "Args" Core..!= Core.mempty)
          Core.<*> (x Core..:? "LineNumber")

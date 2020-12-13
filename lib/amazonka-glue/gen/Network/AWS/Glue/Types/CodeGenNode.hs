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
    cgnArgs,
    cgnLineNumber,
    cgnId,
    cgnNodeType,
  )
where

import Network.AWS.Glue.Types.CodeGenNodeArg
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a node in a directed acyclic graph (DAG)
--
-- /See:/ 'mkCodeGenNode' smart constructor.
data CodeGenNode = CodeGenNode'
  { -- | Properties of the node, in the form of name-value pairs.
    args :: [CodeGenNodeArg],
    -- | The line number of the node.
    lineNumber :: Lude.Maybe Lude.Int,
    -- | A node identifier that is unique within the node's graph.
    id :: Lude.Text,
    -- | The type of node that this is.
    nodeType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeGenNode' with the minimum fields required to make a request.
--
-- * 'args' - Properties of the node, in the form of name-value pairs.
-- * 'lineNumber' - The line number of the node.
-- * 'id' - A node identifier that is unique within the node's graph.
-- * 'nodeType' - The type of node that this is.
mkCodeGenNode ::
  -- | 'id'
  Lude.Text ->
  -- | 'nodeType'
  Lude.Text ->
  CodeGenNode
mkCodeGenNode pId_ pNodeType_ =
  CodeGenNode'
    { args = Lude.mempty,
      lineNumber = Lude.Nothing,
      id = pId_,
      nodeType = pNodeType_
    }

-- | Properties of the node, in the form of name-value pairs.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnArgs :: Lens.Lens' CodeGenNode [CodeGenNodeArg]
cgnArgs = Lens.lens (args :: CodeGenNode -> [CodeGenNodeArg]) (\s a -> s {args = a} :: CodeGenNode)
{-# DEPRECATED cgnArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The line number of the node.
--
-- /Note:/ Consider using 'lineNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnLineNumber :: Lens.Lens' CodeGenNode (Lude.Maybe Lude.Int)
cgnLineNumber = Lens.lens (lineNumber :: CodeGenNode -> Lude.Maybe Lude.Int) (\s a -> s {lineNumber = a} :: CodeGenNode)
{-# DEPRECATED cgnLineNumber "Use generic-lens or generic-optics with 'lineNumber' instead." #-}

-- | A node identifier that is unique within the node's graph.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnId :: Lens.Lens' CodeGenNode Lude.Text
cgnId = Lens.lens (id :: CodeGenNode -> Lude.Text) (\s a -> s {id = a} :: CodeGenNode)
{-# DEPRECATED cgnId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of node that this is.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnNodeType :: Lens.Lens' CodeGenNode Lude.Text
cgnNodeType = Lens.lens (nodeType :: CodeGenNode -> Lude.Text) (\s a -> s {nodeType = a} :: CodeGenNode)
{-# DEPRECATED cgnNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Lude.FromJSON CodeGenNode where
  parseJSON =
    Lude.withObject
      "CodeGenNode"
      ( \x ->
          CodeGenNode'
            Lude.<$> (x Lude..:? "Args" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LineNumber")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "NodeType")
      )

instance Lude.ToJSON CodeGenNode where
  toJSON CodeGenNode' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Args" Lude..= args),
            ("LineNumber" Lude..=) Lude.<$> lineNumber,
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("NodeType" Lude..= nodeType)
          ]
      )

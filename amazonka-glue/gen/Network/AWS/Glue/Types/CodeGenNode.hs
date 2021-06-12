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
-- Module      : Network.AWS.Glue.Types.CodeGenNode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenNode where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.CodeGenNodeArg
import qualified Network.AWS.Lens as Lens

-- | Represents a node in a directed acyclic graph (DAG)
--
-- /See:/ 'newCodeGenNode' smart constructor.
data CodeGenNode = CodeGenNode'
  { -- | The line number of the node.
    lineNumber :: Core.Maybe Core.Int,
    -- | A node identifier that is unique within the node\'s graph.
    id :: Core.Text,
    -- | The type of node that this is.
    nodeType :: Core.Text,
    -- | Properties of the node, in the form of name-value pairs.
    args :: [CodeGenNodeArg]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeGenNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineNumber', 'codeGenNode_lineNumber' - The line number of the node.
--
-- 'id', 'codeGenNode_id' - A node identifier that is unique within the node\'s graph.
--
-- 'nodeType', 'codeGenNode_nodeType' - The type of node that this is.
--
-- 'args', 'codeGenNode_args' - Properties of the node, in the form of name-value pairs.
newCodeGenNode ::
  -- | 'id'
  Core.Text ->
  -- | 'nodeType'
  Core.Text ->
  CodeGenNode
newCodeGenNode pId_ pNodeType_ =
  CodeGenNode'
    { lineNumber = Core.Nothing,
      id = pId_,
      nodeType = pNodeType_,
      args = Core.mempty
    }

-- | The line number of the node.
codeGenNode_lineNumber :: Lens.Lens' CodeGenNode (Core.Maybe Core.Int)
codeGenNode_lineNumber = Lens.lens (\CodeGenNode' {lineNumber} -> lineNumber) (\s@CodeGenNode' {} a -> s {lineNumber = a} :: CodeGenNode)

-- | A node identifier that is unique within the node\'s graph.
codeGenNode_id :: Lens.Lens' CodeGenNode Core.Text
codeGenNode_id = Lens.lens (\CodeGenNode' {id} -> id) (\s@CodeGenNode' {} a -> s {id = a} :: CodeGenNode)

-- | The type of node that this is.
codeGenNode_nodeType :: Lens.Lens' CodeGenNode Core.Text
codeGenNode_nodeType = Lens.lens (\CodeGenNode' {nodeType} -> nodeType) (\s@CodeGenNode' {} a -> s {nodeType = a} :: CodeGenNode)

-- | Properties of the node, in the form of name-value pairs.
codeGenNode_args :: Lens.Lens' CodeGenNode [CodeGenNodeArg]
codeGenNode_args = Lens.lens (\CodeGenNode' {args} -> args) (\s@CodeGenNode' {} a -> s {args = a} :: CodeGenNode) Core.. Lens._Coerce

instance Core.FromJSON CodeGenNode where
  parseJSON =
    Core.withObject
      "CodeGenNode"
      ( \x ->
          CodeGenNode'
            Core.<$> (x Core..:? "LineNumber")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "NodeType")
            Core.<*> (x Core..:? "Args" Core..!= Core.mempty)
      )

instance Core.Hashable CodeGenNode

instance Core.NFData CodeGenNode

instance Core.ToJSON CodeGenNode where
  toJSON CodeGenNode' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LineNumber" Core..=) Core.<$> lineNumber,
            Core.Just ("Id" Core..= id),
            Core.Just ("NodeType" Core..= nodeType),
            Core.Just ("Args" Core..= args)
          ]
      )

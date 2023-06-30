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
-- Module      : Amazonka.Glue.Types.CodeGenNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CodeGenNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.CodeGenNodeArg
import qualified Amazonka.Prelude as Prelude

-- | Represents a node in a directed acyclic graph (DAG)
--
-- /See:/ 'newCodeGenNode' smart constructor.
data CodeGenNode = CodeGenNode'
  { -- | The line number of the node.
    lineNumber :: Prelude.Maybe Prelude.Int,
    -- | A node identifier that is unique within the node\'s graph.
    id :: Prelude.Text,
    -- | The type of node that this is.
    nodeType :: Prelude.Text,
    -- | Properties of the node, in the form of name-value pairs.
    args :: [CodeGenNodeArg]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'nodeType'
  Prelude.Text ->
  CodeGenNode
newCodeGenNode pId_ pNodeType_ =
  CodeGenNode'
    { lineNumber = Prelude.Nothing,
      id = pId_,
      nodeType = pNodeType_,
      args = Prelude.mempty
    }

-- | The line number of the node.
codeGenNode_lineNumber :: Lens.Lens' CodeGenNode (Prelude.Maybe Prelude.Int)
codeGenNode_lineNumber = Lens.lens (\CodeGenNode' {lineNumber} -> lineNumber) (\s@CodeGenNode' {} a -> s {lineNumber = a} :: CodeGenNode)

-- | A node identifier that is unique within the node\'s graph.
codeGenNode_id :: Lens.Lens' CodeGenNode Prelude.Text
codeGenNode_id = Lens.lens (\CodeGenNode' {id} -> id) (\s@CodeGenNode' {} a -> s {id = a} :: CodeGenNode)

-- | The type of node that this is.
codeGenNode_nodeType :: Lens.Lens' CodeGenNode Prelude.Text
codeGenNode_nodeType = Lens.lens (\CodeGenNode' {nodeType} -> nodeType) (\s@CodeGenNode' {} a -> s {nodeType = a} :: CodeGenNode)

-- | Properties of the node, in the form of name-value pairs.
codeGenNode_args :: Lens.Lens' CodeGenNode [CodeGenNodeArg]
codeGenNode_args = Lens.lens (\CodeGenNode' {args} -> args) (\s@CodeGenNode' {} a -> s {args = a} :: CodeGenNode) Prelude.. Lens.coerced

instance Data.FromJSON CodeGenNode where
  parseJSON =
    Data.withObject
      "CodeGenNode"
      ( \x ->
          CodeGenNode'
            Prelude.<$> (x Data..:? "LineNumber")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "NodeType")
            Prelude.<*> (x Data..:? "Args" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CodeGenNode where
  hashWithSalt _salt CodeGenNode' {..} =
    _salt
      `Prelude.hashWithSalt` lineNumber
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` args

instance Prelude.NFData CodeGenNode where
  rnf CodeGenNode' {..} =
    Prelude.rnf lineNumber
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf args

instance Data.ToJSON CodeGenNode where
  toJSON CodeGenNode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LineNumber" Data..=) Prelude.<$> lineNumber,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("NodeType" Data..= nodeType),
            Prelude.Just ("Args" Data..= args)
          ]
      )

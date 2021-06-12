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
-- Module      : Network.AWS.Glue.Types.CodeGenEdge
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenEdge where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a directional edge in a directed acyclic graph (DAG).
--
-- /See:/ 'newCodeGenEdge' smart constructor.
data CodeGenEdge = CodeGenEdge'
  { -- | The target of the edge.
    targetParameter :: Core.Maybe Core.Text,
    -- | The ID of the node at which the edge starts.
    source :: Core.Text,
    -- | The ID of the node at which the edge ends.
    target :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeGenEdge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetParameter', 'codeGenEdge_targetParameter' - The target of the edge.
--
-- 'source', 'codeGenEdge_source' - The ID of the node at which the edge starts.
--
-- 'target', 'codeGenEdge_target' - The ID of the node at which the edge ends.
newCodeGenEdge ::
  -- | 'source'
  Core.Text ->
  -- | 'target'
  Core.Text ->
  CodeGenEdge
newCodeGenEdge pSource_ pTarget_ =
  CodeGenEdge'
    { targetParameter = Core.Nothing,
      source = pSource_,
      target = pTarget_
    }

-- | The target of the edge.
codeGenEdge_targetParameter :: Lens.Lens' CodeGenEdge (Core.Maybe Core.Text)
codeGenEdge_targetParameter = Lens.lens (\CodeGenEdge' {targetParameter} -> targetParameter) (\s@CodeGenEdge' {} a -> s {targetParameter = a} :: CodeGenEdge)

-- | The ID of the node at which the edge starts.
codeGenEdge_source :: Lens.Lens' CodeGenEdge Core.Text
codeGenEdge_source = Lens.lens (\CodeGenEdge' {source} -> source) (\s@CodeGenEdge' {} a -> s {source = a} :: CodeGenEdge)

-- | The ID of the node at which the edge ends.
codeGenEdge_target :: Lens.Lens' CodeGenEdge Core.Text
codeGenEdge_target = Lens.lens (\CodeGenEdge' {target} -> target) (\s@CodeGenEdge' {} a -> s {target = a} :: CodeGenEdge)

instance Core.FromJSON CodeGenEdge where
  parseJSON =
    Core.withObject
      "CodeGenEdge"
      ( \x ->
          CodeGenEdge'
            Core.<$> (x Core..:? "TargetParameter")
            Core.<*> (x Core..: "Source")
            Core.<*> (x Core..: "Target")
      )

instance Core.Hashable CodeGenEdge

instance Core.NFData CodeGenEdge

instance Core.ToJSON CodeGenEdge where
  toJSON CodeGenEdge' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TargetParameter" Core..=)
              Core.<$> targetParameter,
            Core.Just ("Source" Core..= source),
            Core.Just ("Target" Core..= target)
          ]
      )

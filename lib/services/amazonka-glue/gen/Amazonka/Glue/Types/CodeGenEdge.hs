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
-- Module      : Amazonka.Glue.Types.CodeGenEdge
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CodeGenEdge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a directional edge in a directed acyclic graph (DAG).
--
-- /See:/ 'newCodeGenEdge' smart constructor.
data CodeGenEdge = CodeGenEdge'
  { -- | The target of the edge.
    targetParameter :: Prelude.Maybe Prelude.Text,
    -- | The ID of the node at which the edge starts.
    source :: Prelude.Text,
    -- | The ID of the node at which the edge ends.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  CodeGenEdge
newCodeGenEdge pSource_ pTarget_ =
  CodeGenEdge'
    { targetParameter = Prelude.Nothing,
      source = pSource_,
      target = pTarget_
    }

-- | The target of the edge.
codeGenEdge_targetParameter :: Lens.Lens' CodeGenEdge (Prelude.Maybe Prelude.Text)
codeGenEdge_targetParameter = Lens.lens (\CodeGenEdge' {targetParameter} -> targetParameter) (\s@CodeGenEdge' {} a -> s {targetParameter = a} :: CodeGenEdge)

-- | The ID of the node at which the edge starts.
codeGenEdge_source :: Lens.Lens' CodeGenEdge Prelude.Text
codeGenEdge_source = Lens.lens (\CodeGenEdge' {source} -> source) (\s@CodeGenEdge' {} a -> s {source = a} :: CodeGenEdge)

-- | The ID of the node at which the edge ends.
codeGenEdge_target :: Lens.Lens' CodeGenEdge Prelude.Text
codeGenEdge_target = Lens.lens (\CodeGenEdge' {target} -> target) (\s@CodeGenEdge' {} a -> s {target = a} :: CodeGenEdge)

instance Data.FromJSON CodeGenEdge where
  parseJSON =
    Data.withObject
      "CodeGenEdge"
      ( \x ->
          CodeGenEdge'
            Prelude.<$> (x Data..:? "TargetParameter")
            Prelude.<*> (x Data..: "Source")
            Prelude.<*> (x Data..: "Target")
      )

instance Prelude.Hashable CodeGenEdge where
  hashWithSalt _salt CodeGenEdge' {..} =
    _salt
      `Prelude.hashWithSalt` targetParameter
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` target

instance Prelude.NFData CodeGenEdge where
  rnf CodeGenEdge' {..} =
    Prelude.rnf targetParameter
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf target

instance Data.ToJSON CodeGenEdge where
  toJSON CodeGenEdge' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetParameter" Data..=)
              Prelude.<$> targetParameter,
            Prelude.Just ("Source" Data..= source),
            Prelude.Just ("Target" Data..= target)
          ]
      )

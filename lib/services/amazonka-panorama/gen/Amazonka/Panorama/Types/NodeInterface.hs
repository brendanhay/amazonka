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
-- Module      : Amazonka.Panorama.Types.NodeInterface
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types.NodeInputPort
import Amazonka.Panorama.Types.NodeOutputPort
import qualified Amazonka.Prelude as Prelude

-- | A node interface.
--
-- /See:/ 'newNodeInterface' smart constructor.
data NodeInterface = NodeInterface'
  { -- | The node interface\'s inputs.
    inputs :: [NodeInputPort],
    -- | The node interface\'s outputs.
    outputs :: [NodeOutputPort]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputs', 'nodeInterface_inputs' - The node interface\'s inputs.
--
-- 'outputs', 'nodeInterface_outputs' - The node interface\'s outputs.
newNodeInterface ::
  NodeInterface
newNodeInterface =
  NodeInterface'
    { inputs = Prelude.mempty,
      outputs = Prelude.mempty
    }

-- | The node interface\'s inputs.
nodeInterface_inputs :: Lens.Lens' NodeInterface [NodeInputPort]
nodeInterface_inputs = Lens.lens (\NodeInterface' {inputs} -> inputs) (\s@NodeInterface' {} a -> s {inputs = a} :: NodeInterface) Prelude.. Lens.coerced

-- | The node interface\'s outputs.
nodeInterface_outputs :: Lens.Lens' NodeInterface [NodeOutputPort]
nodeInterface_outputs = Lens.lens (\NodeInterface' {outputs} -> outputs) (\s@NodeInterface' {} a -> s {outputs = a} :: NodeInterface) Prelude.. Lens.coerced

instance Core.FromJSON NodeInterface where
  parseJSON =
    Core.withObject
      "NodeInterface"
      ( \x ->
          NodeInterface'
            Prelude.<$> (x Core..:? "Inputs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Outputs" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable NodeInterface where
  hashWithSalt _salt NodeInterface' {..} =
    _salt `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` outputs

instance Prelude.NFData NodeInterface where
  rnf NodeInterface' {..} =
    Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf outputs

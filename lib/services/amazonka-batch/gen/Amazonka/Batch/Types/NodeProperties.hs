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
-- Module      : Amazonka.Batch.Types.NodeProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.NodeProperties where

import Amazonka.Batch.Types.NodeRangeProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the node properties of a multi-node parallel
-- job.
--
-- Node properties can\'t be specified for Amazon EKS based job
-- definitions.
--
-- /See:/ 'newNodeProperties' smart constructor.
data NodeProperties = NodeProperties'
  { -- | The number of nodes that are associated with a multi-node parallel job.
    numNodes :: Prelude.Int,
    -- | Specifies the node index for the main node of a multi-node parallel job.
    -- This node index value must be fewer than the number of nodes.
    mainNode :: Prelude.Int,
    -- | A list of node ranges and their properties that are associated with a
    -- multi-node parallel job.
    nodeRangeProperties :: [NodeRangeProperty]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numNodes', 'nodeProperties_numNodes' - The number of nodes that are associated with a multi-node parallel job.
--
-- 'mainNode', 'nodeProperties_mainNode' - Specifies the node index for the main node of a multi-node parallel job.
-- This node index value must be fewer than the number of nodes.
--
-- 'nodeRangeProperties', 'nodeProperties_nodeRangeProperties' - A list of node ranges and their properties that are associated with a
-- multi-node parallel job.
newNodeProperties ::
  -- | 'numNodes'
  Prelude.Int ->
  -- | 'mainNode'
  Prelude.Int ->
  NodeProperties
newNodeProperties pNumNodes_ pMainNode_ =
  NodeProperties'
    { numNodes = pNumNodes_,
      mainNode = pMainNode_,
      nodeRangeProperties = Prelude.mempty
    }

-- | The number of nodes that are associated with a multi-node parallel job.
nodeProperties_numNodes :: Lens.Lens' NodeProperties Prelude.Int
nodeProperties_numNodes = Lens.lens (\NodeProperties' {numNodes} -> numNodes) (\s@NodeProperties' {} a -> s {numNodes = a} :: NodeProperties)

-- | Specifies the node index for the main node of a multi-node parallel job.
-- This node index value must be fewer than the number of nodes.
nodeProperties_mainNode :: Lens.Lens' NodeProperties Prelude.Int
nodeProperties_mainNode = Lens.lens (\NodeProperties' {mainNode} -> mainNode) (\s@NodeProperties' {} a -> s {mainNode = a} :: NodeProperties)

-- | A list of node ranges and their properties that are associated with a
-- multi-node parallel job.
nodeProperties_nodeRangeProperties :: Lens.Lens' NodeProperties [NodeRangeProperty]
nodeProperties_nodeRangeProperties = Lens.lens (\NodeProperties' {nodeRangeProperties} -> nodeRangeProperties) (\s@NodeProperties' {} a -> s {nodeRangeProperties = a} :: NodeProperties) Prelude.. Lens.coerced

instance Data.FromJSON NodeProperties where
  parseJSON =
    Data.withObject
      "NodeProperties"
      ( \x ->
          NodeProperties'
            Prelude.<$> (x Data..: "numNodes")
            Prelude.<*> (x Data..: "mainNode")
            Prelude.<*> ( x
                            Data..:? "nodeRangeProperties"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable NodeProperties where
  hashWithSalt _salt NodeProperties' {..} =
    _salt
      `Prelude.hashWithSalt` numNodes
      `Prelude.hashWithSalt` mainNode
      `Prelude.hashWithSalt` nodeRangeProperties

instance Prelude.NFData NodeProperties where
  rnf NodeProperties' {..} =
    Prelude.rnf numNodes `Prelude.seq`
      Prelude.rnf mainNode `Prelude.seq`
        Prelude.rnf nodeRangeProperties

instance Data.ToJSON NodeProperties where
  toJSON NodeProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("numNodes" Data..= numNodes),
            Prelude.Just ("mainNode" Data..= mainNode),
            Prelude.Just
              ("nodeRangeProperties" Data..= nodeRangeProperties)
          ]
      )

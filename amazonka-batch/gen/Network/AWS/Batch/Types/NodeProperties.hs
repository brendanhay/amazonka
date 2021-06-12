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
-- Module      : Network.AWS.Batch.Types.NodeProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeProperties where

import Network.AWS.Batch.Types.NodeRangeProperty
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the node properties of a multi-node parallel job.
--
-- /See:/ 'newNodeProperties' smart constructor.
data NodeProperties = NodeProperties'
  { -- | The number of nodes associated with a multi-node parallel job.
    numNodes :: Core.Int,
    -- | Specifies the node index for the main node of a multi-node parallel job.
    -- This node index value must be fewer than the number of nodes.
    mainNode :: Core.Int,
    -- | A list of node ranges and their properties associated with a multi-node
    -- parallel job.
    nodeRangeProperties :: [NodeRangeProperty]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numNodes', 'nodeProperties_numNodes' - The number of nodes associated with a multi-node parallel job.
--
-- 'mainNode', 'nodeProperties_mainNode' - Specifies the node index for the main node of a multi-node parallel job.
-- This node index value must be fewer than the number of nodes.
--
-- 'nodeRangeProperties', 'nodeProperties_nodeRangeProperties' - A list of node ranges and their properties associated with a multi-node
-- parallel job.
newNodeProperties ::
  -- | 'numNodes'
  Core.Int ->
  -- | 'mainNode'
  Core.Int ->
  NodeProperties
newNodeProperties pNumNodes_ pMainNode_ =
  NodeProperties'
    { numNodes = pNumNodes_,
      mainNode = pMainNode_,
      nodeRangeProperties = Core.mempty
    }

-- | The number of nodes associated with a multi-node parallel job.
nodeProperties_numNodes :: Lens.Lens' NodeProperties Core.Int
nodeProperties_numNodes = Lens.lens (\NodeProperties' {numNodes} -> numNodes) (\s@NodeProperties' {} a -> s {numNodes = a} :: NodeProperties)

-- | Specifies the node index for the main node of a multi-node parallel job.
-- This node index value must be fewer than the number of nodes.
nodeProperties_mainNode :: Lens.Lens' NodeProperties Core.Int
nodeProperties_mainNode = Lens.lens (\NodeProperties' {mainNode} -> mainNode) (\s@NodeProperties' {} a -> s {mainNode = a} :: NodeProperties)

-- | A list of node ranges and their properties associated with a multi-node
-- parallel job.
nodeProperties_nodeRangeProperties :: Lens.Lens' NodeProperties [NodeRangeProperty]
nodeProperties_nodeRangeProperties = Lens.lens (\NodeProperties' {nodeRangeProperties} -> nodeRangeProperties) (\s@NodeProperties' {} a -> s {nodeRangeProperties = a} :: NodeProperties) Core.. Lens._Coerce

instance Core.FromJSON NodeProperties where
  parseJSON =
    Core.withObject
      "NodeProperties"
      ( \x ->
          NodeProperties'
            Core.<$> (x Core..: "numNodes")
            Core.<*> (x Core..: "mainNode")
            Core.<*> ( x Core..:? "nodeRangeProperties"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable NodeProperties

instance Core.NFData NodeProperties

instance Core.ToJSON NodeProperties where
  toJSON NodeProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("numNodes" Core..= numNodes),
            Core.Just ("mainNode" Core..= mainNode),
            Core.Just
              ("nodeRangeProperties" Core..= nodeRangeProperties)
          ]
      )

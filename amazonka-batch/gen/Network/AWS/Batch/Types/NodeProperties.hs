{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the node properties of a multi-node parallel job.
--
-- /See:/ 'newNodeProperties' smart constructor.
data NodeProperties = NodeProperties'
  { -- | The number of nodes associated with a multi-node parallel job.
    numNodes :: Prelude.Int,
    -- | Specifies the node index for the main node of a multi-node parallel job.
    -- This node index value must be fewer than the number of nodes.
    mainNode :: Prelude.Int,
    -- | A list of node ranges and their properties associated with a multi-node
    -- parallel job.
    nodeRangeProperties :: [NodeRangeProperty]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | The number of nodes associated with a multi-node parallel job.
nodeProperties_numNodes :: Lens.Lens' NodeProperties Prelude.Int
nodeProperties_numNodes = Lens.lens (\NodeProperties' {numNodes} -> numNodes) (\s@NodeProperties' {} a -> s {numNodes = a} :: NodeProperties)

-- | Specifies the node index for the main node of a multi-node parallel job.
-- This node index value must be fewer than the number of nodes.
nodeProperties_mainNode :: Lens.Lens' NodeProperties Prelude.Int
nodeProperties_mainNode = Lens.lens (\NodeProperties' {mainNode} -> mainNode) (\s@NodeProperties' {} a -> s {mainNode = a} :: NodeProperties)

-- | A list of node ranges and their properties associated with a multi-node
-- parallel job.
nodeProperties_nodeRangeProperties :: Lens.Lens' NodeProperties [NodeRangeProperty]
nodeProperties_nodeRangeProperties = Lens.lens (\NodeProperties' {nodeRangeProperties} -> nodeRangeProperties) (\s@NodeProperties' {} a -> s {nodeRangeProperties = a} :: NodeProperties) Prelude.. Prelude._Coerce

instance Prelude.FromJSON NodeProperties where
  parseJSON =
    Prelude.withObject
      "NodeProperties"
      ( \x ->
          NodeProperties'
            Prelude.<$> (x Prelude..: "numNodes")
            Prelude.<*> (x Prelude..: "mainNode")
            Prelude.<*> ( x Prelude..:? "nodeRangeProperties"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable NodeProperties

instance Prelude.NFData NodeProperties

instance Prelude.ToJSON NodeProperties where
  toJSON NodeProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("numNodes" Prelude..= numNodes),
            Prelude.Just ("mainNode" Prelude..= mainNode),
            Prelude.Just
              ( "nodeRangeProperties"
                  Prelude..= nodeRangeProperties
              )
          ]
      )

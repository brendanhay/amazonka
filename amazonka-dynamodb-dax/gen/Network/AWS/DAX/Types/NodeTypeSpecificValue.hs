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
-- Module      : Network.AWS.DAX.Types.NodeTypeSpecificValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.NodeTypeSpecificValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a parameter value that is applicable to a particular node
-- type.
--
-- /See:/ 'newNodeTypeSpecificValue' smart constructor.
data NodeTypeSpecificValue = NodeTypeSpecificValue'
  { -- | The parameter value for this node type.
    value :: Core.Maybe Core.Text,
    -- | A node type to which the parameter value applies.
    nodeType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodeTypeSpecificValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'nodeTypeSpecificValue_value' - The parameter value for this node type.
--
-- 'nodeType', 'nodeTypeSpecificValue_nodeType' - A node type to which the parameter value applies.
newNodeTypeSpecificValue ::
  NodeTypeSpecificValue
newNodeTypeSpecificValue =
  NodeTypeSpecificValue'
    { value = Core.Nothing,
      nodeType = Core.Nothing
    }

-- | The parameter value for this node type.
nodeTypeSpecificValue_value :: Lens.Lens' NodeTypeSpecificValue (Core.Maybe Core.Text)
nodeTypeSpecificValue_value = Lens.lens (\NodeTypeSpecificValue' {value} -> value) (\s@NodeTypeSpecificValue' {} a -> s {value = a} :: NodeTypeSpecificValue)

-- | A node type to which the parameter value applies.
nodeTypeSpecificValue_nodeType :: Lens.Lens' NodeTypeSpecificValue (Core.Maybe Core.Text)
nodeTypeSpecificValue_nodeType = Lens.lens (\NodeTypeSpecificValue' {nodeType} -> nodeType) (\s@NodeTypeSpecificValue' {} a -> s {nodeType = a} :: NodeTypeSpecificValue)

instance Core.FromJSON NodeTypeSpecificValue where
  parseJSON =
    Core.withObject
      "NodeTypeSpecificValue"
      ( \x ->
          NodeTypeSpecificValue'
            Core.<$> (x Core..:? "Value")
            Core.<*> (x Core..:? "NodeType")
      )

instance Core.Hashable NodeTypeSpecificValue

instance Core.NFData NodeTypeSpecificValue

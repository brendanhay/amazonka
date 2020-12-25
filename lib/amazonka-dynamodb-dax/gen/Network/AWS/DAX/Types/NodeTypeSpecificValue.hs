{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.NodeTypeSpecificValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.NodeTypeSpecificValue
  ( NodeTypeSpecificValue (..),

    -- * Smart constructor
    mkNodeTypeSpecificValue,

    -- * Lenses
    ntsvNodeType,
    ntsvValue,
  )
where

import qualified Network.AWS.DAX.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a parameter value that is applicable to a particular node type.
--
-- /See:/ 'mkNodeTypeSpecificValue' smart constructor.
data NodeTypeSpecificValue = NodeTypeSpecificValue'
  { -- | A node type to which the parameter value applies.
    nodeType :: Core.Maybe Types.String,
    -- | The parameter value for this node type.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeTypeSpecificValue' value with any optional fields omitted.
mkNodeTypeSpecificValue ::
  NodeTypeSpecificValue
mkNodeTypeSpecificValue =
  NodeTypeSpecificValue'
    { nodeType = Core.Nothing,
      value = Core.Nothing
    }

-- | A node type to which the parameter value applies.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntsvNodeType :: Lens.Lens' NodeTypeSpecificValue (Core.Maybe Types.String)
ntsvNodeType = Lens.field @"nodeType"
{-# DEPRECATED ntsvNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The parameter value for this node type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntsvValue :: Lens.Lens' NodeTypeSpecificValue (Core.Maybe Types.String)
ntsvValue = Lens.field @"value"
{-# DEPRECATED ntsvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON NodeTypeSpecificValue where
  parseJSON =
    Core.withObject "NodeTypeSpecificValue" Core.$
      \x ->
        NodeTypeSpecificValue'
          Core.<$> (x Core..:? "NodeType") Core.<*> (x Core..:? "Value")

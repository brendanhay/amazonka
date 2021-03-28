{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.NodeTypeSpecificValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.NodeTypeSpecificValue
  ( NodeTypeSpecificValue (..)
  -- * Smart constructor
  , mkNodeTypeSpecificValue
  -- * Lenses
  , ntsvNodeType
  , ntsvValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a parameter value that is applicable to a particular node type.
--
-- /See:/ 'mkNodeTypeSpecificValue' smart constructor.
data NodeTypeSpecificValue = NodeTypeSpecificValue'
  { nodeType :: Core.Maybe Core.Text
    -- ^ A node type to which the parameter value applies.
  , value :: Core.Maybe Core.Text
    -- ^ The parameter value for this node type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeTypeSpecificValue' value with any optional fields omitted.
mkNodeTypeSpecificValue
    :: NodeTypeSpecificValue
mkNodeTypeSpecificValue
  = NodeTypeSpecificValue'{nodeType = Core.Nothing,
                           value = Core.Nothing}

-- | A node type to which the parameter value applies.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntsvNodeType :: Lens.Lens' NodeTypeSpecificValue (Core.Maybe Core.Text)
ntsvNodeType = Lens.field @"nodeType"
{-# INLINEABLE ntsvNodeType #-}
{-# DEPRECATED nodeType "Use generic-lens or generic-optics with 'nodeType' instead"  #-}

-- | The parameter value for this node type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ntsvValue :: Lens.Lens' NodeTypeSpecificValue (Core.Maybe Core.Text)
ntsvValue = Lens.field @"value"
{-# INLINEABLE ntsvValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON NodeTypeSpecificValue where
        parseJSON
          = Core.withObject "NodeTypeSpecificValue" Core.$
              \ x ->
                NodeTypeSpecificValue' Core.<$>
                  (x Core..:? "NodeType") Core.<*> x Core..:? "Value"

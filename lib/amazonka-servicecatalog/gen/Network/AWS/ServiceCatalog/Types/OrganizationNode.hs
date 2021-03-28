{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.OrganizationNode
  ( OrganizationNode (..)
  -- * Smart constructor
  , mkOrganizationNode
  -- * Lenses
  , onType
  , onValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.OrganizationNodeType as Types
import qualified Network.AWS.ServiceCatalog.Types.OrganizationNodeValue as Types

-- | Information about the organization node.
--
-- /See:/ 'mkOrganizationNode' smart constructor.
data OrganizationNode = OrganizationNode'
  { type' :: Core.Maybe Types.OrganizationNodeType
    -- ^ The organization node type.
  , value :: Core.Maybe Types.OrganizationNodeValue
    -- ^ The identifier of the organization node.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationNode' value with any optional fields omitted.
mkOrganizationNode
    :: OrganizationNode
mkOrganizationNode
  = OrganizationNode'{type' = Core.Nothing, value = Core.Nothing}

-- | The organization node type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
onType :: Lens.Lens' OrganizationNode (Core.Maybe Types.OrganizationNodeType)
onType = Lens.field @"type'"
{-# INLINEABLE onType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The identifier of the organization node.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
onValue :: Lens.Lens' OrganizationNode (Core.Maybe Types.OrganizationNodeValue)
onValue = Lens.field @"value"
{-# INLINEABLE onValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON OrganizationNode where
        toJSON OrganizationNode{..}
          = Core.object
              (Core.catMaybes
                 [("Type" Core..=) Core.<$> type',
                  ("Value" Core..=) Core.<$> value])

instance Core.FromJSON OrganizationNode where
        parseJSON
          = Core.withObject "OrganizationNode" Core.$
              \ x ->
                OrganizationNode' Core.<$>
                  (x Core..:? "Type") Core.<*> x Core..:? "Value"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition
  ( ResourceTargetDefinition (..)
  -- * Smart constructor
  , mkResourceTargetDefinition
  -- * Lenses
  , rtdAttribute
  , rtdName
  , rtdRequiresRecreation
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.PropertyName as Types
import qualified Network.AWS.ServiceCatalog.Types.RequiresRecreation as Types
import qualified Network.AWS.ServiceCatalog.Types.ResourceAttribute as Types

-- | Information about a change to a resource attribute.
--
-- /See:/ 'mkResourceTargetDefinition' smart constructor.
data ResourceTargetDefinition = ResourceTargetDefinition'
  { attribute :: Core.Maybe Types.ResourceAttribute
    -- ^ The attribute to be changed.
  , name :: Core.Maybe Types.PropertyName
    -- ^ If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
  , requiresRecreation :: Core.Maybe Types.RequiresRecreation
    -- ^ If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceTargetDefinition' value with any optional fields omitted.
mkResourceTargetDefinition
    :: ResourceTargetDefinition
mkResourceTargetDefinition
  = ResourceTargetDefinition'{attribute = Core.Nothing,
                              name = Core.Nothing, requiresRecreation = Core.Nothing}

-- | The attribute to be changed.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdAttribute :: Lens.Lens' ResourceTargetDefinition (Core.Maybe Types.ResourceAttribute)
rtdAttribute = Lens.field @"attribute"
{-# INLINEABLE rtdAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | If the attribute is @Properties@ , the value is the name of the property. Otherwise, the value is null.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdName :: Lens.Lens' ResourceTargetDefinition (Core.Maybe Types.PropertyName)
rtdName = Lens.field @"name"
{-# INLINEABLE rtdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | If the attribute is @Properties@ , indicates whether a change to this property causes the resource to be re-created.
--
-- /Note:/ Consider using 'requiresRecreation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtdRequiresRecreation :: Lens.Lens' ResourceTargetDefinition (Core.Maybe Types.RequiresRecreation)
rtdRequiresRecreation = Lens.field @"requiresRecreation"
{-# INLINEABLE rtdRequiresRecreation #-}
{-# DEPRECATED requiresRecreation "Use generic-lens or generic-optics with 'requiresRecreation' instead"  #-}

instance Core.FromJSON ResourceTargetDefinition where
        parseJSON
          = Core.withObject "ResourceTargetDefinition" Core.$
              \ x ->
                ResourceTargetDefinition' Core.<$>
                  (x Core..:? "Attribute") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "RequiresRecreation"

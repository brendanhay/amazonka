{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TagDescription
  ( TagDescription (..)
  -- * Smart constructor
  , mkTagDescription
  -- * Lenses
  , tdKey
  , tdResourceId
  , tdResourceType
  , tdValue
  ) where

import qualified Network.AWS.EC2.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a tag.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { key :: Core.Text
    -- ^ The tag key.
  , resourceId :: Core.Text
    -- ^ The ID of the resource.
  , resourceType :: Types.ResourceType
    -- ^ The resource type.
  , value :: Core.Text
    -- ^ The tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagDescription' value with any optional fields omitted.
mkTagDescription
    :: Core.Text -- ^ 'key'
    -> Core.Text -- ^ 'resourceId'
    -> Types.ResourceType -- ^ 'resourceType'
    -> Core.Text -- ^ 'value'
    -> TagDescription
mkTagDescription key resourceId resourceType value
  = TagDescription'{key, resourceId, resourceType, value}

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdKey :: Lens.Lens' TagDescription Core.Text
tdKey = Lens.field @"key"
{-# INLINEABLE tdKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceId :: Lens.Lens' TagDescription Core.Text
tdResourceId = Lens.field @"resourceId"
{-# INLINEABLE tdResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceType :: Lens.Lens' TagDescription Types.ResourceType
tdResourceType = Lens.field @"resourceType"
{-# INLINEABLE tdResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdValue :: Lens.Lens' TagDescription Core.Text
tdValue = Lens.field @"value"
{-# INLINEABLE tdValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML TagDescription where
        parseXML x
          = TagDescription' Core.<$>
              (x Core..@ "key") Core.<*> x Core..@ "resourceId" Core.<*>
                x Core..@ "resourceType"
                Core.<*> x Core..@ "value"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.Tag
  ( Tag (..)
  -- * Smart constructor
  , mkTag
  -- * Lenses
  , tKey
  , tPropagateAtLaunch
  , tResourceId
  , tResourceType
  , tValue
  ) where

import qualified Network.AWS.AutoScaling.Types.Key as Types
import qualified Network.AWS.AutoScaling.Types.Value as Types
import qualified Network.AWS.AutoScaling.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
  { key :: Types.Key
    -- ^ The tag key.
  , propagateAtLaunch :: Core.Bool
    -- ^ Determines whether the tag is added to new instances as they are launched in the group.
  , resourceId :: Types.XmlString
    -- ^ The name of the group.
  , resourceType :: Types.XmlString
    -- ^ The type of resource. The only supported value is @auto-scaling-group@ .
  , value :: Types.Value
    -- ^ The tag value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag
    :: Types.Key -- ^ 'key'
    -> Core.Bool -- ^ 'propagateAtLaunch'
    -> Types.XmlString -- ^ 'resourceId'
    -> Types.XmlString -- ^ 'resourceType'
    -> Types.Value -- ^ 'value'
    -> Tag
mkTag key propagateAtLaunch resourceId resourceType value
  = Tag'{key, propagateAtLaunch, resourceId, resourceType, value}

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# INLINEABLE tKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Determines whether the tag is added to new instances as they are launched in the group.
--
-- /Note:/ Consider using 'propagateAtLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPropagateAtLaunch :: Lens.Lens' Tag Core.Bool
tPropagateAtLaunch = Lens.field @"propagateAtLaunch"
{-# INLINEABLE tPropagateAtLaunch #-}
{-# DEPRECATED propagateAtLaunch "Use generic-lens or generic-optics with 'propagateAtLaunch' instead"  #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceId :: Lens.Lens' Tag Types.XmlString
tResourceId = Lens.field @"resourceId"
{-# INLINEABLE tResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The type of resource. The only supported value is @auto-scaling-group@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceType :: Lens.Lens' Tag Types.XmlString
tResourceType = Lens.field @"resourceType"
{-# INLINEABLE tResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Types.Value
tValue = Lens.field @"value"
{-# INLINEABLE tValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery Tag where
        toQuery Tag{..}
          = Core.toQueryPair "Key" key Core.<>
              Core.toQueryPair "PropagateAtLaunch" propagateAtLaunch
              Core.<> Core.toQueryPair "ResourceId" resourceId
              Core.<> Core.toQueryPair "ResourceType" resourceType
              Core.<> Core.toQueryPair "Value" value

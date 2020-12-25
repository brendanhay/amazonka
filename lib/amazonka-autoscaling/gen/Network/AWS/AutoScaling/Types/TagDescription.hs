{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.TagDescription
  ( TagDescription (..),

    -- * Smart constructor
    mkTagDescription,

    -- * Lenses
    tdKey,
    tdPropagateAtLaunch,
    tdResourceId,
    tdResourceType,
    tdValue,
  )
where

import qualified Network.AWS.AutoScaling.Types.Key as Types
import qualified Network.AWS.AutoScaling.Types.Value as Types
import qualified Network.AWS.AutoScaling.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The tag key.
    key :: Types.Key,
    -- | Determines whether the tag is added to new instances as they are launched in the group.
    propagateAtLaunch :: Core.Bool,
    -- | The name of the group.
    resourceId :: Types.XmlString,
    -- | The type of resource. The only supported value is @auto-scaling-group@ .
    resourceType :: Types.XmlString,
    -- | The tag value.
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagDescription' value with any optional fields omitted.
mkTagDescription ::
  -- | 'key'
  Types.Key ->
  -- | 'propagateAtLaunch'
  Core.Bool ->
  -- | 'resourceId'
  Types.XmlString ->
  -- | 'resourceType'
  Types.XmlString ->
  -- | 'value'
  Types.Value ->
  TagDescription
mkTagDescription
  key
  propagateAtLaunch
  resourceId
  resourceType
  value =
    TagDescription'
      { key,
        propagateAtLaunch,
        resourceId,
        resourceType,
        value
      }

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdKey :: Lens.Lens' TagDescription Types.Key
tdKey = Lens.field @"key"
{-# DEPRECATED tdKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Determines whether the tag is added to new instances as they are launched in the group.
--
-- /Note:/ Consider using 'propagateAtLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdPropagateAtLaunch :: Lens.Lens' TagDescription Core.Bool
tdPropagateAtLaunch = Lens.field @"propagateAtLaunch"
{-# DEPRECATED tdPropagateAtLaunch "Use generic-lens or generic-optics with 'propagateAtLaunch' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceId :: Lens.Lens' TagDescription Types.XmlString
tdResourceId = Lens.field @"resourceId"
{-# DEPRECATED tdResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource. The only supported value is @auto-scaling-group@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdResourceType :: Lens.Lens' TagDescription Types.XmlString
tdResourceType = Lens.field @"resourceType"
{-# DEPRECATED tdResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdValue :: Lens.Lens' TagDescription Types.Value
tdValue = Lens.field @"value"
{-# DEPRECATED tdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Core.<$> (x Core..@ "Key")
      Core.<*> (x Core..@ "PropagateAtLaunch")
      Core.<*> (x Core..@ "ResourceId")
      Core.<*> (x Core..@ "ResourceType")
      Core.<*> (x Core..@ "Value")

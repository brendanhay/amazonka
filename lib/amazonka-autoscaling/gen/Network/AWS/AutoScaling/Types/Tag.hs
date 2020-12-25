{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Tag
  ( Tag (..),

    -- * Smart constructor
    mkTag,

    -- * Lenses
    tKey,
    tPropagateAtLaunch,
    tResourceId,
    tResourceType,
    tValue,
  )
where

import qualified Network.AWS.AutoScaling.Types.Key as Types
import qualified Network.AWS.AutoScaling.Types.Value as Types
import qualified Network.AWS.AutoScaling.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'mkTag' smart constructor.
data Tag = Tag'
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

-- | Creates a 'Tag' value with any optional fields omitted.
mkTag ::
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
  Tag
mkTag key propagateAtLaunch resourceId resourceType value =
  Tag' {key, propagateAtLaunch, resourceId, resourceType, value}

-- | The tag key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Tag Types.Key
tKey = Lens.field @"key"
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Determines whether the tag is added to new instances as they are launched in the group.
--
-- /Note:/ Consider using 'propagateAtLaunch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPropagateAtLaunch :: Lens.Lens' Tag Core.Bool
tPropagateAtLaunch = Lens.field @"propagateAtLaunch"
{-# DEPRECATED tPropagateAtLaunch "Use generic-lens or generic-optics with 'propagateAtLaunch' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceId :: Lens.Lens' Tag Types.XmlString
tResourceId = Lens.field @"resourceId"
{-# DEPRECATED tResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource. The only supported value is @auto-scaling-group@ .
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tResourceType :: Lens.Lens' Tag Types.XmlString
tResourceType = Lens.field @"resourceType"
{-# DEPRECATED tResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The tag value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValue :: Lens.Lens' Tag Types.Value
tValue = Lens.field @"value"
{-# DEPRECATED tValue "Use generic-lens or generic-optics with 'value' instead." #-}

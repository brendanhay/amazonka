{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceTagNotificationAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTagNotificationAttribute
  ( InstanceTagNotificationAttribute (..),

    -- * Smart constructor
    mkInstanceTagNotificationAttribute,

    -- * Lenses
    itnaIncludeAllTagsOfInstance,
    itnaInstanceTagKeys,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the registered tag keys for the current Region.
--
-- /See:/ 'mkInstanceTagNotificationAttribute' smart constructor.
data InstanceTagNotificationAttribute = InstanceTagNotificationAttribute'
  { -- | Indicates wheter all tag keys in the current Region are registered to appear in scheduled event notifications. @true@ indicates that all tag keys in the current Region are registered.
    includeAllTagsOfInstance :: Core.Maybe Core.Bool,
    -- | The registered tag keys.
    instanceTagKeys :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceTagNotificationAttribute' value with any optional fields omitted.
mkInstanceTagNotificationAttribute ::
  InstanceTagNotificationAttribute
mkInstanceTagNotificationAttribute =
  InstanceTagNotificationAttribute'
    { includeAllTagsOfInstance =
        Core.Nothing,
      instanceTagKeys = Core.Nothing
    }

-- | Indicates wheter all tag keys in the current Region are registered to appear in scheduled event notifications. @true@ indicates that all tag keys in the current Region are registered.
--
-- /Note:/ Consider using 'includeAllTagsOfInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itnaIncludeAllTagsOfInstance :: Lens.Lens' InstanceTagNotificationAttribute (Core.Maybe Core.Bool)
itnaIncludeAllTagsOfInstance = Lens.field @"includeAllTagsOfInstance"
{-# DEPRECATED itnaIncludeAllTagsOfInstance "Use generic-lens or generic-optics with 'includeAllTagsOfInstance' instead." #-}

-- | The registered tag keys.
--
-- /Note:/ Consider using 'instanceTagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
itnaInstanceTagKeys :: Lens.Lens' InstanceTagNotificationAttribute (Core.Maybe [Types.String])
itnaInstanceTagKeys = Lens.field @"instanceTagKeys"
{-# DEPRECATED itnaInstanceTagKeys "Use generic-lens or generic-optics with 'instanceTagKeys' instead." #-}

instance Core.FromXML InstanceTagNotificationAttribute where
  parseXML x =
    InstanceTagNotificationAttribute'
      Core.<$> (x Core..@? "includeAllTagsOfInstance")
      Core.<*> (x Core..@? "instanceTagKeySet" Core..<@> Core.parseXMLList "item")

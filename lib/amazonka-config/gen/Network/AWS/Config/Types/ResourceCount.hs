{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ResourceCount
  ( ResourceCount (..)
  -- * Smart constructor
  , mkResourceCount
  -- * Lenses
  , rcgCount
  , rcgResourceType
  ) where

import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains the resource type and the number of resources.
--
-- /See:/ 'mkResourceCount' smart constructor.
data ResourceCount = ResourceCount'
  { count :: Core.Maybe Core.Integer
    -- ^ The number of resources.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type (for example, @"AWS::EC2::Instance"@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceCount' value with any optional fields omitted.
mkResourceCount
    :: ResourceCount
mkResourceCount
  = ResourceCount'{count = Core.Nothing, resourceType = Core.Nothing}

-- | The number of resources.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgCount :: Lens.Lens' ResourceCount (Core.Maybe Core.Integer)
rcgCount = Lens.field @"count"
{-# INLINEABLE rcgCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The resource type (for example, @"AWS::EC2::Instance"@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgResourceType :: Lens.Lens' ResourceCount (Core.Maybe Types.ResourceType)
rcgResourceType = Lens.field @"resourceType"
{-# INLINEABLE rcgResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON ResourceCount where
        parseJSON
          = Core.withObject "ResourceCount" Core.$
              \ x ->
                ResourceCount' Core.<$>
                  (x Core..:? "count") Core.<*> x Core..:? "resourceType"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceFilters
  ( ResourceFilters (..),

    -- * Smart constructor
    mkResourceFilters,

    -- * Lenses
    rfAccountId,
    rfRegion,
    rfResourceId,
    rfResourceName,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.AwsRegion as Types
import qualified Network.AWS.Config.Types.ResourceId as Types
import qualified Network.AWS.Config.Types.ResourceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters the results by resource account ID, region, resource ID, and resource name.
--
-- /See:/ 'mkResourceFilters' smart constructor.
data ResourceFilters = ResourceFilters'
  { -- | The 12-digit source account ID.
    accountId :: Core.Maybe Types.AccountId,
    -- | The source region.
    region :: Core.Maybe Types.AwsRegion,
    -- | The ID of the resource.
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The name of the resource.
    resourceName :: Core.Maybe Types.ResourceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceFilters' value with any optional fields omitted.
mkResourceFilters ::
  ResourceFilters
mkResourceFilters =
  ResourceFilters'
    { accountId = Core.Nothing,
      region = Core.Nothing,
      resourceId = Core.Nothing,
      resourceName = Core.Nothing
    }

-- | The 12-digit source account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfAccountId :: Lens.Lens' ResourceFilters (Core.Maybe Types.AccountId)
rfAccountId = Lens.field @"accountId"
{-# DEPRECATED rfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfRegion :: Lens.Lens' ResourceFilters (Core.Maybe Types.AwsRegion)
rfRegion = Lens.field @"region"
{-# DEPRECATED rfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfResourceId :: Lens.Lens' ResourceFilters (Core.Maybe Types.ResourceId)
rfResourceId = Lens.field @"resourceId"
{-# DEPRECATED rfResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfResourceName :: Lens.Lens' ResourceFilters (Core.Maybe Types.ResourceName)
rfResourceName = Lens.field @"resourceName"
{-# DEPRECATED rfResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

instance Core.FromJSON ResourceFilters where
  toJSON ResourceFilters {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountId" Core..=) Core.<$> accountId,
            ("Region" Core..=) Core.<$> region,
            ("ResourceId" Core..=) Core.<$> resourceId,
            ("ResourceName" Core..=) Core.<$> resourceName
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCountFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCountFilters
  ( ResourceCountFilters (..),

    -- * Smart constructor
    mkResourceCountFilters,

    -- * Lenses
    rcfAccountId,
    rcfRegion,
    rcfResourceType,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.Region as Types
import qualified Network.AWS.Config.Types.ResourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters the resource count based on account ID, region, and resource type.
--
-- /See:/ 'mkResourceCountFilters' smart constructor.
data ResourceCountFilters = ResourceCountFilters'
  { -- | The 12-digit ID of the account.
    accountId :: Core.Maybe Types.AccountId,
    -- | The region where the account is located.
    region :: Core.Maybe Types.Region,
    -- | The type of the AWS resource.
    resourceType :: Core.Maybe Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceCountFilters' value with any optional fields omitted.
mkResourceCountFilters ::
  ResourceCountFilters
mkResourceCountFilters =
  ResourceCountFilters'
    { accountId = Core.Nothing,
      region = Core.Nothing,
      resourceType = Core.Nothing
    }

-- | The 12-digit ID of the account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfAccountId :: Lens.Lens' ResourceCountFilters (Core.Maybe Types.AccountId)
rcfAccountId = Lens.field @"accountId"
{-# DEPRECATED rcfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The region where the account is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfRegion :: Lens.Lens' ResourceCountFilters (Core.Maybe Types.Region)
rcfRegion = Lens.field @"region"
{-# DEPRECATED rcfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The type of the AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfResourceType :: Lens.Lens' ResourceCountFilters (Core.Maybe Types.ResourceType)
rcfResourceType = Lens.field @"resourceType"
{-# DEPRECATED rcfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Core.FromJSON ResourceCountFilters where
  toJSON ResourceCountFilters {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccountId" Core..=) Core.<$> accountId,
            ("Region" Core..=) Core.<$> region,
            ("ResourceType" Core..=) Core.<$> resourceType
          ]
      )

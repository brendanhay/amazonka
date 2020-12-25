{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceDetail
  ( ResourceDetail (..),

    -- * Smart constructor
    mkResourceDetail,

    -- * Lenses
    rARN,
    rCreatedTime,
    rDescription,
    rId,
    rName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ARN as Types
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.Name as Types
import qualified Network.AWS.ServiceCatalog.Types.ResourceDetailId as Types

-- | Information about a resource.
--
-- /See:/ 'mkResourceDetail' smart constructor.
data ResourceDetail = ResourceDetail'
  { -- | The ARN of the resource.
    arn :: Core.Maybe Types.ARN,
    -- | The creation time of the resource.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the resource.
    description :: Core.Maybe Types.Description,
    -- | The identifier of the resource.
    id :: Core.Maybe Types.ResourceDetailId,
    -- | The name of the resource.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResourceDetail' value with any optional fields omitted.
mkResourceDetail ::
  ResourceDetail
mkResourceDetail =
  ResourceDetail'
    { arn = Core.Nothing,
      createdTime = Core.Nothing,
      description = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rARN :: Lens.Lens' ResourceDetail (Core.Maybe Types.ARN)
rARN = Lens.field @"arn"
{-# DEPRECATED rARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The creation time of the resource.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCreatedTime :: Lens.Lens' ResourceDetail (Core.Maybe Core.NominalDiffTime)
rCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED rCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The description of the resource.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDescription :: Lens.Lens' ResourceDetail (Core.Maybe Types.Description)
rDescription = Lens.field @"description"
{-# DEPRECATED rDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' ResourceDetail (Core.Maybe Types.ResourceDetailId)
rId = Lens.field @"id"
{-# DEPRECATED rId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' ResourceDetail (Core.Maybe Types.Name)
rName = Lens.field @"name"
{-# DEPRECATED rName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON ResourceDetail where
  parseJSON =
    Core.withObject "ResourceDetail" Core.$
      \x ->
        ResourceDetail'
          Core.<$> (x Core..:? "ARN")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.LaunchPathSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.LaunchPathSummary
  ( LaunchPathSummary (..),

    -- * Smart constructor
    mkLaunchPathSummary,

    -- * Lenses
    lpsConstraintSummaries,
    lpsId,
    lpsName,
    lpsTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ConstraintSummary as Types
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.Name as Types
import qualified Network.AWS.ServiceCatalog.Types.Tag as Types

-- | Summary information about a product path for a user.
--
-- /See:/ 'mkLaunchPathSummary' smart constructor.
data LaunchPathSummary = LaunchPathSummary'
  { -- | The constraints on the portfolio-product relationship.
    constraintSummaries :: Core.Maybe [Types.ConstraintSummary],
    -- | The identifier of the product path.
    id :: Core.Maybe Types.Id,
    -- | The name of the portfolio to which the user was assigned.
    name :: Core.Maybe Types.Name,
    -- | The tags associated with this product path.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchPathSummary' value with any optional fields omitted.
mkLaunchPathSummary ::
  LaunchPathSummary
mkLaunchPathSummary =
  LaunchPathSummary'
    { constraintSummaries = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | The constraints on the portfolio-product relationship.
--
-- /Note:/ Consider using 'constraintSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsConstraintSummaries :: Lens.Lens' LaunchPathSummary (Core.Maybe [Types.ConstraintSummary])
lpsConstraintSummaries = Lens.field @"constraintSummaries"
{-# DEPRECATED lpsConstraintSummaries "Use generic-lens or generic-optics with 'constraintSummaries' instead." #-}

-- | The identifier of the product path.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsId :: Lens.Lens' LaunchPathSummary (Core.Maybe Types.Id)
lpsId = Lens.field @"id"
{-# DEPRECATED lpsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the portfolio to which the user was assigned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsName :: Lens.Lens' LaunchPathSummary (Core.Maybe Types.Name)
lpsName = Lens.field @"name"
{-# DEPRECATED lpsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The tags associated with this product path.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsTags :: Lens.Lens' LaunchPathSummary (Core.Maybe [Types.Tag])
lpsTags = Lens.field @"tags"
{-# DEPRECATED lpsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON LaunchPathSummary where
  parseJSON =
    Core.withObject "LaunchPathSummary" Core.$
      \x ->
        LaunchPathSummary'
          Core.<$> (x Core..:? "ConstraintSummaries")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Tags")

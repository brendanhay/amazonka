{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionSummary
  ( ServiceActionSummary (..),

    -- * Smart constructor
    mkServiceActionSummary,

    -- * Lenses
    sasDefinitionType,
    sasDescription,
    sasId,
    sasName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Description as Types
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType as Types
import qualified Network.AWS.ServiceCatalog.Types.ServiceActionName as Types

-- | Detailed information about the self-service action.
--
-- /See:/ 'mkServiceActionSummary' smart constructor.
data ServiceActionSummary = ServiceActionSummary'
  { -- | The self-service action definition type. For example, @SSM_AUTOMATION@ .
    definitionType :: Core.Maybe Types.ServiceActionDefinitionType,
    -- | The self-service action description.
    description :: Core.Maybe Types.Description,
    -- | The self-service action identifier.
    id :: Core.Maybe Types.Id,
    -- | The self-service action name.
    name :: Core.Maybe Types.ServiceActionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceActionSummary' value with any optional fields omitted.
mkServiceActionSummary ::
  ServiceActionSummary
mkServiceActionSummary =
  ServiceActionSummary'
    { definitionType = Core.Nothing,
      description = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The self-service action definition type. For example, @SSM_AUTOMATION@ .
--
-- /Note:/ Consider using 'definitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasDefinitionType :: Lens.Lens' ServiceActionSummary (Core.Maybe Types.ServiceActionDefinitionType)
sasDefinitionType = Lens.field @"definitionType"
{-# DEPRECATED sasDefinitionType "Use generic-lens or generic-optics with 'definitionType' instead." #-}

-- | The self-service action description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasDescription :: Lens.Lens' ServiceActionSummary (Core.Maybe Types.Description)
sasDescription = Lens.field @"description"
{-# DEPRECATED sasDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The self-service action identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasId :: Lens.Lens' ServiceActionSummary (Core.Maybe Types.Id)
sasId = Lens.field @"id"
{-# DEPRECATED sasId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The self-service action name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sasName :: Lens.Lens' ServiceActionSummary (Core.Maybe Types.ServiceActionName)
sasName = Lens.field @"name"
{-# DEPRECATED sasName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON ServiceActionSummary where
  parseJSON =
    Core.withObject "ServiceActionSummary" Core.$
      \x ->
        ServiceActionSummary'
          Core.<$> (x Core..:? "DefinitionType")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")

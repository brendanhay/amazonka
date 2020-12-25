{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceChange
  ( ResourceChange (..),

    -- * Smart constructor
    mkResourceChange,

    -- * Lenses
    rcAction,
    rcDetails,
    rcLogicalResourceId,
    rcPhysicalResourceId,
    rcReplacement,
    rcResourceType,
    rcScope,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ChangeAction as Types
import qualified Network.AWS.ServiceCatalog.Types.LogicalResourceId as Types
import qualified Network.AWS.ServiceCatalog.Types.PhysicalResourceId as Types
import qualified Network.AWS.ServiceCatalog.Types.Replacement as Types
import qualified Network.AWS.ServiceCatalog.Types.ResourceAttribute as Types
import qualified Network.AWS.ServiceCatalog.Types.ResourceChangeDetail as Types
import qualified Network.AWS.ServiceCatalog.Types.ResourceType as Types

-- | Information about a resource change that will occur when a plan is executed.
--
-- /See:/ 'mkResourceChange' smart constructor.
data ResourceChange = ResourceChange'
  { -- | The change action.
    action :: Core.Maybe Types.ChangeAction,
    -- | Information about the resource changes.
    details :: Core.Maybe [Types.ResourceChangeDetail],
    -- | The ID of the resource, as defined in the CloudFormation template.
    logicalResourceId :: Core.Maybe Types.LogicalResourceId,
    -- | The ID of the resource, if it was already created.
    physicalResourceId :: Core.Maybe Types.PhysicalResourceId,
    -- | If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
    replacement :: Core.Maybe Types.Replacement,
    -- | The type of resource.
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The change scope.
    scope :: Core.Maybe [Types.ResourceAttribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceChange' value with any optional fields omitted.
mkResourceChange ::
  ResourceChange
mkResourceChange =
  ResourceChange'
    { action = Core.Nothing,
      details = Core.Nothing,
      logicalResourceId = Core.Nothing,
      physicalResourceId = Core.Nothing,
      replacement = Core.Nothing,
      resourceType = Core.Nothing,
      scope = Core.Nothing
    }

-- | The change action.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcAction :: Lens.Lens' ResourceChange (Core.Maybe Types.ChangeAction)
rcAction = Lens.field @"action"
{-# DEPRECATED rcAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | Information about the resource changes.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcDetails :: Lens.Lens' ResourceChange (Core.Maybe [Types.ResourceChangeDetail])
rcDetails = Lens.field @"details"
{-# DEPRECATED rcDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the resource, as defined in the CloudFormation template.
--
-- /Note:/ Consider using 'logicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcLogicalResourceId :: Lens.Lens' ResourceChange (Core.Maybe Types.LogicalResourceId)
rcLogicalResourceId = Lens.field @"logicalResourceId"
{-# DEPRECATED rcLogicalResourceId "Use generic-lens or generic-optics with 'logicalResourceId' instead." #-}

-- | The ID of the resource, if it was already created.
--
-- /Note:/ Consider using 'physicalResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPhysicalResourceId :: Lens.Lens' ResourceChange (Core.Maybe Types.PhysicalResourceId)
rcPhysicalResourceId = Lens.field @"physicalResourceId"
{-# DEPRECATED rcPhysicalResourceId "Use generic-lens or generic-optics with 'physicalResourceId' instead." #-}

-- | If the change type is @Modify@ , indicates whether the existing resource is deleted and replaced with a new one.
--
-- /Note:/ Consider using 'replacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcReplacement :: Lens.Lens' ResourceChange (Core.Maybe Types.Replacement)
rcReplacement = Lens.field @"replacement"
{-# DEPRECATED rcReplacement "Use generic-lens or generic-optics with 'replacement' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcResourceType :: Lens.Lens' ResourceChange (Core.Maybe Types.ResourceType)
rcResourceType = Lens.field @"resourceType"
{-# DEPRECATED rcResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The change scope.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcScope :: Lens.Lens' ResourceChange (Core.Maybe [Types.ResourceAttribute])
rcScope = Lens.field @"scope"
{-# DEPRECATED rcScope "Use generic-lens or generic-optics with 'scope' instead." #-}

instance Core.FromJSON ResourceChange where
  parseJSON =
    Core.withObject "ResourceChange" Core.$
      \x ->
        ResourceChange'
          Core.<$> (x Core..:? "Action")
          Core.<*> (x Core..:? "Details")
          Core.<*> (x Core..:? "LogicalResourceId")
          Core.<*> (x Core..:? "PhysicalResourceId")
          Core.<*> (x Core..:? "Replacement")
          Core.<*> (x Core..:? "ResourceType")
          Core.<*> (x Core..:? "Scope")

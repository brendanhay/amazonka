{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationExecutionTarget
  ( AssociationExecutionTarget (..)
  -- * Smart constructor
  , mkAssociationExecutionTarget
  -- * Lenses
  , aetAssociationId
  , aetAssociationVersion
  , aetDetailedStatus
  , aetExecutionId
  , aetLastExecutionDate
  , aetOutputSource
  , aetResourceId
  , aetResourceType
  , aetStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationId as Types
import qualified Network.AWS.SSM.Types.AssociationResourceType as Types
import qualified Network.AWS.SSM.Types.AssociationVersion as Types
import qualified Network.AWS.SSM.Types.DetailedStatus as Types
import qualified Network.AWS.SSM.Types.ExecutionId as Types
import qualified Network.AWS.SSM.Types.OutputSource as Types
import qualified Network.AWS.SSM.Types.ResourceId as Types
import qualified Network.AWS.SSM.Types.Status as Types

-- | Includes information about the specified association execution.
--
-- /See:/ 'mkAssociationExecutionTarget' smart constructor.
data AssociationExecutionTarget = AssociationExecutionTarget'
  { associationId :: Core.Maybe Types.AssociationId
    -- ^ The association ID.
  , associationVersion :: Core.Maybe Types.AssociationVersion
    -- ^ The association version.
  , detailedStatus :: Core.Maybe Types.DetailedStatus
    -- ^ Detailed information about the execution status.
  , executionId :: Core.Maybe Types.ExecutionId
    -- ^ The execution ID.
  , lastExecutionDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date of the last execution.
  , outputSource :: Core.Maybe Types.OutputSource
    -- ^ The location where the association details are saved.
  , resourceId :: Core.Maybe Types.ResourceId
    -- ^ The resource ID, for example, the instance ID where the association ran.
  , resourceType :: Core.Maybe Types.AssociationResourceType
    -- ^ The resource type, for example, instance.
  , status :: Core.Maybe Types.Status
    -- ^ The association execution status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssociationExecutionTarget' value with any optional fields omitted.
mkAssociationExecutionTarget
    :: AssociationExecutionTarget
mkAssociationExecutionTarget
  = AssociationExecutionTarget'{associationId = Core.Nothing,
                                associationVersion = Core.Nothing, detailedStatus = Core.Nothing,
                                executionId = Core.Nothing, lastExecutionDate = Core.Nothing,
                                outputSource = Core.Nothing, resourceId = Core.Nothing,
                                resourceType = Core.Nothing, status = Core.Nothing}

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetAssociationId :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.AssociationId)
aetAssociationId = Lens.field @"associationId"
{-# INLINEABLE aetAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetAssociationVersion :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.AssociationVersion)
aetAssociationVersion = Lens.field @"associationVersion"
{-# INLINEABLE aetAssociationVersion #-}
{-# DEPRECATED associationVersion "Use generic-lens or generic-optics with 'associationVersion' instead"  #-}

-- | Detailed information about the execution status.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetDetailedStatus :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.DetailedStatus)
aetDetailedStatus = Lens.field @"detailedStatus"
{-# INLINEABLE aetDetailedStatus #-}
{-# DEPRECATED detailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead"  #-}

-- | The execution ID.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetExecutionId :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.ExecutionId)
aetExecutionId = Lens.field @"executionId"
{-# INLINEABLE aetExecutionId #-}
{-# DEPRECATED executionId "Use generic-lens or generic-optics with 'executionId' instead"  #-}

-- | The date of the last execution.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetLastExecutionDate :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Core.NominalDiffTime)
aetLastExecutionDate = Lens.field @"lastExecutionDate"
{-# INLINEABLE aetLastExecutionDate #-}
{-# DEPRECATED lastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead"  #-}

-- | The location where the association details are saved.
--
-- /Note:/ Consider using 'outputSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetOutputSource :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.OutputSource)
aetOutputSource = Lens.field @"outputSource"
{-# INLINEABLE aetOutputSource #-}
{-# DEPRECATED outputSource "Use generic-lens or generic-optics with 'outputSource' instead"  #-}

-- | The resource ID, for example, the instance ID where the association ran.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetResourceId :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.ResourceId)
aetResourceId = Lens.field @"resourceId"
{-# INLINEABLE aetResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The resource type, for example, instance.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetResourceType :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.AssociationResourceType)
aetResourceType = Lens.field @"resourceType"
{-# INLINEABLE aetResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The association execution status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetStatus :: Lens.Lens' AssociationExecutionTarget (Core.Maybe Types.Status)
aetStatus = Lens.field @"status"
{-# INLINEABLE aetStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON AssociationExecutionTarget where
        parseJSON
          = Core.withObject "AssociationExecutionTarget" Core.$
              \ x ->
                AssociationExecutionTarget' Core.<$>
                  (x Core..:? "AssociationId") Core.<*>
                    x Core..:? "AssociationVersion"
                    Core.<*> x Core..:? "DetailedStatus"
                    Core.<*> x Core..:? "ExecutionId"
                    Core.<*> x Core..:? "LastExecutionDate"
                    Core.<*> x Core..:? "OutputSource"
                    Core.<*> x Core..:? "ResourceId"
                    Core.<*> x Core..:? "ResourceType"
                    Core.<*> x Core..:? "Status"

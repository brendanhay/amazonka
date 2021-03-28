{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationExecution
  ( AssociationExecution (..)
  -- * Smart constructor
  , mkAssociationExecution
  -- * Lenses
  , aeAssociationId
  , aeAssociationVersion
  , aeCreatedTime
  , aeDetailedStatus
  , aeExecutionId
  , aeLastExecutionDate
  , aeResourceCountByStatus
  , aeStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationId as Types
import qualified Network.AWS.SSM.Types.AssociationVersion as Types
import qualified Network.AWS.SSM.Types.ExecutionId as Types
import qualified Network.AWS.SSM.Types.ResourceCountByStatus as Types
import qualified Network.AWS.SSM.Types.StatusName as Types

-- | Includes information about the specified association.
--
-- /See:/ 'mkAssociationExecution' smart constructor.
data AssociationExecution = AssociationExecution'
  { associationId :: Core.Maybe Types.AssociationId
    -- ^ The association ID.
  , associationVersion :: Core.Maybe Types.AssociationVersion
    -- ^ The association version.
  , createdTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the execution started.
  , detailedStatus :: Core.Maybe Types.StatusName
    -- ^ Detailed status information about the execution.
  , executionId :: Core.Maybe Types.ExecutionId
    -- ^ The execution ID for the association.
  , lastExecutionDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date of the last execution.
  , resourceCountByStatus :: Core.Maybe Types.ResourceCountByStatus
    -- ^ An aggregate status of the resources in the execution based on the status type.
  , status :: Core.Maybe Types.StatusName
    -- ^ The status of the association execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssociationExecution' value with any optional fields omitted.
mkAssociationExecution
    :: AssociationExecution
mkAssociationExecution
  = AssociationExecution'{associationId = Core.Nothing,
                          associationVersion = Core.Nothing, createdTime = Core.Nothing,
                          detailedStatus = Core.Nothing, executionId = Core.Nothing,
                          lastExecutionDate = Core.Nothing,
                          resourceCountByStatus = Core.Nothing, status = Core.Nothing}

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAssociationId :: Lens.Lens' AssociationExecution (Core.Maybe Types.AssociationId)
aeAssociationId = Lens.field @"associationId"
{-# INLINEABLE aeAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAssociationVersion :: Lens.Lens' AssociationExecution (Core.Maybe Types.AssociationVersion)
aeAssociationVersion = Lens.field @"associationVersion"
{-# INLINEABLE aeAssociationVersion #-}
{-# DEPRECATED associationVersion "Use generic-lens or generic-optics with 'associationVersion' instead"  #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeCreatedTime :: Lens.Lens' AssociationExecution (Core.Maybe Core.NominalDiffTime)
aeCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE aeCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | Detailed status information about the execution.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeDetailedStatus :: Lens.Lens' AssociationExecution (Core.Maybe Types.StatusName)
aeDetailedStatus = Lens.field @"detailedStatus"
{-# INLINEABLE aeDetailedStatus #-}
{-# DEPRECATED detailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead"  #-}

-- | The execution ID for the association.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutionId :: Lens.Lens' AssociationExecution (Core.Maybe Types.ExecutionId)
aeExecutionId = Lens.field @"executionId"
{-# INLINEABLE aeExecutionId #-}
{-# DEPRECATED executionId "Use generic-lens or generic-optics with 'executionId' instead"  #-}

-- | The date of the last execution.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeLastExecutionDate :: Lens.Lens' AssociationExecution (Core.Maybe Core.NominalDiffTime)
aeLastExecutionDate = Lens.field @"lastExecutionDate"
{-# INLINEABLE aeLastExecutionDate #-}
{-# DEPRECATED lastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead"  #-}

-- | An aggregate status of the resources in the execution based on the status type.
--
-- /Note:/ Consider using 'resourceCountByStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeResourceCountByStatus :: Lens.Lens' AssociationExecution (Core.Maybe Types.ResourceCountByStatus)
aeResourceCountByStatus = Lens.field @"resourceCountByStatus"
{-# INLINEABLE aeResourceCountByStatus #-}
{-# DEPRECATED resourceCountByStatus "Use generic-lens or generic-optics with 'resourceCountByStatus' instead"  #-}

-- | The status of the association execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStatus :: Lens.Lens' AssociationExecution (Core.Maybe Types.StatusName)
aeStatus = Lens.field @"status"
{-# INLINEABLE aeStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON AssociationExecution where
        parseJSON
          = Core.withObject "AssociationExecution" Core.$
              \ x ->
                AssociationExecution' Core.<$>
                  (x Core..:? "AssociationId") Core.<*>
                    x Core..:? "AssociationVersion"
                    Core.<*> x Core..:? "CreatedTime"
                    Core.<*> x Core..:? "DetailedStatus"
                    Core.<*> x Core..:? "ExecutionId"
                    Core.<*> x Core..:? "LastExecutionDate"
                    Core.<*> x Core..:? "ResourceCountByStatus"
                    Core.<*> x Core..:? "Status"

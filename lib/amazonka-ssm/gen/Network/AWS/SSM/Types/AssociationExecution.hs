{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecution
  ( AssociationExecution (..),

    -- * Smart constructor
    mkAssociationExecution,

    -- * Lenses
    aeAssociationId,
    aeAssociationVersion,
    aeCreatedTime,
    aeDetailedStatus,
    aeExecutionId,
    aeLastExecutionDate,
    aeResourceCountByStatus,
    aeStatus,
  )
where

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
  { -- | The association ID.
    associationId :: Core.Maybe Types.AssociationId,
    -- | The association version.
    associationVersion :: Core.Maybe Types.AssociationVersion,
    -- | The time the execution started.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | Detailed status information about the execution.
    detailedStatus :: Core.Maybe Types.StatusName,
    -- | The execution ID for the association.
    executionId :: Core.Maybe Types.ExecutionId,
    -- | The date of the last execution.
    lastExecutionDate :: Core.Maybe Core.NominalDiffTime,
    -- | An aggregate status of the resources in the execution based on the status type.
    resourceCountByStatus :: Core.Maybe Types.ResourceCountByStatus,
    -- | The status of the association execution.
    status :: Core.Maybe Types.StatusName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssociationExecution' value with any optional fields omitted.
mkAssociationExecution ::
  AssociationExecution
mkAssociationExecution =
  AssociationExecution'
    { associationId = Core.Nothing,
      associationVersion = Core.Nothing,
      createdTime = Core.Nothing,
      detailedStatus = Core.Nothing,
      executionId = Core.Nothing,
      lastExecutionDate = Core.Nothing,
      resourceCountByStatus = Core.Nothing,
      status = Core.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAssociationId :: Lens.Lens' AssociationExecution (Core.Maybe Types.AssociationId)
aeAssociationId = Lens.field @"associationId"
{-# DEPRECATED aeAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAssociationVersion :: Lens.Lens' AssociationExecution (Core.Maybe Types.AssociationVersion)
aeAssociationVersion = Lens.field @"associationVersion"
{-# DEPRECATED aeAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeCreatedTime :: Lens.Lens' AssociationExecution (Core.Maybe Core.NominalDiffTime)
aeCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED aeCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Detailed status information about the execution.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeDetailedStatus :: Lens.Lens' AssociationExecution (Core.Maybe Types.StatusName)
aeDetailedStatus = Lens.field @"detailedStatus"
{-# DEPRECATED aeDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

-- | The execution ID for the association.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutionId :: Lens.Lens' AssociationExecution (Core.Maybe Types.ExecutionId)
aeExecutionId = Lens.field @"executionId"
{-# DEPRECATED aeExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The date of the last execution.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeLastExecutionDate :: Lens.Lens' AssociationExecution (Core.Maybe Core.NominalDiffTime)
aeLastExecutionDate = Lens.field @"lastExecutionDate"
{-# DEPRECATED aeLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | An aggregate status of the resources in the execution based on the status type.
--
-- /Note:/ Consider using 'resourceCountByStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeResourceCountByStatus :: Lens.Lens' AssociationExecution (Core.Maybe Types.ResourceCountByStatus)
aeResourceCountByStatus = Lens.field @"resourceCountByStatus"
{-# DEPRECATED aeResourceCountByStatus "Use generic-lens or generic-optics with 'resourceCountByStatus' instead." #-}

-- | The status of the association execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStatus :: Lens.Lens' AssociationExecution (Core.Maybe Types.StatusName)
aeStatus = Lens.field @"status"
{-# DEPRECATED aeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON AssociationExecution where
  parseJSON =
    Core.withObject "AssociationExecution" Core.$
      \x ->
        AssociationExecution'
          Core.<$> (x Core..:? "AssociationId")
          Core.<*> (x Core..:? "AssociationVersion")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "DetailedStatus")
          Core.<*> (x Core..:? "ExecutionId")
          Core.<*> (x Core..:? "LastExecutionDate")
          Core.<*> (x Core..:? "ResourceCountByStatus")
          Core.<*> (x Core..:? "Status")

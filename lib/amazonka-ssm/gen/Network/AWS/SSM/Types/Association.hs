{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Association
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Association
  ( Association (..),

    -- * Smart constructor
    mkAssociation,

    -- * Lenses
    aAssociationId,
    aAssociationName,
    aAssociationVersion,
    aDocumentVersion,
    aInstanceId,
    aLastExecutionDate,
    aName,
    aOverview,
    aScheduleExpression,
    aTargets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationId as Types
import qualified Network.AWS.SSM.Types.AssociationName as Types
import qualified Network.AWS.SSM.Types.AssociationOverview as Types
import qualified Network.AWS.SSM.Types.AssociationVersion as Types
import qualified Network.AWS.SSM.Types.DocumentARN as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types
import qualified Network.AWS.SSM.Types.ScheduleExpression as Types
import qualified Network.AWS.SSM.Types.Target as Types

-- | Describes an association of a Systems Manager document and an instance.
--
-- /See:/ 'mkAssociation' smart constructor.
data Association = Association'
  { -- | The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
    associationId :: Core.Maybe Types.AssociationId,
    -- | The association name.
    associationName :: Core.Maybe Types.AssociationName,
    -- | The association version.
    associationVersion :: Core.Maybe Types.AssociationVersion,
    -- | The version of the document used in the association.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.InstanceId,
    -- | The date on which the association was last run.
    lastExecutionDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Types.DocumentARN,
    -- | Information about the association.
    overview :: Core.Maybe Types.AssociationOverview,
    -- | A cron expression that specifies a schedule when the association runs.
    scheduleExpression :: Core.Maybe Types.ScheduleExpression,
    -- | The instances targeted by the request to create an association.
    targets :: Core.Maybe [Types.Target]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Association' value with any optional fields omitted.
mkAssociation ::
  Association
mkAssociation =
  Association'
    { associationId = Core.Nothing,
      associationName = Core.Nothing,
      associationVersion = Core.Nothing,
      documentVersion = Core.Nothing,
      instanceId = Core.Nothing,
      lastExecutionDate = Core.Nothing,
      name = Core.Nothing,
      overview = Core.Nothing,
      scheduleExpression = Core.Nothing,
      targets = Core.Nothing
    }

-- | The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssociationId :: Lens.Lens' Association (Core.Maybe Types.AssociationId)
aAssociationId = Lens.field @"associationId"
{-# DEPRECATED aAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The association name.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssociationName :: Lens.Lens' Association (Core.Maybe Types.AssociationName)
aAssociationName = Lens.field @"associationName"
{-# DEPRECATED aAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssociationVersion :: Lens.Lens' Association (Core.Maybe Types.AssociationVersion)
aAssociationVersion = Lens.field @"associationVersion"
{-# DEPRECATED aAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The version of the document used in the association.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDocumentVersion :: Lens.Lens' Association (Core.Maybe Types.DocumentVersion)
aDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED aDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInstanceId :: Lens.Lens' Association (Core.Maybe Types.InstanceId)
aInstanceId = Lens.field @"instanceId"
{-# DEPRECATED aInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The date on which the association was last run.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLastExecutionDate :: Lens.Lens' Association (Core.Maybe Core.NominalDiffTime)
aLastExecutionDate = Lens.field @"lastExecutionDate"
{-# DEPRECATED aLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Association (Core.Maybe Types.DocumentARN)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information about the association.
--
-- /Note:/ Consider using 'overview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aOverview :: Lens.Lens' Association (Core.Maybe Types.AssociationOverview)
aOverview = Lens.field @"overview"
{-# DEPRECATED aOverview "Use generic-lens or generic-optics with 'overview' instead." #-}

-- | A cron expression that specifies a schedule when the association runs.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aScheduleExpression :: Lens.Lens' Association (Core.Maybe Types.ScheduleExpression)
aScheduleExpression = Lens.field @"scheduleExpression"
{-# DEPRECATED aScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The instances targeted by the request to create an association.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTargets :: Lens.Lens' Association (Core.Maybe [Types.Target])
aTargets = Lens.field @"targets"
{-# DEPRECATED aTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Core.FromJSON Association where
  parseJSON =
    Core.withObject "Association" Core.$
      \x ->
        Association'
          Core.<$> (x Core..:? "AssociationId")
          Core.<*> (x Core..:? "AssociationName")
          Core.<*> (x Core..:? "AssociationVersion")
          Core.<*> (x Core..:? "DocumentVersion")
          Core.<*> (x Core..:? "InstanceId")
          Core.<*> (x Core..:? "LastExecutionDate")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Overview")
          Core.<*> (x Core..:? "ScheduleExpression")
          Core.<*> (x Core..:? "Targets")

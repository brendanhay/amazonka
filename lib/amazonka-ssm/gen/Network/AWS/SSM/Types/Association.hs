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
    assAssociationId,
    assInstanceId,
    assOverview,
    assLastExecutionDate,
    assScheduleExpression,
    assName,
    assTargets,
    assDocumentVersion,
    assAssociationVersion,
    assAssociationName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationOverview
import Network.AWS.SSM.Types.Target

-- | Describes an association of a Systems Manager document and an instance.
--
-- /See:/ 'mkAssociation' smart constructor.
data Association = Association'
  { associationId ::
      Lude.Maybe Lude.Text,
    instanceId :: Lude.Maybe Lude.Text,
    overview :: Lude.Maybe AssociationOverview,
    lastExecutionDate :: Lude.Maybe Lude.Timestamp,
    scheduleExpression :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    targets :: Lude.Maybe [Target],
    documentVersion :: Lude.Maybe Lude.Text,
    associationVersion :: Lude.Maybe Lude.Text,
    associationName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Association' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
-- * 'associationName' - The association name.
-- * 'associationVersion' - The association version.
-- * 'documentVersion' - The version of the document used in the association.
-- * 'instanceId' - The ID of the instance.
-- * 'lastExecutionDate' - The date on which the association was last run.
-- * 'name' - The name of the Systems Manager document.
-- * 'overview' - Information about the association.
-- * 'scheduleExpression' - A cron expression that specifies a schedule when the association runs.
-- * 'targets' - The instances targeted by the request to create an association.
mkAssociation ::
  Association
mkAssociation =
  Association'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      overview = Lude.Nothing,
      lastExecutionDate = Lude.Nothing,
      scheduleExpression = Lude.Nothing,
      name = Lude.Nothing,
      targets = Lude.Nothing,
      documentVersion = Lude.Nothing,
      associationVersion = Lude.Nothing,
      associationName = Lude.Nothing
    }

-- | The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAssociationId :: Lens.Lens' Association (Lude.Maybe Lude.Text)
assAssociationId = Lens.lens (associationId :: Association -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: Association)
{-# DEPRECATED assAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assInstanceId :: Lens.Lens' Association (Lude.Maybe Lude.Text)
assInstanceId = Lens.lens (instanceId :: Association -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Association)
{-# DEPRECATED assInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Information about the association.
--
-- /Note:/ Consider using 'overview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assOverview :: Lens.Lens' Association (Lude.Maybe AssociationOverview)
assOverview = Lens.lens (overview :: Association -> Lude.Maybe AssociationOverview) (\s a -> s {overview = a} :: Association)
{-# DEPRECATED assOverview "Use generic-lens or generic-optics with 'overview' instead." #-}

-- | The date on which the association was last run.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assLastExecutionDate :: Lens.Lens' Association (Lude.Maybe Lude.Timestamp)
assLastExecutionDate = Lens.lens (lastExecutionDate :: Association -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastExecutionDate = a} :: Association)
{-# DEPRECATED assLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | A cron expression that specifies a schedule when the association runs.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assScheduleExpression :: Lens.Lens' Association (Lude.Maybe Lude.Text)
assScheduleExpression = Lens.lens (scheduleExpression :: Association -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: Association)
{-# DEPRECATED assScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assName :: Lens.Lens' Association (Lude.Maybe Lude.Text)
assName = Lens.lens (name :: Association -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Association)
{-# DEPRECATED assName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The instances targeted by the request to create an association.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assTargets :: Lens.Lens' Association (Lude.Maybe [Target])
assTargets = Lens.lens (targets :: Association -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: Association)
{-# DEPRECATED assTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The version of the document used in the association.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assDocumentVersion :: Lens.Lens' Association (Lude.Maybe Lude.Text)
assDocumentVersion = Lens.lens (documentVersion :: Association -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: Association)
{-# DEPRECATED assDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAssociationVersion :: Lens.Lens' Association (Lude.Maybe Lude.Text)
assAssociationVersion = Lens.lens (associationVersion :: Association -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: Association)
{-# DEPRECATED assAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The association name.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assAssociationName :: Lens.Lens' Association (Lude.Maybe Lude.Text)
assAssociationName = Lens.lens (associationName :: Association -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: Association)
{-# DEPRECATED assAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

instance Lude.FromJSON Association where
  parseJSON =
    Lude.withObject
      "Association"
      ( \x ->
          Association'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Overview")
            Lude.<*> (x Lude..:? "LastExecutionDate")
            Lude.<*> (x Lude..:? "ScheduleExpression")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "AssociationVersion")
            Lude.<*> (x Lude..:? "AssociationName")
      )

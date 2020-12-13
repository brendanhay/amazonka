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
    afAssociationId,
    afInstanceId,
    afOverview,
    afLastExecutionDate,
    afScheduleExpression,
    afName,
    afTargets,
    afDocumentVersion,
    afAssociationVersion,
    afAssociationName,
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
  { -- | The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
    associationId :: Lude.Maybe Lude.Text,
    -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | Information about the association.
    overview :: Lude.Maybe AssociationOverview,
    -- | The date on which the association was last run.
    lastExecutionDate :: Lude.Maybe Lude.Timestamp,
    -- | A cron expression that specifies a schedule when the association runs.
    scheduleExpression :: Lude.Maybe Lude.Text,
    -- | The name of the Systems Manager document.
    name :: Lude.Maybe Lude.Text,
    -- | The instances targeted by the request to create an association.
    targets :: Lude.Maybe [Target],
    -- | The version of the document used in the association.
    documentVersion :: Lude.Maybe Lude.Text,
    -- | The association version.
    associationVersion :: Lude.Maybe Lude.Text,
    -- | The association name.
    associationName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Association' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID created by the system when you create an association. An association is a binding between a document and a set of targets with a schedule.
-- * 'instanceId' - The ID of the instance.
-- * 'overview' - Information about the association.
-- * 'lastExecutionDate' - The date on which the association was last run.
-- * 'scheduleExpression' - A cron expression that specifies a schedule when the association runs.
-- * 'name' - The name of the Systems Manager document.
-- * 'targets' - The instances targeted by the request to create an association.
-- * 'documentVersion' - The version of the document used in the association.
-- * 'associationVersion' - The association version.
-- * 'associationName' - The association name.
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
afAssociationId :: Lens.Lens' Association (Lude.Maybe Lude.Text)
afAssociationId = Lens.lens (associationId :: Association -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: Association)
{-# DEPRECATED afAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afInstanceId :: Lens.Lens' Association (Lude.Maybe Lude.Text)
afInstanceId = Lens.lens (instanceId :: Association -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: Association)
{-# DEPRECATED afInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Information about the association.
--
-- /Note:/ Consider using 'overview' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afOverview :: Lens.Lens' Association (Lude.Maybe AssociationOverview)
afOverview = Lens.lens (overview :: Association -> Lude.Maybe AssociationOverview) (\s a -> s {overview = a} :: Association)
{-# DEPRECATED afOverview "Use generic-lens or generic-optics with 'overview' instead." #-}

-- | The date on which the association was last run.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afLastExecutionDate :: Lens.Lens' Association (Lude.Maybe Lude.Timestamp)
afLastExecutionDate = Lens.lens (lastExecutionDate :: Association -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastExecutionDate = a} :: Association)
{-# DEPRECATED afLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | A cron expression that specifies a schedule when the association runs.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afScheduleExpression :: Lens.Lens' Association (Lude.Maybe Lude.Text)
afScheduleExpression = Lens.lens (scheduleExpression :: Association -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: Association)
{-# DEPRECATED afScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afName :: Lens.Lens' Association (Lude.Maybe Lude.Text)
afName = Lens.lens (name :: Association -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Association)
{-# DEPRECATED afName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The instances targeted by the request to create an association.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afTargets :: Lens.Lens' Association (Lude.Maybe [Target])
afTargets = Lens.lens (targets :: Association -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: Association)
{-# DEPRECATED afTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The version of the document used in the association.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afDocumentVersion :: Lens.Lens' Association (Lude.Maybe Lude.Text)
afDocumentVersion = Lens.lens (documentVersion :: Association -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: Association)
{-# DEPRECATED afDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAssociationVersion :: Lens.Lens' Association (Lude.Maybe Lude.Text)
afAssociationVersion = Lens.lens (associationVersion :: Association -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: Association)
{-# DEPRECATED afAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The association name.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afAssociationName :: Lens.Lens' Association (Lude.Maybe Lude.Text)
afAssociationName = Lens.lens (associationName :: Association -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: Association)
{-# DEPRECATED afAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

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

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
    aeDetailedStatus,
    aeStatus,
    aeExecutionId,
    aeCreatedTime,
    aeResourceCountByStatus,
    aeLastExecutionDate,
    aeAssociationVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Includes information about the specified association.
--
-- /See:/ 'mkAssociationExecution' smart constructor.
data AssociationExecution = AssociationExecution'
  { associationId ::
      Lude.Maybe Lude.Text,
    detailedStatus :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    executionId :: Lude.Maybe Lude.Text,
    createdTime :: Lude.Maybe Lude.Timestamp,
    resourceCountByStatus :: Lude.Maybe Lude.Text,
    lastExecutionDate :: Lude.Maybe Lude.Timestamp,
    associationVersion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationExecution' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID.
-- * 'associationVersion' - The association version.
-- * 'createdTime' - The time the execution started.
-- * 'detailedStatus' - Detailed status information about the execution.
-- * 'executionId' - The execution ID for the association.
-- * 'lastExecutionDate' - The date of the last execution.
-- * 'resourceCountByStatus' - An aggregate status of the resources in the execution based on the status type.
-- * 'status' - The status of the association execution.
mkAssociationExecution ::
  AssociationExecution
mkAssociationExecution =
  AssociationExecution'
    { associationId = Lude.Nothing,
      detailedStatus = Lude.Nothing,
      status = Lude.Nothing,
      executionId = Lude.Nothing,
      createdTime = Lude.Nothing,
      resourceCountByStatus = Lude.Nothing,
      lastExecutionDate = Lude.Nothing,
      associationVersion = Lude.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAssociationId :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Text)
aeAssociationId = Lens.lens (associationId :: AssociationExecution -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociationExecution)
{-# DEPRECATED aeAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | Detailed status information about the execution.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeDetailedStatus :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Text)
aeDetailedStatus = Lens.lens (detailedStatus :: AssociationExecution -> Lude.Maybe Lude.Text) (\s a -> s {detailedStatus = a} :: AssociationExecution)
{-# DEPRECATED aeDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

-- | The status of the association execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeStatus :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Text)
aeStatus = Lens.lens (status :: AssociationExecution -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: AssociationExecution)
{-# DEPRECATED aeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The execution ID for the association.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeExecutionId :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Text)
aeExecutionId = Lens.lens (executionId :: AssociationExecution -> Lude.Maybe Lude.Text) (\s a -> s {executionId = a} :: AssociationExecution)
{-# DEPRECATED aeExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The time the execution started.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeCreatedTime :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Timestamp)
aeCreatedTime = Lens.lens (createdTime :: AssociationExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: AssociationExecution)
{-# DEPRECATED aeCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | An aggregate status of the resources in the execution based on the status type.
--
-- /Note:/ Consider using 'resourceCountByStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeResourceCountByStatus :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Text)
aeResourceCountByStatus = Lens.lens (resourceCountByStatus :: AssociationExecution -> Lude.Maybe Lude.Text) (\s a -> s {resourceCountByStatus = a} :: AssociationExecution)
{-# DEPRECATED aeResourceCountByStatus "Use generic-lens or generic-optics with 'resourceCountByStatus' instead." #-}

-- | The date of the last execution.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeLastExecutionDate :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Timestamp)
aeLastExecutionDate = Lens.lens (lastExecutionDate :: AssociationExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastExecutionDate = a} :: AssociationExecution)
{-# DEPRECATED aeLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeAssociationVersion :: Lens.Lens' AssociationExecution (Lude.Maybe Lude.Text)
aeAssociationVersion = Lens.lens (associationVersion :: AssociationExecution -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: AssociationExecution)
{-# DEPRECATED aeAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

instance Lude.FromJSON AssociationExecution where
  parseJSON =
    Lude.withObject
      "AssociationExecution"
      ( \x ->
          AssociationExecution'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "DetailedStatus")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ExecutionId")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "ResourceCountByStatus")
            Lude.<*> (x Lude..:? "LastExecutionDate")
            Lude.<*> (x Lude..:? "AssociationVersion")
      )

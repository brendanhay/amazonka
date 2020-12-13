{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTarget
  ( AssociationExecutionTarget (..),

    -- * Smart constructor
    mkAssociationExecutionTarget,

    -- * Lenses
    aetAssociationId,
    aetDetailedStatus,
    aetStatus,
    aetExecutionId,
    aetResourceId,
    aetResourceType,
    aetOutputSource,
    aetLastExecutionDate,
    aetAssociationVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OutputSource

-- | Includes information about the specified association execution.
--
-- /See:/ 'mkAssociationExecutionTarget' smart constructor.
data AssociationExecutionTarget = AssociationExecutionTarget'
  { -- | The association ID.
    associationId :: Lude.Maybe Lude.Text,
    -- | Detailed information about the execution status.
    detailedStatus :: Lude.Maybe Lude.Text,
    -- | The association execution status.
    status :: Lude.Maybe Lude.Text,
    -- | The execution ID.
    executionId :: Lude.Maybe Lude.Text,
    -- | The resource ID, for example, the instance ID where the association ran.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The resource type, for example, instance.
    resourceType :: Lude.Maybe Lude.Text,
    -- | The location where the association details are saved.
    outputSource :: Lude.Maybe OutputSource,
    -- | The date of the last execution.
    lastExecutionDate :: Lude.Maybe Lude.Timestamp,
    -- | The association version.
    associationVersion :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationExecutionTarget' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID.
-- * 'detailedStatus' - Detailed information about the execution status.
-- * 'status' - The association execution status.
-- * 'executionId' - The execution ID.
-- * 'resourceId' - The resource ID, for example, the instance ID where the association ran.
-- * 'resourceType' - The resource type, for example, instance.
-- * 'outputSource' - The location where the association details are saved.
-- * 'lastExecutionDate' - The date of the last execution.
-- * 'associationVersion' - The association version.
mkAssociationExecutionTarget ::
  AssociationExecutionTarget
mkAssociationExecutionTarget =
  AssociationExecutionTarget'
    { associationId = Lude.Nothing,
      detailedStatus = Lude.Nothing,
      status = Lude.Nothing,
      executionId = Lude.Nothing,
      resourceId = Lude.Nothing,
      resourceType = Lude.Nothing,
      outputSource = Lude.Nothing,
      lastExecutionDate = Lude.Nothing,
      associationVersion = Lude.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetAssociationId :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Text)
aetAssociationId = Lens.lens (associationId :: AssociationExecutionTarget -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | Detailed information about the execution status.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetDetailedStatus :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Text)
aetDetailedStatus = Lens.lens (detailedStatus :: AssociationExecutionTarget -> Lude.Maybe Lude.Text) (\s a -> s {detailedStatus = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

-- | The association execution status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetStatus :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Text)
aetStatus = Lens.lens (status :: AssociationExecutionTarget -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The execution ID.
--
-- /Note:/ Consider using 'executionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetExecutionId :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Text)
aetExecutionId = Lens.lens (executionId :: AssociationExecutionTarget -> Lude.Maybe Lude.Text) (\s a -> s {executionId = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetExecutionId "Use generic-lens or generic-optics with 'executionId' instead." #-}

-- | The resource ID, for example, the instance ID where the association ran.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetResourceId :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Text)
aetResourceId = Lens.lens (resourceId :: AssociationExecutionTarget -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The resource type, for example, instance.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetResourceType :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Text)
aetResourceType = Lens.lens (resourceType :: AssociationExecutionTarget -> Lude.Maybe Lude.Text) (\s a -> s {resourceType = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The location where the association details are saved.
--
-- /Note:/ Consider using 'outputSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetOutputSource :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe OutputSource)
aetOutputSource = Lens.lens (outputSource :: AssociationExecutionTarget -> Lude.Maybe OutputSource) (\s a -> s {outputSource = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetOutputSource "Use generic-lens or generic-optics with 'outputSource' instead." #-}

-- | The date of the last execution.
--
-- /Note:/ Consider using 'lastExecutionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetLastExecutionDate :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Timestamp)
aetLastExecutionDate = Lens.lens (lastExecutionDate :: AssociationExecutionTarget -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastExecutionDate = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetLastExecutionDate "Use generic-lens or generic-optics with 'lastExecutionDate' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aetAssociationVersion :: Lens.Lens' AssociationExecutionTarget (Lude.Maybe Lude.Text)
aetAssociationVersion = Lens.lens (associationVersion :: AssociationExecutionTarget -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: AssociationExecutionTarget)
{-# DEPRECATED aetAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

instance Lude.FromJSON AssociationExecutionTarget where
  parseJSON =
    Lude.withObject
      "AssociationExecutionTarget"
      ( \x ->
          AssociationExecutionTarget'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "DetailedStatus")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ExecutionId")
            Lude.<*> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "ResourceType")
            Lude.<*> (x Lude..:? "OutputSource")
            Lude.<*> (x Lude..:? "LastExecutionDate")
            Lude.<*> (x Lude..:? "AssociationVersion")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
  ( ReplicationTaskIndividualAssessment (..),

    -- * Smart constructor
    mkReplicationTaskIndividualAssessment,

    -- * Lenses
    rtiaStatus,
    rtiaReplicationTaskIndividualAssessmentStartDate,
    rtiaIndividualAssessmentName,
    rtiaReplicationTaskIndividualAssessmentARN,
    rtiaReplicationTaskAssessmentRunARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that describes an individual assessment from a premigration assessment run.
--
-- /See:/ 'mkReplicationTaskIndividualAssessment' smart constructor.
data ReplicationTaskIndividualAssessment = ReplicationTaskIndividualAssessment'
  { -- | Individual assessment status.
    --
    -- This status can have one of the following values:
    --
    --     * @"cancelled"@
    --
    --
    --     * @"error"@
    --
    --
    --     * @"failed"@
    --
    --
    --     * @"passed"@
    --
    --
    --     * @"pending"@
    --
    --
    --     * @"running"@
    status :: Lude.Maybe Lude.Text,
    -- | Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
    replicationTaskIndividualAssessmentStartDate :: Lude.Maybe Lude.Timestamp,
    -- | Name of this individual assessment.
    individualAssessmentName :: Lude.Maybe Lude.Text,
    -- | Amazon Resource Name (ARN) of this individual assessment.
    replicationTaskIndividualAssessmentARN :: Lude.Maybe Lude.Text,
    -- | ARN of the premigration assessment run that is created to run this individual assessment.
    replicationTaskAssessmentRunARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTaskIndividualAssessment' with the minimum fields required to make a request.
--
-- * 'status' - Individual assessment status.
--
-- This status can have one of the following values:
--
--     * @"cancelled"@
--
--
--     * @"error"@
--
--
--     * @"failed"@
--
--
--     * @"passed"@
--
--
--     * @"pending"@
--
--
--     * @"running"@
--
--
-- * 'replicationTaskIndividualAssessmentStartDate' - Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
-- * 'individualAssessmentName' - Name of this individual assessment.
-- * 'replicationTaskIndividualAssessmentARN' - Amazon Resource Name (ARN) of this individual assessment.
-- * 'replicationTaskAssessmentRunARN' - ARN of the premigration assessment run that is created to run this individual assessment.
mkReplicationTaskIndividualAssessment ::
  ReplicationTaskIndividualAssessment
mkReplicationTaskIndividualAssessment =
  ReplicationTaskIndividualAssessment'
    { status = Lude.Nothing,
      replicationTaskIndividualAssessmentStartDate =
        Lude.Nothing,
      individualAssessmentName = Lude.Nothing,
      replicationTaskIndividualAssessmentARN = Lude.Nothing,
      replicationTaskAssessmentRunARN = Lude.Nothing
    }

-- | Individual assessment status.
--
-- This status can have one of the following values:
--
--     * @"cancelled"@
--
--
--     * @"error"@
--
--
--     * @"failed"@
--
--
--     * @"passed"@
--
--
--     * @"pending"@
--
--
--     * @"running"@
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaStatus :: Lens.Lens' ReplicationTaskIndividualAssessment (Lude.Maybe Lude.Text)
rtiaStatus = Lens.lens (status :: ReplicationTaskIndividualAssessment -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ReplicationTaskIndividualAssessment)
{-# DEPRECATED rtiaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessmentStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskIndividualAssessmentStartDate :: Lens.Lens' ReplicationTaskIndividualAssessment (Lude.Maybe Lude.Timestamp)
rtiaReplicationTaskIndividualAssessmentStartDate = Lens.lens (replicationTaskIndividualAssessmentStartDate :: ReplicationTaskIndividualAssessment -> Lude.Maybe Lude.Timestamp) (\s a -> s {replicationTaskIndividualAssessmentStartDate = a} :: ReplicationTaskIndividualAssessment)
{-# DEPRECATED rtiaReplicationTaskIndividualAssessmentStartDate "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessmentStartDate' instead." #-}

-- | Name of this individual assessment.
--
-- /Note:/ Consider using 'individualAssessmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaIndividualAssessmentName :: Lens.Lens' ReplicationTaskIndividualAssessment (Lude.Maybe Lude.Text)
rtiaIndividualAssessmentName = Lens.lens (individualAssessmentName :: ReplicationTaskIndividualAssessment -> Lude.Maybe Lude.Text) (\s a -> s {individualAssessmentName = a} :: ReplicationTaskIndividualAssessment)
{-# DEPRECATED rtiaIndividualAssessmentName "Use generic-lens or generic-optics with 'individualAssessmentName' instead." #-}

-- | Amazon Resource Name (ARN) of this individual assessment.
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessmentARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskIndividualAssessmentARN :: Lens.Lens' ReplicationTaskIndividualAssessment (Lude.Maybe Lude.Text)
rtiaReplicationTaskIndividualAssessmentARN = Lens.lens (replicationTaskIndividualAssessmentARN :: ReplicationTaskIndividualAssessment -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskIndividualAssessmentARN = a} :: ReplicationTaskIndividualAssessment)
{-# DEPRECATED rtiaReplicationTaskIndividualAssessmentARN "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessmentARN' instead." #-}

-- | ARN of the premigration assessment run that is created to run this individual assessment.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRunARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskAssessmentRunARN :: Lens.Lens' ReplicationTaskIndividualAssessment (Lude.Maybe Lude.Text)
rtiaReplicationTaskAssessmentRunARN = Lens.lens (replicationTaskAssessmentRunARN :: ReplicationTaskIndividualAssessment -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskAssessmentRunARN = a} :: ReplicationTaskIndividualAssessment)
{-# DEPRECATED rtiaReplicationTaskAssessmentRunARN "Use generic-lens or generic-optics with 'replicationTaskAssessmentRunARN' instead." #-}

instance Lude.FromJSON ReplicationTaskIndividualAssessment where
  parseJSON =
    Lude.withObject
      "ReplicationTaskIndividualAssessment"
      ( \x ->
          ReplicationTaskIndividualAssessment'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ReplicationTaskIndividualAssessmentStartDate")
            Lude.<*> (x Lude..:? "IndividualAssessmentName")
            Lude.<*> (x Lude..:? "ReplicationTaskIndividualAssessmentArn")
            Lude.<*> (x Lude..:? "ReplicationTaskAssessmentRunArn")
      )

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
    rtiaIndividualAssessmentName,
    rtiaReplicationTaskAssessmentRunArn,
    rtiaReplicationTaskIndividualAssessmentArn,
    rtiaReplicationTaskIndividualAssessmentStartDate,
    rtiaStatus,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that describes an individual assessment from a premigration assessment run.
--
-- /See:/ 'mkReplicationTaskIndividualAssessment' smart constructor.
data ReplicationTaskIndividualAssessment = ReplicationTaskIndividualAssessment'
  { -- | Name of this individual assessment.
    individualAssessmentName :: Core.Maybe Types.String,
    -- | ARN of the premigration assessment run that is created to run this individual assessment.
    replicationTaskAssessmentRunArn :: Core.Maybe Types.String,
    -- | Amazon Resource Name (ARN) of this individual assessment.
    replicationTaskIndividualAssessmentArn :: Core.Maybe Types.String,
    -- | Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
    replicationTaskIndividualAssessmentStartDate :: Core.Maybe Core.NominalDiffTime,
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
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReplicationTaskIndividualAssessment' value with any optional fields omitted.
mkReplicationTaskIndividualAssessment ::
  ReplicationTaskIndividualAssessment
mkReplicationTaskIndividualAssessment =
  ReplicationTaskIndividualAssessment'
    { individualAssessmentName =
        Core.Nothing,
      replicationTaskAssessmentRunArn = Core.Nothing,
      replicationTaskIndividualAssessmentArn = Core.Nothing,
      replicationTaskIndividualAssessmentStartDate =
        Core.Nothing,
      status = Core.Nothing
    }

-- | Name of this individual assessment.
--
-- /Note:/ Consider using 'individualAssessmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaIndividualAssessmentName :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Types.String)
rtiaIndividualAssessmentName = Lens.field @"individualAssessmentName"
{-# DEPRECATED rtiaIndividualAssessmentName "Use generic-lens or generic-optics with 'individualAssessmentName' instead." #-}

-- | ARN of the premigration assessment run that is created to run this individual assessment.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskAssessmentRunArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Types.String)
rtiaReplicationTaskAssessmentRunArn = Lens.field @"replicationTaskAssessmentRunArn"
{-# DEPRECATED rtiaReplicationTaskAssessmentRunArn "Use generic-lens or generic-optics with 'replicationTaskAssessmentRunArn' instead." #-}

-- | Amazon Resource Name (ARN) of this individual assessment.
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessmentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskIndividualAssessmentArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Types.String)
rtiaReplicationTaskIndividualAssessmentArn = Lens.field @"replicationTaskIndividualAssessmentArn"
{-# DEPRECATED rtiaReplicationTaskIndividualAssessmentArn "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessmentArn' instead." #-}

-- | Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessmentStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskIndividualAssessmentStartDate :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.NominalDiffTime)
rtiaReplicationTaskIndividualAssessmentStartDate = Lens.field @"replicationTaskIndividualAssessmentStartDate"
{-# DEPRECATED rtiaReplicationTaskIndividualAssessmentStartDate "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessmentStartDate' instead." #-}

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
rtiaStatus :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Types.String)
rtiaStatus = Lens.field @"status"
{-# DEPRECATED rtiaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ReplicationTaskIndividualAssessment where
  parseJSON =
    Core.withObject "ReplicationTaskIndividualAssessment" Core.$
      \x ->
        ReplicationTaskIndividualAssessment'
          Core.<$> (x Core..:? "IndividualAssessmentName")
          Core.<*> (x Core..:? "ReplicationTaskAssessmentRunArn")
          Core.<*> (x Core..:? "ReplicationTaskIndividualAssessmentArn")
          Core.<*> (x Core..:? "ReplicationTaskIndividualAssessmentStartDate")
          Core.<*> (x Core..:? "Status")

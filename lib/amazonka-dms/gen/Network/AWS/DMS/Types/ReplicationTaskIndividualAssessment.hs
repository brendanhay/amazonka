{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationTaskIndividualAssessment
  ( ReplicationTaskIndividualAssessment (..)
  -- * Smart constructor
  , mkReplicationTaskIndividualAssessment
  -- * Lenses
  , rtiaIndividualAssessmentName
  , rtiaReplicationTaskAssessmentRunArn
  , rtiaReplicationTaskIndividualAssessmentArn
  , rtiaReplicationTaskIndividualAssessmentStartDate
  , rtiaStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that describes an individual assessment from a premigration assessment run.
--
-- /See:/ 'mkReplicationTaskIndividualAssessment' smart constructor.
data ReplicationTaskIndividualAssessment = ReplicationTaskIndividualAssessment'
  { individualAssessmentName :: Core.Maybe Core.Text
    -- ^ Name of this individual assessment.
  , replicationTaskAssessmentRunArn :: Core.Maybe Core.Text
    -- ^ ARN of the premigration assessment run that is created to run this individual assessment.
  , replicationTaskIndividualAssessmentArn :: Core.Maybe Core.Text
    -- ^ Amazon Resource Name (ARN) of this individual assessment.
  , replicationTaskIndividualAssessmentStartDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
  , status :: Core.Maybe Core.Text
    -- ^ Individual assessment status.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReplicationTaskIndividualAssessment' value with any optional fields omitted.
mkReplicationTaskIndividualAssessment
    :: ReplicationTaskIndividualAssessment
mkReplicationTaskIndividualAssessment
  = ReplicationTaskIndividualAssessment'{individualAssessmentName =
                                           Core.Nothing,
                                         replicationTaskAssessmentRunArn = Core.Nothing,
                                         replicationTaskIndividualAssessmentArn = Core.Nothing,
                                         replicationTaskIndividualAssessmentStartDate =
                                           Core.Nothing,
                                         status = Core.Nothing}

-- | Name of this individual assessment.
--
-- /Note:/ Consider using 'individualAssessmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaIndividualAssessmentName :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
rtiaIndividualAssessmentName = Lens.field @"individualAssessmentName"
{-# INLINEABLE rtiaIndividualAssessmentName #-}
{-# DEPRECATED individualAssessmentName "Use generic-lens or generic-optics with 'individualAssessmentName' instead"  #-}

-- | ARN of the premigration assessment run that is created to run this individual assessment.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRunArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskAssessmentRunArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
rtiaReplicationTaskAssessmentRunArn = Lens.field @"replicationTaskAssessmentRunArn"
{-# INLINEABLE rtiaReplicationTaskAssessmentRunArn #-}
{-# DEPRECATED replicationTaskAssessmentRunArn "Use generic-lens or generic-optics with 'replicationTaskAssessmentRunArn' instead"  #-}

-- | Amazon Resource Name (ARN) of this individual assessment.
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessmentArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskIndividualAssessmentArn :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
rtiaReplicationTaskIndividualAssessmentArn = Lens.field @"replicationTaskIndividualAssessmentArn"
{-# INLINEABLE rtiaReplicationTaskIndividualAssessmentArn #-}
{-# DEPRECATED replicationTaskIndividualAssessmentArn "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessmentArn' instead"  #-}

-- | Date when this individual assessment was started as part of running the @StartReplicationTaskAssessmentRun@ operation.
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessmentStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtiaReplicationTaskIndividualAssessmentStartDate :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.NominalDiffTime)
rtiaReplicationTaskIndividualAssessmentStartDate = Lens.field @"replicationTaskIndividualAssessmentStartDate"
{-# INLINEABLE rtiaReplicationTaskIndividualAssessmentStartDate #-}
{-# DEPRECATED replicationTaskIndividualAssessmentStartDate "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessmentStartDate' instead"  #-}

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
rtiaStatus :: Lens.Lens' ReplicationTaskIndividualAssessment (Core.Maybe Core.Text)
rtiaStatus = Lens.field @"status"
{-# INLINEABLE rtiaStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ReplicationTaskIndividualAssessment where
        parseJSON
          = Core.withObject "ReplicationTaskIndividualAssessment" Core.$
              \ x ->
                ReplicationTaskIndividualAssessment' Core.<$>
                  (x Core..:? "IndividualAssessmentName") Core.<*>
                    x Core..:? "ReplicationTaskAssessmentRunArn"
                    Core.<*> x Core..:? "ReplicationTaskIndividualAssessmentArn"
                    Core.<*> x Core..:? "ReplicationTaskIndividualAssessmentStartDate"
                    Core.<*> x Core..:? "Status"

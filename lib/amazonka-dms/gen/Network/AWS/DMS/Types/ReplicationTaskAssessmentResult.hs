{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
  ( ReplicationTaskAssessmentResult (..)
  -- * Smart constructor
  , mkReplicationTaskAssessmentResult
  -- * Lenses
  , rAssessmentResults
  , rAssessmentResultsFile
  , rAssessmentStatus
  , rReplicationTaskArn
  , rReplicationTaskIdentifier
  , rReplicationTaskLastAssessmentDate
  , rS3ObjectUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The task assessment report in JSON format. 
--
-- /See:/ 'mkReplicationTaskAssessmentResult' smart constructor.
data ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult'
  { assessmentResults :: Core.Maybe Core.Text
    -- ^ The task assessment results in JSON format. 
  , assessmentResultsFile :: Core.Maybe Core.Text
    -- ^ The file containing the results of the task assessment. 
  , assessmentStatus :: Core.Maybe Core.Text
    -- ^ The status of the task assessment. 
  , replicationTaskArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the replication task. 
  , replicationTaskIdentifier :: Core.Maybe Core.Text
    -- ^ The replication task identifier of the task on which the task assessment was run. 
  , replicationTaskLastAssessmentDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the task assessment was completed. 
  , s3ObjectUrl :: Core.Maybe Core.Text
    -- ^ The URL of the S3 object containing the task assessment results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReplicationTaskAssessmentResult' value with any optional fields omitted.
mkReplicationTaskAssessmentResult
    :: ReplicationTaskAssessmentResult
mkReplicationTaskAssessmentResult
  = ReplicationTaskAssessmentResult'{assessmentResults =
                                       Core.Nothing,
                                     assessmentResultsFile = Core.Nothing,
                                     assessmentStatus = Core.Nothing,
                                     replicationTaskArn = Core.Nothing,
                                     replicationTaskIdentifier = Core.Nothing,
                                     replicationTaskLastAssessmentDate = Core.Nothing,
                                     s3ObjectUrl = Core.Nothing}

-- | The task assessment results in JSON format. 
--
-- /Note:/ Consider using 'assessmentResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssessmentResults :: Lens.Lens' ReplicationTaskAssessmentResult (Core.Maybe Core.Text)
rAssessmentResults = Lens.field @"assessmentResults"
{-# INLINEABLE rAssessmentResults #-}
{-# DEPRECATED assessmentResults "Use generic-lens or generic-optics with 'assessmentResults' instead"  #-}

-- | The file containing the results of the task assessment. 
--
-- /Note:/ Consider using 'assessmentResultsFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssessmentResultsFile :: Lens.Lens' ReplicationTaskAssessmentResult (Core.Maybe Core.Text)
rAssessmentResultsFile = Lens.field @"assessmentResultsFile"
{-# INLINEABLE rAssessmentResultsFile #-}
{-# DEPRECATED assessmentResultsFile "Use generic-lens or generic-optics with 'assessmentResultsFile' instead"  #-}

-- | The status of the task assessment. 
--
-- /Note:/ Consider using 'assessmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssessmentStatus :: Lens.Lens' ReplicationTaskAssessmentResult (Core.Maybe Core.Text)
rAssessmentStatus = Lens.field @"assessmentStatus"
{-# INLINEABLE rAssessmentStatus #-}
{-# DEPRECATED assessmentStatus "Use generic-lens or generic-optics with 'assessmentStatus' instead"  #-}

-- | The Amazon Resource Name (ARN) of the replication task. 
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplicationTaskArn :: Lens.Lens' ReplicationTaskAssessmentResult (Core.Maybe Core.Text)
rReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# INLINEABLE rReplicationTaskArn #-}
{-# DEPRECATED replicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead"  #-}

-- | The replication task identifier of the task on which the task assessment was run. 
--
-- /Note:/ Consider using 'replicationTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplicationTaskIdentifier :: Lens.Lens' ReplicationTaskAssessmentResult (Core.Maybe Core.Text)
rReplicationTaskIdentifier = Lens.field @"replicationTaskIdentifier"
{-# INLINEABLE rReplicationTaskIdentifier #-}
{-# DEPRECATED replicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead"  #-}

-- | The date the task assessment was completed. 
--
-- /Note:/ Consider using 'replicationTaskLastAssessmentDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplicationTaskLastAssessmentDate :: Lens.Lens' ReplicationTaskAssessmentResult (Core.Maybe Core.NominalDiffTime)
rReplicationTaskLastAssessmentDate = Lens.field @"replicationTaskLastAssessmentDate"
{-# INLINEABLE rReplicationTaskLastAssessmentDate #-}
{-# DEPRECATED replicationTaskLastAssessmentDate "Use generic-lens or generic-optics with 'replicationTaskLastAssessmentDate' instead"  #-}

-- | The URL of the S3 object containing the task assessment results. 
--
-- /Note:/ Consider using 's3ObjectUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rS3ObjectUrl :: Lens.Lens' ReplicationTaskAssessmentResult (Core.Maybe Core.Text)
rS3ObjectUrl = Lens.field @"s3ObjectUrl"
{-# INLINEABLE rS3ObjectUrl #-}
{-# DEPRECATED s3ObjectUrl "Use generic-lens or generic-optics with 's3ObjectUrl' instead"  #-}

instance Core.FromJSON ReplicationTaskAssessmentResult where
        parseJSON
          = Core.withObject "ReplicationTaskAssessmentResult" Core.$
              \ x ->
                ReplicationTaskAssessmentResult' Core.<$>
                  (x Core..:? "AssessmentResults") Core.<*>
                    x Core..:? "AssessmentResultsFile"
                    Core.<*> x Core..:? "AssessmentStatus"
                    Core.<*> x Core..:? "ReplicationTaskArn"
                    Core.<*> x Core..:? "ReplicationTaskIdentifier"
                    Core.<*> x Core..:? "ReplicationTaskLastAssessmentDate"
                    Core.<*> x Core..:? "S3ObjectUrl"

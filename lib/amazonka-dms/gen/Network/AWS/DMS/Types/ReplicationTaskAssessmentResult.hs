{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskAssessmentResult
  ( ReplicationTaskAssessmentResult (..),

    -- * Smart constructor
    mkReplicationTaskAssessmentResult,

    -- * Lenses
    rAssessmentResults,
    rAssessmentResultsFile,
    rReplicationTaskIdentifier,
    rAssessmentStatus,
    rS3ObjectURL,
    rReplicationTaskLastAssessmentDate,
    rReplicationTaskARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The task assessment report in JSON format.
--
-- /See:/ 'mkReplicationTaskAssessmentResult' smart constructor.
data ReplicationTaskAssessmentResult = ReplicationTaskAssessmentResult'
  { assessmentResults ::
      Lude.Maybe Lude.Text,
    assessmentResultsFile ::
      Lude.Maybe Lude.Text,
    replicationTaskIdentifier ::
      Lude.Maybe Lude.Text,
    assessmentStatus ::
      Lude.Maybe Lude.Text,
    s3ObjectURL ::
      Lude.Maybe Lude.Text,
    replicationTaskLastAssessmentDate ::
      Lude.Maybe Lude.Timestamp,
    replicationTaskARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationTaskAssessmentResult' with the minimum fields required to make a request.
--
-- * 'assessmentResults' - The task assessment results in JSON format.
-- * 'assessmentResultsFile' - The file containing the results of the task assessment.
-- * 'assessmentStatus' - The status of the task assessment.
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
-- * 'replicationTaskIdentifier' - The replication task identifier of the task on which the task assessment was run.
-- * 'replicationTaskLastAssessmentDate' - The date the task assessment was completed.
-- * 's3ObjectURL' - The URL of the S3 object containing the task assessment results.
mkReplicationTaskAssessmentResult ::
  ReplicationTaskAssessmentResult
mkReplicationTaskAssessmentResult =
  ReplicationTaskAssessmentResult'
    { assessmentResults =
        Lude.Nothing,
      assessmentResultsFile = Lude.Nothing,
      replicationTaskIdentifier = Lude.Nothing,
      assessmentStatus = Lude.Nothing,
      s3ObjectURL = Lude.Nothing,
      replicationTaskLastAssessmentDate = Lude.Nothing,
      replicationTaskARN = Lude.Nothing
    }

-- | The task assessment results in JSON format.
--
-- /Note:/ Consider using 'assessmentResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssessmentResults :: Lens.Lens' ReplicationTaskAssessmentResult (Lude.Maybe Lude.Text)
rAssessmentResults = Lens.lens (assessmentResults :: ReplicationTaskAssessmentResult -> Lude.Maybe Lude.Text) (\s a -> s {assessmentResults = a} :: ReplicationTaskAssessmentResult)
{-# DEPRECATED rAssessmentResults "Use generic-lens or generic-optics with 'assessmentResults' instead." #-}

-- | The file containing the results of the task assessment.
--
-- /Note:/ Consider using 'assessmentResultsFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssessmentResultsFile :: Lens.Lens' ReplicationTaskAssessmentResult (Lude.Maybe Lude.Text)
rAssessmentResultsFile = Lens.lens (assessmentResultsFile :: ReplicationTaskAssessmentResult -> Lude.Maybe Lude.Text) (\s a -> s {assessmentResultsFile = a} :: ReplicationTaskAssessmentResult)
{-# DEPRECATED rAssessmentResultsFile "Use generic-lens or generic-optics with 'assessmentResultsFile' instead." #-}

-- | The replication task identifier of the task on which the task assessment was run.
--
-- /Note:/ Consider using 'replicationTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplicationTaskIdentifier :: Lens.Lens' ReplicationTaskAssessmentResult (Lude.Maybe Lude.Text)
rReplicationTaskIdentifier = Lens.lens (replicationTaskIdentifier :: ReplicationTaskAssessmentResult -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskIdentifier = a} :: ReplicationTaskAssessmentResult)
{-# DEPRECATED rReplicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead." #-}

-- | The status of the task assessment.
--
-- /Note:/ Consider using 'assessmentStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rAssessmentStatus :: Lens.Lens' ReplicationTaskAssessmentResult (Lude.Maybe Lude.Text)
rAssessmentStatus = Lens.lens (assessmentStatus :: ReplicationTaskAssessmentResult -> Lude.Maybe Lude.Text) (\s a -> s {assessmentStatus = a} :: ReplicationTaskAssessmentResult)
{-# DEPRECATED rAssessmentStatus "Use generic-lens or generic-optics with 'assessmentStatus' instead." #-}

-- | The URL of the S3 object containing the task assessment results.
--
-- /Note:/ Consider using 's3ObjectURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rS3ObjectURL :: Lens.Lens' ReplicationTaskAssessmentResult (Lude.Maybe Lude.Text)
rS3ObjectURL = Lens.lens (s3ObjectURL :: ReplicationTaskAssessmentResult -> Lude.Maybe Lude.Text) (\s a -> s {s3ObjectURL = a} :: ReplicationTaskAssessmentResult)
{-# DEPRECATED rS3ObjectURL "Use generic-lens or generic-optics with 's3ObjectURL' instead." #-}

-- | The date the task assessment was completed.
--
-- /Note:/ Consider using 'replicationTaskLastAssessmentDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplicationTaskLastAssessmentDate :: Lens.Lens' ReplicationTaskAssessmentResult (Lude.Maybe Lude.Timestamp)
rReplicationTaskLastAssessmentDate = Lens.lens (replicationTaskLastAssessmentDate :: ReplicationTaskAssessmentResult -> Lude.Maybe Lude.Timestamp) (\s a -> s {replicationTaskLastAssessmentDate = a} :: ReplicationTaskAssessmentResult)
{-# DEPRECATED rReplicationTaskLastAssessmentDate "Use generic-lens or generic-optics with 'replicationTaskLastAssessmentDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReplicationTaskARN :: Lens.Lens' ReplicationTaskAssessmentResult (Lude.Maybe Lude.Text)
rReplicationTaskARN = Lens.lens (replicationTaskARN :: ReplicationTaskAssessmentResult -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: ReplicationTaskAssessmentResult)
{-# DEPRECATED rReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

instance Lude.FromJSON ReplicationTaskAssessmentResult where
  parseJSON =
    Lude.withObject
      "ReplicationTaskAssessmentResult"
      ( \x ->
          ReplicationTaskAssessmentResult'
            Lude.<$> (x Lude..:? "AssessmentResults")
            Lude.<*> (x Lude..:? "AssessmentResultsFile")
            Lude.<*> (x Lude..:? "ReplicationTaskIdentifier")
            Lude.<*> (x Lude..:? "AssessmentStatus")
            Lude.<*> (x Lude..:? "S3ObjectUrl")
            Lude.<*> (x Lude..:? "ReplicationTaskLastAssessmentDate")
            Lude.<*> (x Lude..:? "ReplicationTaskArn")
      )

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new premigration assessment run for one or more individual assessments of a migration task.
--
-- The assessments that you can specify depend on the source and target database engine and the migration type defined for the given task. To run this operation, your migration task must already be created. After you run this operation, you can review the status of each individual assessment. You can also run the migration task manually after the assessment run and its individual assessments complete.
module Network.AWS.DMS.StartReplicationTaskAssessmentRun
  ( -- * Creating a request
    StartReplicationTaskAssessmentRun (..),
    mkStartReplicationTaskAssessmentRun,

    -- ** Request lenses
    srtarServiceAccessRoleARN,
    srtarIncludeOnly,
    srtarResultKMSKeyARN,
    srtarReplicationTaskARN,
    srtarResultLocationBucket,
    srtarResultLocationFolder,
    srtarResultEncryptionMode,
    srtarAssessmentRunName,
    srtarExclude,

    -- * Destructuring the response
    StartReplicationTaskAssessmentRunResponse (..),
    mkStartReplicationTaskAssessmentRunResponse,

    -- ** Response lenses
    srtarrsReplicationTaskAssessmentRun,
    srtarrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkStartReplicationTaskAssessmentRun' smart constructor.
data StartReplicationTaskAssessmentRun = StartReplicationTaskAssessmentRun'
  { -- | ARN of a service role needed to start the assessment run.
    serviceAccessRoleARN :: Lude.Text,
    -- | Space-separated list of names for specific individual assessments that you want to include. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
    includeOnly :: Lude.Maybe [Lude.Text],
    -- | ARN of a custom KMS encryption key that you specify when you set @ResultEncryptionMode@ to @"SSE_KMS@ ".
    resultKMSKeyARN :: Lude.Maybe Lude.Text,
    -- | Amazon Resource Name (ARN) of the migration task associated with the premigration assessment run that you want to start.
    replicationTaskARN :: Lude.Text,
    -- | Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
    resultLocationBucket :: Lude.Text,
    -- | Folder within an Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
    resultLocationFolder :: Lude.Maybe Lude.Text,
    -- | Encryption mode that you can specify to encrypt the results of this assessment run. If you don't specify this request parameter, AWS DMS stores the assessment run results without encryption. You can specify one of the options following:
    --
    --
    --     * @"SSE_S3"@ – The server-side encryption provided as a default by Amazon S3.
    --
    --
    --     * @"SSE_KMS"@ – AWS Key Management Service (AWS KMS) encryption. This encryption can use either a custom KMS encryption key that you specify or the default KMS encryption key that DMS provides.
    resultEncryptionMode :: Lude.Maybe Lude.Text,
    -- | Unique name to identify the assessment run.
    assessmentRunName :: Lude.Text,
    -- | Space-separated list of names for specific individual assessments that you want to exclude. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
    exclude :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplicationTaskAssessmentRun' with the minimum fields required to make a request.
--
-- * 'serviceAccessRoleARN' - ARN of a service role needed to start the assessment run.
-- * 'includeOnly' - Space-separated list of names for specific individual assessments that you want to include. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
-- * 'resultKMSKeyARN' - ARN of a custom KMS encryption key that you specify when you set @ResultEncryptionMode@ to @"SSE_KMS@ ".
-- * 'replicationTaskARN' - Amazon Resource Name (ARN) of the migration task associated with the premigration assessment run that you want to start.
-- * 'resultLocationBucket' - Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
-- * 'resultLocationFolder' - Folder within an Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
-- * 'resultEncryptionMode' - Encryption mode that you can specify to encrypt the results of this assessment run. If you don't specify this request parameter, AWS DMS stores the assessment run results without encryption. You can specify one of the options following:
--
--
--     * @"SSE_S3"@ – The server-side encryption provided as a default by Amazon S3.
--
--
--     * @"SSE_KMS"@ – AWS Key Management Service (AWS KMS) encryption. This encryption can use either a custom KMS encryption key that you specify or the default KMS encryption key that DMS provides.
--
--
-- * 'assessmentRunName' - Unique name to identify the assessment run.
-- * 'exclude' - Space-separated list of names for specific individual assessments that you want to exclude. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
mkStartReplicationTaskAssessmentRun ::
  -- | 'serviceAccessRoleARN'
  Lude.Text ->
  -- | 'replicationTaskARN'
  Lude.Text ->
  -- | 'resultLocationBucket'
  Lude.Text ->
  -- | 'assessmentRunName'
  Lude.Text ->
  StartReplicationTaskAssessmentRun
mkStartReplicationTaskAssessmentRun
  pServiceAccessRoleARN_
  pReplicationTaskARN_
  pResultLocationBucket_
  pAssessmentRunName_ =
    StartReplicationTaskAssessmentRun'
      { serviceAccessRoleARN =
          pServiceAccessRoleARN_,
        includeOnly = Lude.Nothing,
        resultKMSKeyARN = Lude.Nothing,
        replicationTaskARN = pReplicationTaskARN_,
        resultLocationBucket = pResultLocationBucket_,
        resultLocationFolder = Lude.Nothing,
        resultEncryptionMode = Lude.Nothing,
        assessmentRunName = pAssessmentRunName_,
        exclude = Lude.Nothing
      }

-- | ARN of a service role needed to start the assessment run.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarServiceAccessRoleARN :: Lens.Lens' StartReplicationTaskAssessmentRun Lude.Text
srtarServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: StartReplicationTaskAssessmentRun -> Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | Space-separated list of names for specific individual assessments that you want to include. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
--
-- /Note:/ Consider using 'includeOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarIncludeOnly :: Lens.Lens' StartReplicationTaskAssessmentRun (Lude.Maybe [Lude.Text])
srtarIncludeOnly = Lens.lens (includeOnly :: StartReplicationTaskAssessmentRun -> Lude.Maybe [Lude.Text]) (\s a -> s {includeOnly = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarIncludeOnly "Use generic-lens or generic-optics with 'includeOnly' instead." #-}

-- | ARN of a custom KMS encryption key that you specify when you set @ResultEncryptionMode@ to @"SSE_KMS@ ".
--
-- /Note:/ Consider using 'resultKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultKMSKeyARN :: Lens.Lens' StartReplicationTaskAssessmentRun (Lude.Maybe Lude.Text)
srtarResultKMSKeyARN = Lens.lens (resultKMSKeyARN :: StartReplicationTaskAssessmentRun -> Lude.Maybe Lude.Text) (\s a -> s {resultKMSKeyARN = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarResultKMSKeyARN "Use generic-lens or generic-optics with 'resultKMSKeyARN' instead." #-}

-- | Amazon Resource Name (ARN) of the migration task associated with the premigration assessment run that you want to start.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarReplicationTaskARN :: Lens.Lens' StartReplicationTaskAssessmentRun Lude.Text
srtarReplicationTaskARN = Lens.lens (replicationTaskARN :: StartReplicationTaskAssessmentRun -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
--
-- /Note:/ Consider using 'resultLocationBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultLocationBucket :: Lens.Lens' StartReplicationTaskAssessmentRun Lude.Text
srtarResultLocationBucket = Lens.lens (resultLocationBucket :: StartReplicationTaskAssessmentRun -> Lude.Text) (\s a -> s {resultLocationBucket = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarResultLocationBucket "Use generic-lens or generic-optics with 'resultLocationBucket' instead." #-}

-- | Folder within an Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
--
-- /Note:/ Consider using 'resultLocationFolder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultLocationFolder :: Lens.Lens' StartReplicationTaskAssessmentRun (Lude.Maybe Lude.Text)
srtarResultLocationFolder = Lens.lens (resultLocationFolder :: StartReplicationTaskAssessmentRun -> Lude.Maybe Lude.Text) (\s a -> s {resultLocationFolder = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarResultLocationFolder "Use generic-lens or generic-optics with 'resultLocationFolder' instead." #-}

-- | Encryption mode that you can specify to encrypt the results of this assessment run. If you don't specify this request parameter, AWS DMS stores the assessment run results without encryption. You can specify one of the options following:
--
--
--     * @"SSE_S3"@ – The server-side encryption provided as a default by Amazon S3.
--
--
--     * @"SSE_KMS"@ – AWS Key Management Service (AWS KMS) encryption. This encryption can use either a custom KMS encryption key that you specify or the default KMS encryption key that DMS provides.
--
--
--
-- /Note:/ Consider using 'resultEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultEncryptionMode :: Lens.Lens' StartReplicationTaskAssessmentRun (Lude.Maybe Lude.Text)
srtarResultEncryptionMode = Lens.lens (resultEncryptionMode :: StartReplicationTaskAssessmentRun -> Lude.Maybe Lude.Text) (\s a -> s {resultEncryptionMode = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarResultEncryptionMode "Use generic-lens or generic-optics with 'resultEncryptionMode' instead." #-}

-- | Unique name to identify the assessment run.
--
-- /Note:/ Consider using 'assessmentRunName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarAssessmentRunName :: Lens.Lens' StartReplicationTaskAssessmentRun Lude.Text
srtarAssessmentRunName = Lens.lens (assessmentRunName :: StartReplicationTaskAssessmentRun -> Lude.Text) (\s a -> s {assessmentRunName = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarAssessmentRunName "Use generic-lens or generic-optics with 'assessmentRunName' instead." #-}

-- | Space-separated list of names for specific individual assessments that you want to exclude. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
--
-- /Note:/ Consider using 'exclude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarExclude :: Lens.Lens' StartReplicationTaskAssessmentRun (Lude.Maybe [Lude.Text])
srtarExclude = Lens.lens (exclude :: StartReplicationTaskAssessmentRun -> Lude.Maybe [Lude.Text]) (\s a -> s {exclude = a} :: StartReplicationTaskAssessmentRun)
{-# DEPRECATED srtarExclude "Use generic-lens or generic-optics with 'exclude' instead." #-}

instance Lude.AWSRequest StartReplicationTaskAssessmentRun where
  type
    Rs StartReplicationTaskAssessmentRun =
      StartReplicationTaskAssessmentRunResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartReplicationTaskAssessmentRunResponse'
            Lude.<$> (x Lude..?> "ReplicationTaskAssessmentRun")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartReplicationTaskAssessmentRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.StartReplicationTaskAssessmentRun" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartReplicationTaskAssessmentRun where
  toJSON StartReplicationTaskAssessmentRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServiceAccessRoleArn" Lude..= serviceAccessRoleARN),
            ("IncludeOnly" Lude..=) Lude.<$> includeOnly,
            ("ResultKmsKeyArn" Lude..=) Lude.<$> resultKMSKeyARN,
            Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN),
            Lude.Just ("ResultLocationBucket" Lude..= resultLocationBucket),
            ("ResultLocationFolder" Lude..=) Lude.<$> resultLocationFolder,
            ("ResultEncryptionMode" Lude..=) Lude.<$> resultEncryptionMode,
            Lude.Just ("AssessmentRunName" Lude..= assessmentRunName),
            ("Exclude" Lude..=) Lude.<$> exclude
          ]
      )

instance Lude.ToPath StartReplicationTaskAssessmentRun where
  toPath = Lude.const "/"

instance Lude.ToQuery StartReplicationTaskAssessmentRun where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkStartReplicationTaskAssessmentRunResponse' smart constructor.
data StartReplicationTaskAssessmentRunResponse = StartReplicationTaskAssessmentRunResponse'
  { -- | The premigration assessment run that was started.
    replicationTaskAssessmentRun :: Lude.Maybe ReplicationTaskAssessmentRun,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplicationTaskAssessmentRunResponse' with the minimum fields required to make a request.
--
-- * 'replicationTaskAssessmentRun' - The premigration assessment run that was started.
-- * 'responseStatus' - The response status code.
mkStartReplicationTaskAssessmentRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartReplicationTaskAssessmentRunResponse
mkStartReplicationTaskAssessmentRunResponse pResponseStatus_ =
  StartReplicationTaskAssessmentRunResponse'
    { replicationTaskAssessmentRun =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The premigration assessment run that was started.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarrsReplicationTaskAssessmentRun :: Lens.Lens' StartReplicationTaskAssessmentRunResponse (Lude.Maybe ReplicationTaskAssessmentRun)
srtarrsReplicationTaskAssessmentRun = Lens.lens (replicationTaskAssessmentRun :: StartReplicationTaskAssessmentRunResponse -> Lude.Maybe ReplicationTaskAssessmentRun) (\s a -> s {replicationTaskAssessmentRun = a} :: StartReplicationTaskAssessmentRunResponse)
{-# DEPRECATED srtarrsReplicationTaskAssessmentRun "Use generic-lens or generic-optics with 'replicationTaskAssessmentRun' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarrsResponseStatus :: Lens.Lens' StartReplicationTaskAssessmentRunResponse Lude.Int
srtarrsResponseStatus = Lens.lens (responseStatus :: StartReplicationTaskAssessmentRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartReplicationTaskAssessmentRunResponse)
{-# DEPRECATED srtarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

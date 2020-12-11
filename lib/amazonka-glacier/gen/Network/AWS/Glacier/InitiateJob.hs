{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.InitiateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates a job of the specified type, which can be a select, an archival retrieval, or a vault retrieval. For more information about using this operation, see the documentation for the underlying REST API <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html Initiate a Job> .
module Network.AWS.Glacier.InitiateJob
  ( -- * Creating a request
    InitiateJob (..),
    mkInitiateJob,

    -- ** Request lenses
    ijJobParameters,
    ijAccountId,
    ijVaultName,

    -- * Destructuring the response
    InitiateJobResponse (..),
    mkInitiateJobResponse,

    -- ** Response lenses
    ijrsJobId,
    ijrsJobOutputPath,
    ijrsLocation,
    ijrsResponseStatus,
  )
where

import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Provides options for initiating an Amazon S3 Glacier job.
--
-- /See:/ 'mkInitiateJob' smart constructor.
data InitiateJob = InitiateJob'
  { jobParameters ::
      Lude.Maybe JobParameters,
    accountId :: Lude.Text,
    vaultName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateJob' with the minimum fields required to make a request.
--
-- * 'accountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
-- * 'jobParameters' - Provides options for specifying job information.
-- * 'vaultName' - The name of the vault.
mkInitiateJob ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'vaultName'
  Lude.Text ->
  InitiateJob
mkInitiateJob pAccountId_ pVaultName_ =
  InitiateJob'
    { jobParameters = Lude.Nothing,
      accountId = pAccountId_,
      vaultName = pVaultName_
    }

-- | Provides options for specifying job information.
--
-- /Note:/ Consider using 'jobParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijJobParameters :: Lens.Lens' InitiateJob (Lude.Maybe JobParameters)
ijJobParameters = Lens.lens (jobParameters :: InitiateJob -> Lude.Maybe JobParameters) (\s a -> s {jobParameters = a} :: InitiateJob)
{-# DEPRECATED ijJobParameters "Use generic-lens or generic-optics with 'jobParameters' instead." #-}

-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon S3 Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijAccountId :: Lens.Lens' InitiateJob Lude.Text
ijAccountId = Lens.lens (accountId :: InitiateJob -> Lude.Text) (\s a -> s {accountId = a} :: InitiateJob)
{-# DEPRECATED ijAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the vault.
--
-- /Note:/ Consider using 'vaultName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijVaultName :: Lens.Lens' InitiateJob Lude.Text
ijVaultName = Lens.lens (vaultName :: InitiateJob -> Lude.Text) (\s a -> s {vaultName = a} :: InitiateJob)
{-# DEPRECATED ijVaultName "Use generic-lens or generic-optics with 'vaultName' instead." #-}

instance Lude.AWSRequest InitiateJob where
  type Rs InitiateJob = InitiateJobResponse
  request = Req.postJSON glacierService
  response =
    Res.receiveEmpty
      ( \s h x ->
          InitiateJobResponse'
            Lude.<$> (h Lude..#? "x-amz-job-id")
            Lude.<*> (h Lude..#? "x-amz-job-output-path")
            Lude.<*> (h Lude..#? "Location")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InitiateJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON InitiateJob where
  toJSON InitiateJob' {..} =
    Lude.object
      (Lude.catMaybes [("jobParameters" Lude..=) Lude.<$> jobParameters])

instance Lude.ToPath InitiateJob where
  toPath InitiateJob' {..} =
    Lude.mconcat
      [ "/",
        Lude.toBS accountId,
        "/vaults/",
        Lude.toBS vaultName,
        "/jobs"
      ]

instance Lude.ToQuery InitiateJob where
  toQuery = Lude.const Lude.mempty

-- | Contains the Amazon S3 Glacier response to your request.
--
-- /See:/ 'mkInitiateJobResponse' smart constructor.
data InitiateJobResponse = InitiateJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobOutputPath :: Lude.Maybe Lude.Text,
    location :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The ID of the job.
-- * 'jobOutputPath' - The path to the location of where the select results are stored.
-- * 'location' - The relative URI path of the job.
-- * 'responseStatus' - The response status code.
mkInitiateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InitiateJobResponse
mkInitiateJobResponse pResponseStatus_ =
  InitiateJobResponse'
    { jobId = Lude.Nothing,
      jobOutputPath = Lude.Nothing,
      location = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrsJobId :: Lens.Lens' InitiateJobResponse (Lude.Maybe Lude.Text)
ijrsJobId = Lens.lens (jobId :: InitiateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: InitiateJobResponse)
{-# DEPRECATED ijrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The path to the location of where the select results are stored.
--
-- /Note:/ Consider using 'jobOutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrsJobOutputPath :: Lens.Lens' InitiateJobResponse (Lude.Maybe Lude.Text)
ijrsJobOutputPath = Lens.lens (jobOutputPath :: InitiateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobOutputPath = a} :: InitiateJobResponse)
{-# DEPRECATED ijrsJobOutputPath "Use generic-lens or generic-optics with 'jobOutputPath' instead." #-}

-- | The relative URI path of the job.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrsLocation :: Lens.Lens' InitiateJobResponse (Lude.Maybe Lude.Text)
ijrsLocation = Lens.lens (location :: InitiateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: InitiateJobResponse)
{-# DEPRECATED ijrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ijrsResponseStatus :: Lens.Lens' InitiateJobResponse Lude.Int
ijrsResponseStatus = Lens.lens (responseStatus :: InitiateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InitiateJobResponse)
{-# DEPRECATED ijrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

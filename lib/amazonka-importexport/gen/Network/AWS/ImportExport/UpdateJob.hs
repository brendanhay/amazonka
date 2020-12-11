{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You use this operation to change the parameters specified in the original manifest file by supplying a new manifest file. The manifest file attached to this request replaces the original manifest file. You can only use the operation after a CreateJob request but before the data transfer starts and you can only use it on jobs you own.
module Network.AWS.ImportExport.UpdateJob
  ( -- * Creating a request
    UpdateJob (..),
    mkUpdateJob,

    -- ** Request lenses
    ujAPIVersion,
    ujJobId,
    ujManifest,
    ujJobType,
    ujValidateOnly,

    -- * Destructuring the response
    UpdateJobResponse (..),
    mkUpdateJobResponse,

    -- ** Response lenses
    ujrsSuccess,
    ujrsWarningMessage,
    ujrsArtifactList,
    ujrsResponseStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input structure for the UpateJob operation.
--
-- /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { apiVersion :: Lude.Maybe Lude.Text,
    jobId :: Lude.Text,
    manifest :: Lude.Text,
    jobType :: JobType,
    validateOnly :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateJob' with the minimum fields required to make a request.
--
-- * 'apiVersion' - Undocumented field.
-- * 'jobId' - Undocumented field.
-- * 'jobType' - Undocumented field.
-- * 'manifest' - Undocumented field.
-- * 'validateOnly' - Undocumented field.
mkUpdateJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'manifest'
  Lude.Text ->
  -- | 'jobType'
  JobType ->
  -- | 'validateOnly'
  Lude.Bool ->
  UpdateJob
mkUpdateJob pJobId_ pManifest_ pJobType_ pValidateOnly_ =
  UpdateJob'
    { apiVersion = Lude.Nothing,
      jobId = pJobId_,
      manifest = pManifest_,
      jobType = pJobType_,
      validateOnly = pValidateOnly_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujAPIVersion :: Lens.Lens' UpdateJob (Lude.Maybe Lude.Text)
ujAPIVersion = Lens.lens (apiVersion :: UpdateJob -> Lude.Maybe Lude.Text) (\s a -> s {apiVersion = a} :: UpdateJob)
{-# DEPRECATED ujAPIVersion "Use generic-lens or generic-optics with 'apiVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobId :: Lens.Lens' UpdateJob Lude.Text
ujJobId = Lens.lens (jobId :: UpdateJob -> Lude.Text) (\s a -> s {jobId = a} :: UpdateJob)
{-# DEPRECATED ujJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujManifest :: Lens.Lens' UpdateJob Lude.Text
ujManifest = Lens.lens (manifest :: UpdateJob -> Lude.Text) (\s a -> s {manifest = a} :: UpdateJob)
{-# DEPRECATED ujManifest "Use generic-lens or generic-optics with 'manifest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobType :: Lens.Lens' UpdateJob JobType
ujJobType = Lens.lens (jobType :: UpdateJob -> JobType) (\s a -> s {jobType = a} :: UpdateJob)
{-# DEPRECATED ujJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'validateOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujValidateOnly :: Lens.Lens' UpdateJob Lude.Bool
ujValidateOnly = Lens.lens (validateOnly :: UpdateJob -> Lude.Bool) (\s a -> s {validateOnly = a} :: UpdateJob)
{-# DEPRECATED ujValidateOnly "Use generic-lens or generic-optics with 'validateOnly' instead." #-}

instance Lude.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request = Req.postQuery importExportService
  response =
    Res.receiveXMLWrapper
      "UpdateJobResult"
      ( \s h x ->
          UpdateJobResponse'
            Lude.<$> (x Lude..@? "Success")
            Lude.<*> (x Lude..@? "WarningMessage")
            Lude.<*> ( x Lude..@? "ArtifactList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateJob where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateJob where
  toQuery UpdateJob' {..} =
    Lude.mconcat
      [ "Operation=UpdateJob",
        "Action" Lude.=: ("UpdateJob" :: Lude.ByteString),
        "Version" Lude.=: ("2010-06-01" :: Lude.ByteString),
        "APIVersion" Lude.=: apiVersion,
        "JobId" Lude.=: jobId,
        "Manifest" Lude.=: manifest,
        "JobType" Lude.=: jobType,
        "ValidateOnly" Lude.=: validateOnly
      ]

-- | Output structure for the UpateJob operation.
--
-- /See:/ 'mkUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { success ::
      Lude.Maybe Lude.Bool,
    warningMessage :: Lude.Maybe Lude.Text,
    artifactList :: Lude.Maybe [Artifact],
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

-- | Creates a value of 'UpdateJobResponse' with the minimum fields required to make a request.
--
-- * 'artifactList' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'success' - Undocumented field.
-- * 'warningMessage' - Undocumented field.
mkUpdateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateJobResponse
mkUpdateJobResponse pResponseStatus_ =
  UpdateJobResponse'
    { success = Lude.Nothing,
      warningMessage = Lude.Nothing,
      artifactList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsSuccess :: Lens.Lens' UpdateJobResponse (Lude.Maybe Lude.Bool)
ujrsSuccess = Lens.lens (success :: UpdateJobResponse -> Lude.Maybe Lude.Bool) (\s a -> s {success = a} :: UpdateJobResponse)
{-# DEPRECATED ujrsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsWarningMessage :: Lens.Lens' UpdateJobResponse (Lude.Maybe Lude.Text)
ujrsWarningMessage = Lens.lens (warningMessage :: UpdateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {warningMessage = a} :: UpdateJobResponse)
{-# DEPRECATED ujrsWarningMessage "Use generic-lens or generic-optics with 'warningMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'artifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsArtifactList :: Lens.Lens' UpdateJobResponse (Lude.Maybe [Artifact])
ujrsArtifactList = Lens.lens (artifactList :: UpdateJobResponse -> Lude.Maybe [Artifact]) (\s a -> s {artifactList = a} :: UpdateJobResponse)
{-# DEPRECATED ujrsArtifactList "Use generic-lens or generic-optics with 'artifactList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrsResponseStatus :: Lens.Lens' UpdateJobResponse Lude.Int
ujrsResponseStatus = Lens.lens (responseStatus :: UpdateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateJobResponse)
{-# DEPRECATED ujrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

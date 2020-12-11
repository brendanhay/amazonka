{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the process of scheduling an upload or download of your data. You include in the request a manifest that describes the data transfer specifics. The response to the request includes a job ID, which you can use in other operations, a signature that you use to identify your storage device, and the address where you should ship your storage device.
module Network.AWS.ImportExport.CreateJob
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjAPIVersion,
    cjManifestAddendum,
    cjJobType,
    cjManifest,
    cjValidateOnly,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrsSignature,
    cjrsJobType,
    cjrsJobId,
    cjrsSignatureFileContents,
    cjrsWarningMessage,
    cjrsArtifactList,
    cjrsResponseStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input structure for the CreateJob operation.
--
-- /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { apiVersion :: Lude.Maybe Lude.Text,
    manifestAddendum :: Lude.Maybe Lude.Text,
    jobType :: JobType,
    manifest :: Lude.Text,
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

-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- * 'apiVersion' - Undocumented field.
-- * 'jobType' - Undocumented field.
-- * 'manifest' - Undocumented field.
-- * 'manifestAddendum' - Undocumented field.
-- * 'validateOnly' - Undocumented field.
mkCreateJob ::
  -- | 'jobType'
  JobType ->
  -- | 'manifest'
  Lude.Text ->
  -- | 'validateOnly'
  Lude.Bool ->
  CreateJob
mkCreateJob pJobType_ pManifest_ pValidateOnly_ =
  CreateJob'
    { apiVersion = Lude.Nothing,
      manifestAddendum = Lude.Nothing,
      jobType = pJobType_,
      manifest = pManifest_,
      validateOnly = pValidateOnly_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'apiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAPIVersion :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjAPIVersion = Lens.lens (apiVersion :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {apiVersion = a} :: CreateJob)
{-# DEPRECATED cjAPIVersion "Use generic-lens or generic-optics with 'apiVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifestAddendum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjManifestAddendum :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjManifestAddendum = Lens.lens (manifestAddendum :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {manifestAddendum = a} :: CreateJob)
{-# DEPRECATED cjManifestAddendum "Use generic-lens or generic-optics with 'manifestAddendum' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobType :: Lens.Lens' CreateJob JobType
cjJobType = Lens.lens (jobType :: CreateJob -> JobType) (\s a -> s {jobType = a} :: CreateJob)
{-# DEPRECATED cjJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjManifest :: Lens.Lens' CreateJob Lude.Text
cjManifest = Lens.lens (manifest :: CreateJob -> Lude.Text) (\s a -> s {manifest = a} :: CreateJob)
{-# DEPRECATED cjManifest "Use generic-lens or generic-optics with 'manifest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'validateOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjValidateOnly :: Lens.Lens' CreateJob Lude.Bool
cjValidateOnly = Lens.lens (validateOnly :: CreateJob -> Lude.Bool) (\s a -> s {validateOnly = a} :: CreateJob)
{-# DEPRECATED cjValidateOnly "Use generic-lens or generic-optics with 'validateOnly' instead." #-}

instance Lude.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = Req.postQuery importExportService
  response =
    Res.receiveXMLWrapper
      "CreateJobResult"
      ( \s h x ->
          CreateJobResponse'
            Lude.<$> (x Lude..@? "Signature")
            Lude.<*> (x Lude..@? "JobType")
            Lude.<*> (x Lude..@? "JobId")
            Lude.<*> (x Lude..@? "SignatureFileContents")
            Lude.<*> (x Lude..@? "WarningMessage")
            Lude.<*> ( x Lude..@? "ArtifactList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateJob where
  toQuery CreateJob' {..} =
    Lude.mconcat
      [ "Operation=CreateJob",
        "Action" Lude.=: ("CreateJob" :: Lude.ByteString),
        "Version" Lude.=: ("2010-06-01" :: Lude.ByteString),
        "APIVersion" Lude.=: apiVersion,
        "ManifestAddendum" Lude.=: manifestAddendum,
        "JobType" Lude.=: jobType,
        "Manifest" Lude.=: manifest,
        "ValidateOnly" Lude.=: validateOnly
      ]

-- | Output structure for the CreateJob operation.
--
-- /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { signature ::
      Lude.Maybe Lude.Text,
    jobType :: Lude.Maybe JobType,
    jobId :: Lude.Maybe Lude.Text,
    signatureFileContents :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- * 'artifactList' - Undocumented field.
-- * 'jobId' - Undocumented field.
-- * 'jobType' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'signature' - Undocumented field.
-- * 'signatureFileContents' - Undocumented field.
-- * 'warningMessage' - Undocumented field.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobResponse
mkCreateJobResponse pResponseStatus_ =
  CreateJobResponse'
    { signature = Lude.Nothing,
      jobType = Lude.Nothing,
      jobId = Lude.Nothing,
      signatureFileContents = Lude.Nothing,
      warningMessage = Lude.Nothing,
      artifactList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsSignature :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsSignature = Lens.lens (signature :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: CreateJobResponse)
{-# DEPRECATED cjrsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJobType :: Lens.Lens' CreateJobResponse (Lude.Maybe JobType)
cjrsJobType = Lens.lens (jobType :: CreateJobResponse -> Lude.Maybe JobType) (\s a -> s {jobType = a} :: CreateJobResponse)
{-# DEPRECATED cjrsJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJobId :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsJobId = Lens.lens (jobId :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: CreateJobResponse)
{-# DEPRECATED cjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signatureFileContents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsSignatureFileContents :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsSignatureFileContents = Lens.lens (signatureFileContents :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {signatureFileContents = a} :: CreateJobResponse)
{-# DEPRECATED cjrsSignatureFileContents "Use generic-lens or generic-optics with 'signatureFileContents' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsWarningMessage :: Lens.Lens' CreateJobResponse (Lude.Maybe Lude.Text)
cjrsWarningMessage = Lens.lens (warningMessage :: CreateJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {warningMessage = a} :: CreateJobResponse)
{-# DEPRECATED cjrsWarningMessage "Use generic-lens or generic-optics with 'warningMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'artifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsArtifactList :: Lens.Lens' CreateJobResponse (Lude.Maybe [Artifact])
cjrsArtifactList = Lens.lens (artifactList :: CreateJobResponse -> Lude.Maybe [Artifact]) (\s a -> s {artifactList = a} :: CreateJobResponse)
{-# DEPRECATED cjrsArtifactList "Use generic-lens or generic-optics with 'artifactList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CreateJobResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CreateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

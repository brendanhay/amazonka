{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms a job worker has received the specified job. Used for partner actions only.
module Network.AWS.CodePipeline.AcknowledgeThirdPartyJob
  ( -- * Creating a request
    AcknowledgeThirdPartyJob (..),
    mkAcknowledgeThirdPartyJob,

    -- ** Request lenses
    atpjJobId,
    atpjClientToken,
    atpjNonce,

    -- * Destructuring the response
    AcknowledgeThirdPartyJobResponse (..),
    mkAcknowledgeThirdPartyJobResponse,

    -- ** Response lenses
    atpjrsStatus,
    atpjrsResponseStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an AcknowledgeThirdPartyJob action.
--
-- /See:/ 'mkAcknowledgeThirdPartyJob' smart constructor.
data AcknowledgeThirdPartyJob = AcknowledgeThirdPartyJob'
  { -- | The unique system-generated ID of the job.
    jobId :: Lude.Text,
    -- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
    clientToken :: Lude.Text,
    -- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response to a 'GetThirdPartyJobDetails' request.
    nonce :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcknowledgeThirdPartyJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique system-generated ID of the job.
-- * 'clientToken' - The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
-- * 'nonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response to a 'GetThirdPartyJobDetails' request.
mkAcknowledgeThirdPartyJob ::
  -- | 'jobId'
  Lude.Text ->
  -- | 'clientToken'
  Lude.Text ->
  -- | 'nonce'
  Lude.Text ->
  AcknowledgeThirdPartyJob
mkAcknowledgeThirdPartyJob pJobId_ pClientToken_ pNonce_ =
  AcknowledgeThirdPartyJob'
    { jobId = pJobId_,
      clientToken = pClientToken_,
      nonce = pNonce_
    }

-- | The unique system-generated ID of the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjJobId :: Lens.Lens' AcknowledgeThirdPartyJob Lude.Text
atpjJobId = Lens.lens (jobId :: AcknowledgeThirdPartyJob -> Lude.Text) (\s a -> s {jobId = a} :: AcknowledgeThirdPartyJob)
{-# DEPRECATED atpjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The clientToken portion of the clientId and clientToken pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjClientToken :: Lens.Lens' AcknowledgeThirdPartyJob Lude.Text
atpjClientToken = Lens.lens (clientToken :: AcknowledgeThirdPartyJob -> Lude.Text) (\s a -> s {clientToken = a} :: AcknowledgeThirdPartyJob)
{-# DEPRECATED atpjClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Get this number from the response to a 'GetThirdPartyJobDetails' request.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjNonce :: Lens.Lens' AcknowledgeThirdPartyJob Lude.Text
atpjNonce = Lens.lens (nonce :: AcknowledgeThirdPartyJob -> Lude.Text) (\s a -> s {nonce = a} :: AcknowledgeThirdPartyJob)
{-# DEPRECATED atpjNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

instance Lude.AWSRequest AcknowledgeThirdPartyJob where
  type Rs AcknowledgeThirdPartyJob = AcknowledgeThirdPartyJobResponse
  request = Req.postJSON codePipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          AcknowledgeThirdPartyJobResponse'
            Lude.<$> (x Lude..?> "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcknowledgeThirdPartyJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodePipeline_20150709.AcknowledgeThirdPartyJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcknowledgeThirdPartyJob where
  toJSON AcknowledgeThirdPartyJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("jobId" Lude..= jobId),
            Lude.Just ("clientToken" Lude..= clientToken),
            Lude.Just ("nonce" Lude..= nonce)
          ]
      )

instance Lude.ToPath AcknowledgeThirdPartyJob where
  toPath = Lude.const "/"

instance Lude.ToQuery AcknowledgeThirdPartyJob where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of an AcknowledgeThirdPartyJob action.
--
-- /See:/ 'mkAcknowledgeThirdPartyJobResponse' smart constructor.
data AcknowledgeThirdPartyJobResponse = AcknowledgeThirdPartyJobResponse'
  { -- | The status information for the third party job, if any.
    status :: Lude.Maybe JobStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcknowledgeThirdPartyJobResponse' with the minimum fields required to make a request.
--
-- * 'status' - The status information for the third party job, if any.
-- * 'responseStatus' - The response status code.
mkAcknowledgeThirdPartyJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcknowledgeThirdPartyJobResponse
mkAcknowledgeThirdPartyJobResponse pResponseStatus_ =
  AcknowledgeThirdPartyJobResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status information for the third party job, if any.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjrsStatus :: Lens.Lens' AcknowledgeThirdPartyJobResponse (Lude.Maybe JobStatus)
atpjrsStatus = Lens.lens (status :: AcknowledgeThirdPartyJobResponse -> Lude.Maybe JobStatus) (\s a -> s {status = a} :: AcknowledgeThirdPartyJobResponse)
{-# DEPRECATED atpjrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpjrsResponseStatus :: Lens.Lens' AcknowledgeThirdPartyJobResponse Lude.Int
atpjrsResponseStatus = Lens.lens (responseStatus :: AcknowledgeThirdPartyJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcknowledgeThirdPartyJobResponse)
{-# DEPRECATED atpjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

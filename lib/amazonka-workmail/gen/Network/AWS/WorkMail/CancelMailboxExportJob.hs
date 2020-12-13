{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CancelMailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mailbox export job.
module Network.AWS.WorkMail.CancelMailboxExportJob
  ( -- * Creating a request
    CancelMailboxExportJob (..),
    mkCancelMailboxExportJob,

    -- ** Request lenses
    cmejClientToken,
    cmejJobId,
    cmejOrganizationId,

    -- * Destructuring the response
    CancelMailboxExportJobResponse (..),
    mkCancelMailboxExportJobResponse,

    -- ** Response lenses
    cmejrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkMail.Types

-- | /See:/ 'mkCancelMailboxExportJob' smart constructor.
data CancelMailboxExportJob = CancelMailboxExportJob'
  { -- | The idempotency token for the client request.
    clientToken :: Lude.Text,
    -- | The job ID.
    jobId :: Lude.Text,
    -- | The organization ID.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelMailboxExportJob' with the minimum fields required to make a request.
--
-- * 'clientToken' - The idempotency token for the client request.
-- * 'jobId' - The job ID.
-- * 'organizationId' - The organization ID.
mkCancelMailboxExportJob ::
  -- | 'clientToken'
  Lude.Text ->
  -- | 'jobId'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  CancelMailboxExportJob
mkCancelMailboxExportJob pClientToken_ pJobId_ pOrganizationId_ =
  CancelMailboxExportJob'
    { clientToken = pClientToken_,
      jobId = pJobId_,
      organizationId = pOrganizationId_
    }

-- | The idempotency token for the client request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejClientToken :: Lens.Lens' CancelMailboxExportJob Lude.Text
cmejClientToken = Lens.lens (clientToken :: CancelMailboxExportJob -> Lude.Text) (\s a -> s {clientToken = a} :: CancelMailboxExportJob)
{-# DEPRECATED cmejClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The job ID.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejJobId :: Lens.Lens' CancelMailboxExportJob Lude.Text
cmejJobId = Lens.lens (jobId :: CancelMailboxExportJob -> Lude.Text) (\s a -> s {jobId = a} :: CancelMailboxExportJob)
{-# DEPRECATED cmejJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejOrganizationId :: Lens.Lens' CancelMailboxExportJob Lude.Text
cmejOrganizationId = Lens.lens (organizationId :: CancelMailboxExportJob -> Lude.Text) (\s a -> s {organizationId = a} :: CancelMailboxExportJob)
{-# DEPRECATED cmejOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest CancelMailboxExportJob where
  type Rs CancelMailboxExportJob = CancelMailboxExportJobResponse
  request = Req.postJSON workMailService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelMailboxExportJobResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelMailboxExportJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkMailService.CancelMailboxExportJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelMailboxExportJob where
  toJSON CancelMailboxExportJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientToken" Lude..= clientToken),
            Lude.Just ("JobId" Lude..= jobId),
            Lude.Just ("OrganizationId" Lude..= organizationId)
          ]
      )

instance Lude.ToPath CancelMailboxExportJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelMailboxExportJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelMailboxExportJobResponse' smart constructor.
newtype CancelMailboxExportJobResponse = CancelMailboxExportJobResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelMailboxExportJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelMailboxExportJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelMailboxExportJobResponse
mkCancelMailboxExportJobResponse pResponseStatus_ =
  CancelMailboxExportJobResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejrsResponseStatus :: Lens.Lens' CancelMailboxExportJobResponse Lude.Int
cmejrsResponseStatus = Lens.lens (responseStatus :: CancelMailboxExportJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelMailboxExportJobResponse)
{-# DEPRECATED cmejrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

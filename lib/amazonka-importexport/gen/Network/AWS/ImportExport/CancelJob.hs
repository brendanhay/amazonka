{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.CancelJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation cancels a specified job. Only the job owner can cancel it. The operation fails if the job has already started or is complete.
module Network.AWS.ImportExport.CancelJob
  ( -- * Creating a request
    CancelJob (..),
    mkCancelJob,

    -- ** Request lenses
    cAPIVersion,
    cJobId,

    -- * Destructuring the response
    CancelJobResponse (..),
    mkCancelJobResponse,

    -- ** Response lenses
    crsSuccess,
    crsResponseStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input structure for the CancelJob operation.
--
-- /See:/ 'mkCancelJob' smart constructor.
data CancelJob = CancelJob'
  { apiVersion :: Lude.Maybe Lude.Text,
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- * 'apiVersion' -
-- * 'jobId' -
mkCancelJob ::
  -- | 'jobId'
  Lude.Text ->
  CancelJob
mkCancelJob pJobId_ =
  CancelJob' {apiVersion = Lude.Nothing, jobId = pJobId_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAPIVersion :: Lens.Lens' CancelJob (Lude.Maybe Lude.Text)
cAPIVersion = Lens.lens (apiVersion :: CancelJob -> Lude.Maybe Lude.Text) (\s a -> s {apiVersion = a} :: CancelJob)
{-# DEPRECATED cAPIVersion "Use generic-lens or generic-optics with 'apiVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cJobId :: Lens.Lens' CancelJob Lude.Text
cJobId = Lens.lens (jobId :: CancelJob -> Lude.Text) (\s a -> s {jobId = a} :: CancelJob)
{-# DEPRECATED cJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest CancelJob where
  type Rs CancelJob = CancelJobResponse
  request = Req.postQuery importExportService
  response =
    Res.receiveXMLWrapper
      "CancelJobResult"
      ( \s h x ->
          CancelJobResponse'
            Lude.<$> (x Lude..@? "Success") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CancelJob where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelJob where
  toQuery CancelJob' {..} =
    Lude.mconcat
      [ "Operation=CancelJob",
        "Action" Lude.=: ("CancelJob" :: Lude.ByteString),
        "Version" Lude.=: ("2010-06-01" :: Lude.ByteString),
        "APIVersion" Lude.=: apiVersion,
        "JobId" Lude.=: jobId
      ]

-- | Output structure for the CancelJob operation.
--
-- /See:/ 'mkCancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
  { success :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- * 'success' -
-- * 'responseStatus' - The response status code.
mkCancelJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelJobResponse
mkCancelJobResponse pResponseStatus_ =
  CancelJobResponse'
    { success = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSuccess :: Lens.Lens' CancelJobResponse (Lude.Maybe Lude.Bool)
crsSuccess = Lens.lens (success :: CancelJobResponse -> Lude.Maybe Lude.Bool) (\s a -> s {success = a} :: CancelJobResponse)
{-# DEPRECATED crsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CancelJobResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CancelJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelJobResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

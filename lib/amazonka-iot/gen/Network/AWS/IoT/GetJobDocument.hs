{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetJobDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a job document.
module Network.AWS.IoT.GetJobDocument
  ( -- * Creating a request
    GetJobDocument (..),
    mkGetJobDocument,

    -- ** Request lenses
    gjdJobId,

    -- * Destructuring the response
    GetJobDocumentResponse (..),
    mkGetJobDocumentResponse,

    -- ** Response lenses
    gjdrsDocument,
    gjdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetJobDocument' smart constructor.
newtype GetJobDocument = GetJobDocument' {jobId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetJobDocument' with the minimum fields required to make a request.
--
-- * 'jobId' - The unique identifier you assigned to this job when it was created.
mkGetJobDocument ::
  -- | 'jobId'
  Lude.Text ->
  GetJobDocument
mkGetJobDocument pJobId_ = GetJobDocument' {jobId = pJobId_}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdJobId :: Lens.Lens' GetJobDocument Lude.Text
gjdJobId = Lens.lens (jobId :: GetJobDocument -> Lude.Text) (\s a -> s {jobId = a} :: GetJobDocument)
{-# DEPRECATED gjdJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetJobDocument where
  type Rs GetJobDocument = GetJobDocumentResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetJobDocumentResponse'
            Lude.<$> (x Lude..?> "document") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetJobDocument where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetJobDocument where
  toPath GetJobDocument' {..} =
    Lude.mconcat ["/jobs/", Lude.toBS jobId, "/job-document"]

instance Lude.ToQuery GetJobDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetJobDocumentResponse' smart constructor.
data GetJobDocumentResponse = GetJobDocumentResponse'
  { document ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetJobDocumentResponse' with the minimum fields required to make a request.
--
-- * 'document' - The job document content.
-- * 'responseStatus' - The response status code.
mkGetJobDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetJobDocumentResponse
mkGetJobDocumentResponse pResponseStatus_ =
  GetJobDocumentResponse'
    { document = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The job document content.
--
-- /Note:/ Consider using 'document' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrsDocument :: Lens.Lens' GetJobDocumentResponse (Lude.Maybe Lude.Text)
gjdrsDocument = Lens.lens (document :: GetJobDocumentResponse -> Lude.Maybe Lude.Text) (\s a -> s {document = a} :: GetJobDocumentResponse)
{-# DEPRECATED gjdrsDocument "Use generic-lens or generic-optics with 'document' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gjdrsResponseStatus :: Lens.Lens' GetJobDocumentResponse Lude.Int
gjdrsResponseStatus = Lens.lens (responseStatus :: GetJobDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetJobDocumentResponse)
{-# DEPRECATED gjdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

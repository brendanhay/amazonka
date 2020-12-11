{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribeDocumentClassificationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a document classification job. Use this operation to get the status of a classification job.
module Network.AWS.Comprehend.DescribeDocumentClassificationJob
  ( -- * Creating a request
    DescribeDocumentClassificationJob (..),
    mkDescribeDocumentClassificationJob,

    -- ** Request lenses
    ddcjJobId,

    -- * Destructuring the response
    DescribeDocumentClassificationJobResponse (..),
    mkDescribeDocumentClassificationJobResponse,

    -- ** Response lenses
    ddcjrsDocumentClassificationJobProperties,
    ddcjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeDocumentClassificationJob' smart constructor.
newtype DescribeDocumentClassificationJob = DescribeDocumentClassificationJob'
  { jobId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentClassificationJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
mkDescribeDocumentClassificationJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeDocumentClassificationJob
mkDescribeDocumentClassificationJob pJobId_ =
  DescribeDocumentClassificationJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcjJobId :: Lens.Lens' DescribeDocumentClassificationJob Lude.Text
ddcjJobId = Lens.lens (jobId :: DescribeDocumentClassificationJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeDocumentClassificationJob)
{-# DEPRECATED ddcjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeDocumentClassificationJob where
  type
    Rs DescribeDocumentClassificationJob =
      DescribeDocumentClassificationJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeDocumentClassificationJobResponse'
            Lude.<$> (x Lude..?> "DocumentClassificationJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeDocumentClassificationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribeDocumentClassificationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeDocumentClassificationJob where
  toJSON DescribeDocumentClassificationJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeDocumentClassificationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeDocumentClassificationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeDocumentClassificationJobResponse' smart constructor.
data DescribeDocumentClassificationJobResponse = DescribeDocumentClassificationJobResponse'
  { documentClassificationJobProperties ::
      Lude.Maybe
        DocumentClassificationJobProperties,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeDocumentClassificationJobResponse' with the minimum fields required to make a request.
--
-- * 'documentClassificationJobProperties' - An object that describes the properties associated with the document classification job.
-- * 'responseStatus' - The response status code.
mkDescribeDocumentClassificationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeDocumentClassificationJobResponse
mkDescribeDocumentClassificationJobResponse pResponseStatus_ =
  DescribeDocumentClassificationJobResponse'
    { documentClassificationJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that describes the properties associated with the document classification job.
--
-- /Note:/ Consider using 'documentClassificationJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcjrsDocumentClassificationJobProperties :: Lens.Lens' DescribeDocumentClassificationJobResponse (Lude.Maybe DocumentClassificationJobProperties)
ddcjrsDocumentClassificationJobProperties = Lens.lens (documentClassificationJobProperties :: DescribeDocumentClassificationJobResponse -> Lude.Maybe DocumentClassificationJobProperties) (\s a -> s {documentClassificationJobProperties = a} :: DescribeDocumentClassificationJobResponse)
{-# DEPRECATED ddcjrsDocumentClassificationJobProperties "Use generic-lens or generic-optics with 'documentClassificationJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcjrsResponseStatus :: Lens.Lens' DescribeDocumentClassificationJobResponse Lude.Int
ddcjrsResponseStatus = Lens.lens (responseStatus :: DescribeDocumentClassificationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeDocumentClassificationJobResponse)
{-# DEPRECATED ddcjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

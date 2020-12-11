{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.DescribeTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an asycnhronous batch translation job including name, ID, status, source and target languages, input/output S3 buckets, and so on.
module Network.AWS.Translate.DescribeTextTranslationJob
  ( -- * Creating a request
    DescribeTextTranslationJob (..),
    mkDescribeTextTranslationJob,

    -- ** Request lenses
    dttjJobId,

    -- * Destructuring the response
    DescribeTextTranslationJobResponse (..),
    mkDescribeTextTranslationJobResponse,

    -- ** Response lenses
    dttjrsTextTranslationJobProperties,
    dttjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkDescribeTextTranslationJob' smart constructor.
newtype DescribeTextTranslationJob = DescribeTextTranslationJob'
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

-- | Creates a value of 'DescribeTextTranslationJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
mkDescribeTextTranslationJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribeTextTranslationJob
mkDescribeTextTranslationJob pJobId_ =
  DescribeTextTranslationJob' {jobId = pJobId_}

-- | The identifier that Amazon Translate generated for the job. The 'StartTextTranslationJob' operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjJobId :: Lens.Lens' DescribeTextTranslationJob Lude.Text
dttjJobId = Lens.lens (jobId :: DescribeTextTranslationJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribeTextTranslationJob)
{-# DEPRECATED dttjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeTextTranslationJob where
  type
    Rs DescribeTextTranslationJob =
      DescribeTextTranslationJobResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTextTranslationJobResponse'
            Lude.<$> (x Lude..?> "TextTranslationJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTextTranslationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.DescribeTextTranslationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTextTranslationJob where
  toJSON DescribeTextTranslationJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribeTextTranslationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTextTranslationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTextTranslationJobResponse' smart constructor.
data DescribeTextTranslationJobResponse = DescribeTextTranslationJobResponse'
  { textTranslationJobProperties ::
      Lude.Maybe
        TextTranslationJobProperties,
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

-- | Creates a value of 'DescribeTextTranslationJobResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'textTranslationJobProperties' - An object that contains the properties associated with an asynchronous batch translation job.
mkDescribeTextTranslationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTextTranslationJobResponse
mkDescribeTextTranslationJobResponse pResponseStatus_ =
  DescribeTextTranslationJobResponse'
    { textTranslationJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object that contains the properties associated with an asynchronous batch translation job.
--
-- /Note:/ Consider using 'textTranslationJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjrsTextTranslationJobProperties :: Lens.Lens' DescribeTextTranslationJobResponse (Lude.Maybe TextTranslationJobProperties)
dttjrsTextTranslationJobProperties = Lens.lens (textTranslationJobProperties :: DescribeTextTranslationJobResponse -> Lude.Maybe TextTranslationJobProperties) (\s a -> s {textTranslationJobProperties = a} :: DescribeTextTranslationJobResponse)
{-# DEPRECATED dttjrsTextTranslationJobProperties "Use generic-lens or generic-optics with 'textTranslationJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttjrsResponseStatus :: Lens.Lens' DescribeTextTranslationJobResponse Lude.Int
dttjrsResponseStatus = Lens.lens (responseStatus :: DescribeTextTranslationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTextTranslationJobResponse)
{-# DEPRECATED dttjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

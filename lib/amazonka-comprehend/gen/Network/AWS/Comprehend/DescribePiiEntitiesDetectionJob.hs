{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a PII entities detection job. For example, you can use this operation to get the job status.
module Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
  ( -- * Creating a request
    DescribePiiEntitiesDetectionJob (..),
    mkDescribePiiEntitiesDetectionJob,

    -- ** Request lenses
    dpedjJobId,

    -- * Destructuring the response
    DescribePiiEntitiesDetectionJobResponse (..),
    mkDescribePiiEntitiesDetectionJobResponse,

    -- ** Response lenses
    dpedjrsPiiEntitiesDetectionJobProperties,
    dpedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePiiEntitiesDetectionJob' smart constructor.
newtype DescribePiiEntitiesDetectionJob = DescribePiiEntitiesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
    jobId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePiiEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
mkDescribePiiEntitiesDetectionJob ::
  -- | 'jobId'
  Lude.Text ->
  DescribePiiEntitiesDetectionJob
mkDescribePiiEntitiesDetectionJob pJobId_ =
  DescribePiiEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The operation returns this identifier in its response.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjJobId :: Lens.Lens' DescribePiiEntitiesDetectionJob Lude.Text
dpedjJobId = Lens.lens (jobId :: DescribePiiEntitiesDetectionJob -> Lude.Text) (\s a -> s {jobId = a} :: DescribePiiEntitiesDetectionJob)
{-# DEPRECATED dpedjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribePiiEntitiesDetectionJob where
  type
    Rs DescribePiiEntitiesDetectionJob =
      DescribePiiEntitiesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePiiEntitiesDetectionJobResponse'
            Lude.<$> (x Lude..?> "PiiEntitiesDetectionJobProperties")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePiiEntitiesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.DescribePiiEntitiesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePiiEntitiesDetectionJob where
  toJSON DescribePiiEntitiesDetectionJob' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("JobId" Lude..= jobId)])

instance Lude.ToPath DescribePiiEntitiesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePiiEntitiesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePiiEntitiesDetectionJobResponse' smart constructor.
data DescribePiiEntitiesDetectionJobResponse = DescribePiiEntitiesDetectionJobResponse'
  { piiEntitiesDetectionJobProperties :: Lude.Maybe PiiEntitiesDetectionJobProperties,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePiiEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'piiEntitiesDetectionJobProperties' -
-- * 'responseStatus' - The response status code.
mkDescribePiiEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePiiEntitiesDetectionJobResponse
mkDescribePiiEntitiesDetectionJobResponse pResponseStatus_ =
  DescribePiiEntitiesDetectionJobResponse'
    { piiEntitiesDetectionJobProperties =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'piiEntitiesDetectionJobProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjrsPiiEntitiesDetectionJobProperties :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse (Lude.Maybe PiiEntitiesDetectionJobProperties)
dpedjrsPiiEntitiesDetectionJobProperties = Lens.lens (piiEntitiesDetectionJobProperties :: DescribePiiEntitiesDetectionJobResponse -> Lude.Maybe PiiEntitiesDetectionJobProperties) (\s a -> s {piiEntitiesDetectionJobProperties = a} :: DescribePiiEntitiesDetectionJobResponse)
{-# DEPRECATED dpedjrsPiiEntitiesDetectionJobProperties "Use generic-lens or generic-optics with 'piiEntitiesDetectionJobProperties' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpedjrsResponseStatus :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse Lude.Int
dpedjrsResponseStatus = Lens.lens (responseStatus :: DescribePiiEntitiesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePiiEntitiesDetectionJobResponse)
{-# DEPRECATED dpedjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.DescribeReturnShippingLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information on the shipping label of a Snow device that is being returned to AWS.
module Network.AWS.Snowball.DescribeReturnShippingLabel
  ( -- * Creating a request
    DescribeReturnShippingLabel (..),
    mkDescribeReturnShippingLabel,

    -- ** Request lenses
    drslJobId,

    -- * Destructuring the response
    DescribeReturnShippingLabelResponse (..),
    mkDescribeReturnShippingLabelResponse,

    -- ** Response lenses
    drslrsStatus,
    drslrsExpirationDate,
    drslrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkDescribeReturnShippingLabel' smart constructor.
newtype DescribeReturnShippingLabel = DescribeReturnShippingLabel'
  { jobId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReturnShippingLabel' with the minimum fields required to make a request.
--
-- * 'jobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
mkDescribeReturnShippingLabel ::
  DescribeReturnShippingLabel
mkDescribeReturnShippingLabel =
  DescribeReturnShippingLabel' {jobId = Lude.Nothing}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslJobId :: Lens.Lens' DescribeReturnShippingLabel (Lude.Maybe Lude.Text)
drslJobId = Lens.lens (jobId :: DescribeReturnShippingLabel -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: DescribeReturnShippingLabel)
{-# DEPRECATED drslJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest DescribeReturnShippingLabel where
  type
    Rs DescribeReturnShippingLabel =
      DescribeReturnShippingLabelResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReturnShippingLabelResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "ExpirationDate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReturnShippingLabel where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.DescribeReturnShippingLabel" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReturnShippingLabel where
  toJSON DescribeReturnShippingLabel' {..} =
    Lude.object (Lude.catMaybes [("JobId" Lude..=) Lude.<$> jobId])

instance Lude.ToPath DescribeReturnShippingLabel where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReturnShippingLabel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeReturnShippingLabelResponse' smart constructor.
data DescribeReturnShippingLabelResponse = DescribeReturnShippingLabelResponse'
  { status ::
      Lude.Maybe
        ShippingLabelStatus,
    expirationDate ::
      Lude.Maybe
        Lude.Timestamp,
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

-- | Creates a value of 'DescribeReturnShippingLabelResponse' with the minimum fields required to make a request.
--
-- * 'expirationDate' - The expiration date of the current return shipping label.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status information of the task on a Snow device that is being returned to AWS.
mkDescribeReturnShippingLabelResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReturnShippingLabelResponse
mkDescribeReturnShippingLabelResponse pResponseStatus_ =
  DescribeReturnShippingLabelResponse'
    { status = Lude.Nothing,
      expirationDate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status information of the task on a Snow device that is being returned to AWS.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrsStatus :: Lens.Lens' DescribeReturnShippingLabelResponse (Lude.Maybe ShippingLabelStatus)
drslrsStatus = Lens.lens (status :: DescribeReturnShippingLabelResponse -> Lude.Maybe ShippingLabelStatus) (\s a -> s {status = a} :: DescribeReturnShippingLabelResponse)
{-# DEPRECATED drslrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The expiration date of the current return shipping label.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrsExpirationDate :: Lens.Lens' DescribeReturnShippingLabelResponse (Lude.Maybe Lude.Timestamp)
drslrsExpirationDate = Lens.lens (expirationDate :: DescribeReturnShippingLabelResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: DescribeReturnShippingLabelResponse)
{-# DEPRECATED drslrsExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drslrsResponseStatus :: Lens.Lens' DescribeReturnShippingLabelResponse Lude.Int
drslrsResponseStatus = Lens.lens (responseStatus :: DescribeReturnShippingLabelResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReturnShippingLabelResponse)
{-# DEPRECATED drslrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

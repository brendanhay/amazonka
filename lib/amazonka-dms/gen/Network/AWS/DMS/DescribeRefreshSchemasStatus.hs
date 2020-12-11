{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeRefreshSchemasStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of the RefreshSchemas operation.
module Network.AWS.DMS.DescribeRefreshSchemasStatus
  ( -- * Creating a request
    DescribeRefreshSchemasStatus (..),
    mkDescribeRefreshSchemasStatus,

    -- ** Request lenses
    drssEndpointARN,

    -- * Destructuring the response
    DescribeRefreshSchemasStatusResponse (..),
    mkDescribeRefreshSchemasStatusResponse,

    -- ** Response lenses
    drssrsRefreshSchemasStatus,
    drssrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeRefreshSchemasStatus' smart constructor.
newtype DescribeRefreshSchemasStatus = DescribeRefreshSchemasStatus'
  { endpointARN ::
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

-- | Creates a value of 'DescribeRefreshSchemasStatus' with the minimum fields required to make a request.
--
-- * 'endpointARN' - The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
mkDescribeRefreshSchemasStatus ::
  -- | 'endpointARN'
  Lude.Text ->
  DescribeRefreshSchemasStatus
mkDescribeRefreshSchemasStatus pEndpointARN_ =
  DescribeRefreshSchemasStatus' {endpointARN = pEndpointARN_}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssEndpointARN :: Lens.Lens' DescribeRefreshSchemasStatus Lude.Text
drssEndpointARN = Lens.lens (endpointARN :: DescribeRefreshSchemasStatus -> Lude.Text) (\s a -> s {endpointARN = a} :: DescribeRefreshSchemasStatus)
{-# DEPRECATED drssEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest DescribeRefreshSchemasStatus where
  type
    Rs DescribeRefreshSchemasStatus =
      DescribeRefreshSchemasStatusResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeRefreshSchemasStatusResponse'
            Lude.<$> (x Lude..?> "RefreshSchemasStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRefreshSchemasStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeRefreshSchemasStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeRefreshSchemasStatus where
  toJSON DescribeRefreshSchemasStatus' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("EndpointArn" Lude..= endpointARN)])

instance Lude.ToPath DescribeRefreshSchemasStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRefreshSchemasStatus where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeRefreshSchemasStatusResponse' smart constructor.
data DescribeRefreshSchemasStatusResponse = DescribeRefreshSchemasStatusResponse'
  { refreshSchemasStatus ::
      Lude.Maybe
        RefreshSchemasStatus,
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

-- | Creates a value of 'DescribeRefreshSchemasStatusResponse' with the minimum fields required to make a request.
--
-- * 'refreshSchemasStatus' - The status of the schema.
-- * 'responseStatus' - The response status code.
mkDescribeRefreshSchemasStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRefreshSchemasStatusResponse
mkDescribeRefreshSchemasStatusResponse pResponseStatus_ =
  DescribeRefreshSchemasStatusResponse'
    { refreshSchemasStatus =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the schema.
--
-- /Note:/ Consider using 'refreshSchemasStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssrsRefreshSchemasStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse (Lude.Maybe RefreshSchemasStatus)
drssrsRefreshSchemasStatus = Lens.lens (refreshSchemasStatus :: DescribeRefreshSchemasStatusResponse -> Lude.Maybe RefreshSchemasStatus) (\s a -> s {refreshSchemasStatus = a} :: DescribeRefreshSchemasStatusResponse)
{-# DEPRECATED drssrsRefreshSchemasStatus "Use generic-lens or generic-optics with 'refreshSchemasStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssrsResponseStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse Lude.Int
drssrsResponseStatus = Lens.lens (responseStatus :: DescribeRefreshSchemasStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRefreshSchemasStatusResponse)
{-# DEPRECATED drssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

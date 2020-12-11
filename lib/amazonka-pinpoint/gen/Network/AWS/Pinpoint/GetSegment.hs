{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for a specific segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegment
  ( -- * Creating a request
    GetSegment (..),
    mkGetSegment,

    -- ** Request lenses
    gsSegmentId,
    gsApplicationId,

    -- * Destructuring the response
    GetSegmentResponse (..),
    mkGetSegmentResponse,

    -- ** Response lenses
    gssrsResponseStatus,
    gssrsSegmentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegment' smart constructor.
data GetSegment = GetSegment'
  { segmentId :: Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegment' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'segmentId' - The unique identifier for the segment.
mkGetSegment ::
  -- | 'segmentId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetSegment
mkGetSegment pSegmentId_ pApplicationId_ =
  GetSegment'
    { segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsSegmentId :: Lens.Lens' GetSegment Lude.Text
gsSegmentId = Lens.lens (segmentId :: GetSegment -> Lude.Text) (\s a -> s {segmentId = a} :: GetSegment)
{-# DEPRECATED gsSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsApplicationId :: Lens.Lens' GetSegment Lude.Text
gsApplicationId = Lens.lens (applicationId :: GetSegment -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegment)
{-# DEPRECATED gsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetSegment where
  type Rs GetSegment = GetSegmentResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetSegment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSegment where
  toPath GetSegment' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/segments/",
        Lude.toBS segmentId
      ]

instance Lude.ToQuery GetSegment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSegmentResponse' smart constructor.
data GetSegmentResponse = GetSegmentResponse'
  { responseStatus ::
      Lude.Int,
    segmentResponse :: SegmentResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'segmentResponse' - Undocumented field.
mkGetSegmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  GetSegmentResponse
mkGetSegmentResponse pResponseStatus_ pSegmentResponse_ =
  GetSegmentResponse'
    { responseStatus = pResponseStatus_,
      segmentResponse = pSegmentResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsResponseStatus :: Lens.Lens' GetSegmentResponse Lude.Int
gssrsResponseStatus = Lens.lens (responseStatus :: GetSegmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentResponse)
{-# DEPRECATED gssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssrsSegmentResponse :: Lens.Lens' GetSegmentResponse SegmentResponse
gssrsSegmentResponse = Lens.lens (segmentResponse :: GetSegmentResponse -> SegmentResponse) (\s a -> s {segmentResponse = a} :: GetSegmentResponse)
{-# DEPRECATED gssrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

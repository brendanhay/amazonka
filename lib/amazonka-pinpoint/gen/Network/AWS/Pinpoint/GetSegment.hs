{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gsApplicationId,
    gsSegmentId,

    -- * Destructuring the response
    GetSegmentResponse (..),
    mkGetSegmentResponse,

    -- ** Response lenses
    gsfrsSegmentResponse,
    gsfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegment' smart constructor.
data GetSegment = GetSegment'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The unique identifier for the segment.
    segmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegment' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'segmentId' - The unique identifier for the segment.
mkGetSegment ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'segmentId'
  Lude.Text ->
  GetSegment
mkGetSegment pApplicationId_ pSegmentId_ =
  GetSegment'
    { applicationId = pApplicationId_,
      segmentId = pSegmentId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsApplicationId :: Lens.Lens' GetSegment Lude.Text
gsApplicationId = Lens.lens (applicationId :: GetSegment -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegment)
{-# DEPRECATED gsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsSegmentId :: Lens.Lens' GetSegment Lude.Text
gsSegmentId = Lens.lens (segmentId :: GetSegment -> Lude.Text) (\s a -> s {segmentId = a} :: GetSegment)
{-# DEPRECATED gsSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Lude.AWSRequest GetSegment where
  type Rs GetSegment = GetSegmentResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { segmentResponse :: SegmentResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentResponse' with the minimum fields required to make a request.
--
-- * 'segmentResponse' -
-- * 'responseStatus' - The response status code.
mkGetSegmentResponse ::
  -- | 'segmentResponse'
  SegmentResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetSegmentResponse
mkGetSegmentResponse pSegmentResponse_ pResponseStatus_ =
  GetSegmentResponse'
    { segmentResponse = pSegmentResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsfrsSegmentResponse :: Lens.Lens' GetSegmentResponse SegmentResponse
gsfrsSegmentResponse = Lens.lens (segmentResponse :: GetSegmentResponse -> SegmentResponse) (\s a -> s {segmentResponse = a} :: GetSegmentResponse)
{-# DEPRECATED gsfrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsfrsResponseStatus :: Lens.Lens' GetSegmentResponse Lude.Int
gsfrsResponseStatus = Lens.lens (responseStatus :: GetSegmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentResponse)
{-# DEPRECATED gsfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

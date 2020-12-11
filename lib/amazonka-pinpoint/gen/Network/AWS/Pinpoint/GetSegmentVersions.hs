{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for all the versions of a specific segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegmentVersions
  ( -- * Creating a request
    GetSegmentVersions (..),
    mkGetSegmentVersions,

    -- ** Request lenses
    gsvToken,
    gsvPageSize,
    gsvSegmentId,
    gsvApplicationId,

    -- * Destructuring the response
    GetSegmentVersionsResponse (..),
    mkGetSegmentVersionsResponse,

    -- ** Response lenses
    grsResponseStatus,
    grsSegmentsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegmentVersions' smart constructor.
data GetSegmentVersions = GetSegmentVersions'
  { token ::
      Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Text,
    segmentId :: Lude.Text,
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

-- | Creates a value of 'GetSegmentVersions' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'segmentId' - The unique identifier for the segment.
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
mkGetSegmentVersions ::
  -- | 'segmentId'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetSegmentVersions
mkGetSegmentVersions pSegmentId_ pApplicationId_ =
  GetSegmentVersions'
    { token = Lude.Nothing,
      pageSize = Lude.Nothing,
      segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvToken :: Lens.Lens' GetSegmentVersions (Lude.Maybe Lude.Text)
gsvToken = Lens.lens (token :: GetSegmentVersions -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetSegmentVersions)
{-# DEPRECATED gsvToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvPageSize :: Lens.Lens' GetSegmentVersions (Lude.Maybe Lude.Text)
gsvPageSize = Lens.lens (pageSize :: GetSegmentVersions -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetSegmentVersions)
{-# DEPRECATED gsvPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSegmentId :: Lens.Lens' GetSegmentVersions Lude.Text
gsvSegmentId = Lens.lens (segmentId :: GetSegmentVersions -> Lude.Text) (\s a -> s {segmentId = a} :: GetSegmentVersions)
{-# DEPRECATED gsvSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvApplicationId :: Lens.Lens' GetSegmentVersions Lude.Text
gsvApplicationId = Lens.lens (applicationId :: GetSegmentVersions -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegmentVersions)
{-# DEPRECATED gsvApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetSegmentVersions where
  type Rs GetSegmentVersions = GetSegmentVersionsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentVersionsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetSegmentVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSegmentVersions where
  toPath GetSegmentVersions' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/segments/",
        Lude.toBS segmentId,
        "/versions"
      ]

instance Lude.ToQuery GetSegmentVersions where
  toQuery GetSegmentVersions' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetSegmentVersionsResponse' smart constructor.
data GetSegmentVersionsResponse = GetSegmentVersionsResponse'
  { responseStatus ::
      Lude.Int,
    segmentsResponse :: SegmentsResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentVersionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'segmentsResponse' - Undocumented field.
mkGetSegmentVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'segmentsResponse'
  SegmentsResponse ->
  GetSegmentVersionsResponse
mkGetSegmentVersionsResponse pResponseStatus_ pSegmentsResponse_ =
  GetSegmentVersionsResponse'
    { responseStatus = pResponseStatus_,
      segmentsResponse = pSegmentsResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetSegmentVersionsResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetSegmentVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentVersionsResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsSegmentsResponse :: Lens.Lens' GetSegmentVersionsResponse SegmentsResponse
grsSegmentsResponse = Lens.lens (segmentsResponse :: GetSegmentVersionsResponse -> SegmentsResponse) (\s a -> s {segmentsResponse = a} :: GetSegmentVersionsResponse)
{-# DEPRECATED grsSegmentsResponse "Use generic-lens or generic-optics with 'segmentsResponse' instead." #-}

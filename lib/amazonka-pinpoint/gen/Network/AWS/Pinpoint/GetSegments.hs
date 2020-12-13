{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for all the segments that are associated with an application.
module Network.AWS.Pinpoint.GetSegments
  ( -- * Creating a request
    GetSegments (..),
    mkGetSegments,

    -- ** Request lenses
    gssToken,
    gssApplicationId,
    gssPageSize,

    -- * Destructuring the response
    GetSegmentsResponse (..),
    mkGetSegmentsResponse,

    -- ** Response lenses
    gsrsSegmentsResponse,
    gsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegments' smart constructor.
data GetSegments = GetSegments'
  { -- | The NextToken string that specifies which page of results to return in a paginated response.
    token :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegments' with the minimum fields required to make a request.
--
-- * 'token' - The NextToken string that specifies which page of results to return in a paginated response.
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkGetSegments ::
  -- | 'applicationId'
  Lude.Text ->
  GetSegments
mkGetSegments pApplicationId_ =
  GetSegments'
    { token = Lude.Nothing,
      applicationId = pApplicationId_,
      pageSize = Lude.Nothing
    }

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssToken :: Lens.Lens' GetSegments (Lude.Maybe Lude.Text)
gssToken = Lens.lens (token :: GetSegments -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: GetSegments)
{-# DEPRECATED gssToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssApplicationId :: Lens.Lens' GetSegments Lude.Text
gssApplicationId = Lens.lens (applicationId :: GetSegments -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegments)
{-# DEPRECATED gssApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssPageSize :: Lens.Lens' GetSegments (Lude.Maybe Lude.Text)
gssPageSize = Lens.lens (pageSize :: GetSegments -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: GetSegments)
{-# DEPRECATED gssPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest GetSegments where
  type Rs GetSegments = GetSegmentsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSegments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSegments where
  toPath GetSegments' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/segments"]

instance Lude.ToQuery GetSegments where
  toQuery GetSegments' {..} =
    Lude.mconcat
      ["token" Lude.=: token, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkGetSegmentsResponse' smart constructor.
data GetSegmentsResponse = GetSegmentsResponse'
  { segmentsResponse :: SegmentsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentsResponse' with the minimum fields required to make a request.
--
-- * 'segmentsResponse' -
-- * 'responseStatus' - The response status code.
mkGetSegmentsResponse ::
  -- | 'segmentsResponse'
  SegmentsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetSegmentsResponse
mkGetSegmentsResponse pSegmentsResponse_ pResponseStatus_ =
  GetSegmentsResponse'
    { segmentsResponse = pSegmentsResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSegmentsResponse :: Lens.Lens' GetSegmentsResponse SegmentsResponse
gsrsSegmentsResponse = Lens.lens (segmentsResponse :: GetSegmentsResponse -> SegmentsResponse) (\s a -> s {segmentsResponse = a} :: GetSegmentsResponse)
{-# DEPRECATED gsrsSegmentsResponse "Use generic-lens or generic-optics with 'segmentsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetSegmentsResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetSegmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentsResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegmentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for a specific version of a segment that's associated with an application.
module Network.AWS.Pinpoint.GetSegmentVersion
  ( -- * Creating a request
    GetSegmentVersion (..),
    mkGetSegmentVersion,

    -- ** Request lenses
    gSegmentId,
    gVersion,
    gApplicationId,

    -- * Destructuring the response
    GetSegmentVersionResponse (..),
    mkGetSegmentVersionResponse,

    -- ** Response lenses
    gsvrsResponseStatus,
    gsvrsSegmentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegmentVersion' smart constructor.
data GetSegmentVersion = GetSegmentVersion'
  { segmentId :: Lude.Text,
    version :: Lude.Text,
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

-- | Creates a value of 'GetSegmentVersion' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'segmentId' - The unique identifier for the segment.
-- * 'version' - The unique version number (Version property) for the campaign version.
mkGetSegmentVersion ::
  -- | 'segmentId'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  GetSegmentVersion
mkGetSegmentVersion pSegmentId_ pVersion_ pApplicationId_ =
  GetSegmentVersion'
    { segmentId = pSegmentId_,
      version = pVersion_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gSegmentId :: Lens.Lens' GetSegmentVersion Lude.Text
gSegmentId = Lens.lens (segmentId :: GetSegmentVersion -> Lude.Text) (\s a -> s {segmentId = a} :: GetSegmentVersion)
{-# DEPRECATED gSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

-- | The unique version number (Version property) for the campaign version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gVersion :: Lens.Lens' GetSegmentVersion Lude.Text
gVersion = Lens.lens (version :: GetSegmentVersion -> Lude.Text) (\s a -> s {version = a} :: GetSegmentVersion)
{-# DEPRECATED gVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gApplicationId :: Lens.Lens' GetSegmentVersion Lude.Text
gApplicationId = Lens.lens (applicationId :: GetSegmentVersion -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegmentVersion)
{-# DEPRECATED gApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetSegmentVersion where
  type Rs GetSegmentVersion = GetSegmentVersionResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentVersionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders GetSegmentVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetSegmentVersion where
  toPath GetSegmentVersion' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/segments/",
        Lude.toBS segmentId,
        "/versions/",
        Lude.toBS version
      ]

instance Lude.ToQuery GetSegmentVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSegmentVersionResponse' smart constructor.
data GetSegmentVersionResponse = GetSegmentVersionResponse'
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

-- | Creates a value of 'GetSegmentVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'segmentResponse' - Undocumented field.
mkGetSegmentVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  GetSegmentVersionResponse
mkGetSegmentVersionResponse pResponseStatus_ pSegmentResponse_ =
  GetSegmentVersionResponse'
    { responseStatus = pResponseStatus_,
      segmentResponse = pSegmentResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsResponseStatus :: Lens.Lens' GetSegmentVersionResponse Lude.Int
gsvrsResponseStatus = Lens.lens (responseStatus :: GetSegmentVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentVersionResponse)
{-# DEPRECATED gsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsSegmentResponse :: Lens.Lens' GetSegmentVersionResponse SegmentResponse
gsvrsSegmentResponse = Lens.lens (segmentResponse :: GetSegmentVersionResponse -> SegmentResponse) (\s a -> s {segmentResponse = a} :: GetSegmentVersionResponse)
{-# DEPRECATED gsvrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

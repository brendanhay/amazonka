{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gsvfApplicationId,
    gsvfVersion,
    gsvfSegmentId,

    -- * Destructuring the response
    GetSegmentVersionResponse (..),
    mkGetSegmentVersionResponse,

    -- ** Response lenses
    gsvrsSegmentResponse,
    gsvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSegmentVersion' smart constructor.
data GetSegmentVersion = GetSegmentVersion'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The unique version number (Version property) for the campaign version.
    version :: Lude.Text,
    -- | The unique identifier for the segment.
    segmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentVersion' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'version' - The unique version number (Version property) for the campaign version.
-- * 'segmentId' - The unique identifier for the segment.
mkGetSegmentVersion ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  -- | 'segmentId'
  Lude.Text ->
  GetSegmentVersion
mkGetSegmentVersion pApplicationId_ pVersion_ pSegmentId_ =
  GetSegmentVersion'
    { applicationId = pApplicationId_,
      version = pVersion_,
      segmentId = pSegmentId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvfApplicationId :: Lens.Lens' GetSegmentVersion Lude.Text
gsvfApplicationId = Lens.lens (applicationId :: GetSegmentVersion -> Lude.Text) (\s a -> s {applicationId = a} :: GetSegmentVersion)
{-# DEPRECATED gsvfApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique version number (Version property) for the campaign version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvfVersion :: Lens.Lens' GetSegmentVersion Lude.Text
gsvfVersion = Lens.lens (version :: GetSegmentVersion -> Lude.Text) (\s a -> s {version = a} :: GetSegmentVersion)
{-# DEPRECATED gsvfVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvfSegmentId :: Lens.Lens' GetSegmentVersion Lude.Text
gsvfSegmentId = Lens.lens (segmentId :: GetSegmentVersion -> Lude.Text) (\s a -> s {segmentId = a} :: GetSegmentVersion)
{-# DEPRECATED gsvfSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Lude.AWSRequest GetSegmentVersion where
  type Rs GetSegmentVersion = GetSegmentVersionResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSegmentVersionResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { segmentResponse :: SegmentResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSegmentVersionResponse' with the minimum fields required to make a request.
--
-- * 'segmentResponse' -
-- * 'responseStatus' - The response status code.
mkGetSegmentVersionResponse ::
  -- | 'segmentResponse'
  SegmentResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  GetSegmentVersionResponse
mkGetSegmentVersionResponse pSegmentResponse_ pResponseStatus_ =
  GetSegmentVersionResponse'
    { segmentResponse = pSegmentResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsSegmentResponse :: Lens.Lens' GetSegmentVersionResponse SegmentResponse
gsvrsSegmentResponse = Lens.lens (segmentResponse :: GetSegmentVersionResponse -> SegmentResponse) (\s a -> s {segmentResponse = a} :: GetSegmentVersionResponse)
{-# DEPRECATED gsvrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrsResponseStatus :: Lens.Lens' GetSegmentVersionResponse Lude.Int
gsvrsResponseStatus = Lens.lens (responseStatus :: GetSegmentVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSegmentVersionResponse)
{-# DEPRECATED gsvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration, dimension, and other settings for an existing segment that's associated with an application.
module Network.AWS.Pinpoint.CreateSegment
  ( -- * Creating a request
    CreateSegment (..),
    mkCreateSegment,

    -- ** Request lenses
    csApplicationId,
    csWriteSegmentRequest,

    -- * Destructuring the response
    CreateSegmentResponse (..),
    mkCreateSegmentResponse,

    -- ** Response lenses
    csrsResponseStatus,
    csrsSegmentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSegment' smart constructor.
data CreateSegment = CreateSegment'
  { applicationId :: Lude.Text,
    writeSegmentRequest :: WriteSegmentRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSegment' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'writeSegmentRequest' - Undocumented field.
mkCreateSegment ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'writeSegmentRequest'
  WriteSegmentRequest ->
  CreateSegment
mkCreateSegment pApplicationId_ pWriteSegmentRequest_ =
  CreateSegment'
    { applicationId = pApplicationId_,
      writeSegmentRequest = pWriteSegmentRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csApplicationId :: Lens.Lens' CreateSegment Lude.Text
csApplicationId = Lens.lens (applicationId :: CreateSegment -> Lude.Text) (\s a -> s {applicationId = a} :: CreateSegment)
{-# DEPRECATED csApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'writeSegmentRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csWriteSegmentRequest :: Lens.Lens' CreateSegment WriteSegmentRequest
csWriteSegmentRequest = Lens.lens (writeSegmentRequest :: CreateSegment -> WriteSegmentRequest) (\s a -> s {writeSegmentRequest = a} :: CreateSegment)
{-# DEPRECATED csWriteSegmentRequest "Use generic-lens or generic-optics with 'writeSegmentRequest' instead." #-}

instance Lude.AWSRequest CreateSegment where
  type Rs CreateSegment = CreateSegmentResponse
  request = Req.postJSON pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSegmentResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders CreateSegment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSegment where
  toJSON CreateSegment' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("WriteSegmentRequest" Lude..= writeSegmentRequest)]
      )

instance Lude.ToPath CreateSegment where
  toPath CreateSegment' {..} =
    Lude.mconcat ["/v1/apps/", Lude.toBS applicationId, "/segments"]

instance Lude.ToQuery CreateSegment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSegmentResponse' smart constructor.
data CreateSegmentResponse = CreateSegmentResponse'
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

-- | Creates a value of 'CreateSegmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'segmentResponse' - Undocumented field.
mkCreateSegmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  CreateSegmentResponse
mkCreateSegmentResponse pResponseStatus_ pSegmentResponse_ =
  CreateSegmentResponse'
    { responseStatus = pResponseStatus_,
      segmentResponse = pSegmentResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateSegmentResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateSegmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSegmentResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsSegmentResponse :: Lens.Lens' CreateSegmentResponse SegmentResponse
csrsSegmentResponse = Lens.lens (segmentResponse :: CreateSegmentResponse -> SegmentResponse) (\s a -> s {segmentResponse = a} :: CreateSegmentResponse)
{-# DEPRECATED csrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

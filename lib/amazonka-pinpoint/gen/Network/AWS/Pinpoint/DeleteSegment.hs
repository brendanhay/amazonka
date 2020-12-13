{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteSegment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a segment from an application.
module Network.AWS.Pinpoint.DeleteSegment
  ( -- * Creating a request
    DeleteSegment (..),
    mkDeleteSegment,

    -- ** Request lenses
    dsApplicationId,
    dsSegmentId,

    -- * Destructuring the response
    DeleteSegmentResponse (..),
    mkDeleteSegmentResponse,

    -- ** Response lenses
    dsrsSegmentResponse,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteSegment' smart constructor.
data DeleteSegment = DeleteSegment'
  { -- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Lude.Text,
    -- | The unique identifier for the segment.
    segmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSegment' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
-- * 'segmentId' - The unique identifier for the segment.
mkDeleteSegment ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'segmentId'
  Lude.Text ->
  DeleteSegment
mkDeleteSegment pApplicationId_ pSegmentId_ =
  DeleteSegment'
    { applicationId = pApplicationId_,
      segmentId = pSegmentId_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsApplicationId :: Lens.Lens' DeleteSegment Lude.Text
dsApplicationId = Lens.lens (applicationId :: DeleteSegment -> Lude.Text) (\s a -> s {applicationId = a} :: DeleteSegment)
{-# DEPRECATED dsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'segmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSegmentId :: Lens.Lens' DeleteSegment Lude.Text
dsSegmentId = Lens.lens (segmentId :: DeleteSegment -> Lude.Text) (\s a -> s {segmentId = a} :: DeleteSegment)
{-# DEPRECATED dsSegmentId "Use generic-lens or generic-optics with 'segmentId' instead." #-}

instance Lude.AWSRequest DeleteSegment where
  type Rs DeleteSegment = DeleteSegmentResponse
  request = Req.delete pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSegmentResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSegment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteSegment where
  toPath DeleteSegment' {..} =
    Lude.mconcat
      [ "/v1/apps/",
        Lude.toBS applicationId,
        "/segments/",
        Lude.toBS segmentId
      ]

instance Lude.ToQuery DeleteSegment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteSegmentResponse' smart constructor.
data DeleteSegmentResponse = DeleteSegmentResponse'
  { segmentResponse :: SegmentResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSegmentResponse' with the minimum fields required to make a request.
--
-- * 'segmentResponse' -
-- * 'responseStatus' - The response status code.
mkDeleteSegmentResponse ::
  -- | 'segmentResponse'
  SegmentResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSegmentResponse
mkDeleteSegmentResponse pSegmentResponse_ pResponseStatus_ =
  DeleteSegmentResponse'
    { segmentResponse = pSegmentResponse_,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSegmentResponse :: Lens.Lens' DeleteSegmentResponse SegmentResponse
dsrsSegmentResponse = Lens.lens (segmentResponse :: DeleteSegmentResponse -> SegmentResponse) (\s a -> s {segmentResponse = a} :: DeleteSegmentResponse)
{-# DEPRECATED dsrsSegmentResponse "Use generic-lens or generic-optics with 'segmentResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteSegmentResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteSegmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSegmentResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

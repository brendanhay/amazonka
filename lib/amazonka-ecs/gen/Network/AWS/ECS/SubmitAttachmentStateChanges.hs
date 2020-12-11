{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitAttachmentStateChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sent to acknowledge that an attachment changed states.
module Network.AWS.ECS.SubmitAttachmentStateChanges
  ( -- * Creating a request
    SubmitAttachmentStateChanges (..),
    mkSubmitAttachmentStateChanges,

    -- ** Request lenses
    sascCluster,
    sascAttachments,

    -- * Destructuring the response
    SubmitAttachmentStateChangesResponse (..),
    mkSubmitAttachmentStateChangesResponse,

    -- ** Response lenses
    sascrsAcknowledgment,
    sascrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSubmitAttachmentStateChanges' smart constructor.
data SubmitAttachmentStateChanges = SubmitAttachmentStateChanges'
  { cluster ::
      Lude.Maybe Lude.Text,
    attachments ::
      [AttachmentStateChange]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubmitAttachmentStateChanges' with the minimum fields required to make a request.
--
-- * 'attachments' - Any attachments associated with the state change request.
-- * 'cluster' - The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
mkSubmitAttachmentStateChanges ::
  SubmitAttachmentStateChanges
mkSubmitAttachmentStateChanges =
  SubmitAttachmentStateChanges'
    { cluster = Lude.Nothing,
      attachments = Lude.mempty
    }

-- | The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascCluster :: Lens.Lens' SubmitAttachmentStateChanges (Lude.Maybe Lude.Text)
sascCluster = Lens.lens (cluster :: SubmitAttachmentStateChanges -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: SubmitAttachmentStateChanges)
{-# DEPRECATED sascCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | Any attachments associated with the state change request.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascAttachments :: Lens.Lens' SubmitAttachmentStateChanges [AttachmentStateChange]
sascAttachments = Lens.lens (attachments :: SubmitAttachmentStateChanges -> [AttachmentStateChange]) (\s a -> s {attachments = a} :: SubmitAttachmentStateChanges)
{-# DEPRECATED sascAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

instance Lude.AWSRequest SubmitAttachmentStateChanges where
  type
    Rs SubmitAttachmentStateChanges =
      SubmitAttachmentStateChangesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SubmitAttachmentStateChangesResponse'
            Lude.<$> (x Lude..?> "acknowledgment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SubmitAttachmentStateChanges where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.SubmitAttachmentStateChanges" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SubmitAttachmentStateChanges where
  toJSON SubmitAttachmentStateChanges' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("attachments" Lude..= attachments)
          ]
      )

instance Lude.ToPath SubmitAttachmentStateChanges where
  toPath = Lude.const "/"

instance Lude.ToQuery SubmitAttachmentStateChanges where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSubmitAttachmentStateChangesResponse' smart constructor.
data SubmitAttachmentStateChangesResponse = SubmitAttachmentStateChangesResponse'
  { acknowledgment ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'SubmitAttachmentStateChangesResponse' with the minimum fields required to make a request.
--
-- * 'acknowledgment' - Acknowledgement of the state change.
-- * 'responseStatus' - The response status code.
mkSubmitAttachmentStateChangesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SubmitAttachmentStateChangesResponse
mkSubmitAttachmentStateChangesResponse pResponseStatus_ =
  SubmitAttachmentStateChangesResponse'
    { acknowledgment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascrsAcknowledgment :: Lens.Lens' SubmitAttachmentStateChangesResponse (Lude.Maybe Lude.Text)
sascrsAcknowledgment = Lens.lens (acknowledgment :: SubmitAttachmentStateChangesResponse -> Lude.Maybe Lude.Text) (\s a -> s {acknowledgment = a} :: SubmitAttachmentStateChangesResponse)
{-# DEPRECATED sascrsAcknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascrsResponseStatus :: Lens.Lens' SubmitAttachmentStateChangesResponse Lude.Int
sascrsResponseStatus = Lens.lens (responseStatus :: SubmitAttachmentStateChangesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SubmitAttachmentStateChangesResponse)
{-# DEPRECATED sascrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.RejectQualificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectQualificationRequest@ operation rejects a user's request for a Qualification.
--
-- You can provide a text message explaining why the request was rejected. The Worker who made the request can see this message.
module Network.AWS.MechanicalTurk.RejectQualificationRequest
  ( -- * Creating a request
    RejectQualificationRequest (..),
    mkRejectQualificationRequest,

    -- ** Request lenses
    rqrReason,
    rqrQualificationRequestId,

    -- * Destructuring the response
    RejectQualificationRequestResponse (..),
    mkRejectQualificationRequestResponse,

    -- ** Response lenses
    rqrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectQualificationRequest' smart constructor.
data RejectQualificationRequest = RejectQualificationRequest'
  { reason ::
      Lude.Maybe Lude.Text,
    qualificationRequestId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectQualificationRequest' with the minimum fields required to make a request.
--
-- * 'qualificationRequestId' - The ID of the Qualification request, as returned by the @ListQualificationRequests@ operation.
-- * 'reason' - A text message explaining why the request was rejected, to be shown to the Worker who made the request.
mkRejectQualificationRequest ::
  -- | 'qualificationRequestId'
  Lude.Text ->
  RejectQualificationRequest
mkRejectQualificationRequest pQualificationRequestId_ =
  RejectQualificationRequest'
    { reason = Lude.Nothing,
      qualificationRequestId = pQualificationRequestId_
    }

-- | A text message explaining why the request was rejected, to be shown to the Worker who made the request.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqrReason :: Lens.Lens' RejectQualificationRequest (Lude.Maybe Lude.Text)
rqrReason = Lens.lens (reason :: RejectQualificationRequest -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: RejectQualificationRequest)
{-# DEPRECATED rqrReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Qualification request, as returned by the @ListQualificationRequests@ operation.
--
-- /Note:/ Consider using 'qualificationRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqrQualificationRequestId :: Lens.Lens' RejectQualificationRequest Lude.Text
rqrQualificationRequestId = Lens.lens (qualificationRequestId :: RejectQualificationRequest -> Lude.Text) (\s a -> s {qualificationRequestId = a} :: RejectQualificationRequest)
{-# DEPRECATED rqrQualificationRequestId "Use generic-lens or generic-optics with 'qualificationRequestId' instead." #-}

instance Lude.AWSRequest RejectQualificationRequest where
  type
    Rs RejectQualificationRequest =
      RejectQualificationRequestResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RejectQualificationRequestResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectQualificationRequest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.RejectQualificationRequest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RejectQualificationRequest where
  toJSON RejectQualificationRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Reason" Lude..=) Lude.<$> reason,
            Lude.Just
              ("QualificationRequestId" Lude..= qualificationRequestId)
          ]
      )

instance Lude.ToPath RejectQualificationRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectQualificationRequest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRejectQualificationRequestResponse' smart constructor.
newtype RejectQualificationRequestResponse = RejectQualificationRequestResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectQualificationRequestResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRejectQualificationRequestResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectQualificationRequestResponse
mkRejectQualificationRequestResponse pResponseStatus_ =
  RejectQualificationRequestResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqrrsResponseStatus :: Lens.Lens' RejectQualificationRequestResponse Lude.Int
rqrrsResponseStatus = Lens.lens (responseStatus :: RejectQualificationRequestResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectQualificationRequestResponse)
{-# DEPRECATED rqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

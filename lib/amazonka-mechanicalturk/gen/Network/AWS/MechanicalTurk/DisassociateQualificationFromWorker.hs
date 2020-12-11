{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DisassociateQualificationFromWorker@ revokes a previously granted Qualification from a user.
--
-- You can provide a text message explaining why the Qualification was revoked. The user who had the Qualification can see this message.
module Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
  ( -- * Creating a request
    DisassociateQualificationFromWorker (..),
    mkDisassociateQualificationFromWorker,

    -- ** Request lenses
    dqfwReason,
    dqfwWorkerId,
    dqfwQualificationTypeId,

    -- * Destructuring the response
    DisassociateQualificationFromWorkerResponse (..),
    mkDisassociateQualificationFromWorkerResponse,

    -- ** Response lenses
    dqfwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateQualificationFromWorker' smart constructor.
data DisassociateQualificationFromWorker = DisassociateQualificationFromWorker'
  { reason ::
      Lude.Maybe
        Lude.Text,
    workerId ::
      Lude.Text,
    qualificationTypeId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateQualificationFromWorker' with the minimum fields required to make a request.
--
-- * 'qualificationTypeId' - The ID of the Qualification type of the Qualification to be revoked.
-- * 'reason' - A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
-- * 'workerId' - The ID of the Worker who possesses the Qualification to be revoked.
mkDisassociateQualificationFromWorker ::
  -- | 'workerId'
  Lude.Text ->
  -- | 'qualificationTypeId'
  Lude.Text ->
  DisassociateQualificationFromWorker
mkDisassociateQualificationFromWorker
  pWorkerId_
  pQualificationTypeId_ =
    DisassociateQualificationFromWorker'
      { reason = Lude.Nothing,
        workerId = pWorkerId_,
        qualificationTypeId = pQualificationTypeId_
      }

-- | A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwReason :: Lens.Lens' DisassociateQualificationFromWorker (Lude.Maybe Lude.Text)
dqfwReason = Lens.lens (reason :: DisassociateQualificationFromWorker -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: DisassociateQualificationFromWorker)
{-# DEPRECATED dqfwReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Worker who possesses the Qualification to be revoked.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwWorkerId :: Lens.Lens' DisassociateQualificationFromWorker Lude.Text
dqfwWorkerId = Lens.lens (workerId :: DisassociateQualificationFromWorker -> Lude.Text) (\s a -> s {workerId = a} :: DisassociateQualificationFromWorker)
{-# DEPRECATED dqfwWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | The ID of the Qualification type of the Qualification to be revoked.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwQualificationTypeId :: Lens.Lens' DisassociateQualificationFromWorker Lude.Text
dqfwQualificationTypeId = Lens.lens (qualificationTypeId :: DisassociateQualificationFromWorker -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: DisassociateQualificationFromWorker)
{-# DEPRECATED dqfwQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

instance Lude.AWSRequest DisassociateQualificationFromWorker where
  type
    Rs DisassociateQualificationFromWorker =
      DisassociateQualificationFromWorkerResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateQualificationFromWorkerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateQualificationFromWorker where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.DisassociateQualificationFromWorker" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateQualificationFromWorker where
  toJSON DisassociateQualificationFromWorker' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Reason" Lude..=) Lude.<$> reason,
            Lude.Just ("WorkerId" Lude..= workerId),
            Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId)
          ]
      )

instance Lude.ToPath DisassociateQualificationFromWorker where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateQualificationFromWorker where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateQualificationFromWorkerResponse' smart constructor.
newtype DisassociateQualificationFromWorkerResponse = DisassociateQualificationFromWorkerResponse'
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
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DisassociateQualificationFromWorkerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateQualificationFromWorkerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateQualificationFromWorkerResponse
mkDisassociateQualificationFromWorkerResponse pResponseStatus_ =
  DisassociateQualificationFromWorkerResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqfwrsResponseStatus :: Lens.Lens' DisassociateQualificationFromWorkerResponse Lude.Int
dqfwrsResponseStatus = Lens.lens (responseStatus :: DisassociateQualificationFromWorkerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateQualificationFromWorkerResponse)
{-# DEPRECATED dqfwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

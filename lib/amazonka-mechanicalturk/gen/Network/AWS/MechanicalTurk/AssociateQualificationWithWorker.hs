{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AssociateQualificationWithWorker@ operation gives a Worker a Qualification. @AssociateQualificationWithWorker@ does not require that the Worker submit a Qualification request. It gives the Qualification directly to the Worker.
--
-- You can only assign a Qualification of a Qualification type that you created (using the @CreateQualificationType@ operation).
module Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
  ( -- * Creating a request
    AssociateQualificationWithWorker (..),
    mkAssociateQualificationWithWorker,

    -- ** Request lenses
    aqwwIntegerValue,
    aqwwSendNotification,
    aqwwQualificationTypeId,
    aqwwWorkerId,

    -- * Destructuring the response
    AssociateQualificationWithWorkerResponse (..),
    mkAssociateQualificationWithWorkerResponse,

    -- ** Response lenses
    aqwwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateQualificationWithWorker' smart constructor.
data AssociateQualificationWithWorker = AssociateQualificationWithWorker'
  { integerValue ::
      Lude.Maybe Lude.Int,
    sendNotification ::
      Lude.Maybe Lude.Bool,
    qualificationTypeId ::
      Lude.Text,
    workerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateQualificationWithWorker' with the minimum fields required to make a request.
--
-- * 'integerValue' - The value of the Qualification to assign.
-- * 'qualificationTypeId' - The ID of the Qualification type to use for the assigned Qualification.
-- * 'sendNotification' - Specifies whether to send a notification email message to the Worker saying that the qualification was assigned to the Worker. Note: this is true by default.
-- * 'workerId' - The ID of the Worker to whom the Qualification is being assigned. Worker IDs are included with submitted HIT assignments and Qualification requests.
mkAssociateQualificationWithWorker ::
  -- | 'qualificationTypeId'
  Lude.Text ->
  -- | 'workerId'
  Lude.Text ->
  AssociateQualificationWithWorker
mkAssociateQualificationWithWorker pQualificationTypeId_ pWorkerId_ =
  AssociateQualificationWithWorker'
    { integerValue = Lude.Nothing,
      sendNotification = Lude.Nothing,
      qualificationTypeId = pQualificationTypeId_,
      workerId = pWorkerId_
    }

-- | The value of the Qualification to assign.
--
-- /Note:/ Consider using 'integerValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwIntegerValue :: Lens.Lens' AssociateQualificationWithWorker (Lude.Maybe Lude.Int)
aqwwIntegerValue = Lens.lens (integerValue :: AssociateQualificationWithWorker -> Lude.Maybe Lude.Int) (\s a -> s {integerValue = a} :: AssociateQualificationWithWorker)
{-# DEPRECATED aqwwIntegerValue "Use generic-lens or generic-optics with 'integerValue' instead." #-}

-- | Specifies whether to send a notification email message to the Worker saying that the qualification was assigned to the Worker. Note: this is true by default.
--
-- /Note:/ Consider using 'sendNotification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwSendNotification :: Lens.Lens' AssociateQualificationWithWorker (Lude.Maybe Lude.Bool)
aqwwSendNotification = Lens.lens (sendNotification :: AssociateQualificationWithWorker -> Lude.Maybe Lude.Bool) (\s a -> s {sendNotification = a} :: AssociateQualificationWithWorker)
{-# DEPRECATED aqwwSendNotification "Use generic-lens or generic-optics with 'sendNotification' instead." #-}

-- | The ID of the Qualification type to use for the assigned Qualification.
--
-- /Note:/ Consider using 'qualificationTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwQualificationTypeId :: Lens.Lens' AssociateQualificationWithWorker Lude.Text
aqwwQualificationTypeId = Lens.lens (qualificationTypeId :: AssociateQualificationWithWorker -> Lude.Text) (\s a -> s {qualificationTypeId = a} :: AssociateQualificationWithWorker)
{-# DEPRECATED aqwwQualificationTypeId "Use generic-lens or generic-optics with 'qualificationTypeId' instead." #-}

-- | The ID of the Worker to whom the Qualification is being assigned. Worker IDs are included with submitted HIT assignments and Qualification requests.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwWorkerId :: Lens.Lens' AssociateQualificationWithWorker Lude.Text
aqwwWorkerId = Lens.lens (workerId :: AssociateQualificationWithWorker -> Lude.Text) (\s a -> s {workerId = a} :: AssociateQualificationWithWorker)
{-# DEPRECATED aqwwWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Lude.AWSRequest AssociateQualificationWithWorker where
  type
    Rs AssociateQualificationWithWorker =
      AssociateQualificationWithWorkerResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateQualificationWithWorkerResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateQualificationWithWorker where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.AssociateQualificationWithWorker" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateQualificationWithWorker where
  toJSON AssociateQualificationWithWorker' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IntegerValue" Lude..=) Lude.<$> integerValue,
            ("SendNotification" Lude..=) Lude.<$> sendNotification,
            Lude.Just ("QualificationTypeId" Lude..= qualificationTypeId),
            Lude.Just ("WorkerId" Lude..= workerId)
          ]
      )

instance Lude.ToPath AssociateQualificationWithWorker where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateQualificationWithWorker where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateQualificationWithWorkerResponse' smart constructor.
newtype AssociateQualificationWithWorkerResponse = AssociateQualificationWithWorkerResponse'
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

-- | Creates a value of 'AssociateQualificationWithWorkerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateQualificationWithWorkerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateQualificationWithWorkerResponse
mkAssociateQualificationWithWorkerResponse pResponseStatus_ =
  AssociateQualificationWithWorkerResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aqwwrsResponseStatus :: Lens.Lens' AssociateQualificationWithWorkerResponse Lude.Int
aqwwrsResponseStatus = Lens.lens (responseStatus :: AssociateQualificationWithWorkerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateQualificationWithWorkerResponse)
{-# DEPRECATED aqwwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

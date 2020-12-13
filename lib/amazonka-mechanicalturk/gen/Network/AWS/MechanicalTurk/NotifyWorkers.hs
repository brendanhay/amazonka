{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.NotifyWorkers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @NotifyWorkers@ operation sends an email to one or more Workers that you specify with the Worker ID. You can specify up to 100 Worker IDs to send the same message with a single call to the NotifyWorkers operation. The NotifyWorkers operation will send a notification email to a Worker only if you have previously approved or rejected work from the Worker.
module Network.AWS.MechanicalTurk.NotifyWorkers
  ( -- * Creating a request
    NotifyWorkers (..),
    mkNotifyWorkers,

    -- ** Request lenses
    nwSubject,
    nwMessageText,
    nwWorkerIds,

    -- * Destructuring the response
    NotifyWorkersResponse (..),
    mkNotifyWorkersResponse,

    -- ** Response lenses
    nwrsNotifyWorkersFailureStatuses,
    nwrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkNotifyWorkers' smart constructor.
data NotifyWorkers = NotifyWorkers'
  { -- | The subject line of the email message to send. Can include up to 200 characters.
    subject :: Lude.Text,
    -- | The text of the email message to send. Can include up to 4,096 characters
    messageText :: Lude.Text,
    -- | A list of Worker IDs you wish to notify. You can notify upto 100 Workers at a time.
    workerIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyWorkers' with the minimum fields required to make a request.
--
-- * 'subject' - The subject line of the email message to send. Can include up to 200 characters.
-- * 'messageText' - The text of the email message to send. Can include up to 4,096 characters
-- * 'workerIds' - A list of Worker IDs you wish to notify. You can notify upto 100 Workers at a time.
mkNotifyWorkers ::
  -- | 'subject'
  Lude.Text ->
  -- | 'messageText'
  Lude.Text ->
  NotifyWorkers
mkNotifyWorkers pSubject_ pMessageText_ =
  NotifyWorkers'
    { subject = pSubject_,
      messageText = pMessageText_,
      workerIds = Lude.mempty
    }

-- | The subject line of the email message to send. Can include up to 200 characters.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwSubject :: Lens.Lens' NotifyWorkers Lude.Text
nwSubject = Lens.lens (subject :: NotifyWorkers -> Lude.Text) (\s a -> s {subject = a} :: NotifyWorkers)
{-# DEPRECATED nwSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

-- | The text of the email message to send. Can include up to 4,096 characters
--
-- /Note:/ Consider using 'messageText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwMessageText :: Lens.Lens' NotifyWorkers Lude.Text
nwMessageText = Lens.lens (messageText :: NotifyWorkers -> Lude.Text) (\s a -> s {messageText = a} :: NotifyWorkers)
{-# DEPRECATED nwMessageText "Use generic-lens or generic-optics with 'messageText' instead." #-}

-- | A list of Worker IDs you wish to notify. You can notify upto 100 Workers at a time.
--
-- /Note:/ Consider using 'workerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwWorkerIds :: Lens.Lens' NotifyWorkers [Lude.Text]
nwWorkerIds = Lens.lens (workerIds :: NotifyWorkers -> [Lude.Text]) (\s a -> s {workerIds = a} :: NotifyWorkers)
{-# DEPRECATED nwWorkerIds "Use generic-lens or generic-optics with 'workerIds' instead." #-}

instance Lude.AWSRequest NotifyWorkers where
  type Rs NotifyWorkers = NotifyWorkersResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          NotifyWorkersResponse'
            Lude.<$> (x Lude..?> "NotifyWorkersFailureStatuses" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders NotifyWorkers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.NotifyWorkers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON NotifyWorkers where
  toJSON NotifyWorkers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Subject" Lude..= subject),
            Lude.Just ("MessageText" Lude..= messageText),
            Lude.Just ("WorkerIds" Lude..= workerIds)
          ]
      )

instance Lude.ToPath NotifyWorkers where
  toPath = Lude.const "/"

instance Lude.ToQuery NotifyWorkers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkNotifyWorkersResponse' smart constructor.
data NotifyWorkersResponse = NotifyWorkersResponse'
  { -- | When MTurk sends notifications to the list of Workers, it returns back any failures it encounters in this list of NotifyWorkersFailureStatus objects.
    notifyWorkersFailureStatuses :: Lude.Maybe [NotifyWorkersFailureStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyWorkersResponse' with the minimum fields required to make a request.
--
-- * 'notifyWorkersFailureStatuses' - When MTurk sends notifications to the list of Workers, it returns back any failures it encounters in this list of NotifyWorkersFailureStatus objects.
-- * 'responseStatus' - The response status code.
mkNotifyWorkersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  NotifyWorkersResponse
mkNotifyWorkersResponse pResponseStatus_ =
  NotifyWorkersResponse'
    { notifyWorkersFailureStatuses =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When MTurk sends notifications to the list of Workers, it returns back any failures it encounters in this list of NotifyWorkersFailureStatus objects.
--
-- /Note:/ Consider using 'notifyWorkersFailureStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwrsNotifyWorkersFailureStatuses :: Lens.Lens' NotifyWorkersResponse (Lude.Maybe [NotifyWorkersFailureStatus])
nwrsNotifyWorkersFailureStatuses = Lens.lens (notifyWorkersFailureStatuses :: NotifyWorkersResponse -> Lude.Maybe [NotifyWorkersFailureStatus]) (\s a -> s {notifyWorkersFailureStatuses = a} :: NotifyWorkersResponse)
{-# DEPRECATED nwrsNotifyWorkersFailureStatuses "Use generic-lens or generic-optics with 'notifyWorkersFailureStatuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nwrsResponseStatus :: Lens.Lens' NotifyWorkersResponse Lude.Int
nwrsResponseStatus = Lens.lens (responseStatus :: NotifyWorkersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: NotifyWorkersResponse)
{-# DEPRECATED nwrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

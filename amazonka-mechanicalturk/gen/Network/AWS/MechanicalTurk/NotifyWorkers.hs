{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.NotifyWorkers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @NotifyWorkers@ operation sends an email to one or more Workers that
-- you specify with the Worker ID. You can specify up to 100 Worker IDs to
-- send the same message with a single call to the NotifyWorkers operation.
-- The NotifyWorkers operation will send a notification email to a Worker
-- only if you have previously approved or rejected work from the Worker.
module Network.AWS.MechanicalTurk.NotifyWorkers
  ( -- * Creating a Request
    NotifyWorkers (..),
    newNotifyWorkers,

    -- * Request Lenses
    notifyWorkers_subject,
    notifyWorkers_messageText,
    notifyWorkers_workerIds,

    -- * Destructuring the Response
    NotifyWorkersResponse (..),
    newNotifyWorkersResponse,

    -- * Response Lenses
    notifyWorkersResponse_notifyWorkersFailureStatuses,
    notifyWorkersResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newNotifyWorkers' smart constructor.
data NotifyWorkers = NotifyWorkers'
  { -- | The subject line of the email message to send. Can include up to 200
    -- characters.
    subject :: Prelude.Text,
    -- | The text of the email message to send. Can include up to 4,096
    -- characters
    messageText :: Prelude.Text,
    -- | A list of Worker IDs you wish to notify. You can notify upto 100 Workers
    -- at a time.
    workerIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotifyWorkers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subject', 'notifyWorkers_subject' - The subject line of the email message to send. Can include up to 200
-- characters.
--
-- 'messageText', 'notifyWorkers_messageText' - The text of the email message to send. Can include up to 4,096
-- characters
--
-- 'workerIds', 'notifyWorkers_workerIds' - A list of Worker IDs you wish to notify. You can notify upto 100 Workers
-- at a time.
newNotifyWorkers ::
  -- | 'subject'
  Prelude.Text ->
  -- | 'messageText'
  Prelude.Text ->
  NotifyWorkers
newNotifyWorkers pSubject_ pMessageText_ =
  NotifyWorkers'
    { subject = pSubject_,
      messageText = pMessageText_,
      workerIds = Prelude.mempty
    }

-- | The subject line of the email message to send. Can include up to 200
-- characters.
notifyWorkers_subject :: Lens.Lens' NotifyWorkers Prelude.Text
notifyWorkers_subject = Lens.lens (\NotifyWorkers' {subject} -> subject) (\s@NotifyWorkers' {} a -> s {subject = a} :: NotifyWorkers)

-- | The text of the email message to send. Can include up to 4,096
-- characters
notifyWorkers_messageText :: Lens.Lens' NotifyWorkers Prelude.Text
notifyWorkers_messageText = Lens.lens (\NotifyWorkers' {messageText} -> messageText) (\s@NotifyWorkers' {} a -> s {messageText = a} :: NotifyWorkers)

-- | A list of Worker IDs you wish to notify. You can notify upto 100 Workers
-- at a time.
notifyWorkers_workerIds :: Lens.Lens' NotifyWorkers [Prelude.Text]
notifyWorkers_workerIds = Lens.lens (\NotifyWorkers' {workerIds} -> workerIds) (\s@NotifyWorkers' {} a -> s {workerIds = a} :: NotifyWorkers) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest NotifyWorkers where
  type Rs NotifyWorkers = NotifyWorkersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          NotifyWorkersResponse'
            Prelude.<$> ( x Prelude..?> "NotifyWorkersFailureStatuses"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable NotifyWorkers

instance Prelude.NFData NotifyWorkers

instance Prelude.ToHeaders NotifyWorkers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.NotifyWorkers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON NotifyWorkers where
  toJSON NotifyWorkers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Subject" Prelude..= subject),
            Prelude.Just ("MessageText" Prelude..= messageText),
            Prelude.Just ("WorkerIds" Prelude..= workerIds)
          ]
      )

instance Prelude.ToPath NotifyWorkers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery NotifyWorkers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyWorkersResponse' smart constructor.
data NotifyWorkersResponse = NotifyWorkersResponse'
  { -- | When MTurk sends notifications to the list of Workers, it returns back
    -- any failures it encounters in this list of NotifyWorkersFailureStatus
    -- objects.
    notifyWorkersFailureStatuses :: Prelude.Maybe [NotifyWorkersFailureStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NotifyWorkersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notifyWorkersFailureStatuses', 'notifyWorkersResponse_notifyWorkersFailureStatuses' - When MTurk sends notifications to the list of Workers, it returns back
-- any failures it encounters in this list of NotifyWorkersFailureStatus
-- objects.
--
-- 'httpStatus', 'notifyWorkersResponse_httpStatus' - The response's http status code.
newNotifyWorkersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyWorkersResponse
newNotifyWorkersResponse pHttpStatus_ =
  NotifyWorkersResponse'
    { notifyWorkersFailureStatuses =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When MTurk sends notifications to the list of Workers, it returns back
-- any failures it encounters in this list of NotifyWorkersFailureStatus
-- objects.
notifyWorkersResponse_notifyWorkersFailureStatuses :: Lens.Lens' NotifyWorkersResponse (Prelude.Maybe [NotifyWorkersFailureStatus])
notifyWorkersResponse_notifyWorkersFailureStatuses = Lens.lens (\NotifyWorkersResponse' {notifyWorkersFailureStatuses} -> notifyWorkersFailureStatuses) (\s@NotifyWorkersResponse' {} a -> s {notifyWorkersFailureStatuses = a} :: NotifyWorkersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
notifyWorkersResponse_httpStatus :: Lens.Lens' NotifyWorkersResponse Prelude.Int
notifyWorkersResponse_httpStatus = Lens.lens (\NotifyWorkersResponse' {httpStatus} -> httpStatus) (\s@NotifyWorkersResponse' {} a -> s {httpStatus = a} :: NotifyWorkersResponse)

instance Prelude.NFData NotifyWorkersResponse

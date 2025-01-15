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
-- Module      : Amazonka.MechanicalTurk.NotifyWorkers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @NotifyWorkers@ operation sends an email to one or more Workers that
-- you specify with the Worker ID. You can specify up to 100 Worker IDs to
-- send the same message with a single call to the NotifyWorkers operation.
-- The NotifyWorkers operation will send a notification email to a Worker
-- only if you have previously approved or rejected work from the Worker.
module Amazonka.MechanicalTurk.NotifyWorkers
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
notifyWorkers_workerIds = Lens.lens (\NotifyWorkers' {workerIds} -> workerIds) (\s@NotifyWorkers' {} a -> s {workerIds = a} :: NotifyWorkers) Prelude.. Lens.coerced

instance Core.AWSRequest NotifyWorkers where
  type
    AWSResponse NotifyWorkers =
      NotifyWorkersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          NotifyWorkersResponse'
            Prelude.<$> ( x
                            Data..?> "NotifyWorkersFailureStatuses"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable NotifyWorkers where
  hashWithSalt _salt NotifyWorkers' {..} =
    _salt
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` messageText
      `Prelude.hashWithSalt` workerIds

instance Prelude.NFData NotifyWorkers where
  rnf NotifyWorkers' {..} =
    Prelude.rnf subject `Prelude.seq`
      Prelude.rnf messageText `Prelude.seq`
        Prelude.rnf workerIds

instance Data.ToHeaders NotifyWorkers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.NotifyWorkers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON NotifyWorkers where
  toJSON NotifyWorkers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Subject" Data..= subject),
            Prelude.Just ("MessageText" Data..= messageText),
            Prelude.Just ("WorkerIds" Data..= workerIds)
          ]
      )

instance Data.ToPath NotifyWorkers where
  toPath = Prelude.const "/"

instance Data.ToQuery NotifyWorkers where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
notifyWorkersResponse_notifyWorkersFailureStatuses = Lens.lens (\NotifyWorkersResponse' {notifyWorkersFailureStatuses} -> notifyWorkersFailureStatuses) (\s@NotifyWorkersResponse' {} a -> s {notifyWorkersFailureStatuses = a} :: NotifyWorkersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
notifyWorkersResponse_httpStatus :: Lens.Lens' NotifyWorkersResponse Prelude.Int
notifyWorkersResponse_httpStatus = Lens.lens (\NotifyWorkersResponse' {httpStatus} -> httpStatus) (\s@NotifyWorkersResponse' {} a -> s {httpStatus = a} :: NotifyWorkersResponse)

instance Prelude.NFData NotifyWorkersResponse where
  rnf NotifyWorkersResponse' {..} =
    Prelude.rnf notifyWorkersFailureStatuses `Prelude.seq`
      Prelude.rnf httpStatus

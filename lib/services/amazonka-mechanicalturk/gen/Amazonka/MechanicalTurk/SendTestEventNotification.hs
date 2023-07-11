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
-- Module      : Amazonka.MechanicalTurk.SendTestEventNotification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @SendTestEventNotification@ operation causes Amazon Mechanical Turk
-- to send a notification message as if a HIT event occurred, according to
-- the provided notification specification. This allows you to test
-- notifications without setting up notifications for a real HIT type and
-- trying to trigger them using the website. When you call this operation,
-- the service attempts to send the test notification immediately.
module Amazonka.MechanicalTurk.SendTestEventNotification
  ( -- * Creating a Request
    SendTestEventNotification (..),
    newSendTestEventNotification,

    -- * Request Lenses
    sendTestEventNotification_notification,
    sendTestEventNotification_testEventType,

    -- * Destructuring the Response
    SendTestEventNotificationResponse (..),
    newSendTestEventNotificationResponse,

    -- * Response Lenses
    sendTestEventNotificationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendTestEventNotification' smart constructor.
data SendTestEventNotification = SendTestEventNotification'
  { -- | The notification specification to test. This value is identical to the
    -- value you would provide to the UpdateNotificationSettings operation when
    -- you establish the notification specification for a HIT type.
    notification :: NotificationSpecification,
    -- | The event to simulate to test the notification specification. This event
    -- is included in the test message even if the notification specification
    -- does not include the event type. The notification specification does not
    -- filter out the test event.
    testEventType :: EventType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendTestEventNotification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notification', 'sendTestEventNotification_notification' - The notification specification to test. This value is identical to the
-- value you would provide to the UpdateNotificationSettings operation when
-- you establish the notification specification for a HIT type.
--
-- 'testEventType', 'sendTestEventNotification_testEventType' - The event to simulate to test the notification specification. This event
-- is included in the test message even if the notification specification
-- does not include the event type. The notification specification does not
-- filter out the test event.
newSendTestEventNotification ::
  -- | 'notification'
  NotificationSpecification ->
  -- | 'testEventType'
  EventType ->
  SendTestEventNotification
newSendTestEventNotification
  pNotification_
  pTestEventType_ =
    SendTestEventNotification'
      { notification =
          pNotification_,
        testEventType = pTestEventType_
      }

-- | The notification specification to test. This value is identical to the
-- value you would provide to the UpdateNotificationSettings operation when
-- you establish the notification specification for a HIT type.
sendTestEventNotification_notification :: Lens.Lens' SendTestEventNotification NotificationSpecification
sendTestEventNotification_notification = Lens.lens (\SendTestEventNotification' {notification} -> notification) (\s@SendTestEventNotification' {} a -> s {notification = a} :: SendTestEventNotification)

-- | The event to simulate to test the notification specification. This event
-- is included in the test message even if the notification specification
-- does not include the event type. The notification specification does not
-- filter out the test event.
sendTestEventNotification_testEventType :: Lens.Lens' SendTestEventNotification EventType
sendTestEventNotification_testEventType = Lens.lens (\SendTestEventNotification' {testEventType} -> testEventType) (\s@SendTestEventNotification' {} a -> s {testEventType = a} :: SendTestEventNotification)

instance Core.AWSRequest SendTestEventNotification where
  type
    AWSResponse SendTestEventNotification =
      SendTestEventNotificationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SendTestEventNotificationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendTestEventNotification where
  hashWithSalt _salt SendTestEventNotification' {..} =
    _salt
      `Prelude.hashWithSalt` notification
      `Prelude.hashWithSalt` testEventType

instance Prelude.NFData SendTestEventNotification where
  rnf SendTestEventNotification' {..} =
    Prelude.rnf notification
      `Prelude.seq` Prelude.rnf testEventType

instance Data.ToHeaders SendTestEventNotification where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.SendTestEventNotification" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendTestEventNotification where
  toJSON SendTestEventNotification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Notification" Data..= notification),
            Prelude.Just
              ("TestEventType" Data..= testEventType)
          ]
      )

instance Data.ToPath SendTestEventNotification where
  toPath = Prelude.const "/"

instance Data.ToQuery SendTestEventNotification where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendTestEventNotificationResponse' smart constructor.
data SendTestEventNotificationResponse = SendTestEventNotificationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendTestEventNotificationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'sendTestEventNotificationResponse_httpStatus' - The response's http status code.
newSendTestEventNotificationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendTestEventNotificationResponse
newSendTestEventNotificationResponse pHttpStatus_ =
  SendTestEventNotificationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
sendTestEventNotificationResponse_httpStatus :: Lens.Lens' SendTestEventNotificationResponse Prelude.Int
sendTestEventNotificationResponse_httpStatus = Lens.lens (\SendTestEventNotificationResponse' {httpStatus} -> httpStatus) (\s@SendTestEventNotificationResponse' {} a -> s {httpStatus = a} :: SendTestEventNotificationResponse)

instance
  Prelude.NFData
    SendTestEventNotificationResponse
  where
  rnf SendTestEventNotificationResponse' {..} =
    Prelude.rnf httpStatus

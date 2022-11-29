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
-- Module      : Amazonka.Inspector.SubscribeToEvent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the process of sending Amazon Simple Notification Service (SNS)
-- notifications about a specified event to a specified SNS topic.
module Amazonka.Inspector.SubscribeToEvent
  ( -- * Creating a Request
    SubscribeToEvent (..),
    newSubscribeToEvent,

    -- * Request Lenses
    subscribeToEvent_resourceArn,
    subscribeToEvent_event,
    subscribeToEvent_topicArn,

    -- * Destructuring the Response
    SubscribeToEventResponse (..),
    newSubscribeToEventResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSubscribeToEvent' smart constructor.
data SubscribeToEvent = SubscribeToEvent'
  { -- | The ARN of the assessment template that is used during the event for
    -- which you want to receive SNS notifications.
    resourceArn :: Prelude.Text,
    -- | The event for which you want to receive SNS notifications.
    event :: InspectorEvent,
    -- | The ARN of the SNS topic to which the SNS notifications are sent.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubscribeToEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'subscribeToEvent_resourceArn' - The ARN of the assessment template that is used during the event for
-- which you want to receive SNS notifications.
--
-- 'event', 'subscribeToEvent_event' - The event for which you want to receive SNS notifications.
--
-- 'topicArn', 'subscribeToEvent_topicArn' - The ARN of the SNS topic to which the SNS notifications are sent.
newSubscribeToEvent ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'event'
  InspectorEvent ->
  -- | 'topicArn'
  Prelude.Text ->
  SubscribeToEvent
newSubscribeToEvent pResourceArn_ pEvent_ pTopicArn_ =
  SubscribeToEvent'
    { resourceArn = pResourceArn_,
      event = pEvent_,
      topicArn = pTopicArn_
    }

-- | The ARN of the assessment template that is used during the event for
-- which you want to receive SNS notifications.
subscribeToEvent_resourceArn :: Lens.Lens' SubscribeToEvent Prelude.Text
subscribeToEvent_resourceArn = Lens.lens (\SubscribeToEvent' {resourceArn} -> resourceArn) (\s@SubscribeToEvent' {} a -> s {resourceArn = a} :: SubscribeToEvent)

-- | The event for which you want to receive SNS notifications.
subscribeToEvent_event :: Lens.Lens' SubscribeToEvent InspectorEvent
subscribeToEvent_event = Lens.lens (\SubscribeToEvent' {event} -> event) (\s@SubscribeToEvent' {} a -> s {event = a} :: SubscribeToEvent)

-- | The ARN of the SNS topic to which the SNS notifications are sent.
subscribeToEvent_topicArn :: Lens.Lens' SubscribeToEvent Prelude.Text
subscribeToEvent_topicArn = Lens.lens (\SubscribeToEvent' {topicArn} -> topicArn) (\s@SubscribeToEvent' {} a -> s {topicArn = a} :: SubscribeToEvent)

instance Core.AWSRequest SubscribeToEvent where
  type
    AWSResponse SubscribeToEvent =
      SubscribeToEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull SubscribeToEventResponse'

instance Prelude.Hashable SubscribeToEvent where
  hashWithSalt _salt SubscribeToEvent' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` topicArn

instance Prelude.NFData SubscribeToEvent where
  rnf SubscribeToEvent' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf topicArn

instance Core.ToHeaders SubscribeToEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.SubscribeToEvent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SubscribeToEvent where
  toJSON SubscribeToEvent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceArn" Core..= resourceArn),
            Prelude.Just ("event" Core..= event),
            Prelude.Just ("topicArn" Core..= topicArn)
          ]
      )

instance Core.ToPath SubscribeToEvent where
  toPath = Prelude.const "/"

instance Core.ToQuery SubscribeToEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubscribeToEventResponse' smart constructor.
data SubscribeToEventResponse = SubscribeToEventResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubscribeToEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSubscribeToEventResponse ::
  SubscribeToEventResponse
newSubscribeToEventResponse =
  SubscribeToEventResponse'

instance Prelude.NFData SubscribeToEventResponse where
  rnf _ = ()

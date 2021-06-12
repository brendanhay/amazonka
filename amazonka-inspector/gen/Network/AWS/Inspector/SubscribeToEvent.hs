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
-- Module      : Network.AWS.Inspector.SubscribeToEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the process of sending Amazon Simple Notification Service (SNS)
-- notifications about a specified event to a specified SNS topic.
module Network.AWS.Inspector.SubscribeToEvent
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

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSubscribeToEvent' smart constructor.
data SubscribeToEvent = SubscribeToEvent'
  { -- | The ARN of the assessment template that is used during the event for
    -- which you want to receive SNS notifications.
    resourceArn :: Core.Text,
    -- | The event for which you want to receive SNS notifications.
    event :: InspectorEvent,
    -- | The ARN of the SNS topic to which the SNS notifications are sent.
    topicArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'event'
  InspectorEvent ->
  -- | 'topicArn'
  Core.Text ->
  SubscribeToEvent
newSubscribeToEvent pResourceArn_ pEvent_ pTopicArn_ =
  SubscribeToEvent'
    { resourceArn = pResourceArn_,
      event = pEvent_,
      topicArn = pTopicArn_
    }

-- | The ARN of the assessment template that is used during the event for
-- which you want to receive SNS notifications.
subscribeToEvent_resourceArn :: Lens.Lens' SubscribeToEvent Core.Text
subscribeToEvent_resourceArn = Lens.lens (\SubscribeToEvent' {resourceArn} -> resourceArn) (\s@SubscribeToEvent' {} a -> s {resourceArn = a} :: SubscribeToEvent)

-- | The event for which you want to receive SNS notifications.
subscribeToEvent_event :: Lens.Lens' SubscribeToEvent InspectorEvent
subscribeToEvent_event = Lens.lens (\SubscribeToEvent' {event} -> event) (\s@SubscribeToEvent' {} a -> s {event = a} :: SubscribeToEvent)

-- | The ARN of the SNS topic to which the SNS notifications are sent.
subscribeToEvent_topicArn :: Lens.Lens' SubscribeToEvent Core.Text
subscribeToEvent_topicArn = Lens.lens (\SubscribeToEvent' {topicArn} -> topicArn) (\s@SubscribeToEvent' {} a -> s {topicArn = a} :: SubscribeToEvent)

instance Core.AWSRequest SubscribeToEvent where
  type
    AWSResponse SubscribeToEvent =
      SubscribeToEventResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SubscribeToEventResponse'

instance Core.Hashable SubscribeToEvent

instance Core.NFData SubscribeToEvent

instance Core.ToHeaders SubscribeToEvent where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.SubscribeToEvent" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SubscribeToEvent where
  toJSON SubscribeToEvent' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceArn" Core..= resourceArn),
            Core.Just ("event" Core..= event),
            Core.Just ("topicArn" Core..= topicArn)
          ]
      )

instance Core.ToPath SubscribeToEvent where
  toPath = Core.const "/"

instance Core.ToQuery SubscribeToEvent where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSubscribeToEventResponse' smart constructor.
data SubscribeToEventResponse = SubscribeToEventResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubscribeToEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSubscribeToEventResponse ::
  SubscribeToEventResponse
newSubscribeToEventResponse =
  SubscribeToEventResponse'

instance Core.NFData SubscribeToEventResponse

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
-- Module      : Network.AWS.Inspector.UnsubscribeFromEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the process of sending Amazon Simple Notification Service (SNS)
-- notifications about a specified event to a specified SNS topic.
module Network.AWS.Inspector.UnsubscribeFromEvent
  ( -- * Creating a Request
    UnsubscribeFromEvent (..),
    newUnsubscribeFromEvent,

    -- * Request Lenses
    unsubscribeFromEvent_resourceArn,
    unsubscribeFromEvent_event,
    unsubscribeFromEvent_topicArn,

    -- * Destructuring the Response
    UnsubscribeFromEventResponse (..),
    newUnsubscribeFromEventResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUnsubscribeFromEvent' smart constructor.
data UnsubscribeFromEvent = UnsubscribeFromEvent'
  { -- | The ARN of the assessment template that is used during the event for
    -- which you want to stop receiving SNS notifications.
    resourceArn :: Core.Text,
    -- | The event for which you want to stop receiving SNS notifications.
    event :: InspectorEvent,
    -- | The ARN of the SNS topic to which SNS notifications are sent.
    topicArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnsubscribeFromEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'unsubscribeFromEvent_resourceArn' - The ARN of the assessment template that is used during the event for
-- which you want to stop receiving SNS notifications.
--
-- 'event', 'unsubscribeFromEvent_event' - The event for which you want to stop receiving SNS notifications.
--
-- 'topicArn', 'unsubscribeFromEvent_topicArn' - The ARN of the SNS topic to which SNS notifications are sent.
newUnsubscribeFromEvent ::
  -- | 'resourceArn'
  Core.Text ->
  -- | 'event'
  InspectorEvent ->
  -- | 'topicArn'
  Core.Text ->
  UnsubscribeFromEvent
newUnsubscribeFromEvent
  pResourceArn_
  pEvent_
  pTopicArn_ =
    UnsubscribeFromEvent'
      { resourceArn = pResourceArn_,
        event = pEvent_,
        topicArn = pTopicArn_
      }

-- | The ARN of the assessment template that is used during the event for
-- which you want to stop receiving SNS notifications.
unsubscribeFromEvent_resourceArn :: Lens.Lens' UnsubscribeFromEvent Core.Text
unsubscribeFromEvent_resourceArn = Lens.lens (\UnsubscribeFromEvent' {resourceArn} -> resourceArn) (\s@UnsubscribeFromEvent' {} a -> s {resourceArn = a} :: UnsubscribeFromEvent)

-- | The event for which you want to stop receiving SNS notifications.
unsubscribeFromEvent_event :: Lens.Lens' UnsubscribeFromEvent InspectorEvent
unsubscribeFromEvent_event = Lens.lens (\UnsubscribeFromEvent' {event} -> event) (\s@UnsubscribeFromEvent' {} a -> s {event = a} :: UnsubscribeFromEvent)

-- | The ARN of the SNS topic to which SNS notifications are sent.
unsubscribeFromEvent_topicArn :: Lens.Lens' UnsubscribeFromEvent Core.Text
unsubscribeFromEvent_topicArn = Lens.lens (\UnsubscribeFromEvent' {topicArn} -> topicArn) (\s@UnsubscribeFromEvent' {} a -> s {topicArn = a} :: UnsubscribeFromEvent)

instance Core.AWSRequest UnsubscribeFromEvent where
  type
    AWSResponse UnsubscribeFromEvent =
      UnsubscribeFromEventResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UnsubscribeFromEventResponse'

instance Core.Hashable UnsubscribeFromEvent

instance Core.NFData UnsubscribeFromEvent

instance Core.ToHeaders UnsubscribeFromEvent where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.UnsubscribeFromEvent" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UnsubscribeFromEvent where
  toJSON UnsubscribeFromEvent' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceArn" Core..= resourceArn),
            Core.Just ("event" Core..= event),
            Core.Just ("topicArn" Core..= topicArn)
          ]
      )

instance Core.ToPath UnsubscribeFromEvent where
  toPath = Core.const "/"

instance Core.ToQuery UnsubscribeFromEvent where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUnsubscribeFromEventResponse' smart constructor.
data UnsubscribeFromEventResponse = UnsubscribeFromEventResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnsubscribeFromEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnsubscribeFromEventResponse ::
  UnsubscribeFromEventResponse
newUnsubscribeFromEventResponse =
  UnsubscribeFromEventResponse'

instance Core.NFData UnsubscribeFromEventResponse

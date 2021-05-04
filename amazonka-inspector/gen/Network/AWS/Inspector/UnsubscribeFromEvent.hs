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

import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUnsubscribeFromEvent' smart constructor.
data UnsubscribeFromEvent = UnsubscribeFromEvent'
  { -- | The ARN of the assessment template that is used during the event for
    -- which you want to stop receiving SNS notifications.
    resourceArn :: Prelude.Text,
    -- | The event for which you want to stop receiving SNS notifications.
    event :: InspectorEvent,
    -- | The ARN of the SNS topic to which SNS notifications are sent.
    topicArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'event'
  InspectorEvent ->
  -- | 'topicArn'
  Prelude.Text ->
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
unsubscribeFromEvent_resourceArn :: Lens.Lens' UnsubscribeFromEvent Prelude.Text
unsubscribeFromEvent_resourceArn = Lens.lens (\UnsubscribeFromEvent' {resourceArn} -> resourceArn) (\s@UnsubscribeFromEvent' {} a -> s {resourceArn = a} :: UnsubscribeFromEvent)

-- | The event for which you want to stop receiving SNS notifications.
unsubscribeFromEvent_event :: Lens.Lens' UnsubscribeFromEvent InspectorEvent
unsubscribeFromEvent_event = Lens.lens (\UnsubscribeFromEvent' {event} -> event) (\s@UnsubscribeFromEvent' {} a -> s {event = a} :: UnsubscribeFromEvent)

-- | The ARN of the SNS topic to which SNS notifications are sent.
unsubscribeFromEvent_topicArn :: Lens.Lens' UnsubscribeFromEvent Prelude.Text
unsubscribeFromEvent_topicArn = Lens.lens (\UnsubscribeFromEvent' {topicArn} -> topicArn) (\s@UnsubscribeFromEvent' {} a -> s {topicArn = a} :: UnsubscribeFromEvent)

instance Prelude.AWSRequest UnsubscribeFromEvent where
  type
    Rs UnsubscribeFromEvent =
      UnsubscribeFromEventResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UnsubscribeFromEventResponse'

instance Prelude.Hashable UnsubscribeFromEvent

instance Prelude.NFData UnsubscribeFromEvent

instance Prelude.ToHeaders UnsubscribeFromEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "InspectorService.UnsubscribeFromEvent" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UnsubscribeFromEvent where
  toJSON UnsubscribeFromEvent' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceArn" Prelude..= resourceArn),
            Prelude.Just ("event" Prelude..= event),
            Prelude.Just ("topicArn" Prelude..= topicArn)
          ]
      )

instance Prelude.ToPath UnsubscribeFromEvent where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UnsubscribeFromEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnsubscribeFromEventResponse' smart constructor.
data UnsubscribeFromEventResponse = UnsubscribeFromEventResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UnsubscribeFromEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnsubscribeFromEventResponse ::
  UnsubscribeFromEventResponse
newUnsubscribeFromEventResponse =
  UnsubscribeFromEventResponse'

instance Prelude.NFData UnsubscribeFromEventResponse

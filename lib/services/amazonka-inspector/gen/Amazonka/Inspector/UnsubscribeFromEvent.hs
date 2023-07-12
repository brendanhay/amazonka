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
-- Module      : Amazonka.Inspector.UnsubscribeFromEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the process of sending Amazon Simple Notification Service (SNS)
-- notifications about a specified event to a specified SNS topic.
module Amazonka.Inspector.UnsubscribeFromEvent
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UnsubscribeFromEvent where
  type
    AWSResponse UnsubscribeFromEvent =
      UnsubscribeFromEventResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UnsubscribeFromEventResponse'

instance Prelude.Hashable UnsubscribeFromEvent where
  hashWithSalt _salt UnsubscribeFromEvent' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` topicArn

instance Prelude.NFData UnsubscribeFromEvent where
  rnf UnsubscribeFromEvent' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf topicArn

instance Data.ToHeaders UnsubscribeFromEvent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.UnsubscribeFromEvent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnsubscribeFromEvent where
  toJSON UnsubscribeFromEvent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceArn" Data..= resourceArn),
            Prelude.Just ("event" Data..= event),
            Prelude.Just ("topicArn" Data..= topicArn)
          ]
      )

instance Data.ToPath UnsubscribeFromEvent where
  toPath = Prelude.const "/"

instance Data.ToQuery UnsubscribeFromEvent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUnsubscribeFromEventResponse' smart constructor.
data UnsubscribeFromEventResponse = UnsubscribeFromEventResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsubscribeFromEventResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUnsubscribeFromEventResponse ::
  UnsubscribeFromEventResponse
newUnsubscribeFromEventResponse =
  UnsubscribeFromEventResponse'

instance Prelude.NFData UnsubscribeFromEventResponse where
  rnf _ = ()

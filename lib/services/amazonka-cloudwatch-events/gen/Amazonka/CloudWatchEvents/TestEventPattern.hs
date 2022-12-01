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
-- Module      : Amazonka.CloudWatchEvents.TestEventPattern
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests whether the specified event pattern matches the provided event.
--
-- Most services in Amazon Web Services treat : or \/ as the same character
-- in Amazon Resource Names (ARNs). However, EventBridge uses an exact
-- match in event patterns and rules. Be sure to use the correct ARN
-- characters when creating event patterns so that they match the ARN
-- syntax in the event you want to match.
module Amazonka.CloudWatchEvents.TestEventPattern
  ( -- * Creating a Request
    TestEventPattern (..),
    newTestEventPattern,

    -- * Request Lenses
    testEventPattern_eventPattern,
    testEventPattern_event,

    -- * Destructuring the Response
    TestEventPatternResponse (..),
    newTestEventPatternResponse,

    -- * Response Lenses
    testEventPatternResponse_result,
    testEventPatternResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestEventPattern' smart constructor.
data TestEventPattern = TestEventPattern'
  { -- | The event pattern. For more information, see
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
    -- in the /Amazon EventBridge User Guide/.
    eventPattern :: Prelude.Text,
    -- | The event, in JSON format, to test against the event pattern. The JSON
    -- must follow the format specified in
    -- <https://docs.aws.amazon.com/eventbridge/latest/userguide/aws-events.html Amazon Web Services Events>,
    -- and the following fields are mandatory:
    --
    -- -   @id@
    --
    -- -   @account@
    --
    -- -   @source@
    --
    -- -   @time@
    --
    -- -   @region@
    --
    -- -   @resources@
    --
    -- -   @detail-type@
    event :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestEventPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventPattern', 'testEventPattern_eventPattern' - The event pattern. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
--
-- 'event', 'testEventPattern_event' - The event, in JSON format, to test against the event pattern. The JSON
-- must follow the format specified in
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/aws-events.html Amazon Web Services Events>,
-- and the following fields are mandatory:
--
-- -   @id@
--
-- -   @account@
--
-- -   @source@
--
-- -   @time@
--
-- -   @region@
--
-- -   @resources@
--
-- -   @detail-type@
newTestEventPattern ::
  -- | 'eventPattern'
  Prelude.Text ->
  -- | 'event'
  Prelude.Text ->
  TestEventPattern
newTestEventPattern pEventPattern_ pEvent_ =
  TestEventPattern'
    { eventPattern = pEventPattern_,
      event = pEvent_
    }

-- | The event pattern. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns>
-- in the /Amazon EventBridge User Guide/.
testEventPattern_eventPattern :: Lens.Lens' TestEventPattern Prelude.Text
testEventPattern_eventPattern = Lens.lens (\TestEventPattern' {eventPattern} -> eventPattern) (\s@TestEventPattern' {} a -> s {eventPattern = a} :: TestEventPattern)

-- | The event, in JSON format, to test against the event pattern. The JSON
-- must follow the format specified in
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/aws-events.html Amazon Web Services Events>,
-- and the following fields are mandatory:
--
-- -   @id@
--
-- -   @account@
--
-- -   @source@
--
-- -   @time@
--
-- -   @region@
--
-- -   @resources@
--
-- -   @detail-type@
testEventPattern_event :: Lens.Lens' TestEventPattern Prelude.Text
testEventPattern_event = Lens.lens (\TestEventPattern' {event} -> event) (\s@TestEventPattern' {} a -> s {event = a} :: TestEventPattern)

instance Core.AWSRequest TestEventPattern where
  type
    AWSResponse TestEventPattern =
      TestEventPatternResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TestEventPatternResponse'
            Prelude.<$> (x Core..?> "Result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestEventPattern where
  hashWithSalt _salt TestEventPattern' {..} =
    _salt `Prelude.hashWithSalt` eventPattern
      `Prelude.hashWithSalt` event

instance Prelude.NFData TestEventPattern where
  rnf TestEventPattern' {..} =
    Prelude.rnf eventPattern
      `Prelude.seq` Prelude.rnf event

instance Core.ToHeaders TestEventPattern where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.TestEventPattern" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TestEventPattern where
  toJSON TestEventPattern' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EventPattern" Core..= eventPattern),
            Prelude.Just ("Event" Core..= event)
          ]
      )

instance Core.ToPath TestEventPattern where
  toPath = Prelude.const "/"

instance Core.ToQuery TestEventPattern where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestEventPatternResponse' smart constructor.
data TestEventPatternResponse = TestEventPatternResponse'
  { -- | Indicates whether the event matches the event pattern.
    result :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestEventPatternResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'testEventPatternResponse_result' - Indicates whether the event matches the event pattern.
--
-- 'httpStatus', 'testEventPatternResponse_httpStatus' - The response's http status code.
newTestEventPatternResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestEventPatternResponse
newTestEventPatternResponse pHttpStatus_ =
  TestEventPatternResponse'
    { result = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the event matches the event pattern.
testEventPatternResponse_result :: Lens.Lens' TestEventPatternResponse (Prelude.Maybe Prelude.Bool)
testEventPatternResponse_result = Lens.lens (\TestEventPatternResponse' {result} -> result) (\s@TestEventPatternResponse' {} a -> s {result = a} :: TestEventPatternResponse)

-- | The response's http status code.
testEventPatternResponse_httpStatus :: Lens.Lens' TestEventPatternResponse Prelude.Int
testEventPatternResponse_httpStatus = Lens.lens (\TestEventPatternResponse' {httpStatus} -> httpStatus) (\s@TestEventPatternResponse' {} a -> s {httpStatus = a} :: TestEventPatternResponse)

instance Prelude.NFData TestEventPatternResponse where
  rnf TestEventPatternResponse' {..} =
    Prelude.rnf result
      `Prelude.seq` Prelude.rnf httpStatus

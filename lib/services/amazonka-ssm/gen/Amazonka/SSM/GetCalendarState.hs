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
-- Module      : Amazonka.SSM.GetCalendarState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the state of a Amazon Web Services Systems Manager change calendar
-- at the current time or a specified time. If you specify a time,
-- @GetCalendarState@ returns the state of the calendar at that specific
-- time, and returns the next time that the change calendar state will
-- transition. If you don\'t specify a time, @GetCalendarState@ uses the
-- current time. Change Calendar entries have two possible states: @OPEN@
-- or @CLOSED@.
--
-- If you specify more than one calendar in a request, the command returns
-- the status of @OPEN@ only if all calendars in the request are open. If
-- one or more calendars in the request are closed, the status returned is
-- @CLOSED@.
--
-- For more information about Change Calendar, a capability of Amazon Web
-- Services Systems Manager, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-change-calendar.html Amazon Web Services Systems Manager Change Calendar>
-- in the /Amazon Web Services Systems Manager User Guide/.
module Amazonka.SSM.GetCalendarState
  ( -- * Creating a Request
    GetCalendarState (..),
    newGetCalendarState,

    -- * Request Lenses
    getCalendarState_atTime,
    getCalendarState_calendarNames,

    -- * Destructuring the Response
    GetCalendarStateResponse (..),
    newGetCalendarStateResponse,

    -- * Response Lenses
    getCalendarStateResponse_atTime,
    getCalendarStateResponse_nextTransitionTime,
    getCalendarStateResponse_state,
    getCalendarStateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetCalendarState' smart constructor.
data GetCalendarState = GetCalendarState'
  { -- | (Optional) The specific time for which you want to get calendar state
    -- information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
    -- format. If you don\'t specify a value or @AtTime@, the current time is
    -- used.
    atTime :: Prelude.Maybe Prelude.Text,
    -- | The names or Amazon Resource Names (ARNs) of the Systems Manager
    -- documents (SSM documents) that represent the calendar entries for which
    -- you want to get the state.
    calendarNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalendarState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'atTime', 'getCalendarState_atTime' - (Optional) The specific time for which you want to get calendar state
-- information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- format. If you don\'t specify a value or @AtTime@, the current time is
-- used.
--
-- 'calendarNames', 'getCalendarState_calendarNames' - The names or Amazon Resource Names (ARNs) of the Systems Manager
-- documents (SSM documents) that represent the calendar entries for which
-- you want to get the state.
newGetCalendarState ::
  GetCalendarState
newGetCalendarState =
  GetCalendarState'
    { atTime = Prelude.Nothing,
      calendarNames = Prelude.mempty
    }

-- | (Optional) The specific time for which you want to get calendar state
-- information, in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- format. If you don\'t specify a value or @AtTime@, the current time is
-- used.
getCalendarState_atTime :: Lens.Lens' GetCalendarState (Prelude.Maybe Prelude.Text)
getCalendarState_atTime = Lens.lens (\GetCalendarState' {atTime} -> atTime) (\s@GetCalendarState' {} a -> s {atTime = a} :: GetCalendarState)

-- | The names or Amazon Resource Names (ARNs) of the Systems Manager
-- documents (SSM documents) that represent the calendar entries for which
-- you want to get the state.
getCalendarState_calendarNames :: Lens.Lens' GetCalendarState [Prelude.Text]
getCalendarState_calendarNames = Lens.lens (\GetCalendarState' {calendarNames} -> calendarNames) (\s@GetCalendarState' {} a -> s {calendarNames = a} :: GetCalendarState) Prelude.. Lens.coerced

instance Core.AWSRequest GetCalendarState where
  type
    AWSResponse GetCalendarState =
      GetCalendarStateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCalendarStateResponse'
            Prelude.<$> (x Data..?> "AtTime")
            Prelude.<*> (x Data..?> "NextTransitionTime")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCalendarState where
  hashWithSalt _salt GetCalendarState' {..} =
    _salt `Prelude.hashWithSalt` atTime
      `Prelude.hashWithSalt` calendarNames

instance Prelude.NFData GetCalendarState where
  rnf GetCalendarState' {..} =
    Prelude.rnf atTime
      `Prelude.seq` Prelude.rnf calendarNames

instance Data.ToHeaders GetCalendarState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.GetCalendarState" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCalendarState where
  toJSON GetCalendarState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AtTime" Data..=) Prelude.<$> atTime,
            Prelude.Just
              ("CalendarNames" Data..= calendarNames)
          ]
      )

instance Data.ToPath GetCalendarState where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCalendarState where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCalendarStateResponse' smart constructor.
data GetCalendarStateResponse = GetCalendarStateResponse'
  { -- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
    -- string, that you specified in your command. If you don\'t specify a
    -- time, @GetCalendarState@ uses the current time.
    atTime :: Prelude.Maybe Prelude.Text,
    -- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
    -- string, that the calendar state will change. If the current calendar
    -- state is @OPEN@, @NextTransitionTime@ indicates when the calendar state
    -- changes to @CLOSED@, and vice-versa.
    nextTransitionTime :: Prelude.Maybe Prelude.Text,
    -- | The state of the calendar. An @OPEN@ calendar indicates that actions are
    -- allowed to proceed, and a @CLOSED@ calendar indicates that actions
    -- aren\'t allowed to proceed.
    state :: Prelude.Maybe CalendarState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCalendarStateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'atTime', 'getCalendarStateResponse_atTime' - The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- string, that you specified in your command. If you don\'t specify a
-- time, @GetCalendarState@ uses the current time.
--
-- 'nextTransitionTime', 'getCalendarStateResponse_nextTransitionTime' - The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- string, that the calendar state will change. If the current calendar
-- state is @OPEN@, @NextTransitionTime@ indicates when the calendar state
-- changes to @CLOSED@, and vice-versa.
--
-- 'state', 'getCalendarStateResponse_state' - The state of the calendar. An @OPEN@ calendar indicates that actions are
-- allowed to proceed, and a @CLOSED@ calendar indicates that actions
-- aren\'t allowed to proceed.
--
-- 'httpStatus', 'getCalendarStateResponse_httpStatus' - The response's http status code.
newGetCalendarStateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCalendarStateResponse
newGetCalendarStateResponse pHttpStatus_ =
  GetCalendarStateResponse'
    { atTime = Prelude.Nothing,
      nextTransitionTime = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- string, that you specified in your command. If you don\'t specify a
-- time, @GetCalendarState@ uses the current time.
getCalendarStateResponse_atTime :: Lens.Lens' GetCalendarStateResponse (Prelude.Maybe Prelude.Text)
getCalendarStateResponse_atTime = Lens.lens (\GetCalendarStateResponse' {atTime} -> atTime) (\s@GetCalendarStateResponse' {} a -> s {atTime = a} :: GetCalendarStateResponse)

-- | The time, as an <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601>
-- string, that the calendar state will change. If the current calendar
-- state is @OPEN@, @NextTransitionTime@ indicates when the calendar state
-- changes to @CLOSED@, and vice-versa.
getCalendarStateResponse_nextTransitionTime :: Lens.Lens' GetCalendarStateResponse (Prelude.Maybe Prelude.Text)
getCalendarStateResponse_nextTransitionTime = Lens.lens (\GetCalendarStateResponse' {nextTransitionTime} -> nextTransitionTime) (\s@GetCalendarStateResponse' {} a -> s {nextTransitionTime = a} :: GetCalendarStateResponse)

-- | The state of the calendar. An @OPEN@ calendar indicates that actions are
-- allowed to proceed, and a @CLOSED@ calendar indicates that actions
-- aren\'t allowed to proceed.
getCalendarStateResponse_state :: Lens.Lens' GetCalendarStateResponse (Prelude.Maybe CalendarState)
getCalendarStateResponse_state = Lens.lens (\GetCalendarStateResponse' {state} -> state) (\s@GetCalendarStateResponse' {} a -> s {state = a} :: GetCalendarStateResponse)

-- | The response's http status code.
getCalendarStateResponse_httpStatus :: Lens.Lens' GetCalendarStateResponse Prelude.Int
getCalendarStateResponse_httpStatus = Lens.lens (\GetCalendarStateResponse' {httpStatus} -> httpStatus) (\s@GetCalendarStateResponse' {} a -> s {httpStatus = a} :: GetCalendarStateResponse)

instance Prelude.NFData GetCalendarStateResponse where
  rnf GetCalendarStateResponse' {..} =
    Prelude.rnf atTime
      `Prelude.seq` Prelude.rnf nextTransitionTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus

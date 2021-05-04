{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EventItemResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventItemResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the status code and message that result from processing an
-- event.
--
-- /See:/ 'newEventItemResponse' smart constructor.
data EventItemResponse = EventItemResponse'
  { -- | A custom message that\'s returned in the response as a result of
    -- processing the event.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code that\'s returned in the response as a result of
    -- processing the event. Possible values are: 202, for events that were
    -- accepted; and, 400, for events that weren\'t valid.
    statusCode :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'eventItemResponse_message' - A custom message that\'s returned in the response as a result of
-- processing the event.
--
-- 'statusCode', 'eventItemResponse_statusCode' - The status code that\'s returned in the response as a result of
-- processing the event. Possible values are: 202, for events that were
-- accepted; and, 400, for events that weren\'t valid.
newEventItemResponse ::
  EventItemResponse
newEventItemResponse =
  EventItemResponse'
    { message = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | A custom message that\'s returned in the response as a result of
-- processing the event.
eventItemResponse_message :: Lens.Lens' EventItemResponse (Prelude.Maybe Prelude.Text)
eventItemResponse_message = Lens.lens (\EventItemResponse' {message} -> message) (\s@EventItemResponse' {} a -> s {message = a} :: EventItemResponse)

-- | The status code that\'s returned in the response as a result of
-- processing the event. Possible values are: 202, for events that were
-- accepted; and, 400, for events that weren\'t valid.
eventItemResponse_statusCode :: Lens.Lens' EventItemResponse (Prelude.Maybe Prelude.Int)
eventItemResponse_statusCode = Lens.lens (\EventItemResponse' {statusCode} -> statusCode) (\s@EventItemResponse' {} a -> s {statusCode = a} :: EventItemResponse)

instance Prelude.FromJSON EventItemResponse where
  parseJSON =
    Prelude.withObject
      "EventItemResponse"
      ( \x ->
          EventItemResponse'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "StatusCode")
      )

instance Prelude.Hashable EventItemResponse

instance Prelude.NFData EventItemResponse

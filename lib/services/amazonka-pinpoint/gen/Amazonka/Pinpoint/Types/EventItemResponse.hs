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
-- Module      : Amazonka.Pinpoint.Types.EventItemResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EventItemResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON EventItemResponse where
  parseJSON =
    Data.withObject
      "EventItemResponse"
      ( \x ->
          EventItemResponse'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "StatusCode")
      )

instance Prelude.Hashable EventItemResponse where
  hashWithSalt _salt EventItemResponse' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData EventItemResponse where
  rnf EventItemResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf statusCode

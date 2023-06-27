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
-- Module      : Amazonka.CustomerProfiles.Types.EventStreamDestinationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.EventStreamDestinationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.EventStreamDestinationStatus
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the destination being used for the EventStream.
--
-- /See:/ 'newEventStreamDestinationDetails' smart constructor.
data EventStreamDestinationDetails = EventStreamDestinationDetails'
  { -- | The human-readable string that corresponds to the error or success while
    -- enabling the streaming destination.
    message :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the status last changed to @UNHEALHY@.
    unhealthySince :: Prelude.Maybe Data.POSIX,
    -- | The StreamARN of the destination to deliver profile events to. For
    -- example, arn:aws:kinesis:region:account-id:stream\/stream-name.
    uri :: Prelude.Text,
    -- | The status of enabling the Kinesis stream as a destination for export.
    status :: EventStreamDestinationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventStreamDestinationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'eventStreamDestinationDetails_message' - The human-readable string that corresponds to the error or success while
-- enabling the streaming destination.
--
-- 'unhealthySince', 'eventStreamDestinationDetails_unhealthySince' - The timestamp when the status last changed to @UNHEALHY@.
--
-- 'uri', 'eventStreamDestinationDetails_uri' - The StreamARN of the destination to deliver profile events to. For
-- example, arn:aws:kinesis:region:account-id:stream\/stream-name.
--
-- 'status', 'eventStreamDestinationDetails_status' - The status of enabling the Kinesis stream as a destination for export.
newEventStreamDestinationDetails ::
  -- | 'uri'
  Prelude.Text ->
  -- | 'status'
  EventStreamDestinationStatus ->
  EventStreamDestinationDetails
newEventStreamDestinationDetails pUri_ pStatus_ =
  EventStreamDestinationDetails'
    { message =
        Prelude.Nothing,
      unhealthySince = Prelude.Nothing,
      uri = pUri_,
      status = pStatus_
    }

-- | The human-readable string that corresponds to the error or success while
-- enabling the streaming destination.
eventStreamDestinationDetails_message :: Lens.Lens' EventStreamDestinationDetails (Prelude.Maybe Prelude.Text)
eventStreamDestinationDetails_message = Lens.lens (\EventStreamDestinationDetails' {message} -> message) (\s@EventStreamDestinationDetails' {} a -> s {message = a} :: EventStreamDestinationDetails)

-- | The timestamp when the status last changed to @UNHEALHY@.
eventStreamDestinationDetails_unhealthySince :: Lens.Lens' EventStreamDestinationDetails (Prelude.Maybe Prelude.UTCTime)
eventStreamDestinationDetails_unhealthySince = Lens.lens (\EventStreamDestinationDetails' {unhealthySince} -> unhealthySince) (\s@EventStreamDestinationDetails' {} a -> s {unhealthySince = a} :: EventStreamDestinationDetails) Prelude.. Lens.mapping Data._Time

-- | The StreamARN of the destination to deliver profile events to. For
-- example, arn:aws:kinesis:region:account-id:stream\/stream-name.
eventStreamDestinationDetails_uri :: Lens.Lens' EventStreamDestinationDetails Prelude.Text
eventStreamDestinationDetails_uri = Lens.lens (\EventStreamDestinationDetails' {uri} -> uri) (\s@EventStreamDestinationDetails' {} a -> s {uri = a} :: EventStreamDestinationDetails)

-- | The status of enabling the Kinesis stream as a destination for export.
eventStreamDestinationDetails_status :: Lens.Lens' EventStreamDestinationDetails EventStreamDestinationStatus
eventStreamDestinationDetails_status = Lens.lens (\EventStreamDestinationDetails' {status} -> status) (\s@EventStreamDestinationDetails' {} a -> s {status = a} :: EventStreamDestinationDetails)

instance Data.FromJSON EventStreamDestinationDetails where
  parseJSON =
    Data.withObject
      "EventStreamDestinationDetails"
      ( \x ->
          EventStreamDestinationDetails'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "UnhealthySince")
            Prelude.<*> (x Data..: "Uri")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    EventStreamDestinationDetails
  where
  hashWithSalt _salt EventStreamDestinationDetails' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` unhealthySince
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` status

instance Prelude.NFData EventStreamDestinationDetails where
  rnf EventStreamDestinationDetails' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf unhealthySince
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf status

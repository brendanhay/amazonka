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
-- Module      : Amazonka.CustomerProfiles.Types.EventStreamSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.EventStreamSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.DestinationSummary
import Amazonka.CustomerProfiles.Types.EventStreamState
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An instance of EventStream in a list of EventStreams.
--
-- /See:/ 'newEventStreamSummary' smart constructor.
data EventStreamSummary = EventStreamSummary'
  { -- | Summary information about the Kinesis data stream.
    destinationSummary :: Prelude.Maybe DestinationSummary,
    -- | The timestamp when the @State@ changed to @STOPPED@.
    stoppedSince :: Prelude.Maybe Data.POSIX,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the event stream.
    eventStreamName :: Prelude.Text,
    -- | A unique identifier for the event stream.
    eventStreamArn :: Prelude.Text,
    -- | The operational state of destination stream for export.
    state :: EventStreamState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventStreamSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationSummary', 'eventStreamSummary_destinationSummary' - Summary information about the Kinesis data stream.
--
-- 'stoppedSince', 'eventStreamSummary_stoppedSince' - The timestamp when the @State@ changed to @STOPPED@.
--
-- 'tags', 'eventStreamSummary_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'domainName', 'eventStreamSummary_domainName' - The unique name of the domain.
--
-- 'eventStreamName', 'eventStreamSummary_eventStreamName' - The name of the event stream.
--
-- 'eventStreamArn', 'eventStreamSummary_eventStreamArn' - A unique identifier for the event stream.
--
-- 'state', 'eventStreamSummary_state' - The operational state of destination stream for export.
newEventStreamSummary ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'eventStreamName'
  Prelude.Text ->
  -- | 'eventStreamArn'
  Prelude.Text ->
  -- | 'state'
  EventStreamState ->
  EventStreamSummary
newEventStreamSummary
  pDomainName_
  pEventStreamName_
  pEventStreamArn_
  pState_ =
    EventStreamSummary'
      { destinationSummary =
          Prelude.Nothing,
        stoppedSince = Prelude.Nothing,
        tags = Prelude.Nothing,
        domainName = pDomainName_,
        eventStreamName = pEventStreamName_,
        eventStreamArn = pEventStreamArn_,
        state = pState_
      }

-- | Summary information about the Kinesis data stream.
eventStreamSummary_destinationSummary :: Lens.Lens' EventStreamSummary (Prelude.Maybe DestinationSummary)
eventStreamSummary_destinationSummary = Lens.lens (\EventStreamSummary' {destinationSummary} -> destinationSummary) (\s@EventStreamSummary' {} a -> s {destinationSummary = a} :: EventStreamSummary)

-- | The timestamp when the @State@ changed to @STOPPED@.
eventStreamSummary_stoppedSince :: Lens.Lens' EventStreamSummary (Prelude.Maybe Prelude.UTCTime)
eventStreamSummary_stoppedSince = Lens.lens (\EventStreamSummary' {stoppedSince} -> stoppedSince) (\s@EventStreamSummary' {} a -> s {stoppedSince = a} :: EventStreamSummary) Prelude.. Lens.mapping Data._Time

-- | The tags used to organize, track, or control access for this resource.
eventStreamSummary_tags :: Lens.Lens' EventStreamSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
eventStreamSummary_tags = Lens.lens (\EventStreamSummary' {tags} -> tags) (\s@EventStreamSummary' {} a -> s {tags = a} :: EventStreamSummary) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the domain.
eventStreamSummary_domainName :: Lens.Lens' EventStreamSummary Prelude.Text
eventStreamSummary_domainName = Lens.lens (\EventStreamSummary' {domainName} -> domainName) (\s@EventStreamSummary' {} a -> s {domainName = a} :: EventStreamSummary)

-- | The name of the event stream.
eventStreamSummary_eventStreamName :: Lens.Lens' EventStreamSummary Prelude.Text
eventStreamSummary_eventStreamName = Lens.lens (\EventStreamSummary' {eventStreamName} -> eventStreamName) (\s@EventStreamSummary' {} a -> s {eventStreamName = a} :: EventStreamSummary)

-- | A unique identifier for the event stream.
eventStreamSummary_eventStreamArn :: Lens.Lens' EventStreamSummary Prelude.Text
eventStreamSummary_eventStreamArn = Lens.lens (\EventStreamSummary' {eventStreamArn} -> eventStreamArn) (\s@EventStreamSummary' {} a -> s {eventStreamArn = a} :: EventStreamSummary)

-- | The operational state of destination stream for export.
eventStreamSummary_state :: Lens.Lens' EventStreamSummary EventStreamState
eventStreamSummary_state = Lens.lens (\EventStreamSummary' {state} -> state) (\s@EventStreamSummary' {} a -> s {state = a} :: EventStreamSummary)

instance Data.FromJSON EventStreamSummary where
  parseJSON =
    Data.withObject
      "EventStreamSummary"
      ( \x ->
          EventStreamSummary'
            Prelude.<$> (x Data..:? "DestinationSummary")
            Prelude.<*> (x Data..:? "StoppedSince")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "DomainName")
            Prelude.<*> (x Data..: "EventStreamName")
            Prelude.<*> (x Data..: "EventStreamArn")
            Prelude.<*> (x Data..: "State")
      )

instance Prelude.Hashable EventStreamSummary where
  hashWithSalt _salt EventStreamSummary' {..} =
    _salt
      `Prelude.hashWithSalt` destinationSummary
      `Prelude.hashWithSalt` stoppedSince
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` eventStreamName
      `Prelude.hashWithSalt` eventStreamArn
      `Prelude.hashWithSalt` state

instance Prelude.NFData EventStreamSummary where
  rnf EventStreamSummary' {..} =
    Prelude.rnf destinationSummary
      `Prelude.seq` Prelude.rnf stoppedSince
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf eventStreamName
      `Prelude.seq` Prelude.rnf eventStreamArn
      `Prelude.seq` Prelude.rnf state

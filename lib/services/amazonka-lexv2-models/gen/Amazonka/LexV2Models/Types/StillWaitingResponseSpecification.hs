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
-- Module      : Amazonka.LexV2Models.Types.StillWaitingResponseSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.StillWaitingResponseSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.MessageGroup
import qualified Amazonka.Prelude as Prelude

-- | Defines the messages that Amazon Lex sends to a user to remind them that
-- the bot is waiting for a response.
--
-- /See:/ 'newStillWaitingResponseSpecification' smart constructor.
data StillWaitingResponseSpecification = StillWaitingResponseSpecification'
  { -- | Indicates that the user can interrupt the response by speaking while the
    -- message is being played.
    allowInterrupt :: Prelude.Maybe Prelude.Bool,
    -- | One or more message groups, each containing one or more messages, that
    -- define the prompts that Amazon Lex sends to the user.
    messageGroups :: Prelude.NonEmpty MessageGroup,
    -- | How often a message should be sent to the user. Minimum of 1 second,
    -- maximum of 5 minutes.
    frequencyInSeconds :: Prelude.Natural,
    -- | If Amazon Lex waits longer than this length of time for a response, it
    -- will stop sending messages.
    timeoutInSeconds :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StillWaitingResponseSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowInterrupt', 'stillWaitingResponseSpecification_allowInterrupt' - Indicates that the user can interrupt the response by speaking while the
-- message is being played.
--
-- 'messageGroups', 'stillWaitingResponseSpecification_messageGroups' - One or more message groups, each containing one or more messages, that
-- define the prompts that Amazon Lex sends to the user.
--
-- 'frequencyInSeconds', 'stillWaitingResponseSpecification_frequencyInSeconds' - How often a message should be sent to the user. Minimum of 1 second,
-- maximum of 5 minutes.
--
-- 'timeoutInSeconds', 'stillWaitingResponseSpecification_timeoutInSeconds' - If Amazon Lex waits longer than this length of time for a response, it
-- will stop sending messages.
newStillWaitingResponseSpecification ::
  -- | 'messageGroups'
  Prelude.NonEmpty MessageGroup ->
  -- | 'frequencyInSeconds'
  Prelude.Natural ->
  -- | 'timeoutInSeconds'
  Prelude.Natural ->
  StillWaitingResponseSpecification
newStillWaitingResponseSpecification
  pMessageGroups_
  pFrequencyInSeconds_
  pTimeoutInSeconds_ =
    StillWaitingResponseSpecification'
      { allowInterrupt =
          Prelude.Nothing,
        messageGroups =
          Lens.coerced Lens.# pMessageGroups_,
        frequencyInSeconds =
          pFrequencyInSeconds_,
        timeoutInSeconds = pTimeoutInSeconds_
      }

-- | Indicates that the user can interrupt the response by speaking while the
-- message is being played.
stillWaitingResponseSpecification_allowInterrupt :: Lens.Lens' StillWaitingResponseSpecification (Prelude.Maybe Prelude.Bool)
stillWaitingResponseSpecification_allowInterrupt = Lens.lens (\StillWaitingResponseSpecification' {allowInterrupt} -> allowInterrupt) (\s@StillWaitingResponseSpecification' {} a -> s {allowInterrupt = a} :: StillWaitingResponseSpecification)

-- | One or more message groups, each containing one or more messages, that
-- define the prompts that Amazon Lex sends to the user.
stillWaitingResponseSpecification_messageGroups :: Lens.Lens' StillWaitingResponseSpecification (Prelude.NonEmpty MessageGroup)
stillWaitingResponseSpecification_messageGroups = Lens.lens (\StillWaitingResponseSpecification' {messageGroups} -> messageGroups) (\s@StillWaitingResponseSpecification' {} a -> s {messageGroups = a} :: StillWaitingResponseSpecification) Prelude.. Lens.coerced

-- | How often a message should be sent to the user. Minimum of 1 second,
-- maximum of 5 minutes.
stillWaitingResponseSpecification_frequencyInSeconds :: Lens.Lens' StillWaitingResponseSpecification Prelude.Natural
stillWaitingResponseSpecification_frequencyInSeconds = Lens.lens (\StillWaitingResponseSpecification' {frequencyInSeconds} -> frequencyInSeconds) (\s@StillWaitingResponseSpecification' {} a -> s {frequencyInSeconds = a} :: StillWaitingResponseSpecification)

-- | If Amazon Lex waits longer than this length of time for a response, it
-- will stop sending messages.
stillWaitingResponseSpecification_timeoutInSeconds :: Lens.Lens' StillWaitingResponseSpecification Prelude.Natural
stillWaitingResponseSpecification_timeoutInSeconds = Lens.lens (\StillWaitingResponseSpecification' {timeoutInSeconds} -> timeoutInSeconds) (\s@StillWaitingResponseSpecification' {} a -> s {timeoutInSeconds = a} :: StillWaitingResponseSpecification)

instance
  Data.FromJSON
    StillWaitingResponseSpecification
  where
  parseJSON =
    Data.withObject
      "StillWaitingResponseSpecification"
      ( \x ->
          StillWaitingResponseSpecification'
            Prelude.<$> (x Data..:? "allowInterrupt")
            Prelude.<*> (x Data..: "messageGroups")
            Prelude.<*> (x Data..: "frequencyInSeconds")
            Prelude.<*> (x Data..: "timeoutInSeconds")
      )

instance
  Prelude.Hashable
    StillWaitingResponseSpecification
  where
  hashWithSalt
    _salt
    StillWaitingResponseSpecification' {..} =
      _salt `Prelude.hashWithSalt` allowInterrupt
        `Prelude.hashWithSalt` messageGroups
        `Prelude.hashWithSalt` frequencyInSeconds
        `Prelude.hashWithSalt` timeoutInSeconds

instance
  Prelude.NFData
    StillWaitingResponseSpecification
  where
  rnf StillWaitingResponseSpecification' {..} =
    Prelude.rnf allowInterrupt
      `Prelude.seq` Prelude.rnf messageGroups
      `Prelude.seq` Prelude.rnf frequencyInSeconds
      `Prelude.seq` Prelude.rnf timeoutInSeconds

instance
  Data.ToJSON
    StillWaitingResponseSpecification
  where
  toJSON StillWaitingResponseSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowInterrupt" Data..=)
              Prelude.<$> allowInterrupt,
            Prelude.Just ("messageGroups" Data..= messageGroups),
            Prelude.Just
              ("frequencyInSeconds" Data..= frequencyInSeconds),
            Prelude.Just
              ("timeoutInSeconds" Data..= timeoutInSeconds)
          ]
      )

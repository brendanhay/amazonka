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
-- Module      : Amazonka.LexV2Models.Types.FulfillmentUpdateResponseSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.FulfillmentUpdateResponseSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.MessageGroup
import qualified Amazonka.Prelude as Prelude

-- | Provides settings for a message that is sent periodically to the user
-- while a fulfillment Lambda function is running.
--
-- /See:/ 'newFulfillmentUpdateResponseSpecification' smart constructor.
data FulfillmentUpdateResponseSpecification = FulfillmentUpdateResponseSpecification'
  { -- | Determines whether the user can interrupt an update message while it is
    -- playing.
    allowInterrupt :: Prelude.Maybe Prelude.Bool,
    -- | The frequency that a message is sent to the user. When the period ends,
    -- Amazon Lex chooses a message from the message groups and plays it to the
    -- user. If the fulfillment Lambda returns before the first period ends, an
    -- update message is not played to the user.
    frequencyInSeconds :: Prelude.Natural,
    -- | One to 5 message groups that contain update messages. Amazon Lex chooses
    -- one of the messages to play to the user.
    messageGroups :: Prelude.NonEmpty MessageGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FulfillmentUpdateResponseSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowInterrupt', 'fulfillmentUpdateResponseSpecification_allowInterrupt' - Determines whether the user can interrupt an update message while it is
-- playing.
--
-- 'frequencyInSeconds', 'fulfillmentUpdateResponseSpecification_frequencyInSeconds' - The frequency that a message is sent to the user. When the period ends,
-- Amazon Lex chooses a message from the message groups and plays it to the
-- user. If the fulfillment Lambda returns before the first period ends, an
-- update message is not played to the user.
--
-- 'messageGroups', 'fulfillmentUpdateResponseSpecification_messageGroups' - One to 5 message groups that contain update messages. Amazon Lex chooses
-- one of the messages to play to the user.
newFulfillmentUpdateResponseSpecification ::
  -- | 'frequencyInSeconds'
  Prelude.Natural ->
  -- | 'messageGroups'
  Prelude.NonEmpty MessageGroup ->
  FulfillmentUpdateResponseSpecification
newFulfillmentUpdateResponseSpecification
  pFrequencyInSeconds_
  pMessageGroups_ =
    FulfillmentUpdateResponseSpecification'
      { allowInterrupt =
          Prelude.Nothing,
        frequencyInSeconds =
          pFrequencyInSeconds_,
        messageGroups =
          Lens.coerced
            Lens.# pMessageGroups_
      }

-- | Determines whether the user can interrupt an update message while it is
-- playing.
fulfillmentUpdateResponseSpecification_allowInterrupt :: Lens.Lens' FulfillmentUpdateResponseSpecification (Prelude.Maybe Prelude.Bool)
fulfillmentUpdateResponseSpecification_allowInterrupt = Lens.lens (\FulfillmentUpdateResponseSpecification' {allowInterrupt} -> allowInterrupt) (\s@FulfillmentUpdateResponseSpecification' {} a -> s {allowInterrupt = a} :: FulfillmentUpdateResponseSpecification)

-- | The frequency that a message is sent to the user. When the period ends,
-- Amazon Lex chooses a message from the message groups and plays it to the
-- user. If the fulfillment Lambda returns before the first period ends, an
-- update message is not played to the user.
fulfillmentUpdateResponseSpecification_frequencyInSeconds :: Lens.Lens' FulfillmentUpdateResponseSpecification Prelude.Natural
fulfillmentUpdateResponseSpecification_frequencyInSeconds = Lens.lens (\FulfillmentUpdateResponseSpecification' {frequencyInSeconds} -> frequencyInSeconds) (\s@FulfillmentUpdateResponseSpecification' {} a -> s {frequencyInSeconds = a} :: FulfillmentUpdateResponseSpecification)

-- | One to 5 message groups that contain update messages. Amazon Lex chooses
-- one of the messages to play to the user.
fulfillmentUpdateResponseSpecification_messageGroups :: Lens.Lens' FulfillmentUpdateResponseSpecification (Prelude.NonEmpty MessageGroup)
fulfillmentUpdateResponseSpecification_messageGroups = Lens.lens (\FulfillmentUpdateResponseSpecification' {messageGroups} -> messageGroups) (\s@FulfillmentUpdateResponseSpecification' {} a -> s {messageGroups = a} :: FulfillmentUpdateResponseSpecification) Prelude.. Lens.coerced

instance
  Data.FromJSON
    FulfillmentUpdateResponseSpecification
  where
  parseJSON =
    Data.withObject
      "FulfillmentUpdateResponseSpecification"
      ( \x ->
          FulfillmentUpdateResponseSpecification'
            Prelude.<$> (x Data..:? "allowInterrupt")
            Prelude.<*> (x Data..: "frequencyInSeconds")
            Prelude.<*> (x Data..: "messageGroups")
      )

instance
  Prelude.Hashable
    FulfillmentUpdateResponseSpecification
  where
  hashWithSalt
    _salt
    FulfillmentUpdateResponseSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` allowInterrupt
        `Prelude.hashWithSalt` frequencyInSeconds
        `Prelude.hashWithSalt` messageGroups

instance
  Prelude.NFData
    FulfillmentUpdateResponseSpecification
  where
  rnf FulfillmentUpdateResponseSpecification' {..} =
    Prelude.rnf allowInterrupt
      `Prelude.seq` Prelude.rnf frequencyInSeconds
      `Prelude.seq` Prelude.rnf messageGroups

instance
  Data.ToJSON
    FulfillmentUpdateResponseSpecification
  where
  toJSON FulfillmentUpdateResponseSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowInterrupt" Data..=)
              Prelude.<$> allowInterrupt,
            Prelude.Just
              ("frequencyInSeconds" Data..= frequencyInSeconds),
            Prelude.Just
              ("messageGroups" Data..= messageGroups)
          ]
      )

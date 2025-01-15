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
-- Module      : Amazonka.LexV2Models.Types.FulfillmentStartResponseSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.FulfillmentStartResponseSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.MessageGroup
import qualified Amazonka.Prelude as Prelude

-- | Provides settings for a message that is sent to the user when a
-- fulfillment Lambda function starts running.
--
-- /See:/ 'newFulfillmentStartResponseSpecification' smart constructor.
data FulfillmentStartResponseSpecification = FulfillmentStartResponseSpecification'
  { -- | Determines whether the user can interrupt the start message while it is
    -- playing.
    allowInterrupt :: Prelude.Maybe Prelude.Bool,
    -- | The delay between when the Lambda fulfillment function starts running
    -- and the start message is played. If the Lambda function returns before
    -- the delay is over, the start message isn\'t played.
    delayInSeconds :: Prelude.Natural,
    -- | One to 5 message groups that contain start messages. Amazon Lex chooses
    -- one of the messages to play to the user.
    messageGroups :: Prelude.NonEmpty MessageGroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FulfillmentStartResponseSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowInterrupt', 'fulfillmentStartResponseSpecification_allowInterrupt' - Determines whether the user can interrupt the start message while it is
-- playing.
--
-- 'delayInSeconds', 'fulfillmentStartResponseSpecification_delayInSeconds' - The delay between when the Lambda fulfillment function starts running
-- and the start message is played. If the Lambda function returns before
-- the delay is over, the start message isn\'t played.
--
-- 'messageGroups', 'fulfillmentStartResponseSpecification_messageGroups' - One to 5 message groups that contain start messages. Amazon Lex chooses
-- one of the messages to play to the user.
newFulfillmentStartResponseSpecification ::
  -- | 'delayInSeconds'
  Prelude.Natural ->
  -- | 'messageGroups'
  Prelude.NonEmpty MessageGroup ->
  FulfillmentStartResponseSpecification
newFulfillmentStartResponseSpecification
  pDelayInSeconds_
  pMessageGroups_ =
    FulfillmentStartResponseSpecification'
      { allowInterrupt =
          Prelude.Nothing,
        delayInSeconds = pDelayInSeconds_,
        messageGroups =
          Lens.coerced
            Lens.# pMessageGroups_
      }

-- | Determines whether the user can interrupt the start message while it is
-- playing.
fulfillmentStartResponseSpecification_allowInterrupt :: Lens.Lens' FulfillmentStartResponseSpecification (Prelude.Maybe Prelude.Bool)
fulfillmentStartResponseSpecification_allowInterrupt = Lens.lens (\FulfillmentStartResponseSpecification' {allowInterrupt} -> allowInterrupt) (\s@FulfillmentStartResponseSpecification' {} a -> s {allowInterrupt = a} :: FulfillmentStartResponseSpecification)

-- | The delay between when the Lambda fulfillment function starts running
-- and the start message is played. If the Lambda function returns before
-- the delay is over, the start message isn\'t played.
fulfillmentStartResponseSpecification_delayInSeconds :: Lens.Lens' FulfillmentStartResponseSpecification Prelude.Natural
fulfillmentStartResponseSpecification_delayInSeconds = Lens.lens (\FulfillmentStartResponseSpecification' {delayInSeconds} -> delayInSeconds) (\s@FulfillmentStartResponseSpecification' {} a -> s {delayInSeconds = a} :: FulfillmentStartResponseSpecification)

-- | One to 5 message groups that contain start messages. Amazon Lex chooses
-- one of the messages to play to the user.
fulfillmentStartResponseSpecification_messageGroups :: Lens.Lens' FulfillmentStartResponseSpecification (Prelude.NonEmpty MessageGroup)
fulfillmentStartResponseSpecification_messageGroups = Lens.lens (\FulfillmentStartResponseSpecification' {messageGroups} -> messageGroups) (\s@FulfillmentStartResponseSpecification' {} a -> s {messageGroups = a} :: FulfillmentStartResponseSpecification) Prelude.. Lens.coerced

instance
  Data.FromJSON
    FulfillmentStartResponseSpecification
  where
  parseJSON =
    Data.withObject
      "FulfillmentStartResponseSpecification"
      ( \x ->
          FulfillmentStartResponseSpecification'
            Prelude.<$> (x Data..:? "allowInterrupt")
            Prelude.<*> (x Data..: "delayInSeconds")
            Prelude.<*> (x Data..: "messageGroups")
      )

instance
  Prelude.Hashable
    FulfillmentStartResponseSpecification
  where
  hashWithSalt
    _salt
    FulfillmentStartResponseSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` allowInterrupt
        `Prelude.hashWithSalt` delayInSeconds
        `Prelude.hashWithSalt` messageGroups

instance
  Prelude.NFData
    FulfillmentStartResponseSpecification
  where
  rnf FulfillmentStartResponseSpecification' {..} =
    Prelude.rnf allowInterrupt `Prelude.seq`
      Prelude.rnf delayInSeconds `Prelude.seq`
        Prelude.rnf messageGroups

instance
  Data.ToJSON
    FulfillmentStartResponseSpecification
  where
  toJSON FulfillmentStartResponseSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowInterrupt" Data..=)
              Prelude.<$> allowInterrupt,
            Prelude.Just
              ("delayInSeconds" Data..= delayInSeconds),
            Prelude.Just
              ("messageGroups" Data..= messageGroups)
          ]
      )

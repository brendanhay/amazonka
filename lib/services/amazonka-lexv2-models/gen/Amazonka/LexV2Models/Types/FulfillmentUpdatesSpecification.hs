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
-- Module      : Amazonka.LexV2Models.Types.FulfillmentUpdatesSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.FulfillmentUpdatesSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.FulfillmentStartResponseSpecification
import Amazonka.LexV2Models.Types.FulfillmentUpdateResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Provides information for updating the user on the progress of fulfilling
-- an intent.
--
-- /See:/ 'newFulfillmentUpdatesSpecification' smart constructor.
data FulfillmentUpdatesSpecification = FulfillmentUpdatesSpecification'
  { -- | Provides configuration information for the message sent to users when
    -- the fulfillment Lambda functions starts running.
    startResponse :: Prelude.Maybe FulfillmentStartResponseSpecification,
    -- | The length of time that the fulfillment Lambda function should run
    -- before it times out.
    timeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Provides configuration information for messages sent periodically to the
    -- user while the fulfillment Lambda function is running.
    updateResponse :: Prelude.Maybe FulfillmentUpdateResponseSpecification,
    -- | Determines whether fulfillment updates are sent to the user. When this
    -- field is true, updates are sent.
    --
    -- If the @active@ field is set to true, the @startResponse@,
    -- @updateResponse@, and @timeoutInSeconds@ fields are required.
    active :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FulfillmentUpdatesSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startResponse', 'fulfillmentUpdatesSpecification_startResponse' - Provides configuration information for the message sent to users when
-- the fulfillment Lambda functions starts running.
--
-- 'timeoutInSeconds', 'fulfillmentUpdatesSpecification_timeoutInSeconds' - The length of time that the fulfillment Lambda function should run
-- before it times out.
--
-- 'updateResponse', 'fulfillmentUpdatesSpecification_updateResponse' - Provides configuration information for messages sent periodically to the
-- user while the fulfillment Lambda function is running.
--
-- 'active', 'fulfillmentUpdatesSpecification_active' - Determines whether fulfillment updates are sent to the user. When this
-- field is true, updates are sent.
--
-- If the @active@ field is set to true, the @startResponse@,
-- @updateResponse@, and @timeoutInSeconds@ fields are required.
newFulfillmentUpdatesSpecification ::
  -- | 'active'
  Prelude.Bool ->
  FulfillmentUpdatesSpecification
newFulfillmentUpdatesSpecification pActive_ =
  FulfillmentUpdatesSpecification'
    { startResponse =
        Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      updateResponse = Prelude.Nothing,
      active = pActive_
    }

-- | Provides configuration information for the message sent to users when
-- the fulfillment Lambda functions starts running.
fulfillmentUpdatesSpecification_startResponse :: Lens.Lens' FulfillmentUpdatesSpecification (Prelude.Maybe FulfillmentStartResponseSpecification)
fulfillmentUpdatesSpecification_startResponse = Lens.lens (\FulfillmentUpdatesSpecification' {startResponse} -> startResponse) (\s@FulfillmentUpdatesSpecification' {} a -> s {startResponse = a} :: FulfillmentUpdatesSpecification)

-- | The length of time that the fulfillment Lambda function should run
-- before it times out.
fulfillmentUpdatesSpecification_timeoutInSeconds :: Lens.Lens' FulfillmentUpdatesSpecification (Prelude.Maybe Prelude.Natural)
fulfillmentUpdatesSpecification_timeoutInSeconds = Lens.lens (\FulfillmentUpdatesSpecification' {timeoutInSeconds} -> timeoutInSeconds) (\s@FulfillmentUpdatesSpecification' {} a -> s {timeoutInSeconds = a} :: FulfillmentUpdatesSpecification)

-- | Provides configuration information for messages sent periodically to the
-- user while the fulfillment Lambda function is running.
fulfillmentUpdatesSpecification_updateResponse :: Lens.Lens' FulfillmentUpdatesSpecification (Prelude.Maybe FulfillmentUpdateResponseSpecification)
fulfillmentUpdatesSpecification_updateResponse = Lens.lens (\FulfillmentUpdatesSpecification' {updateResponse} -> updateResponse) (\s@FulfillmentUpdatesSpecification' {} a -> s {updateResponse = a} :: FulfillmentUpdatesSpecification)

-- | Determines whether fulfillment updates are sent to the user. When this
-- field is true, updates are sent.
--
-- If the @active@ field is set to true, the @startResponse@,
-- @updateResponse@, and @timeoutInSeconds@ fields are required.
fulfillmentUpdatesSpecification_active :: Lens.Lens' FulfillmentUpdatesSpecification Prelude.Bool
fulfillmentUpdatesSpecification_active = Lens.lens (\FulfillmentUpdatesSpecification' {active} -> active) (\s@FulfillmentUpdatesSpecification' {} a -> s {active = a} :: FulfillmentUpdatesSpecification)

instance
  Data.FromJSON
    FulfillmentUpdatesSpecification
  where
  parseJSON =
    Data.withObject
      "FulfillmentUpdatesSpecification"
      ( \x ->
          FulfillmentUpdatesSpecification'
            Prelude.<$> (x Data..:? "startResponse")
            Prelude.<*> (x Data..:? "timeoutInSeconds")
            Prelude.<*> (x Data..:? "updateResponse")
            Prelude.<*> (x Data..: "active")
      )

instance
  Prelude.Hashable
    FulfillmentUpdatesSpecification
  where
  hashWithSalt
    _salt
    FulfillmentUpdatesSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` startResponse
        `Prelude.hashWithSalt` timeoutInSeconds
        `Prelude.hashWithSalt` updateResponse
        `Prelude.hashWithSalt` active

instance
  Prelude.NFData
    FulfillmentUpdatesSpecification
  where
  rnf FulfillmentUpdatesSpecification' {..} =
    Prelude.rnf startResponse `Prelude.seq`
      Prelude.rnf timeoutInSeconds `Prelude.seq`
        Prelude.rnf updateResponse `Prelude.seq`
          Prelude.rnf active

instance Data.ToJSON FulfillmentUpdatesSpecification where
  toJSON FulfillmentUpdatesSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("startResponse" Data..=) Prelude.<$> startResponse,
            ("timeoutInSeconds" Data..=)
              Prelude.<$> timeoutInSeconds,
            ("updateResponse" Data..=)
              Prelude.<$> updateResponse,
            Prelude.Just ("active" Data..= active)
          ]
      )

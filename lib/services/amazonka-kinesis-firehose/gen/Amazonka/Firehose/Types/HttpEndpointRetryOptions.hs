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
-- Module      : Amazonka.Firehose.Types.HttpEndpointRetryOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.HttpEndpointRetryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the retry behavior in case Kinesis Data Firehose is unable to
-- deliver data to the specified HTTP endpoint destination, or if it
-- doesn\'t receive a valid acknowledgment of receipt from the specified
-- HTTP endpoint destination.
--
-- /See:/ 'newHttpEndpointRetryOptions' smart constructor.
data HttpEndpointRetryOptions = HttpEndpointRetryOptions'
  { -- | The total amount of time that Kinesis Data Firehose spends on retries.
    -- This duration starts after the initial attempt to send data to the
    -- custom destination via HTTPS endpoint fails. It doesn\'t include the
    -- periods during which Kinesis Data Firehose waits for acknowledgment from
    -- the specified destination after each attempt.
    durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpEndpointRetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'httpEndpointRetryOptions_durationInSeconds' - The total amount of time that Kinesis Data Firehose spends on retries.
-- This duration starts after the initial attempt to send data to the
-- custom destination via HTTPS endpoint fails. It doesn\'t include the
-- periods during which Kinesis Data Firehose waits for acknowledgment from
-- the specified destination after each attempt.
newHttpEndpointRetryOptions ::
  HttpEndpointRetryOptions
newHttpEndpointRetryOptions =
  HttpEndpointRetryOptions'
    { durationInSeconds =
        Prelude.Nothing
    }

-- | The total amount of time that Kinesis Data Firehose spends on retries.
-- This duration starts after the initial attempt to send data to the
-- custom destination via HTTPS endpoint fails. It doesn\'t include the
-- periods during which Kinesis Data Firehose waits for acknowledgment from
-- the specified destination after each attempt.
httpEndpointRetryOptions_durationInSeconds :: Lens.Lens' HttpEndpointRetryOptions (Prelude.Maybe Prelude.Natural)
httpEndpointRetryOptions_durationInSeconds = Lens.lens (\HttpEndpointRetryOptions' {durationInSeconds} -> durationInSeconds) (\s@HttpEndpointRetryOptions' {} a -> s {durationInSeconds = a} :: HttpEndpointRetryOptions)

instance Data.FromJSON HttpEndpointRetryOptions where
  parseJSON =
    Data.withObject
      "HttpEndpointRetryOptions"
      ( \x ->
          HttpEndpointRetryOptions'
            Prelude.<$> (x Data..:? "DurationInSeconds")
      )

instance Prelude.Hashable HttpEndpointRetryOptions where
  hashWithSalt _salt HttpEndpointRetryOptions' {..} =
    _salt `Prelude.hashWithSalt` durationInSeconds

instance Prelude.NFData HttpEndpointRetryOptions where
  rnf HttpEndpointRetryOptions' {..} =
    Prelude.rnf durationInSeconds

instance Data.ToJSON HttpEndpointRetryOptions where
  toJSON HttpEndpointRetryOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Data..=)
              Prelude.<$> durationInSeconds
          ]
      )

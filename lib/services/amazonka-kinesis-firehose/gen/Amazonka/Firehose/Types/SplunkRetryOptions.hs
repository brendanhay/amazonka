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
-- Module      : Amazonka.Firehose.Types.SplunkRetryOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.SplunkRetryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to
-- deliver documents to Splunk, or if it doesn\'t receive an acknowledgment
-- from Splunk.
--
-- /See:/ 'newSplunkRetryOptions' smart constructor.
data SplunkRetryOptions = SplunkRetryOptions'
  { -- | The total amount of time that Kinesis Data Firehose spends on retries.
    -- This duration starts after the initial attempt to send data to Splunk
    -- fails. It doesn\'t include the periods during which Kinesis Data
    -- Firehose waits for acknowledgment from Splunk after each attempt.
    durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SplunkRetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'splunkRetryOptions_durationInSeconds' - The total amount of time that Kinesis Data Firehose spends on retries.
-- This duration starts after the initial attempt to send data to Splunk
-- fails. It doesn\'t include the periods during which Kinesis Data
-- Firehose waits for acknowledgment from Splunk after each attempt.
newSplunkRetryOptions ::
  SplunkRetryOptions
newSplunkRetryOptions =
  SplunkRetryOptions'
    { durationInSeconds =
        Prelude.Nothing
    }

-- | The total amount of time that Kinesis Data Firehose spends on retries.
-- This duration starts after the initial attempt to send data to Splunk
-- fails. It doesn\'t include the periods during which Kinesis Data
-- Firehose waits for acknowledgment from Splunk after each attempt.
splunkRetryOptions_durationInSeconds :: Lens.Lens' SplunkRetryOptions (Prelude.Maybe Prelude.Natural)
splunkRetryOptions_durationInSeconds = Lens.lens (\SplunkRetryOptions' {durationInSeconds} -> durationInSeconds) (\s@SplunkRetryOptions' {} a -> s {durationInSeconds = a} :: SplunkRetryOptions)

instance Data.FromJSON SplunkRetryOptions where
  parseJSON =
    Data.withObject
      "SplunkRetryOptions"
      ( \x ->
          SplunkRetryOptions'
            Prelude.<$> (x Data..:? "DurationInSeconds")
      )

instance Prelude.Hashable SplunkRetryOptions where
  hashWithSalt _salt SplunkRetryOptions' {..} =
    _salt `Prelude.hashWithSalt` durationInSeconds

instance Prelude.NFData SplunkRetryOptions where
  rnf SplunkRetryOptions' {..} =
    Prelude.rnf durationInSeconds

instance Data.ToJSON SplunkRetryOptions where
  toJSON SplunkRetryOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Data..=)
              Prelude.<$> durationInSeconds
          ]
      )

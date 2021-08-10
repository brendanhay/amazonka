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
-- Module      : Network.AWS.Firehose.Types.SplunkRetryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.SplunkRetryOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON SplunkRetryOptions where
  parseJSON =
    Core.withObject
      "SplunkRetryOptions"
      ( \x ->
          SplunkRetryOptions'
            Prelude.<$> (x Core..:? "DurationInSeconds")
      )

instance Prelude.Hashable SplunkRetryOptions

instance Prelude.NFData SplunkRetryOptions

instance Core.ToJSON SplunkRetryOptions where
  toJSON SplunkRetryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Core..=)
              Prelude.<$> durationInSeconds
          ]
      )

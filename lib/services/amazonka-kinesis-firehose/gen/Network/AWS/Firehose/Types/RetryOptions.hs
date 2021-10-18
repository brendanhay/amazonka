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
-- Module      : Network.AWS.Firehose.Types.RetryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RetryOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The retry behavior in case Kinesis Data Firehose is unable to deliver
-- data to an Amazon S3 prefix.
--
-- /See:/ 'newRetryOptions' smart constructor.
data RetryOptions = RetryOptions'
  { -- | The period of time during which Kinesis Data Firehose retries to deliver
    -- data to the specified Amazon S3 prefix.
    durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'retryOptions_durationInSeconds' - The period of time during which Kinesis Data Firehose retries to deliver
-- data to the specified Amazon S3 prefix.
newRetryOptions ::
  RetryOptions
newRetryOptions =
  RetryOptions' {durationInSeconds = Prelude.Nothing}

-- | The period of time during which Kinesis Data Firehose retries to deliver
-- data to the specified Amazon S3 prefix.
retryOptions_durationInSeconds :: Lens.Lens' RetryOptions (Prelude.Maybe Prelude.Natural)
retryOptions_durationInSeconds = Lens.lens (\RetryOptions' {durationInSeconds} -> durationInSeconds) (\s@RetryOptions' {} a -> s {durationInSeconds = a} :: RetryOptions)

instance Core.FromJSON RetryOptions where
  parseJSON =
    Core.withObject
      "RetryOptions"
      ( \x ->
          RetryOptions'
            Prelude.<$> (x Core..:? "DurationInSeconds")
      )

instance Prelude.Hashable RetryOptions

instance Prelude.NFData RetryOptions

instance Core.ToJSON RetryOptions where
  toJSON RetryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Core..=)
              Prelude.<$> durationInSeconds
          ]
      )

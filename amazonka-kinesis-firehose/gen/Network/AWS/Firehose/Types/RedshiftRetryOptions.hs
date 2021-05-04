{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Firehose.Types.RedshiftRetryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.RedshiftRetryOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to
-- deliver documents to Amazon Redshift.
--
-- /See:/ 'newRedshiftRetryOptions' smart constructor.
data RedshiftRetryOptions = RedshiftRetryOptions'
  { -- | The length of time during which Kinesis Data Firehose retries delivery
    -- after a failure, starting from the initial request and including the
    -- first attempt. The default value is 3600 seconds (60 minutes). Kinesis
    -- Data Firehose does not retry if the value of @DurationInSeconds@ is 0
    -- (zero) or if the first delivery attempt takes longer than the current
    -- value.
    durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RedshiftRetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'redshiftRetryOptions_durationInSeconds' - The length of time during which Kinesis Data Firehose retries delivery
-- after a failure, starting from the initial request and including the
-- first attempt. The default value is 3600 seconds (60 minutes). Kinesis
-- Data Firehose does not retry if the value of @DurationInSeconds@ is 0
-- (zero) or if the first delivery attempt takes longer than the current
-- value.
newRedshiftRetryOptions ::
  RedshiftRetryOptions
newRedshiftRetryOptions =
  RedshiftRetryOptions'
    { durationInSeconds =
        Prelude.Nothing
    }

-- | The length of time during which Kinesis Data Firehose retries delivery
-- after a failure, starting from the initial request and including the
-- first attempt. The default value is 3600 seconds (60 minutes). Kinesis
-- Data Firehose does not retry if the value of @DurationInSeconds@ is 0
-- (zero) or if the first delivery attempt takes longer than the current
-- value.
redshiftRetryOptions_durationInSeconds :: Lens.Lens' RedshiftRetryOptions (Prelude.Maybe Prelude.Natural)
redshiftRetryOptions_durationInSeconds = Lens.lens (\RedshiftRetryOptions' {durationInSeconds} -> durationInSeconds) (\s@RedshiftRetryOptions' {} a -> s {durationInSeconds = a} :: RedshiftRetryOptions)

instance Prelude.FromJSON RedshiftRetryOptions where
  parseJSON =
    Prelude.withObject
      "RedshiftRetryOptions"
      ( \x ->
          RedshiftRetryOptions'
            Prelude.<$> (x Prelude..:? "DurationInSeconds")
      )

instance Prelude.Hashable RedshiftRetryOptions

instance Prelude.NFData RedshiftRetryOptions

instance Prelude.ToJSON RedshiftRetryOptions where
  toJSON RedshiftRetryOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Prelude..=)
              Prelude.<$> durationInSeconds
          ]
      )

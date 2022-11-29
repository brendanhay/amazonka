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
-- Module      : Amazonka.Firehose.Types.RedshiftRetryOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.RedshiftRetryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.FromJSON RedshiftRetryOptions where
  parseJSON =
    Core.withObject
      "RedshiftRetryOptions"
      ( \x ->
          RedshiftRetryOptions'
            Prelude.<$> (x Core..:? "DurationInSeconds")
      )

instance Prelude.Hashable RedshiftRetryOptions where
  hashWithSalt _salt RedshiftRetryOptions' {..} =
    _salt `Prelude.hashWithSalt` durationInSeconds

instance Prelude.NFData RedshiftRetryOptions where
  rnf RedshiftRetryOptions' {..} =
    Prelude.rnf durationInSeconds

instance Core.ToJSON RedshiftRetryOptions where
  toJSON RedshiftRetryOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Core..=)
              Prelude.<$> durationInSeconds
          ]
      )

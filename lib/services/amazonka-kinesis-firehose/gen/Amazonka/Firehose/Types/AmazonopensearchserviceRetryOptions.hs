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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceRetryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to
-- deliver documents to Amazon OpenSearch Service.
--
-- /See:/ 'newAmazonopensearchserviceRetryOptions' smart constructor.
data AmazonopensearchserviceRetryOptions = AmazonopensearchserviceRetryOptions'
  { -- | After an initial failure to deliver to Amazon OpenSearch Service, the
    -- total amount of time during which Kinesis Data Firehose retries delivery
    -- (including the first attempt). After this time has elapsed, the failed
    -- documents are written to Amazon S3. Default value is 300 seconds (5
    -- minutes). A value of 0 (zero) results in no retries.
    durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonopensearchserviceRetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'amazonopensearchserviceRetryOptions_durationInSeconds' - After an initial failure to deliver to Amazon OpenSearch Service, the
-- total amount of time during which Kinesis Data Firehose retries delivery
-- (including the first attempt). After this time has elapsed, the failed
-- documents are written to Amazon S3. Default value is 300 seconds (5
-- minutes). A value of 0 (zero) results in no retries.
newAmazonopensearchserviceRetryOptions ::
  AmazonopensearchserviceRetryOptions
newAmazonopensearchserviceRetryOptions =
  AmazonopensearchserviceRetryOptions'
    { durationInSeconds =
        Prelude.Nothing
    }

-- | After an initial failure to deliver to Amazon OpenSearch Service, the
-- total amount of time during which Kinesis Data Firehose retries delivery
-- (including the first attempt). After this time has elapsed, the failed
-- documents are written to Amazon S3. Default value is 300 seconds (5
-- minutes). A value of 0 (zero) results in no retries.
amazonopensearchserviceRetryOptions_durationInSeconds :: Lens.Lens' AmazonopensearchserviceRetryOptions (Prelude.Maybe Prelude.Natural)
amazonopensearchserviceRetryOptions_durationInSeconds = Lens.lens (\AmazonopensearchserviceRetryOptions' {durationInSeconds} -> durationInSeconds) (\s@AmazonopensearchserviceRetryOptions' {} a -> s {durationInSeconds = a} :: AmazonopensearchserviceRetryOptions)

instance
  Data.FromJSON
    AmazonopensearchserviceRetryOptions
  where
  parseJSON =
    Data.withObject
      "AmazonopensearchserviceRetryOptions"
      ( \x ->
          AmazonopensearchserviceRetryOptions'
            Prelude.<$> (x Data..:? "DurationInSeconds")
      )

instance
  Prelude.Hashable
    AmazonopensearchserviceRetryOptions
  where
  hashWithSalt
    _salt
    AmazonopensearchserviceRetryOptions' {..} =
      _salt `Prelude.hashWithSalt` durationInSeconds

instance
  Prelude.NFData
    AmazonopensearchserviceRetryOptions
  where
  rnf AmazonopensearchserviceRetryOptions' {..} =
    Prelude.rnf durationInSeconds

instance
  Data.ToJSON
    AmazonopensearchserviceRetryOptions
  where
  toJSON AmazonopensearchserviceRetryOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Data..=)
              Prelude.<$> durationInSeconds
          ]
      )

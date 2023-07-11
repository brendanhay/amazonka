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
-- Module      : Amazonka.Firehose.Types.AmazonOpenSearchServerlessRetryOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonOpenSearchServerlessRetryOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures retry behavior in case Kinesis Data Firehose is unable to
-- deliver documents to the Serverless offering for Amazon OpenSearch
-- Service.
--
-- /See:/ 'newAmazonOpenSearchServerlessRetryOptions' smart constructor.
data AmazonOpenSearchServerlessRetryOptions = AmazonOpenSearchServerlessRetryOptions'
  { -- | After an initial failure to deliver to the Serverless offering for
    -- Amazon OpenSearch Service, the total amount of time during which Kinesis
    -- Data Firehose retries delivery (including the first attempt). After this
    -- time has elapsed, the failed documents are written to Amazon S3. Default
    -- value is 300 seconds (5 minutes). A value of 0 (zero) results in no
    -- retries.
    durationInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonOpenSearchServerlessRetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'amazonOpenSearchServerlessRetryOptions_durationInSeconds' - After an initial failure to deliver to the Serverless offering for
-- Amazon OpenSearch Service, the total amount of time during which Kinesis
-- Data Firehose retries delivery (including the first attempt). After this
-- time has elapsed, the failed documents are written to Amazon S3. Default
-- value is 300 seconds (5 minutes). A value of 0 (zero) results in no
-- retries.
newAmazonOpenSearchServerlessRetryOptions ::
  AmazonOpenSearchServerlessRetryOptions
newAmazonOpenSearchServerlessRetryOptions =
  AmazonOpenSearchServerlessRetryOptions'
    { durationInSeconds =
        Prelude.Nothing
    }

-- | After an initial failure to deliver to the Serverless offering for
-- Amazon OpenSearch Service, the total amount of time during which Kinesis
-- Data Firehose retries delivery (including the first attempt). After this
-- time has elapsed, the failed documents are written to Amazon S3. Default
-- value is 300 seconds (5 minutes). A value of 0 (zero) results in no
-- retries.
amazonOpenSearchServerlessRetryOptions_durationInSeconds :: Lens.Lens' AmazonOpenSearchServerlessRetryOptions (Prelude.Maybe Prelude.Natural)
amazonOpenSearchServerlessRetryOptions_durationInSeconds = Lens.lens (\AmazonOpenSearchServerlessRetryOptions' {durationInSeconds} -> durationInSeconds) (\s@AmazonOpenSearchServerlessRetryOptions' {} a -> s {durationInSeconds = a} :: AmazonOpenSearchServerlessRetryOptions)

instance
  Data.FromJSON
    AmazonOpenSearchServerlessRetryOptions
  where
  parseJSON =
    Data.withObject
      "AmazonOpenSearchServerlessRetryOptions"
      ( \x ->
          AmazonOpenSearchServerlessRetryOptions'
            Prelude.<$> (x Data..:? "DurationInSeconds")
      )

instance
  Prelude.Hashable
    AmazonOpenSearchServerlessRetryOptions
  where
  hashWithSalt
    _salt
    AmazonOpenSearchServerlessRetryOptions' {..} =
      _salt `Prelude.hashWithSalt` durationInSeconds

instance
  Prelude.NFData
    AmazonOpenSearchServerlessRetryOptions
  where
  rnf AmazonOpenSearchServerlessRetryOptions' {..} =
    Prelude.rnf durationInSeconds

instance
  Data.ToJSON
    AmazonOpenSearchServerlessRetryOptions
  where
  toJSON AmazonOpenSearchServerlessRetryOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DurationInSeconds" Data..=)
              Prelude.<$> durationInSeconds
          ]
      )

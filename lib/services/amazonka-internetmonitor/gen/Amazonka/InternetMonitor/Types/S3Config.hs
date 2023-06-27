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
-- Module      : Amazonka.InternetMonitor.Types.S3Config
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.S3Config where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.LogDeliveryStatus
import qualified Amazonka.Prelude as Prelude

-- | The configuration for publishing Amazon CloudWatch Internet Monitor
-- internet measurements to Amazon S3. The configuration includes the
-- bucket name and (optionally) prefix for the S3 bucket to store the
-- measurements, and the delivery status. The delivery status is @ENABLED@
-- or @DISABLED@, depending on whether you choose to deliver internet
-- measurements to S3 logs.
--
-- /See:/ 'newS3Config' smart constructor.
data S3Config = S3Config'
  { -- | The Amazon S3 bucket name.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket prefix.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The status of publishing Internet Monitor internet measurements to an
    -- Amazon S3 bucket.
    logDeliveryStatus :: Prelude.Maybe LogDeliveryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Config' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3Config_bucketName' - The Amazon S3 bucket name.
--
-- 'bucketPrefix', 's3Config_bucketPrefix' - The Amazon S3 bucket prefix.
--
-- 'logDeliveryStatus', 's3Config_logDeliveryStatus' - The status of publishing Internet Monitor internet measurements to an
-- Amazon S3 bucket.
newS3Config ::
  S3Config
newS3Config =
  S3Config'
    { bucketName = Prelude.Nothing,
      bucketPrefix = Prelude.Nothing,
      logDeliveryStatus = Prelude.Nothing
    }

-- | The Amazon S3 bucket name.
s3Config_bucketName :: Lens.Lens' S3Config (Prelude.Maybe Prelude.Text)
s3Config_bucketName = Lens.lens (\S3Config' {bucketName} -> bucketName) (\s@S3Config' {} a -> s {bucketName = a} :: S3Config)

-- | The Amazon S3 bucket prefix.
s3Config_bucketPrefix :: Lens.Lens' S3Config (Prelude.Maybe Prelude.Text)
s3Config_bucketPrefix = Lens.lens (\S3Config' {bucketPrefix} -> bucketPrefix) (\s@S3Config' {} a -> s {bucketPrefix = a} :: S3Config)

-- | The status of publishing Internet Monitor internet measurements to an
-- Amazon S3 bucket.
s3Config_logDeliveryStatus :: Lens.Lens' S3Config (Prelude.Maybe LogDeliveryStatus)
s3Config_logDeliveryStatus = Lens.lens (\S3Config' {logDeliveryStatus} -> logDeliveryStatus) (\s@S3Config' {} a -> s {logDeliveryStatus = a} :: S3Config)

instance Data.FromJSON S3Config where
  parseJSON =
    Data.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "BucketPrefix")
            Prelude.<*> (x Data..:? "LogDeliveryStatus")
      )

instance Prelude.Hashable S3Config where
  hashWithSalt _salt S3Config' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` bucketPrefix
      `Prelude.hashWithSalt` logDeliveryStatus

instance Prelude.NFData S3Config where
  rnf S3Config' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf logDeliveryStatus

instance Data.ToJSON S3Config where
  toJSON S3Config' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BucketName" Data..=) Prelude.<$> bucketName,
            ("BucketPrefix" Data..=) Prelude.<$> bucketPrefix,
            ("LogDeliveryStatus" Data..=)
              Prelude.<$> logDeliveryStatus
          ]
      )

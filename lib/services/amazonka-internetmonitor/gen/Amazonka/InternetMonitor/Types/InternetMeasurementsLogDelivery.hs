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
-- Module      : Amazonka.InternetMonitor.Types.InternetMeasurementsLogDelivery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.InternetMeasurementsLogDelivery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.S3Config
import qualified Amazonka.Prelude as Prelude

-- | Publish internet measurements to an Amazon S3 bucket in addition to
-- CloudWatch Logs.
--
-- /See:/ 'newInternetMeasurementsLogDelivery' smart constructor.
data InternetMeasurementsLogDelivery = InternetMeasurementsLogDelivery'
  { -- | The configuration information for publishing Internet Monitor internet
    -- measurements to Amazon S3. The configuration includes the bucket name
    -- and (optionally) prefix for the S3 bucket to store the measurements, and
    -- the delivery status. The delivery status is @ENABLED@ or @DISABLED@,
    -- depending on whether you choose to deliver internet measurements to S3
    -- logs.
    s3Config :: Prelude.Maybe S3Config
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InternetMeasurementsLogDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Config', 'internetMeasurementsLogDelivery_s3Config' - The configuration information for publishing Internet Monitor internet
-- measurements to Amazon S3. The configuration includes the bucket name
-- and (optionally) prefix for the S3 bucket to store the measurements, and
-- the delivery status. The delivery status is @ENABLED@ or @DISABLED@,
-- depending on whether you choose to deliver internet measurements to S3
-- logs.
newInternetMeasurementsLogDelivery ::
  InternetMeasurementsLogDelivery
newInternetMeasurementsLogDelivery =
  InternetMeasurementsLogDelivery'
    { s3Config =
        Prelude.Nothing
    }

-- | The configuration information for publishing Internet Monitor internet
-- measurements to Amazon S3. The configuration includes the bucket name
-- and (optionally) prefix for the S3 bucket to store the measurements, and
-- the delivery status. The delivery status is @ENABLED@ or @DISABLED@,
-- depending on whether you choose to deliver internet measurements to S3
-- logs.
internetMeasurementsLogDelivery_s3Config :: Lens.Lens' InternetMeasurementsLogDelivery (Prelude.Maybe S3Config)
internetMeasurementsLogDelivery_s3Config = Lens.lens (\InternetMeasurementsLogDelivery' {s3Config} -> s3Config) (\s@InternetMeasurementsLogDelivery' {} a -> s {s3Config = a} :: InternetMeasurementsLogDelivery)

instance
  Data.FromJSON
    InternetMeasurementsLogDelivery
  where
  parseJSON =
    Data.withObject
      "InternetMeasurementsLogDelivery"
      ( \x ->
          InternetMeasurementsLogDelivery'
            Prelude.<$> (x Data..:? "S3Config")
      )

instance
  Prelude.Hashable
    InternetMeasurementsLogDelivery
  where
  hashWithSalt
    _salt
    InternetMeasurementsLogDelivery' {..} =
      _salt `Prelude.hashWithSalt` s3Config

instance
  Prelude.NFData
    InternetMeasurementsLogDelivery
  where
  rnf InternetMeasurementsLogDelivery' {..} =
    Prelude.rnf s3Config

instance Data.ToJSON InternetMeasurementsLogDelivery where
  toJSON InternetMeasurementsLogDelivery' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Config" Data..=) Prelude.<$> s3Config]
      )

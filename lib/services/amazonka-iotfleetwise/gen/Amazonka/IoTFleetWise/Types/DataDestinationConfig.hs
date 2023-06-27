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
-- Module      : Amazonka.IoTFleetWise.Types.DataDestinationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.DataDestinationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.S3Config
import Amazonka.IoTFleetWise.Types.TimestreamConfig
import qualified Amazonka.Prelude as Prelude

-- | The destination where the Amazon Web Services IoT FleetWise campaign
-- sends data. You can send data to be stored in Amazon S3 or Amazon
-- Timestream.
--
-- /See:/ 'newDataDestinationConfig' smart constructor.
data DataDestinationConfig = DataDestinationConfig'
  { -- | The Amazon S3 bucket where the Amazon Web Services IoT FleetWise
    -- campaign sends data.
    s3Config :: Prelude.Maybe S3Config,
    -- | The Amazon Timestream table where the campaign sends data.
    timestreamConfig :: Prelude.Maybe TimestreamConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataDestinationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Config', 'dataDestinationConfig_s3Config' - The Amazon S3 bucket where the Amazon Web Services IoT FleetWise
-- campaign sends data.
--
-- 'timestreamConfig', 'dataDestinationConfig_timestreamConfig' - The Amazon Timestream table where the campaign sends data.
newDataDestinationConfig ::
  DataDestinationConfig
newDataDestinationConfig =
  DataDestinationConfig'
    { s3Config = Prelude.Nothing,
      timestreamConfig = Prelude.Nothing
    }

-- | The Amazon S3 bucket where the Amazon Web Services IoT FleetWise
-- campaign sends data.
dataDestinationConfig_s3Config :: Lens.Lens' DataDestinationConfig (Prelude.Maybe S3Config)
dataDestinationConfig_s3Config = Lens.lens (\DataDestinationConfig' {s3Config} -> s3Config) (\s@DataDestinationConfig' {} a -> s {s3Config = a} :: DataDestinationConfig)

-- | The Amazon Timestream table where the campaign sends data.
dataDestinationConfig_timestreamConfig :: Lens.Lens' DataDestinationConfig (Prelude.Maybe TimestreamConfig)
dataDestinationConfig_timestreamConfig = Lens.lens (\DataDestinationConfig' {timestreamConfig} -> timestreamConfig) (\s@DataDestinationConfig' {} a -> s {timestreamConfig = a} :: DataDestinationConfig)

instance Data.FromJSON DataDestinationConfig where
  parseJSON =
    Data.withObject
      "DataDestinationConfig"
      ( \x ->
          DataDestinationConfig'
            Prelude.<$> (x Data..:? "s3Config")
            Prelude.<*> (x Data..:? "timestreamConfig")
      )

instance Prelude.Hashable DataDestinationConfig where
  hashWithSalt _salt DataDestinationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` s3Config
      `Prelude.hashWithSalt` timestreamConfig

instance Prelude.NFData DataDestinationConfig where
  rnf DataDestinationConfig' {..} =
    Prelude.rnf s3Config
      `Prelude.seq` Prelude.rnf timestreamConfig

instance Data.ToJSON DataDestinationConfig where
  toJSON DataDestinationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3Config" Data..=) Prelude.<$> s3Config,
            ("timestreamConfig" Data..=)
              Prelude.<$> timestreamConfig
          ]
      )

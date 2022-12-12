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
-- Module      : Amazonka.LookoutMetrics.Types.CloudWatchConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.CloudWatchConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types.BackTestConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Details about an Amazon CloudWatch datasource.
--
-- /See:/ 'newCloudWatchConfig' smart constructor.
data CloudWatchConfig = CloudWatchConfig'
  { -- | Settings for backtest mode.
    backTestConfiguration :: Prelude.Maybe BackTestConfiguration,
    -- | An IAM role that gives Amazon Lookout for Metrics permission to access
    -- data in Amazon CloudWatch.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backTestConfiguration', 'cloudWatchConfig_backTestConfiguration' - Settings for backtest mode.
--
-- 'roleArn', 'cloudWatchConfig_roleArn' - An IAM role that gives Amazon Lookout for Metrics permission to access
-- data in Amazon CloudWatch.
newCloudWatchConfig ::
  CloudWatchConfig
newCloudWatchConfig =
  CloudWatchConfig'
    { backTestConfiguration =
        Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | Settings for backtest mode.
cloudWatchConfig_backTestConfiguration :: Lens.Lens' CloudWatchConfig (Prelude.Maybe BackTestConfiguration)
cloudWatchConfig_backTestConfiguration = Lens.lens (\CloudWatchConfig' {backTestConfiguration} -> backTestConfiguration) (\s@CloudWatchConfig' {} a -> s {backTestConfiguration = a} :: CloudWatchConfig)

-- | An IAM role that gives Amazon Lookout for Metrics permission to access
-- data in Amazon CloudWatch.
cloudWatchConfig_roleArn :: Lens.Lens' CloudWatchConfig (Prelude.Maybe Prelude.Text)
cloudWatchConfig_roleArn = Lens.lens (\CloudWatchConfig' {roleArn} -> roleArn) (\s@CloudWatchConfig' {} a -> s {roleArn = a} :: CloudWatchConfig)

instance Data.FromJSON CloudWatchConfig where
  parseJSON =
    Data.withObject
      "CloudWatchConfig"
      ( \x ->
          CloudWatchConfig'
            Prelude.<$> (x Data..:? "BackTestConfiguration")
            Prelude.<*> (x Data..:? "RoleArn")
      )

instance Prelude.Hashable CloudWatchConfig where
  hashWithSalt _salt CloudWatchConfig' {..} =
    _salt `Prelude.hashWithSalt` backTestConfiguration
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CloudWatchConfig where
  rnf CloudWatchConfig' {..} =
    Prelude.rnf backTestConfiguration
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON CloudWatchConfig where
  toJSON CloudWatchConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackTestConfiguration" Data..=)
              Prelude.<$> backTestConfiguration,
            ("RoleArn" Data..=) Prelude.<$> roleArn
          ]
      )

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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptionsLogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainLogPublishingOptionsLogConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The log configuration.
--
-- /See:/ 'newAwsElasticsearchDomainLogPublishingOptionsLogConfig' smart constructor.
data AwsElasticsearchDomainLogPublishingOptionsLogConfig = AwsElasticsearchDomainLogPublishingOptionsLogConfig'
  { -- | The ARN of the CloudWatch Logs group to publish the logs to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the log publishing is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainLogPublishingOptionsLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsLogGroupArn', 'awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn' - The ARN of the CloudWatch Logs group to publish the logs to.
--
-- 'enabled', 'awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled' - Whether the log publishing is enabled.
newAwsElasticsearchDomainLogPublishingOptionsLogConfig ::
  AwsElasticsearchDomainLogPublishingOptionsLogConfig
newAwsElasticsearchDomainLogPublishingOptionsLogConfig =
  AwsElasticsearchDomainLogPublishingOptionsLogConfig'
    { cloudWatchLogsLogGroupArn =
        Prelude.Nothing,
      enabled =
        Prelude.Nothing
    }

-- | The ARN of the CloudWatch Logs group to publish the logs to.
awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptionsLogConfig (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn = Lens.lens (\AwsElasticsearchDomainLogPublishingOptionsLogConfig' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@AwsElasticsearchDomainLogPublishingOptionsLogConfig' {} a -> s {cloudWatchLogsLogGroupArn = a} :: AwsElasticsearchDomainLogPublishingOptionsLogConfig)

-- | Whether the log publishing is enabled.
awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptionsLogConfig (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled = Lens.lens (\AwsElasticsearchDomainLogPublishingOptionsLogConfig' {enabled} -> enabled) (\s@AwsElasticsearchDomainLogPublishingOptionsLogConfig' {} a -> s {enabled = a} :: AwsElasticsearchDomainLogPublishingOptionsLogConfig)

instance
  Data.FromJSON
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  parseJSON =
    Data.withObject
      "AwsElasticsearchDomainLogPublishingOptionsLogConfig"
      ( \x ->
          AwsElasticsearchDomainLogPublishingOptionsLogConfig'
            Prelude.<$> (x Data..:? "CloudWatchLogsLogGroupArn")
              Prelude.<*> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainLogPublishingOptionsLogConfig' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  rnf
    AwsElasticsearchDomainLogPublishingOptionsLogConfig' {..} =
      Prelude.rnf cloudWatchLogsLogGroupArn
        `Prelude.seq` Prelude.rnf enabled

instance
  Data.ToJSON
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  toJSON
    AwsElasticsearchDomainLogPublishingOptionsLogConfig' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CloudWatchLogsLogGroupArn" Data..=)
                Prelude.<$> cloudWatchLogsLogGroupArn,
              ("Enabled" Data..=) Prelude.<$> enabled
            ]
        )

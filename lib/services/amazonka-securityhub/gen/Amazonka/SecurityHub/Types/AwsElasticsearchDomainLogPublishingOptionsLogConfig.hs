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
import qualified Amazonka.Prelude as Prelude

-- | The log configuration.
--
-- /See:/ 'newAwsElasticsearchDomainLogPublishingOptionsLogConfig' smart constructor.
data AwsElasticsearchDomainLogPublishingOptionsLogConfig = AwsElasticsearchDomainLogPublishingOptionsLogConfig'
  { -- | Whether the log publishing is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the CloudWatch Logs group to publish the logs to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text
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
-- 'enabled', 'awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled' - Whether the log publishing is enabled.
--
-- 'cloudWatchLogsLogGroupArn', 'awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn' - The ARN of the CloudWatch Logs group to publish the logs to.
newAwsElasticsearchDomainLogPublishingOptionsLogConfig ::
  AwsElasticsearchDomainLogPublishingOptionsLogConfig
newAwsElasticsearchDomainLogPublishingOptionsLogConfig =
  AwsElasticsearchDomainLogPublishingOptionsLogConfig'
    { enabled =
        Prelude.Nothing,
      cloudWatchLogsLogGroupArn =
        Prelude.Nothing
    }

-- | Whether the log publishing is enabled.
awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptionsLogConfig (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainLogPublishingOptionsLogConfig_enabled = Lens.lens (\AwsElasticsearchDomainLogPublishingOptionsLogConfig' {enabled} -> enabled) (\s@AwsElasticsearchDomainLogPublishingOptionsLogConfig' {} a -> s {enabled = a} :: AwsElasticsearchDomainLogPublishingOptionsLogConfig)

-- | The ARN of the CloudWatch Logs group to publish the logs to.
awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn :: Lens.Lens' AwsElasticsearchDomainLogPublishingOptionsLogConfig (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainLogPublishingOptionsLogConfig_cloudWatchLogsLogGroupArn = Lens.lens (\AwsElasticsearchDomainLogPublishingOptionsLogConfig' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@AwsElasticsearchDomainLogPublishingOptionsLogConfig' {} a -> s {cloudWatchLogsLogGroupArn = a} :: AwsElasticsearchDomainLogPublishingOptionsLogConfig)

instance
  Core.FromJSON
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  parseJSON =
    Core.withObject
      "AwsElasticsearchDomainLogPublishingOptionsLogConfig"
      ( \x ->
          AwsElasticsearchDomainLogPublishingOptionsLogConfig'
            Prelude.<$> (x Core..:? "Enabled")
              Prelude.<*> (x Core..:? "CloudWatchLogsLogGroupArn")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainLogPublishingOptionsLogConfig' {..} =
      _salt `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn

instance
  Prelude.NFData
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  rnf
    AwsElasticsearchDomainLogPublishingOptionsLogConfig' {..} =
      Prelude.rnf enabled
        `Prelude.seq` Prelude.rnf cloudWatchLogsLogGroupArn

instance
  Core.ToJSON
    AwsElasticsearchDomainLogPublishingOptionsLogConfig
  where
  toJSON
    AwsElasticsearchDomainLogPublishingOptionsLogConfig' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Enabled" Core..=) Prelude.<$> enabled,
              ("CloudWatchLogsLogGroupArn" Core..=)
                Prelude.<$> cloudWatchLogsLogGroupArn
            ]
        )

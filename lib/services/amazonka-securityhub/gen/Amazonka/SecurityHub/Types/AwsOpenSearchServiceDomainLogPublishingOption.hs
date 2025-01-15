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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration details for a log publishing option.
--
-- /See:/ 'newAwsOpenSearchServiceDomainLogPublishingOption' smart constructor.
data AwsOpenSearchServiceDomainLogPublishingOption = AwsOpenSearchServiceDomainLogPublishingOption'
  { -- | The ARN of the CloudWatch Logs group to publish the logs to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the log publishing is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainLogPublishingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogsLogGroupArn', 'awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn' - The ARN of the CloudWatch Logs group to publish the logs to.
--
-- 'enabled', 'awsOpenSearchServiceDomainLogPublishingOption_enabled' - Whether the log publishing is enabled.
newAwsOpenSearchServiceDomainLogPublishingOption ::
  AwsOpenSearchServiceDomainLogPublishingOption
newAwsOpenSearchServiceDomainLogPublishingOption =
  AwsOpenSearchServiceDomainLogPublishingOption'
    { cloudWatchLogsLogGroupArn =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The ARN of the CloudWatch Logs group to publish the logs to.
awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOption (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOption' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@AwsOpenSearchServiceDomainLogPublishingOption' {} a -> s {cloudWatchLogsLogGroupArn = a} :: AwsOpenSearchServiceDomainLogPublishingOption)

-- | Whether the log publishing is enabled.
awsOpenSearchServiceDomainLogPublishingOption_enabled :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOption (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainLogPublishingOption_enabled = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOption' {enabled} -> enabled) (\s@AwsOpenSearchServiceDomainLogPublishingOption' {} a -> s {enabled = a} :: AwsOpenSearchServiceDomainLogPublishingOption)

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainLogPublishingOption
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainLogPublishingOption"
      ( \x ->
          AwsOpenSearchServiceDomainLogPublishingOption'
            Prelude.<$> (x Data..:? "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Data..:? "Enabled")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainLogPublishingOption
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainLogPublishingOption' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainLogPublishingOption
  where
  rnf
    AwsOpenSearchServiceDomainLogPublishingOption' {..} =
      Prelude.rnf cloudWatchLogsLogGroupArn `Prelude.seq`
        Prelude.rnf enabled

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainLogPublishingOption
  where
  toJSON
    AwsOpenSearchServiceDomainLogPublishingOption' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CloudWatchLogsLogGroupArn" Data..=)
                Prelude.<$> cloudWatchLogsLogGroupArn,
              ("Enabled" Data..=) Prelude.<$> enabled
            ]
        )

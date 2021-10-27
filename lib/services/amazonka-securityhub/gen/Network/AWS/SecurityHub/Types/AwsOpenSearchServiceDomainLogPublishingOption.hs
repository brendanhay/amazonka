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
-- Module      : Network.AWS.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsOpenSearchServiceDomainLogPublishingOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration details for a log publishing option.
--
-- /See:/ 'newAwsOpenSearchServiceDomainLogPublishingOption' smart constructor.
data AwsOpenSearchServiceDomainLogPublishingOption = AwsOpenSearchServiceDomainLogPublishingOption'
  { -- | Whether the log publishing is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the CloudWatch Logs group to publish the logs to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text
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
-- 'enabled', 'awsOpenSearchServiceDomainLogPublishingOption_enabled' - Whether the log publishing is enabled.
--
-- 'cloudWatchLogsLogGroupArn', 'awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn' - The ARN of the CloudWatch Logs group to publish the logs to.
newAwsOpenSearchServiceDomainLogPublishingOption ::
  AwsOpenSearchServiceDomainLogPublishingOption
newAwsOpenSearchServiceDomainLogPublishingOption =
  AwsOpenSearchServiceDomainLogPublishingOption'
    { enabled =
        Prelude.Nothing,
      cloudWatchLogsLogGroupArn =
        Prelude.Nothing
    }

-- | Whether the log publishing is enabled.
awsOpenSearchServiceDomainLogPublishingOption_enabled :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOption (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainLogPublishingOption_enabled = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOption' {enabled} -> enabled) (\s@AwsOpenSearchServiceDomainLogPublishingOption' {} a -> s {enabled = a} :: AwsOpenSearchServiceDomainLogPublishingOption)

-- | The ARN of the CloudWatch Logs group to publish the logs to.
awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn :: Lens.Lens' AwsOpenSearchServiceDomainLogPublishingOption (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainLogPublishingOption_cloudWatchLogsLogGroupArn = Lens.lens (\AwsOpenSearchServiceDomainLogPublishingOption' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@AwsOpenSearchServiceDomainLogPublishingOption' {} a -> s {cloudWatchLogsLogGroupArn = a} :: AwsOpenSearchServiceDomainLogPublishingOption)

instance
  Core.FromJSON
    AwsOpenSearchServiceDomainLogPublishingOption
  where
  parseJSON =
    Core.withObject
      "AwsOpenSearchServiceDomainLogPublishingOption"
      ( \x ->
          AwsOpenSearchServiceDomainLogPublishingOption'
            Prelude.<$> (x Core..:? "Enabled")
              Prelude.<*> (x Core..:? "CloudWatchLogsLogGroupArn")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainLogPublishingOption

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainLogPublishingOption

instance
  Core.ToJSON
    AwsOpenSearchServiceDomainLogPublishingOption
  where
  toJSON
    AwsOpenSearchServiceDomainLogPublishingOption' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Enabled" Core..=) Prelude.<$> enabled,
              ("CloudWatchLogsLogGroupArn" Core..=)
                Prelude.<$> cloudWatchLogsLogGroupArn
            ]
        )

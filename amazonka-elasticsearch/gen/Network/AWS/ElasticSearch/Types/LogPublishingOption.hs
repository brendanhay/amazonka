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
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogPublishingOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Log Publishing option that is set for given domain.
-- Attributes and their details:
--
-- -   CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which
--     log needs to be published.
-- -   Enabled: Whether the log publishing for given log type is enabled or
--     not
--
-- /See:/ 'newLogPublishingOption' smart constructor.
data LogPublishingOption = LogPublishingOption'
  { -- | Specifies whether given log publishing option is enabled or not.
    enabled :: Core.Maybe Core.Bool,
    cloudWatchLogsLogGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LogPublishingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'logPublishingOption_enabled' - Specifies whether given log publishing option is enabled or not.
--
-- 'cloudWatchLogsLogGroupArn', 'logPublishingOption_cloudWatchLogsLogGroupArn' - Undocumented member.
newLogPublishingOption ::
  LogPublishingOption
newLogPublishingOption =
  LogPublishingOption'
    { enabled = Core.Nothing,
      cloudWatchLogsLogGroupArn = Core.Nothing
    }

-- | Specifies whether given log publishing option is enabled or not.
logPublishingOption_enabled :: Lens.Lens' LogPublishingOption (Core.Maybe Core.Bool)
logPublishingOption_enabled = Lens.lens (\LogPublishingOption' {enabled} -> enabled) (\s@LogPublishingOption' {} a -> s {enabled = a} :: LogPublishingOption)

-- | Undocumented member.
logPublishingOption_cloudWatchLogsLogGroupArn :: Lens.Lens' LogPublishingOption (Core.Maybe Core.Text)
logPublishingOption_cloudWatchLogsLogGroupArn = Lens.lens (\LogPublishingOption' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@LogPublishingOption' {} a -> s {cloudWatchLogsLogGroupArn = a} :: LogPublishingOption)

instance Core.FromJSON LogPublishingOption where
  parseJSON =
    Core.withObject
      "LogPublishingOption"
      ( \x ->
          LogPublishingOption'
            Core.<$> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "CloudWatchLogsLogGroupArn")
      )

instance Core.Hashable LogPublishingOption

instance Core.NFData LogPublishingOption

instance Core.ToJSON LogPublishingOption where
  toJSON LogPublishingOption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("CloudWatchLogsLogGroupArn" Core..=)
              Core.<$> cloudWatchLogsLogGroupArn
          ]
      )

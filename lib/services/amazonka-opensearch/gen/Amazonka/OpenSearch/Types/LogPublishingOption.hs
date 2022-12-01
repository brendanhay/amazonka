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
-- Module      : Amazonka.OpenSearch.Types.LogPublishingOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.LogPublishingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether the Amazon OpenSearch Service domain publishes the
-- OpenSearch application and slow logs to Amazon CloudWatch. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createdomain-configure-slow-logs.html Monitoring OpenSearch logs with Amazon CloudWatch Logs>.
--
-- After you enable log publishing, you still have to enable the collection
-- of slow logs using the OpenSearch REST API.
--
-- /See:/ 'newLogPublishingOption' smart constructor.
data LogPublishingOption = LogPublishingOption'
  { -- | Whether the log should be published.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Logs group to publish
    -- logs to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogPublishingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'logPublishingOption_enabled' - Whether the log should be published.
--
-- 'cloudWatchLogsLogGroupArn', 'logPublishingOption_cloudWatchLogsLogGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch Logs group to publish
-- logs to.
newLogPublishingOption ::
  LogPublishingOption
newLogPublishingOption =
  LogPublishingOption'
    { enabled = Prelude.Nothing,
      cloudWatchLogsLogGroupArn = Prelude.Nothing
    }

-- | Whether the log should be published.
logPublishingOption_enabled :: Lens.Lens' LogPublishingOption (Prelude.Maybe Prelude.Bool)
logPublishingOption_enabled = Lens.lens (\LogPublishingOption' {enabled} -> enabled) (\s@LogPublishingOption' {} a -> s {enabled = a} :: LogPublishingOption)

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs group to publish
-- logs to.
logPublishingOption_cloudWatchLogsLogGroupArn :: Lens.Lens' LogPublishingOption (Prelude.Maybe Prelude.Text)
logPublishingOption_cloudWatchLogsLogGroupArn = Lens.lens (\LogPublishingOption' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@LogPublishingOption' {} a -> s {cloudWatchLogsLogGroupArn = a} :: LogPublishingOption)

instance Core.FromJSON LogPublishingOption where
  parseJSON =
    Core.withObject
      "LogPublishingOption"
      ( \x ->
          LogPublishingOption'
            Prelude.<$> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "CloudWatchLogsLogGroupArn")
      )

instance Prelude.Hashable LogPublishingOption where
  hashWithSalt _salt LogPublishingOption' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn

instance Prelude.NFData LogPublishingOption where
  rnf LogPublishingOption' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf cloudWatchLogsLogGroupArn

instance Core.ToJSON LogPublishingOption where
  toJSON LogPublishingOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
            ("CloudWatchLogsLogGroupArn" Core..=)
              Prelude.<$> cloudWatchLogsLogGroupArn
          ]
      )

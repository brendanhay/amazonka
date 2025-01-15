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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.LogPublishingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | The Amazon Resource Name (ARN) of the CloudWatch Logs group to publish
    -- logs to.
    cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the log should be published.
    enabled :: Prelude.Maybe Prelude.Bool
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
-- 'cloudWatchLogsLogGroupArn', 'logPublishingOption_cloudWatchLogsLogGroupArn' - The Amazon Resource Name (ARN) of the CloudWatch Logs group to publish
-- logs to.
--
-- 'enabled', 'logPublishingOption_enabled' - Whether the log should be published.
newLogPublishingOption ::
  LogPublishingOption
newLogPublishingOption =
  LogPublishingOption'
    { cloudWatchLogsLogGroupArn =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs group to publish
-- logs to.
logPublishingOption_cloudWatchLogsLogGroupArn :: Lens.Lens' LogPublishingOption (Prelude.Maybe Prelude.Text)
logPublishingOption_cloudWatchLogsLogGroupArn = Lens.lens (\LogPublishingOption' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@LogPublishingOption' {} a -> s {cloudWatchLogsLogGroupArn = a} :: LogPublishingOption)

-- | Whether the log should be published.
logPublishingOption_enabled :: Lens.Lens' LogPublishingOption (Prelude.Maybe Prelude.Bool)
logPublishingOption_enabled = Lens.lens (\LogPublishingOption' {enabled} -> enabled) (\s@LogPublishingOption' {} a -> s {enabled = a} :: LogPublishingOption)

instance Data.FromJSON LogPublishingOption where
  parseJSON =
    Data.withObject
      "LogPublishingOption"
      ( \x ->
          LogPublishingOption'
            Prelude.<$> (x Data..:? "CloudWatchLogsLogGroupArn")
            Prelude.<*> (x Data..:? "Enabled")
      )

instance Prelude.Hashable LogPublishingOption where
  hashWithSalt _salt LogPublishingOption' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogsLogGroupArn
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData LogPublishingOption where
  rnf LogPublishingOption' {..} =
    Prelude.rnf cloudWatchLogsLogGroupArn `Prelude.seq`
      Prelude.rnf enabled

instance Data.ToJSON LogPublishingOption where
  toJSON LogPublishingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogsLogGroupArn" Data..=)
              Prelude.<$> cloudWatchLogsLogGroupArn,
            ("Enabled" Data..=) Prelude.<$> enabled
          ]
      )

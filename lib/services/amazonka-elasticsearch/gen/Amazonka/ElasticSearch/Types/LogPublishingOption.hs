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
-- Module      : Amazonka.ElasticSearch.Types.LogPublishingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.LogPublishingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  { cloudWatchLogsLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether given log publishing option is enabled or not.
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
-- 'cloudWatchLogsLogGroupArn', 'logPublishingOption_cloudWatchLogsLogGroupArn' - Undocumented member.
--
-- 'enabled', 'logPublishingOption_enabled' - Specifies whether given log publishing option is enabled or not.
newLogPublishingOption ::
  LogPublishingOption
newLogPublishingOption =
  LogPublishingOption'
    { cloudWatchLogsLogGroupArn =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | Undocumented member.
logPublishingOption_cloudWatchLogsLogGroupArn :: Lens.Lens' LogPublishingOption (Prelude.Maybe Prelude.Text)
logPublishingOption_cloudWatchLogsLogGroupArn = Lens.lens (\LogPublishingOption' {cloudWatchLogsLogGroupArn} -> cloudWatchLogsLogGroupArn) (\s@LogPublishingOption' {} a -> s {cloudWatchLogsLogGroupArn = a} :: LogPublishingOption)

-- | Specifies whether given log publishing option is enabled or not.
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

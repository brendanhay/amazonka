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
-- Module      : Amazonka.OpsWorks.Types.CloudWatchLogsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.CloudWatchLogsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorks.Types.CloudWatchLogsLogStream
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
-- /See:/ 'newCloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { -- | A list of configuration options for CloudWatch Logs.
    logStreams :: Prelude.Maybe [CloudWatchLogsLogStream],
    -- | Whether CloudWatch Logs is enabled for a layer.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreams', 'cloudWatchLogsConfiguration_logStreams' - A list of configuration options for CloudWatch Logs.
--
-- 'enabled', 'cloudWatchLogsConfiguration_enabled' - Whether CloudWatch Logs is enabled for a layer.
newCloudWatchLogsConfiguration ::
  CloudWatchLogsConfiguration
newCloudWatchLogsConfiguration =
  CloudWatchLogsConfiguration'
    { logStreams =
        Prelude.Nothing,
      enabled = Prelude.Nothing
    }

-- | A list of configuration options for CloudWatch Logs.
cloudWatchLogsConfiguration_logStreams :: Lens.Lens' CloudWatchLogsConfiguration (Prelude.Maybe [CloudWatchLogsLogStream])
cloudWatchLogsConfiguration_logStreams = Lens.lens (\CloudWatchLogsConfiguration' {logStreams} -> logStreams) (\s@CloudWatchLogsConfiguration' {} a -> s {logStreams = a} :: CloudWatchLogsConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Whether CloudWatch Logs is enabled for a layer.
cloudWatchLogsConfiguration_enabled :: Lens.Lens' CloudWatchLogsConfiguration (Prelude.Maybe Prelude.Bool)
cloudWatchLogsConfiguration_enabled = Lens.lens (\CloudWatchLogsConfiguration' {enabled} -> enabled) (\s@CloudWatchLogsConfiguration' {} a -> s {enabled = a} :: CloudWatchLogsConfiguration)

instance Core.FromJSON CloudWatchLogsConfiguration where
  parseJSON =
    Core.withObject
      "CloudWatchLogsConfiguration"
      ( \x ->
          CloudWatchLogsConfiguration'
            Prelude.<$> (x Core..:? "LogStreams" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Enabled")
      )

instance Prelude.Hashable CloudWatchLogsConfiguration where
  hashWithSalt _salt CloudWatchLogsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` logStreams
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData CloudWatchLogsConfiguration where
  rnf CloudWatchLogsConfiguration' {..} =
    Prelude.rnf logStreams
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToJSON CloudWatchLogsConfiguration where
  toJSON CloudWatchLogsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LogStreams" Core..=) Prelude.<$> logStreams,
            ("Enabled" Core..=) Prelude.<$> enabled
          ]
      )

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.CloudWatchLogsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.CloudWatchLogsLogStream
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
-- /See:/ 'newCloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { -- | Whether CloudWatch Logs is enabled for a layer.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of configuration options for CloudWatch Logs.
    logStreams :: Prelude.Maybe [CloudWatchLogsLogStream]
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
-- 'enabled', 'cloudWatchLogsConfiguration_enabled' - Whether CloudWatch Logs is enabled for a layer.
--
-- 'logStreams', 'cloudWatchLogsConfiguration_logStreams' - A list of configuration options for CloudWatch Logs.
newCloudWatchLogsConfiguration ::
  CloudWatchLogsConfiguration
newCloudWatchLogsConfiguration =
  CloudWatchLogsConfiguration'
    { enabled =
        Prelude.Nothing,
      logStreams = Prelude.Nothing
    }

-- | Whether CloudWatch Logs is enabled for a layer.
cloudWatchLogsConfiguration_enabled :: Lens.Lens' CloudWatchLogsConfiguration (Prelude.Maybe Prelude.Bool)
cloudWatchLogsConfiguration_enabled = Lens.lens (\CloudWatchLogsConfiguration' {enabled} -> enabled) (\s@CloudWatchLogsConfiguration' {} a -> s {enabled = a} :: CloudWatchLogsConfiguration)

-- | A list of configuration options for CloudWatch Logs.
cloudWatchLogsConfiguration_logStreams :: Lens.Lens' CloudWatchLogsConfiguration (Prelude.Maybe [CloudWatchLogsLogStream])
cloudWatchLogsConfiguration_logStreams = Lens.lens (\CloudWatchLogsConfiguration' {logStreams} -> logStreams) (\s@CloudWatchLogsConfiguration' {} a -> s {logStreams = a} :: CloudWatchLogsConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CloudWatchLogsConfiguration where
  parseJSON =
    Data.withObject
      "CloudWatchLogsConfiguration"
      ( \x ->
          CloudWatchLogsConfiguration'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "LogStreams" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CloudWatchLogsConfiguration where
  hashWithSalt _salt CloudWatchLogsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` logStreams

instance Prelude.NFData CloudWatchLogsConfiguration where
  rnf CloudWatchLogsConfiguration' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf logStreams

instance Data.ToJSON CloudWatchLogsConfiguration where
  toJSON CloudWatchLogsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("LogStreams" Data..=) Prelude.<$> logStreams
          ]
      )

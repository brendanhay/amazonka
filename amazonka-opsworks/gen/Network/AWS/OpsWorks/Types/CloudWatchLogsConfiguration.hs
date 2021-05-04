{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsConfiguration where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
import qualified Network.AWS.Prelude as Prelude

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
-- /See:/ 'newCloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { -- | Whether CloudWatch Logs is enabled for a layer.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of configuration options for CloudWatch Logs.
    logStreams :: Prelude.Maybe [CloudWatchLogsLogStream]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
cloudWatchLogsConfiguration_logStreams = Lens.lens (\CloudWatchLogsConfiguration' {logStreams} -> logStreams) (\s@CloudWatchLogsConfiguration' {} a -> s {logStreams = a} :: CloudWatchLogsConfiguration) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON CloudWatchLogsConfiguration where
  parseJSON =
    Prelude.withObject
      "CloudWatchLogsConfiguration"
      ( \x ->
          CloudWatchLogsConfiguration'
            Prelude.<$> (x Prelude..:? "Enabled")
            Prelude.<*> ( x Prelude..:? "LogStreams"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CloudWatchLogsConfiguration

instance Prelude.NFData CloudWatchLogsConfiguration

instance Prelude.ToJSON CloudWatchLogsConfiguration where
  toJSON CloudWatchLogsConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("LogStreams" Prelude..=) Prelude.<$> logStreams
          ]
      )

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
-- Module      : Amazonka.Evidently.Types.ProjectDataDeliveryConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ProjectDataDeliveryConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.CloudWatchLogsDestinationConfig
import Amazonka.Evidently.Types.S3DestinationConfig
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about where Evidently is to store
-- evaluation events for longer term storage.
--
-- /See:/ 'newProjectDataDeliveryConfig' smart constructor.
data ProjectDataDeliveryConfig = ProjectDataDeliveryConfig'
  { -- | If the project stores evaluation events in CloudWatch Logs, this
    -- structure stores the log group name.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsDestinationConfig,
    -- | If the project stores evaluation events in an Amazon S3 bucket, this
    -- structure stores the bucket name and bucket prefix.
    s3Destination :: Prelude.Maybe S3DestinationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectDataDeliveryConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'projectDataDeliveryConfig_cloudWatchLogs' - If the project stores evaluation events in CloudWatch Logs, this
-- structure stores the log group name.
--
-- 's3Destination', 'projectDataDeliveryConfig_s3Destination' - If the project stores evaluation events in an Amazon S3 bucket, this
-- structure stores the bucket name and bucket prefix.
newProjectDataDeliveryConfig ::
  ProjectDataDeliveryConfig
newProjectDataDeliveryConfig =
  ProjectDataDeliveryConfig'
    { cloudWatchLogs =
        Prelude.Nothing,
      s3Destination = Prelude.Nothing
    }

-- | If the project stores evaluation events in CloudWatch Logs, this
-- structure stores the log group name.
projectDataDeliveryConfig_cloudWatchLogs :: Lens.Lens' ProjectDataDeliveryConfig (Prelude.Maybe CloudWatchLogsDestinationConfig)
projectDataDeliveryConfig_cloudWatchLogs = Lens.lens (\ProjectDataDeliveryConfig' {cloudWatchLogs} -> cloudWatchLogs) (\s@ProjectDataDeliveryConfig' {} a -> s {cloudWatchLogs = a} :: ProjectDataDeliveryConfig)

-- | If the project stores evaluation events in an Amazon S3 bucket, this
-- structure stores the bucket name and bucket prefix.
projectDataDeliveryConfig_s3Destination :: Lens.Lens' ProjectDataDeliveryConfig (Prelude.Maybe S3DestinationConfig)
projectDataDeliveryConfig_s3Destination = Lens.lens (\ProjectDataDeliveryConfig' {s3Destination} -> s3Destination) (\s@ProjectDataDeliveryConfig' {} a -> s {s3Destination = a} :: ProjectDataDeliveryConfig)

instance Prelude.Hashable ProjectDataDeliveryConfig where
  hashWithSalt _salt ProjectDataDeliveryConfig' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` s3Destination

instance Prelude.NFData ProjectDataDeliveryConfig where
  rnf ProjectDataDeliveryConfig' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf s3Destination

instance Data.ToJSON ProjectDataDeliveryConfig where
  toJSON ProjectDataDeliveryConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogs" Data..=)
              Prelude.<$> cloudWatchLogs,
            ("s3Destination" Data..=) Prelude.<$> s3Destination
          ]
      )

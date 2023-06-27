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
-- Module      : Amazonka.Evidently.Types.ProjectDataDelivery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ProjectDataDelivery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.CloudWatchLogsDestination
import Amazonka.Evidently.Types.S3Destination
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about where Evidently is to store
-- evaluation events for longer term storage.
--
-- /See:/ 'newProjectDataDelivery' smart constructor.
data ProjectDataDelivery = ProjectDataDelivery'
  { -- | If the project stores evaluation events in CloudWatch Logs, this
    -- structure stores the log group name.
    cloudWatchLogs :: Prelude.Maybe CloudWatchLogsDestination,
    -- | If the project stores evaluation events in an Amazon S3 bucket, this
    -- structure stores the bucket name and bucket prefix.
    s3Destination :: Prelude.Maybe S3Destination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectDataDelivery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogs', 'projectDataDelivery_cloudWatchLogs' - If the project stores evaluation events in CloudWatch Logs, this
-- structure stores the log group name.
--
-- 's3Destination', 'projectDataDelivery_s3Destination' - If the project stores evaluation events in an Amazon S3 bucket, this
-- structure stores the bucket name and bucket prefix.
newProjectDataDelivery ::
  ProjectDataDelivery
newProjectDataDelivery =
  ProjectDataDelivery'
    { cloudWatchLogs =
        Prelude.Nothing,
      s3Destination = Prelude.Nothing
    }

-- | If the project stores evaluation events in CloudWatch Logs, this
-- structure stores the log group name.
projectDataDelivery_cloudWatchLogs :: Lens.Lens' ProjectDataDelivery (Prelude.Maybe CloudWatchLogsDestination)
projectDataDelivery_cloudWatchLogs = Lens.lens (\ProjectDataDelivery' {cloudWatchLogs} -> cloudWatchLogs) (\s@ProjectDataDelivery' {} a -> s {cloudWatchLogs = a} :: ProjectDataDelivery)

-- | If the project stores evaluation events in an Amazon S3 bucket, this
-- structure stores the bucket name and bucket prefix.
projectDataDelivery_s3Destination :: Lens.Lens' ProjectDataDelivery (Prelude.Maybe S3Destination)
projectDataDelivery_s3Destination = Lens.lens (\ProjectDataDelivery' {s3Destination} -> s3Destination) (\s@ProjectDataDelivery' {} a -> s {s3Destination = a} :: ProjectDataDelivery)

instance Data.FromJSON ProjectDataDelivery where
  parseJSON =
    Data.withObject
      "ProjectDataDelivery"
      ( \x ->
          ProjectDataDelivery'
            Prelude.<$> (x Data..:? "cloudWatchLogs")
            Prelude.<*> (x Data..:? "s3Destination")
      )

instance Prelude.Hashable ProjectDataDelivery where
  hashWithSalt _salt ProjectDataDelivery' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogs
      `Prelude.hashWithSalt` s3Destination

instance Prelude.NFData ProjectDataDelivery where
  rnf ProjectDataDelivery' {..} =
    Prelude.rnf cloudWatchLogs
      `Prelude.seq` Prelude.rnf s3Destination

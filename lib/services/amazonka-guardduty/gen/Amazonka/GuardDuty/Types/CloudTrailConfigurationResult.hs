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
-- Module      : Amazonka.GuardDuty.Types.CloudTrailConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CloudTrailConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourceStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the status of CloudTrail as a data source for
-- the detector.
--
-- /See:/ 'newCloudTrailConfigurationResult' smart constructor.
data CloudTrailConfigurationResult = CloudTrailConfigurationResult'
  { -- | Describes whether CloudTrail is enabled as a data source for the
    -- detector.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudTrailConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'cloudTrailConfigurationResult_status' - Describes whether CloudTrail is enabled as a data source for the
-- detector.
newCloudTrailConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  CloudTrailConfigurationResult
newCloudTrailConfigurationResult pStatus_ =
  CloudTrailConfigurationResult' {status = pStatus_}

-- | Describes whether CloudTrail is enabled as a data source for the
-- detector.
cloudTrailConfigurationResult_status :: Lens.Lens' CloudTrailConfigurationResult DataSourceStatus
cloudTrailConfigurationResult_status = Lens.lens (\CloudTrailConfigurationResult' {status} -> status) (\s@CloudTrailConfigurationResult' {} a -> s {status = a} :: CloudTrailConfigurationResult)

instance Data.FromJSON CloudTrailConfigurationResult where
  parseJSON =
    Data.withObject
      "CloudTrailConfigurationResult"
      ( \x ->
          CloudTrailConfigurationResult'
            Prelude.<$> (x Data..: "status")
      )

instance
  Prelude.Hashable
    CloudTrailConfigurationResult
  where
  hashWithSalt _salt CloudTrailConfigurationResult' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData CloudTrailConfigurationResult where
  rnf CloudTrailConfigurationResult' {..} =
    Prelude.rnf status

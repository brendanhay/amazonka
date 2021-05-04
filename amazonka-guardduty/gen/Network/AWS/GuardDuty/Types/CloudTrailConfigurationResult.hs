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
-- Module      : Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on the status of CloudTrail as a data source for
-- the detector.
--
-- /See:/ 'newCloudTrailConfigurationResult' smart constructor.
data CloudTrailConfigurationResult = CloudTrailConfigurationResult'
  { -- | Describes whether CloudTrail is enabled as a data source for the
    -- detector.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromJSON
    CloudTrailConfigurationResult
  where
  parseJSON =
    Prelude.withObject
      "CloudTrailConfigurationResult"
      ( \x ->
          CloudTrailConfigurationResult'
            Prelude.<$> (x Prelude..: "status")
      )

instance
  Prelude.Hashable
    CloudTrailConfigurationResult

instance Prelude.NFData CloudTrailConfigurationResult

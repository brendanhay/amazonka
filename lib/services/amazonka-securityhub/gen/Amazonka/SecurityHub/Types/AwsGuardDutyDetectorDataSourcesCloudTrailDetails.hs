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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesCloudTrailDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesCloudTrailDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of CloudTrail as a
-- data source for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorDataSourcesCloudTrailDetails' smart constructor.
data AwsGuardDutyDetectorDataSourcesCloudTrailDetails = AwsGuardDutyDetectorDataSourcesCloudTrailDetails'
  { -- | Specifies whether CloudTrail is activated as a data source for the
    -- detector.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDataSourcesCloudTrailDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsGuardDutyDetectorDataSourcesCloudTrailDetails_status' - Specifies whether CloudTrail is activated as a data source for the
-- detector.
newAwsGuardDutyDetectorDataSourcesCloudTrailDetails ::
  AwsGuardDutyDetectorDataSourcesCloudTrailDetails
newAwsGuardDutyDetectorDataSourcesCloudTrailDetails =
  AwsGuardDutyDetectorDataSourcesCloudTrailDetails'
    { status =
        Prelude.Nothing
    }

-- | Specifies whether CloudTrail is activated as a data source for the
-- detector.
awsGuardDutyDetectorDataSourcesCloudTrailDetails_status :: Lens.Lens' AwsGuardDutyDetectorDataSourcesCloudTrailDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDataSourcesCloudTrailDetails_status = Lens.lens (\AwsGuardDutyDetectorDataSourcesCloudTrailDetails' {status} -> status) (\s@AwsGuardDutyDetectorDataSourcesCloudTrailDetails' {} a -> s {status = a} :: AwsGuardDutyDetectorDataSourcesCloudTrailDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDataSourcesCloudTrailDetails"
      ( \x ->
          AwsGuardDutyDetectorDataSourcesCloudTrailDetails'
            Prelude.<$> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails
  where
  rnf
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails' {..} =
      Prelude.rnf status

instance
  Data.ToJSON
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails
  where
  toJSON
    AwsGuardDutyDetectorDataSourcesCloudTrailDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Status" Data..=) Prelude.<$> status]
        )

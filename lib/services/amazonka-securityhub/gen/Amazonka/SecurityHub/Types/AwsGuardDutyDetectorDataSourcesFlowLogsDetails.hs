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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesFlowLogsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesFlowLogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of VPC Flow Logs as a
-- data source for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorDataSourcesFlowLogsDetails' smart constructor.
data AwsGuardDutyDetectorDataSourcesFlowLogsDetails = AwsGuardDutyDetectorDataSourcesFlowLogsDetails'
  { -- | Describes whether VPC Flow Logs are activated as a data source for the
    -- detector.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDataSourcesFlowLogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsGuardDutyDetectorDataSourcesFlowLogsDetails_status' - Describes whether VPC Flow Logs are activated as a data source for the
-- detector.
newAwsGuardDutyDetectorDataSourcesFlowLogsDetails ::
  AwsGuardDutyDetectorDataSourcesFlowLogsDetails
newAwsGuardDutyDetectorDataSourcesFlowLogsDetails =
  AwsGuardDutyDetectorDataSourcesFlowLogsDetails'
    { status =
        Prelude.Nothing
    }

-- | Describes whether VPC Flow Logs are activated as a data source for the
-- detector.
awsGuardDutyDetectorDataSourcesFlowLogsDetails_status :: Lens.Lens' AwsGuardDutyDetectorDataSourcesFlowLogsDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDataSourcesFlowLogsDetails_status = Lens.lens (\AwsGuardDutyDetectorDataSourcesFlowLogsDetails' {status} -> status) (\s@AwsGuardDutyDetectorDataSourcesFlowLogsDetails' {} a -> s {status = a} :: AwsGuardDutyDetectorDataSourcesFlowLogsDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDataSourcesFlowLogsDetails"
      ( \x ->
          AwsGuardDutyDetectorDataSourcesFlowLogsDetails'
            Prelude.<$> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails
  where
  rnf
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails' {..} =
      Prelude.rnf status

instance
  Data.ToJSON
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails
  where
  toJSON
    AwsGuardDutyDetectorDataSourcesFlowLogsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Status" Data..=) Prelude.<$> status]
        )

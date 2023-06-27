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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDnsLogsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDnsLogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of DNS logs as a data
-- source for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorDataSourcesDnsLogsDetails' smart constructor.
data AwsGuardDutyDetectorDataSourcesDnsLogsDetails = AwsGuardDutyDetectorDataSourcesDnsLogsDetails'
  { -- | Describes whether DNS logs is enabled as a data source for the detector.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDataSourcesDnsLogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsGuardDutyDetectorDataSourcesDnsLogsDetails_status' - Describes whether DNS logs is enabled as a data source for the detector.
newAwsGuardDutyDetectorDataSourcesDnsLogsDetails ::
  AwsGuardDutyDetectorDataSourcesDnsLogsDetails
newAwsGuardDutyDetectorDataSourcesDnsLogsDetails =
  AwsGuardDutyDetectorDataSourcesDnsLogsDetails'
    { status =
        Prelude.Nothing
    }

-- | Describes whether DNS logs is enabled as a data source for the detector.
awsGuardDutyDetectorDataSourcesDnsLogsDetails_status :: Lens.Lens' AwsGuardDutyDetectorDataSourcesDnsLogsDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDataSourcesDnsLogsDetails_status = Lens.lens (\AwsGuardDutyDetectorDataSourcesDnsLogsDetails' {status} -> status) (\s@AwsGuardDutyDetectorDataSourcesDnsLogsDetails' {} a -> s {status = a} :: AwsGuardDutyDetectorDataSourcesDnsLogsDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDataSourcesDnsLogsDetails"
      ( \x ->
          AwsGuardDutyDetectorDataSourcesDnsLogsDetails'
            Prelude.<$> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails
  where
  rnf
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails' {..} =
      Prelude.rnf status

instance
  Data.ToJSON
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails
  where
  toJSON
    AwsGuardDutyDetectorDataSourcesDnsLogsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Status" Data..=) Prelude.<$> status]
        )

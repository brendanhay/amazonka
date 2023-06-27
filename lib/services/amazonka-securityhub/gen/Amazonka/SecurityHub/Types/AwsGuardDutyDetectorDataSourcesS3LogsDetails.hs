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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesS3LogsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesS3LogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of S3 data event logs
-- as a data source for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorDataSourcesS3LogsDetails' smart constructor.
data AwsGuardDutyDetectorDataSourcesS3LogsDetails = AwsGuardDutyDetectorDataSourcesS3LogsDetails'
  { -- | A value that describes whether S3 data event logs are automatically
    -- enabled for new members of an organization.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDataSourcesS3LogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsGuardDutyDetectorDataSourcesS3LogsDetails_status' - A value that describes whether S3 data event logs are automatically
-- enabled for new members of an organization.
newAwsGuardDutyDetectorDataSourcesS3LogsDetails ::
  AwsGuardDutyDetectorDataSourcesS3LogsDetails
newAwsGuardDutyDetectorDataSourcesS3LogsDetails =
  AwsGuardDutyDetectorDataSourcesS3LogsDetails'
    { status =
        Prelude.Nothing
    }

-- | A value that describes whether S3 data event logs are automatically
-- enabled for new members of an organization.
awsGuardDutyDetectorDataSourcesS3LogsDetails_status :: Lens.Lens' AwsGuardDutyDetectorDataSourcesS3LogsDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDataSourcesS3LogsDetails_status = Lens.lens (\AwsGuardDutyDetectorDataSourcesS3LogsDetails' {status} -> status) (\s@AwsGuardDutyDetectorDataSourcesS3LogsDetails' {} a -> s {status = a} :: AwsGuardDutyDetectorDataSourcesS3LogsDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorDataSourcesS3LogsDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDataSourcesS3LogsDetails"
      ( \x ->
          AwsGuardDutyDetectorDataSourcesS3LogsDetails'
            Prelude.<$> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorDataSourcesS3LogsDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorDataSourcesS3LogsDetails' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsGuardDutyDetectorDataSourcesS3LogsDetails
  where
  rnf AwsGuardDutyDetectorDataSourcesS3LogsDetails' {..} =
    Prelude.rnf status

instance
  Data.ToJSON
    AwsGuardDutyDetectorDataSourcesS3LogsDetails
  where
  toJSON
    AwsGuardDutyDetectorDataSourcesS3LogsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Status" Data..=) Prelude.<$> status]
        )

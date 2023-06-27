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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of Kubernetes audit
-- logs as a data source for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails' smart constructor.
data AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails = AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails'
  { -- | Describes whether Kubernetes audit logs are activated as a data source
    -- for the detector.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails_status' - Describes whether Kubernetes audit logs are activated as a data source
-- for the detector.
newAwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails ::
  AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
newAwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails =
  AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails'
    { status =
        Prelude.Nothing
    }

-- | Describes whether Kubernetes audit logs are activated as a data source
-- for the detector.
awsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails_status :: Lens.Lens' AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails_status = Lens.lens (\AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails' {status} -> status) (\s@AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails' {} a -> s {status = a} :: AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails"
      ( \x ->
          AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails'
            Prelude.<$> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
  where
  rnf
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails' {..} =
      Prelude.rnf status

instance
  Data.ToJSON
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
  where
  toJSON
    AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Status" Data..=) Prelude.<$> status]
        )

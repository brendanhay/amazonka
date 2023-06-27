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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails

-- | An object that contains information on the status of Kubernetes data
-- sources for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorDataSourcesKubernetesDetails' smart constructor.
data AwsGuardDutyDetectorDataSourcesKubernetesDetails = AwsGuardDutyDetectorDataSourcesKubernetesDetails'
  { -- | Describes whether Kubernetes audit logs are activated as a data source
    -- for the detector.
    auditLogs :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDataSourcesKubernetesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogs', 'awsGuardDutyDetectorDataSourcesKubernetesDetails_auditLogs' - Describes whether Kubernetes audit logs are activated as a data source
-- for the detector.
newAwsGuardDutyDetectorDataSourcesKubernetesDetails ::
  AwsGuardDutyDetectorDataSourcesKubernetesDetails
newAwsGuardDutyDetectorDataSourcesKubernetesDetails =
  AwsGuardDutyDetectorDataSourcesKubernetesDetails'
    { auditLogs =
        Prelude.Nothing
    }

-- | Describes whether Kubernetes audit logs are activated as a data source
-- for the detector.
awsGuardDutyDetectorDataSourcesKubernetesDetails_auditLogs :: Lens.Lens' AwsGuardDutyDetectorDataSourcesKubernetesDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesKubernetesAuditLogsDetails)
awsGuardDutyDetectorDataSourcesKubernetesDetails_auditLogs = Lens.lens (\AwsGuardDutyDetectorDataSourcesKubernetesDetails' {auditLogs} -> auditLogs) (\s@AwsGuardDutyDetectorDataSourcesKubernetesDetails' {} a -> s {auditLogs = a} :: AwsGuardDutyDetectorDataSourcesKubernetesDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorDataSourcesKubernetesDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDataSourcesKubernetesDetails"
      ( \x ->
          AwsGuardDutyDetectorDataSourcesKubernetesDetails'
            Prelude.<$> (x Data..:? "AuditLogs")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorDataSourcesKubernetesDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorDataSourcesKubernetesDetails' {..} =
      _salt `Prelude.hashWithSalt` auditLogs

instance
  Prelude.NFData
    AwsGuardDutyDetectorDataSourcesKubernetesDetails
  where
  rnf
    AwsGuardDutyDetectorDataSourcesKubernetesDetails' {..} =
      Prelude.rnf auditLogs

instance
  Data.ToJSON
    AwsGuardDutyDetectorDataSourcesKubernetesDetails
  where
  toJSON
    AwsGuardDutyDetectorDataSourcesKubernetesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("AuditLogs" Data..=) Prelude.<$> auditLogs]
        )

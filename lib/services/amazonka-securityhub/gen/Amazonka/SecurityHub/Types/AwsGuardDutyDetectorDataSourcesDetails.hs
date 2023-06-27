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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesCloudTrailDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDnsLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesFlowLogsDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesKubernetesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesS3LogsDetails

-- | Describes which data sources are activated for the detector.
--
-- /See:/ 'newAwsGuardDutyDetectorDataSourcesDetails' smart constructor.
data AwsGuardDutyDetectorDataSourcesDetails = AwsGuardDutyDetectorDataSourcesDetails'
  { -- | An object that contains information on the status of CloudTrail as a
    -- data source for the detector.
    cloudTrail :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesCloudTrailDetails,
    -- | An object that contains information on the status of DNS logs as a data
    -- source for the detector.
    dnsLogs :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesDnsLogsDetails,
    -- | An object that contains information on the status of VPC Flow Logs as a
    -- data source for the detector.
    flowLogs :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesFlowLogsDetails,
    -- | An object that contains information on the status of Kubernetes data
    -- sources for the detector.
    kubernetes :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesKubernetesDetails,
    -- | An object that contains information on the status of Malware Protection
    -- as a data source for the detector.
    malwareProtection :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails,
    -- | An object that contains information on the status of S3 Data event logs
    -- as a data source for the detector.
    s3Logs :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesS3LogsDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDataSourcesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudTrail', 'awsGuardDutyDetectorDataSourcesDetails_cloudTrail' - An object that contains information on the status of CloudTrail as a
-- data source for the detector.
--
-- 'dnsLogs', 'awsGuardDutyDetectorDataSourcesDetails_dnsLogs' - An object that contains information on the status of DNS logs as a data
-- source for the detector.
--
-- 'flowLogs', 'awsGuardDutyDetectorDataSourcesDetails_flowLogs' - An object that contains information on the status of VPC Flow Logs as a
-- data source for the detector.
--
-- 'kubernetes', 'awsGuardDutyDetectorDataSourcesDetails_kubernetes' - An object that contains information on the status of Kubernetes data
-- sources for the detector.
--
-- 'malwareProtection', 'awsGuardDutyDetectorDataSourcesDetails_malwareProtection' - An object that contains information on the status of Malware Protection
-- as a data source for the detector.
--
-- 's3Logs', 'awsGuardDutyDetectorDataSourcesDetails_s3Logs' - An object that contains information on the status of S3 Data event logs
-- as a data source for the detector.
newAwsGuardDutyDetectorDataSourcesDetails ::
  AwsGuardDutyDetectorDataSourcesDetails
newAwsGuardDutyDetectorDataSourcesDetails =
  AwsGuardDutyDetectorDataSourcesDetails'
    { cloudTrail =
        Prelude.Nothing,
      dnsLogs = Prelude.Nothing,
      flowLogs = Prelude.Nothing,
      kubernetes = Prelude.Nothing,
      malwareProtection = Prelude.Nothing,
      s3Logs = Prelude.Nothing
    }

-- | An object that contains information on the status of CloudTrail as a
-- data source for the detector.
awsGuardDutyDetectorDataSourcesDetails_cloudTrail :: Lens.Lens' AwsGuardDutyDetectorDataSourcesDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesCloudTrailDetails)
awsGuardDutyDetectorDataSourcesDetails_cloudTrail = Lens.lens (\AwsGuardDutyDetectorDataSourcesDetails' {cloudTrail} -> cloudTrail) (\s@AwsGuardDutyDetectorDataSourcesDetails' {} a -> s {cloudTrail = a} :: AwsGuardDutyDetectorDataSourcesDetails)

-- | An object that contains information on the status of DNS logs as a data
-- source for the detector.
awsGuardDutyDetectorDataSourcesDetails_dnsLogs :: Lens.Lens' AwsGuardDutyDetectorDataSourcesDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesDnsLogsDetails)
awsGuardDutyDetectorDataSourcesDetails_dnsLogs = Lens.lens (\AwsGuardDutyDetectorDataSourcesDetails' {dnsLogs} -> dnsLogs) (\s@AwsGuardDutyDetectorDataSourcesDetails' {} a -> s {dnsLogs = a} :: AwsGuardDutyDetectorDataSourcesDetails)

-- | An object that contains information on the status of VPC Flow Logs as a
-- data source for the detector.
awsGuardDutyDetectorDataSourcesDetails_flowLogs :: Lens.Lens' AwsGuardDutyDetectorDataSourcesDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesFlowLogsDetails)
awsGuardDutyDetectorDataSourcesDetails_flowLogs = Lens.lens (\AwsGuardDutyDetectorDataSourcesDetails' {flowLogs} -> flowLogs) (\s@AwsGuardDutyDetectorDataSourcesDetails' {} a -> s {flowLogs = a} :: AwsGuardDutyDetectorDataSourcesDetails)

-- | An object that contains information on the status of Kubernetes data
-- sources for the detector.
awsGuardDutyDetectorDataSourcesDetails_kubernetes :: Lens.Lens' AwsGuardDutyDetectorDataSourcesDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesKubernetesDetails)
awsGuardDutyDetectorDataSourcesDetails_kubernetes = Lens.lens (\AwsGuardDutyDetectorDataSourcesDetails' {kubernetes} -> kubernetes) (\s@AwsGuardDutyDetectorDataSourcesDetails' {} a -> s {kubernetes = a} :: AwsGuardDutyDetectorDataSourcesDetails)

-- | An object that contains information on the status of Malware Protection
-- as a data source for the detector.
awsGuardDutyDetectorDataSourcesDetails_malwareProtection :: Lens.Lens' AwsGuardDutyDetectorDataSourcesDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesMalwareProtectionDetails)
awsGuardDutyDetectorDataSourcesDetails_malwareProtection = Lens.lens (\AwsGuardDutyDetectorDataSourcesDetails' {malwareProtection} -> malwareProtection) (\s@AwsGuardDutyDetectorDataSourcesDetails' {} a -> s {malwareProtection = a} :: AwsGuardDutyDetectorDataSourcesDetails)

-- | An object that contains information on the status of S3 Data event logs
-- as a data source for the detector.
awsGuardDutyDetectorDataSourcesDetails_s3Logs :: Lens.Lens' AwsGuardDutyDetectorDataSourcesDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesS3LogsDetails)
awsGuardDutyDetectorDataSourcesDetails_s3Logs = Lens.lens (\AwsGuardDutyDetectorDataSourcesDetails' {s3Logs} -> s3Logs) (\s@AwsGuardDutyDetectorDataSourcesDetails' {} a -> s {s3Logs = a} :: AwsGuardDutyDetectorDataSourcesDetails)

instance
  Data.FromJSON
    AwsGuardDutyDetectorDataSourcesDetails
  where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDataSourcesDetails"
      ( \x ->
          AwsGuardDutyDetectorDataSourcesDetails'
            Prelude.<$> (x Data..:? "CloudTrail")
            Prelude.<*> (x Data..:? "DnsLogs")
            Prelude.<*> (x Data..:? "FlowLogs")
            Prelude.<*> (x Data..:? "Kubernetes")
            Prelude.<*> (x Data..:? "MalwareProtection")
            Prelude.<*> (x Data..:? "S3Logs")
      )

instance
  Prelude.Hashable
    AwsGuardDutyDetectorDataSourcesDetails
  where
  hashWithSalt
    _salt
    AwsGuardDutyDetectorDataSourcesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` cloudTrail
        `Prelude.hashWithSalt` dnsLogs
        `Prelude.hashWithSalt` flowLogs
        `Prelude.hashWithSalt` kubernetes
        `Prelude.hashWithSalt` malwareProtection
        `Prelude.hashWithSalt` s3Logs

instance
  Prelude.NFData
    AwsGuardDutyDetectorDataSourcesDetails
  where
  rnf AwsGuardDutyDetectorDataSourcesDetails' {..} =
    Prelude.rnf cloudTrail
      `Prelude.seq` Prelude.rnf dnsLogs
      `Prelude.seq` Prelude.rnf flowLogs
      `Prelude.seq` Prelude.rnf kubernetes
      `Prelude.seq` Prelude.rnf malwareProtection
      `Prelude.seq` Prelude.rnf s3Logs

instance
  Data.ToJSON
    AwsGuardDutyDetectorDataSourcesDetails
  where
  toJSON AwsGuardDutyDetectorDataSourcesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudTrail" Data..=) Prelude.<$> cloudTrail,
            ("DnsLogs" Data..=) Prelude.<$> dnsLogs,
            ("FlowLogs" Data..=) Prelude.<$> flowLogs,
            ("Kubernetes" Data..=) Prelude.<$> kubernetes,
            ("MalwareProtection" Data..=)
              Prelude.<$> malwareProtection,
            ("S3Logs" Data..=) Prelude.<$> s3Logs
          ]
      )

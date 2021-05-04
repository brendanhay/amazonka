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
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult where

import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains information on which data sources are
-- automatically enabled for new members within the organization.
--
-- /See:/ 'newOrganizationDataSourceConfigurationsResult' smart constructor.
data OrganizationDataSourceConfigurationsResult = OrganizationDataSourceConfigurationsResult'
  { -- | Describes whether S3 data event logs are enabled as a data source.
    s3Logs :: OrganizationS3LogsConfigurationResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationDataSourceConfigurationsResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Logs', 'organizationDataSourceConfigurationsResult_s3Logs' - Describes whether S3 data event logs are enabled as a data source.
newOrganizationDataSourceConfigurationsResult ::
  -- | 's3Logs'
  OrganizationS3LogsConfigurationResult ->
  OrganizationDataSourceConfigurationsResult
newOrganizationDataSourceConfigurationsResult
  pS3Logs_ =
    OrganizationDataSourceConfigurationsResult'
      { s3Logs =
          pS3Logs_
      }

-- | Describes whether S3 data event logs are enabled as a data source.
organizationDataSourceConfigurationsResult_s3Logs :: Lens.Lens' OrganizationDataSourceConfigurationsResult OrganizationS3LogsConfigurationResult
organizationDataSourceConfigurationsResult_s3Logs = Lens.lens (\OrganizationDataSourceConfigurationsResult' {s3Logs} -> s3Logs) (\s@OrganizationDataSourceConfigurationsResult' {} a -> s {s3Logs = a} :: OrganizationDataSourceConfigurationsResult)

instance
  Prelude.FromJSON
    OrganizationDataSourceConfigurationsResult
  where
  parseJSON =
    Prelude.withObject
      "OrganizationDataSourceConfigurationsResult"
      ( \x ->
          OrganizationDataSourceConfigurationsResult'
            Prelude.<$> (x Prelude..: "s3Logs")
      )

instance
  Prelude.Hashable
    OrganizationDataSourceConfigurationsResult

instance
  Prelude.NFData
    OrganizationDataSourceConfigurationsResult

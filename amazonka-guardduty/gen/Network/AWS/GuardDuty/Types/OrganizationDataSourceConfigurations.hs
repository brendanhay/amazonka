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
-- Module      : Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations where

import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains information on which data sources will be
-- configured to be automatically enabled for new members within the
-- organization.
--
-- /See:/ 'newOrganizationDataSourceConfigurations' smart constructor.
data OrganizationDataSourceConfigurations = OrganizationDataSourceConfigurations'
  { -- | Describes whether S3 data event logs are enabled for new members of the
    -- organization.
    s3Logs :: Prelude.Maybe OrganizationS3LogsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationDataSourceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Logs', 'organizationDataSourceConfigurations_s3Logs' - Describes whether S3 data event logs are enabled for new members of the
-- organization.
newOrganizationDataSourceConfigurations ::
  OrganizationDataSourceConfigurations
newOrganizationDataSourceConfigurations =
  OrganizationDataSourceConfigurations'
    { s3Logs =
        Prelude.Nothing
    }

-- | Describes whether S3 data event logs are enabled for new members of the
-- organization.
organizationDataSourceConfigurations_s3Logs :: Lens.Lens' OrganizationDataSourceConfigurations (Prelude.Maybe OrganizationS3LogsConfiguration)
organizationDataSourceConfigurations_s3Logs = Lens.lens (\OrganizationDataSourceConfigurations' {s3Logs} -> s3Logs) (\s@OrganizationDataSourceConfigurations' {} a -> s {s3Logs = a} :: OrganizationDataSourceConfigurations)

instance
  Prelude.Hashable
    OrganizationDataSourceConfigurations

instance
  Prelude.NFData
    OrganizationDataSourceConfigurations

instance
  Prelude.ToJSON
    OrganizationDataSourceConfigurations
  where
  toJSON OrganizationDataSourceConfigurations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("s3Logs" Prelude..=) Prelude.<$> s3Logs]
      )

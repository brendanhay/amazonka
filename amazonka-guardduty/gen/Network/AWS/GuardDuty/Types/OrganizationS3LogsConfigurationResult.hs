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
-- Module      : Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The current configuration of S3 data event logs as a data source for the
-- organization.
--
-- /See:/ 'newOrganizationS3LogsConfigurationResult' smart constructor.
data OrganizationS3LogsConfigurationResult = OrganizationS3LogsConfigurationResult'
  { -- | A value that describes whether S3 data event logs are automatically
    -- enabled for new members of the organization.
    autoEnable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OrganizationS3LogsConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationS3LogsConfigurationResult_autoEnable' - A value that describes whether S3 data event logs are automatically
-- enabled for new members of the organization.
newOrganizationS3LogsConfigurationResult ::
  -- | 'autoEnable'
  Prelude.Bool ->
  OrganizationS3LogsConfigurationResult
newOrganizationS3LogsConfigurationResult pAutoEnable_ =
  OrganizationS3LogsConfigurationResult'
    { autoEnable =
        pAutoEnable_
    }

-- | A value that describes whether S3 data event logs are automatically
-- enabled for new members of the organization.
organizationS3LogsConfigurationResult_autoEnable :: Lens.Lens' OrganizationS3LogsConfigurationResult Prelude.Bool
organizationS3LogsConfigurationResult_autoEnable = Lens.lens (\OrganizationS3LogsConfigurationResult' {autoEnable} -> autoEnable) (\s@OrganizationS3LogsConfigurationResult' {} a -> s {autoEnable = a} :: OrganizationS3LogsConfigurationResult)

instance
  Prelude.FromJSON
    OrganizationS3LogsConfigurationResult
  where
  parseJSON =
    Prelude.withObject
      "OrganizationS3LogsConfigurationResult"
      ( \x ->
          OrganizationS3LogsConfigurationResult'
            Prelude.<$> (x Prelude..: "autoEnable")
      )

instance
  Prelude.Hashable
    OrganizationS3LogsConfigurationResult

instance
  Prelude.NFData
    OrganizationS3LogsConfigurationResult

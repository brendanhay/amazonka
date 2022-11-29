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
-- Module      : Amazonka.GuardDuty.Types.OrganizationScanEc2InstanceWithFindingsResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationScanEc2InstanceWithFindingsResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.OrganizationEbsVolumesResult
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of scanning EC2
-- instances with findings for an organization.
--
-- /See:/ 'newOrganizationScanEc2InstanceWithFindingsResult' smart constructor.
data OrganizationScanEc2InstanceWithFindingsResult = OrganizationScanEc2InstanceWithFindingsResult'
  { -- | Describes the configuration for scanning EBS volumes for an
    -- organization.
    ebsVolumes :: Prelude.Maybe OrganizationEbsVolumesResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationScanEc2InstanceWithFindingsResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsVolumes', 'organizationScanEc2InstanceWithFindingsResult_ebsVolumes' - Describes the configuration for scanning EBS volumes for an
-- organization.
newOrganizationScanEc2InstanceWithFindingsResult ::
  OrganizationScanEc2InstanceWithFindingsResult
newOrganizationScanEc2InstanceWithFindingsResult =
  OrganizationScanEc2InstanceWithFindingsResult'
    { ebsVolumes =
        Prelude.Nothing
    }

-- | Describes the configuration for scanning EBS volumes for an
-- organization.
organizationScanEc2InstanceWithFindingsResult_ebsVolumes :: Lens.Lens' OrganizationScanEc2InstanceWithFindingsResult (Prelude.Maybe OrganizationEbsVolumesResult)
organizationScanEc2InstanceWithFindingsResult_ebsVolumes = Lens.lens (\OrganizationScanEc2InstanceWithFindingsResult' {ebsVolumes} -> ebsVolumes) (\s@OrganizationScanEc2InstanceWithFindingsResult' {} a -> s {ebsVolumes = a} :: OrganizationScanEc2InstanceWithFindingsResult)

instance
  Core.FromJSON
    OrganizationScanEc2InstanceWithFindingsResult
  where
  parseJSON =
    Core.withObject
      "OrganizationScanEc2InstanceWithFindingsResult"
      ( \x ->
          OrganizationScanEc2InstanceWithFindingsResult'
            Prelude.<$> (x Core..:? "ebsVolumes")
      )

instance
  Prelude.Hashable
    OrganizationScanEc2InstanceWithFindingsResult
  where
  hashWithSalt
    _salt
    OrganizationScanEc2InstanceWithFindingsResult' {..} =
      _salt `Prelude.hashWithSalt` ebsVolumes

instance
  Prelude.NFData
    OrganizationScanEc2InstanceWithFindingsResult
  where
  rnf
    OrganizationScanEc2InstanceWithFindingsResult' {..} =
      Prelude.rnf ebsVolumes

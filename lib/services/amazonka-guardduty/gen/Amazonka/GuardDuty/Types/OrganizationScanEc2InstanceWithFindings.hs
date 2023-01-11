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
-- Module      : Amazonka.GuardDuty.Types.OrganizationScanEc2InstanceWithFindings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationScanEc2InstanceWithFindings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrganizationEbsVolumes
import qualified Amazonka.Prelude as Prelude

-- | Organization-wide EC2 instances with findings scan configuration.
--
-- /See:/ 'newOrganizationScanEc2InstanceWithFindings' smart constructor.
data OrganizationScanEc2InstanceWithFindings = OrganizationScanEc2InstanceWithFindings'
  { -- | Whether scanning EBS volumes should be auto-enabled for new members
    -- joining the organization.
    ebsVolumes :: Prelude.Maybe OrganizationEbsVolumes
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationScanEc2InstanceWithFindings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsVolumes', 'organizationScanEc2InstanceWithFindings_ebsVolumes' - Whether scanning EBS volumes should be auto-enabled for new members
-- joining the organization.
newOrganizationScanEc2InstanceWithFindings ::
  OrganizationScanEc2InstanceWithFindings
newOrganizationScanEc2InstanceWithFindings =
  OrganizationScanEc2InstanceWithFindings'
    { ebsVolumes =
        Prelude.Nothing
    }

-- | Whether scanning EBS volumes should be auto-enabled for new members
-- joining the organization.
organizationScanEc2InstanceWithFindings_ebsVolumes :: Lens.Lens' OrganizationScanEc2InstanceWithFindings (Prelude.Maybe OrganizationEbsVolumes)
organizationScanEc2InstanceWithFindings_ebsVolumes = Lens.lens (\OrganizationScanEc2InstanceWithFindings' {ebsVolumes} -> ebsVolumes) (\s@OrganizationScanEc2InstanceWithFindings' {} a -> s {ebsVolumes = a} :: OrganizationScanEc2InstanceWithFindings)

instance
  Prelude.Hashable
    OrganizationScanEc2InstanceWithFindings
  where
  hashWithSalt
    _salt
    OrganizationScanEc2InstanceWithFindings' {..} =
      _salt `Prelude.hashWithSalt` ebsVolumes

instance
  Prelude.NFData
    OrganizationScanEc2InstanceWithFindings
  where
  rnf OrganizationScanEc2InstanceWithFindings' {..} =
    Prelude.rnf ebsVolumes

instance
  Data.ToJSON
    OrganizationScanEc2InstanceWithFindings
  where
  toJSON OrganizationScanEc2InstanceWithFindings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ebsVolumes" Data..=) Prelude.<$> ebsVolumes]
      )

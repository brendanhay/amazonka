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
-- Module      : Amazonka.GuardDuty.Types.OrganizationEbsVolumesResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationEbsVolumesResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information on the status of whether EBS volumes
-- scanning will be enabled as a data source for an organization.
--
-- /See:/ 'newOrganizationEbsVolumesResult' smart constructor.
data OrganizationEbsVolumesResult = OrganizationEbsVolumesResult'
  { -- | An object that contains the status of whether scanning EBS volumes
    -- should be auto-enabled for new members joining the organization.
    autoEnable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEbsVolumesResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationEbsVolumesResult_autoEnable' - An object that contains the status of whether scanning EBS volumes
-- should be auto-enabled for new members joining the organization.
newOrganizationEbsVolumesResult ::
  OrganizationEbsVolumesResult
newOrganizationEbsVolumesResult =
  OrganizationEbsVolumesResult'
    { autoEnable =
        Prelude.Nothing
    }

-- | An object that contains the status of whether scanning EBS volumes
-- should be auto-enabled for new members joining the organization.
organizationEbsVolumesResult_autoEnable :: Lens.Lens' OrganizationEbsVolumesResult (Prelude.Maybe Prelude.Bool)
organizationEbsVolumesResult_autoEnable = Lens.lens (\OrganizationEbsVolumesResult' {autoEnable} -> autoEnable) (\s@OrganizationEbsVolumesResult' {} a -> s {autoEnable = a} :: OrganizationEbsVolumesResult)

instance Data.FromJSON OrganizationEbsVolumesResult where
  parseJSON =
    Data.withObject
      "OrganizationEbsVolumesResult"
      ( \x ->
          OrganizationEbsVolumesResult'
            Prelude.<$> (x Data..:? "autoEnable")
      )

instance
  Prelude.Hashable
    OrganizationEbsVolumesResult
  where
  hashWithSalt _salt OrganizationEbsVolumesResult' {..} =
    _salt `Prelude.hashWithSalt` autoEnable

instance Prelude.NFData OrganizationEbsVolumesResult where
  rnf OrganizationEbsVolumesResult' {..} =
    Prelude.rnf autoEnable

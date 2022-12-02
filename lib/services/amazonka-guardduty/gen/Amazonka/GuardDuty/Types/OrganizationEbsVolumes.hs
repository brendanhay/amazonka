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
-- Module      : Amazonka.GuardDuty.Types.OrganizationEbsVolumes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationEbsVolumes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Organization-wide EBS volumes scan configuration.
--
-- /See:/ 'newOrganizationEbsVolumes' smart constructor.
data OrganizationEbsVolumes = OrganizationEbsVolumes'
  { -- | Whether scanning EBS volumes should be auto-enabled for new members
    -- joining the organization.
    autoEnable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationEbsVolumes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationEbsVolumes_autoEnable' - Whether scanning EBS volumes should be auto-enabled for new members
-- joining the organization.
newOrganizationEbsVolumes ::
  OrganizationEbsVolumes
newOrganizationEbsVolumes =
  OrganizationEbsVolumes'
    { autoEnable =
        Prelude.Nothing
    }

-- | Whether scanning EBS volumes should be auto-enabled for new members
-- joining the organization.
organizationEbsVolumes_autoEnable :: Lens.Lens' OrganizationEbsVolumes (Prelude.Maybe Prelude.Bool)
organizationEbsVolumes_autoEnable = Lens.lens (\OrganizationEbsVolumes' {autoEnable} -> autoEnable) (\s@OrganizationEbsVolumes' {} a -> s {autoEnable = a} :: OrganizationEbsVolumes)

instance Prelude.Hashable OrganizationEbsVolumes where
  hashWithSalt _salt OrganizationEbsVolumes' {..} =
    _salt `Prelude.hashWithSalt` autoEnable

instance Prelude.NFData OrganizationEbsVolumes where
  rnf OrganizationEbsVolumes' {..} =
    Prelude.rnf autoEnable

instance Data.ToJSON OrganizationEbsVolumes where
  toJSON OrganizationEbsVolumes' {..} =
    Data.object
      ( Prelude.catMaybes
          [("autoEnable" Data..=) Prelude.<$> autoEnable]
      )

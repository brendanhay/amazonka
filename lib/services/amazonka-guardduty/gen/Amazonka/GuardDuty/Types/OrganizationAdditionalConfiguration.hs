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
-- Module      : Amazonka.GuardDuty.Types.OrganizationAdditionalConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationAdditionalConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrgFeatureAdditionalConfiguration
import Amazonka.GuardDuty.Types.OrgFeatureStatus
import qualified Amazonka.Prelude as Prelude

-- | A list of additional configurations which will be configured for the
-- organization.
--
-- /See:/ 'newOrganizationAdditionalConfiguration' smart constructor.
data OrganizationAdditionalConfiguration = OrganizationAdditionalConfiguration'
  { -- | The status of the additional configuration that will be configured for
    -- the organization.
    autoEnable :: Prelude.Maybe OrgFeatureStatus,
    -- | The name of the additional configuration that will be configured for the
    -- organization.
    name :: Prelude.Maybe OrgFeatureAdditionalConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationAdditionalConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationAdditionalConfiguration_autoEnable' - The status of the additional configuration that will be configured for
-- the organization.
--
-- 'name', 'organizationAdditionalConfiguration_name' - The name of the additional configuration that will be configured for the
-- organization.
newOrganizationAdditionalConfiguration ::
  OrganizationAdditionalConfiguration
newOrganizationAdditionalConfiguration =
  OrganizationAdditionalConfiguration'
    { autoEnable =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The status of the additional configuration that will be configured for
-- the organization.
organizationAdditionalConfiguration_autoEnable :: Lens.Lens' OrganizationAdditionalConfiguration (Prelude.Maybe OrgFeatureStatus)
organizationAdditionalConfiguration_autoEnable = Lens.lens (\OrganizationAdditionalConfiguration' {autoEnable} -> autoEnable) (\s@OrganizationAdditionalConfiguration' {} a -> s {autoEnable = a} :: OrganizationAdditionalConfiguration)

-- | The name of the additional configuration that will be configured for the
-- organization.
organizationAdditionalConfiguration_name :: Lens.Lens' OrganizationAdditionalConfiguration (Prelude.Maybe OrgFeatureAdditionalConfiguration)
organizationAdditionalConfiguration_name = Lens.lens (\OrganizationAdditionalConfiguration' {name} -> name) (\s@OrganizationAdditionalConfiguration' {} a -> s {name = a} :: OrganizationAdditionalConfiguration)

instance
  Prelude.Hashable
    OrganizationAdditionalConfiguration
  where
  hashWithSalt
    _salt
    OrganizationAdditionalConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoEnable
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    OrganizationAdditionalConfiguration
  where
  rnf OrganizationAdditionalConfiguration' {..} =
    Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf name

instance
  Data.ToJSON
    OrganizationAdditionalConfiguration
  where
  toJSON OrganizationAdditionalConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoEnable" Data..=) Prelude.<$> autoEnable,
            ("name" Data..=) Prelude.<$> name
          ]
      )

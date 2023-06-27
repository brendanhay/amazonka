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
-- Module      : Amazonka.GuardDuty.Types.OrganizationFeatureConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationFeatureConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrgFeature
import Amazonka.GuardDuty.Types.OrgFeatureStatus
import Amazonka.GuardDuty.Types.OrganizationAdditionalConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A list of features which will be configured for the organization.
--
-- /See:/ 'newOrganizationFeatureConfiguration' smart constructor.
data OrganizationFeatureConfiguration = OrganizationFeatureConfiguration'
  { -- | The additional information that will be configured for the organization.
    additionalConfiguration :: Prelude.Maybe [OrganizationAdditionalConfiguration],
    -- | The status of the feature that will be configured for the organization.
    autoEnable :: Prelude.Maybe OrgFeatureStatus,
    -- | The name of the feature that will be configured for the organization.
    name :: Prelude.Maybe OrgFeature
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationFeatureConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfiguration', 'organizationFeatureConfiguration_additionalConfiguration' - The additional information that will be configured for the organization.
--
-- 'autoEnable', 'organizationFeatureConfiguration_autoEnable' - The status of the feature that will be configured for the organization.
--
-- 'name', 'organizationFeatureConfiguration_name' - The name of the feature that will be configured for the organization.
newOrganizationFeatureConfiguration ::
  OrganizationFeatureConfiguration
newOrganizationFeatureConfiguration =
  OrganizationFeatureConfiguration'
    { additionalConfiguration =
        Prelude.Nothing,
      autoEnable = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The additional information that will be configured for the organization.
organizationFeatureConfiguration_additionalConfiguration :: Lens.Lens' OrganizationFeatureConfiguration (Prelude.Maybe [OrganizationAdditionalConfiguration])
organizationFeatureConfiguration_additionalConfiguration = Lens.lens (\OrganizationFeatureConfiguration' {additionalConfiguration} -> additionalConfiguration) (\s@OrganizationFeatureConfiguration' {} a -> s {additionalConfiguration = a} :: OrganizationFeatureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The status of the feature that will be configured for the organization.
organizationFeatureConfiguration_autoEnable :: Lens.Lens' OrganizationFeatureConfiguration (Prelude.Maybe OrgFeatureStatus)
organizationFeatureConfiguration_autoEnable = Lens.lens (\OrganizationFeatureConfiguration' {autoEnable} -> autoEnable) (\s@OrganizationFeatureConfiguration' {} a -> s {autoEnable = a} :: OrganizationFeatureConfiguration)

-- | The name of the feature that will be configured for the organization.
organizationFeatureConfiguration_name :: Lens.Lens' OrganizationFeatureConfiguration (Prelude.Maybe OrgFeature)
organizationFeatureConfiguration_name = Lens.lens (\OrganizationFeatureConfiguration' {name} -> name) (\s@OrganizationFeatureConfiguration' {} a -> s {name = a} :: OrganizationFeatureConfiguration)

instance
  Prelude.Hashable
    OrganizationFeatureConfiguration
  where
  hashWithSalt
    _salt
    OrganizationFeatureConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` additionalConfiguration
        `Prelude.hashWithSalt` autoEnable
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    OrganizationFeatureConfiguration
  where
  rnf OrganizationFeatureConfiguration' {..} =
    Prelude.rnf additionalConfiguration
      `Prelude.seq` Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON OrganizationFeatureConfiguration where
  toJSON OrganizationFeatureConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalConfiguration" Data..=)
              Prelude.<$> additionalConfiguration,
            ("autoEnable" Data..=) Prelude.<$> autoEnable,
            ("name" Data..=) Prelude.<$> name
          ]
      )

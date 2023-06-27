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
-- Module      : Amazonka.GuardDuty.Types.OrganizationFeatureConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationFeatureConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrgFeature
import Amazonka.GuardDuty.Types.OrgFeatureStatus
import Amazonka.GuardDuty.Types.OrganizationAdditionalConfigurationResult
import qualified Amazonka.Prelude as Prelude

-- | A list of features which will be configured for the organization.
--
-- /See:/ 'newOrganizationFeatureConfigurationResult' smart constructor.
data OrganizationFeatureConfigurationResult = OrganizationFeatureConfigurationResult'
  { -- | The additional configuration that is configured for the member accounts
    -- within the organization.
    additionalConfiguration :: Prelude.Maybe [OrganizationAdditionalConfigurationResult],
    -- | Describes how The status of the feature that are configured for the
    -- member accounts within the organization.
    --
    -- If you set @AutoEnable@ to @NEW@, a feature will be configured for only
    -- the new accounts when they join the organization.
    --
    -- If you set @AutoEnable@ to @NONE@, no feature will be configured for the
    -- accounts when they join the organization.
    autoEnable :: Prelude.Maybe OrgFeatureStatus,
    -- | The name of the feature that is configured for the member accounts
    -- within the organization.
    name :: Prelude.Maybe OrgFeature
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationFeatureConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfiguration', 'organizationFeatureConfigurationResult_additionalConfiguration' - The additional configuration that is configured for the member accounts
-- within the organization.
--
-- 'autoEnable', 'organizationFeatureConfigurationResult_autoEnable' - Describes how The status of the feature that are configured for the
-- member accounts within the organization.
--
-- If you set @AutoEnable@ to @NEW@, a feature will be configured for only
-- the new accounts when they join the organization.
--
-- If you set @AutoEnable@ to @NONE@, no feature will be configured for the
-- accounts when they join the organization.
--
-- 'name', 'organizationFeatureConfigurationResult_name' - The name of the feature that is configured for the member accounts
-- within the organization.
newOrganizationFeatureConfigurationResult ::
  OrganizationFeatureConfigurationResult
newOrganizationFeatureConfigurationResult =
  OrganizationFeatureConfigurationResult'
    { additionalConfiguration =
        Prelude.Nothing,
      autoEnable = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The additional configuration that is configured for the member accounts
-- within the organization.
organizationFeatureConfigurationResult_additionalConfiguration :: Lens.Lens' OrganizationFeatureConfigurationResult (Prelude.Maybe [OrganizationAdditionalConfigurationResult])
organizationFeatureConfigurationResult_additionalConfiguration = Lens.lens (\OrganizationFeatureConfigurationResult' {additionalConfiguration} -> additionalConfiguration) (\s@OrganizationFeatureConfigurationResult' {} a -> s {additionalConfiguration = a} :: OrganizationFeatureConfigurationResult) Prelude.. Lens.mapping Lens.coerced

-- | Describes how The status of the feature that are configured for the
-- member accounts within the organization.
--
-- If you set @AutoEnable@ to @NEW@, a feature will be configured for only
-- the new accounts when they join the organization.
--
-- If you set @AutoEnable@ to @NONE@, no feature will be configured for the
-- accounts when they join the organization.
organizationFeatureConfigurationResult_autoEnable :: Lens.Lens' OrganizationFeatureConfigurationResult (Prelude.Maybe OrgFeatureStatus)
organizationFeatureConfigurationResult_autoEnable = Lens.lens (\OrganizationFeatureConfigurationResult' {autoEnable} -> autoEnable) (\s@OrganizationFeatureConfigurationResult' {} a -> s {autoEnable = a} :: OrganizationFeatureConfigurationResult)

-- | The name of the feature that is configured for the member accounts
-- within the organization.
organizationFeatureConfigurationResult_name :: Lens.Lens' OrganizationFeatureConfigurationResult (Prelude.Maybe OrgFeature)
organizationFeatureConfigurationResult_name = Lens.lens (\OrganizationFeatureConfigurationResult' {name} -> name) (\s@OrganizationFeatureConfigurationResult' {} a -> s {name = a} :: OrganizationFeatureConfigurationResult)

instance
  Data.FromJSON
    OrganizationFeatureConfigurationResult
  where
  parseJSON =
    Data.withObject
      "OrganizationFeatureConfigurationResult"
      ( \x ->
          OrganizationFeatureConfigurationResult'
            Prelude.<$> ( x
                            Data..:? "additionalConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "autoEnable")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    OrganizationFeatureConfigurationResult
  where
  hashWithSalt
    _salt
    OrganizationFeatureConfigurationResult' {..} =
      _salt
        `Prelude.hashWithSalt` additionalConfiguration
        `Prelude.hashWithSalt` autoEnable
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    OrganizationFeatureConfigurationResult
  where
  rnf OrganizationFeatureConfigurationResult' {..} =
    Prelude.rnf additionalConfiguration
      `Prelude.seq` Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf name

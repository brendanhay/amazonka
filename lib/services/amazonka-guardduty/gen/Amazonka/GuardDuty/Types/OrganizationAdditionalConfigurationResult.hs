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
-- Module      : Amazonka.GuardDuty.Types.OrganizationAdditionalConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationAdditionalConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.OrgFeatureAdditionalConfiguration
import Amazonka.GuardDuty.Types.OrgFeatureStatus
import qualified Amazonka.Prelude as Prelude

-- | A list of additional configuration which will be configured for the
-- organization.
--
-- /See:/ 'newOrganizationAdditionalConfigurationResult' smart constructor.
data OrganizationAdditionalConfigurationResult = OrganizationAdditionalConfigurationResult'
  { -- | Describes how The status of the additional configuration that are
    -- configured for the member accounts within the organization.
    --
    -- If you set @AutoEnable@ to @NEW@, a feature will be configured for only
    -- the new accounts when they join the organization.
    --
    -- If you set @AutoEnable@ to @NONE@, no feature will be configured for the
    -- accounts when they join the organization.
    autoEnable :: Prelude.Maybe OrgFeatureStatus,
    -- | The name of the additional configuration that is configured for the
    -- member accounts within the organization.
    name :: Prelude.Maybe OrgFeatureAdditionalConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationAdditionalConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationAdditionalConfigurationResult_autoEnable' - Describes how The status of the additional configuration that are
-- configured for the member accounts within the organization.
--
-- If you set @AutoEnable@ to @NEW@, a feature will be configured for only
-- the new accounts when they join the organization.
--
-- If you set @AutoEnable@ to @NONE@, no feature will be configured for the
-- accounts when they join the organization.
--
-- 'name', 'organizationAdditionalConfigurationResult_name' - The name of the additional configuration that is configured for the
-- member accounts within the organization.
newOrganizationAdditionalConfigurationResult ::
  OrganizationAdditionalConfigurationResult
newOrganizationAdditionalConfigurationResult =
  OrganizationAdditionalConfigurationResult'
    { autoEnable =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Describes how The status of the additional configuration that are
-- configured for the member accounts within the organization.
--
-- If you set @AutoEnable@ to @NEW@, a feature will be configured for only
-- the new accounts when they join the organization.
--
-- If you set @AutoEnable@ to @NONE@, no feature will be configured for the
-- accounts when they join the organization.
organizationAdditionalConfigurationResult_autoEnable :: Lens.Lens' OrganizationAdditionalConfigurationResult (Prelude.Maybe OrgFeatureStatus)
organizationAdditionalConfigurationResult_autoEnable = Lens.lens (\OrganizationAdditionalConfigurationResult' {autoEnable} -> autoEnable) (\s@OrganizationAdditionalConfigurationResult' {} a -> s {autoEnable = a} :: OrganizationAdditionalConfigurationResult)

-- | The name of the additional configuration that is configured for the
-- member accounts within the organization.
organizationAdditionalConfigurationResult_name :: Lens.Lens' OrganizationAdditionalConfigurationResult (Prelude.Maybe OrgFeatureAdditionalConfiguration)
organizationAdditionalConfigurationResult_name = Lens.lens (\OrganizationAdditionalConfigurationResult' {name} -> name) (\s@OrganizationAdditionalConfigurationResult' {} a -> s {name = a} :: OrganizationAdditionalConfigurationResult)

instance
  Data.FromJSON
    OrganizationAdditionalConfigurationResult
  where
  parseJSON =
    Data.withObject
      "OrganizationAdditionalConfigurationResult"
      ( \x ->
          OrganizationAdditionalConfigurationResult'
            Prelude.<$> (x Data..:? "autoEnable")
            Prelude.<*> (x Data..:? "name")
      )

instance
  Prelude.Hashable
    OrganizationAdditionalConfigurationResult
  where
  hashWithSalt
    _salt
    OrganizationAdditionalConfigurationResult' {..} =
      _salt
        `Prelude.hashWithSalt` autoEnable
        `Prelude.hashWithSalt` name

instance
  Prelude.NFData
    OrganizationAdditionalConfigurationResult
  where
  rnf OrganizationAdditionalConfigurationResult' {..} =
    Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf name

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
-- Module      : Network.AWS.LicenseManager.Types.OrganizationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LicenseManager.Types.OrganizationConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration information for Organizations.
--
-- /See:/ 'newOrganizationConfiguration' smart constructor.
data OrganizationConfiguration = OrganizationConfiguration'
  { -- | Enables Organizations integration.
    enableIntegration :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableIntegration', 'organizationConfiguration_enableIntegration' - Enables Organizations integration.
newOrganizationConfiguration ::
  -- | 'enableIntegration'
  Prelude.Bool ->
  OrganizationConfiguration
newOrganizationConfiguration pEnableIntegration_ =
  OrganizationConfiguration'
    { enableIntegration =
        pEnableIntegration_
    }

-- | Enables Organizations integration.
organizationConfiguration_enableIntegration :: Lens.Lens' OrganizationConfiguration Prelude.Bool
organizationConfiguration_enableIntegration = Lens.lens (\OrganizationConfiguration' {enableIntegration} -> enableIntegration) (\s@OrganizationConfiguration' {} a -> s {enableIntegration = a} :: OrganizationConfiguration)

instance Core.FromJSON OrganizationConfiguration where
  parseJSON =
    Core.withObject
      "OrganizationConfiguration"
      ( \x ->
          OrganizationConfiguration'
            Prelude.<$> (x Core..: "EnableIntegration")
      )

instance Prelude.Hashable OrganizationConfiguration

instance Prelude.NFData OrganizationConfiguration

instance Core.ToJSON OrganizationConfiguration where
  toJSON OrganizationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EnableIntegration" Core..= enableIntegration)
          ]
      )

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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.UpdateSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.UpdateSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Updates the registered identity provider’s product related configuration
-- settings such as the subnets to provision VPC endpoints.
--
-- /See:/ 'newUpdateSettings' smart constructor.
data UpdateSettings = UpdateSettings'
  { -- | A security group ID that allows inbound TCP port 1688 communication
    -- between resources in your VPC and the VPC endpoints for activation
    -- servers.
    securityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of one or more subnets in which License Manager will create a VPC
    -- endpoint for products that require connectivity to activation servers.
    addSubnets :: [Prelude.Text],
    -- | The ID of one or more subnets to remove.
    removeSubnets :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupId', 'updateSettings_securityGroupId' - A security group ID that allows inbound TCP port 1688 communication
-- between resources in your VPC and the VPC endpoints for activation
-- servers.
--
-- 'addSubnets', 'updateSettings_addSubnets' - The ID of one or more subnets in which License Manager will create a VPC
-- endpoint for products that require connectivity to activation servers.
--
-- 'removeSubnets', 'updateSettings_removeSubnets' - The ID of one or more subnets to remove.
newUpdateSettings ::
  UpdateSettings
newUpdateSettings =
  UpdateSettings'
    { securityGroupId = Prelude.Nothing,
      addSubnets = Prelude.mempty,
      removeSubnets = Prelude.mempty
    }

-- | A security group ID that allows inbound TCP port 1688 communication
-- between resources in your VPC and the VPC endpoints for activation
-- servers.
updateSettings_securityGroupId :: Lens.Lens' UpdateSettings (Prelude.Maybe Prelude.Text)
updateSettings_securityGroupId = Lens.lens (\UpdateSettings' {securityGroupId} -> securityGroupId) (\s@UpdateSettings' {} a -> s {securityGroupId = a} :: UpdateSettings)

-- | The ID of one or more subnets in which License Manager will create a VPC
-- endpoint for products that require connectivity to activation servers.
updateSettings_addSubnets :: Lens.Lens' UpdateSettings [Prelude.Text]
updateSettings_addSubnets = Lens.lens (\UpdateSettings' {addSubnets} -> addSubnets) (\s@UpdateSettings' {} a -> s {addSubnets = a} :: UpdateSettings) Prelude.. Lens.coerced

-- | The ID of one or more subnets to remove.
updateSettings_removeSubnets :: Lens.Lens' UpdateSettings [Prelude.Text]
updateSettings_removeSubnets = Lens.lens (\UpdateSettings' {removeSubnets} -> removeSubnets) (\s@UpdateSettings' {} a -> s {removeSubnets = a} :: UpdateSettings) Prelude.. Lens.coerced

instance Prelude.Hashable UpdateSettings where
  hashWithSalt _salt UpdateSettings' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupId
      `Prelude.hashWithSalt` addSubnets
      `Prelude.hashWithSalt` removeSubnets

instance Prelude.NFData UpdateSettings where
  rnf UpdateSettings' {..} =
    Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf addSubnets
      `Prelude.seq` Prelude.rnf removeSubnets

instance Data.ToJSON UpdateSettings where
  toJSON UpdateSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SecurityGroupId" Data..=)
              Prelude.<$> securityGroupId,
            Prelude.Just ("AddSubnets" Data..= addSubnets),
            Prelude.Just
              ("RemoveSubnets" Data..= removeSubnets)
          ]
      )

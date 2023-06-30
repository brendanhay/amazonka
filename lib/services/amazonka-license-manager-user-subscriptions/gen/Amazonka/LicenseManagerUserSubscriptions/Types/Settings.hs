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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The registered identity provider’s product related configuration
-- settings such as the subnets to provision VPC endpoints, and the
-- security group ID that is associated with the VPC endpoints. The
-- security group should permit inbound TCP port 1688 communication from
-- resources in the VPC.
--
-- /See:/ 'newSettings' smart constructor.
data Settings = Settings'
  { -- | A security group ID that allows inbound TCP port 1688 communication
    -- between resources in your VPC and the VPC endpoint for activation
    -- servers.
    securityGroupId :: Prelude.Text,
    -- | The subnets defined for the registered identity provider.
    subnets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupId', 'settings_securityGroupId' - A security group ID that allows inbound TCP port 1688 communication
-- between resources in your VPC and the VPC endpoint for activation
-- servers.
--
-- 'subnets', 'settings_subnets' - The subnets defined for the registered identity provider.
newSettings ::
  -- | 'securityGroupId'
  Prelude.Text ->
  -- | 'subnets'
  Prelude.NonEmpty Prelude.Text ->
  Settings
newSettings pSecurityGroupId_ pSubnets_ =
  Settings'
    { securityGroupId = pSecurityGroupId_,
      subnets = Lens.coerced Lens.# pSubnets_
    }

-- | A security group ID that allows inbound TCP port 1688 communication
-- between resources in your VPC and the VPC endpoint for activation
-- servers.
settings_securityGroupId :: Lens.Lens' Settings Prelude.Text
settings_securityGroupId = Lens.lens (\Settings' {securityGroupId} -> securityGroupId) (\s@Settings' {} a -> s {securityGroupId = a} :: Settings)

-- | The subnets defined for the registered identity provider.
settings_subnets :: Lens.Lens' Settings (Prelude.NonEmpty Prelude.Text)
settings_subnets = Lens.lens (\Settings' {subnets} -> subnets) (\s@Settings' {} a -> s {subnets = a} :: Settings) Prelude.. Lens.coerced

instance Data.FromJSON Settings where
  parseJSON =
    Data.withObject
      "Settings"
      ( \x ->
          Settings'
            Prelude.<$> (x Data..: "SecurityGroupId")
            Prelude.<*> (x Data..: "Subnets")
      )

instance Prelude.Hashable Settings where
  hashWithSalt _salt Settings' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupId
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData Settings where
  rnf Settings' {..} =
    Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON Settings where
  toJSON Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityGroupId" Data..= securityGroupId),
            Prelude.Just ("Subnets" Data..= subnets)
          ]
      )

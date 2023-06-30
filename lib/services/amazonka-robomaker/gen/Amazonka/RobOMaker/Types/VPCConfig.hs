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
-- Module      : Amazonka.RobOMaker.Types.VPCConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.VPCConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If your simulation job accesses resources in a VPC, you provide this
-- parameter identifying the list of security group IDs and subnet IDs.
-- These must belong to the same VPC. You must provide at least one
-- security group and two subnet IDs.
--
-- /See:/ 'newVPCConfig' smart constructor.
data VPCConfig = VPCConfig'
  { -- | A boolean indicating whether to assign a public IP address.
    assignPublicIp :: Prelude.Maybe Prelude.Bool,
    -- | A list of one or more security groups IDs in your VPC.
    securityGroups :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of one or more subnet IDs in your VPC.
    subnets :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPCConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignPublicIp', 'vPCConfig_assignPublicIp' - A boolean indicating whether to assign a public IP address.
--
-- 'securityGroups', 'vPCConfig_securityGroups' - A list of one or more security groups IDs in your VPC.
--
-- 'subnets', 'vPCConfig_subnets' - A list of one or more subnet IDs in your VPC.
newVPCConfig ::
  -- | 'subnets'
  Prelude.NonEmpty Prelude.Text ->
  VPCConfig
newVPCConfig pSubnets_ =
  VPCConfig'
    { assignPublicIp = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      subnets = Lens.coerced Lens.# pSubnets_
    }

-- | A boolean indicating whether to assign a public IP address.
vPCConfig_assignPublicIp :: Lens.Lens' VPCConfig (Prelude.Maybe Prelude.Bool)
vPCConfig_assignPublicIp = Lens.lens (\VPCConfig' {assignPublicIp} -> assignPublicIp) (\s@VPCConfig' {} a -> s {assignPublicIp = a} :: VPCConfig)

-- | A list of one or more security groups IDs in your VPC.
vPCConfig_securityGroups :: Lens.Lens' VPCConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
vPCConfig_securityGroups = Lens.lens (\VPCConfig' {securityGroups} -> securityGroups) (\s@VPCConfig' {} a -> s {securityGroups = a} :: VPCConfig) Prelude.. Lens.mapping Lens.coerced

-- | A list of one or more subnet IDs in your VPC.
vPCConfig_subnets :: Lens.Lens' VPCConfig (Prelude.NonEmpty Prelude.Text)
vPCConfig_subnets = Lens.lens (\VPCConfig' {subnets} -> subnets) (\s@VPCConfig' {} a -> s {subnets = a} :: VPCConfig) Prelude.. Lens.coerced

instance Data.FromJSON VPCConfig where
  parseJSON =
    Data.withObject
      "VPCConfig"
      ( \x ->
          VPCConfig'
            Prelude.<$> (x Data..:? "assignPublicIp")
            Prelude.<*> (x Data..:? "securityGroups")
            Prelude.<*> (x Data..: "subnets")
      )

instance Prelude.Hashable VPCConfig where
  hashWithSalt _salt VPCConfig' {..} =
    _salt
      `Prelude.hashWithSalt` assignPublicIp
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData VPCConfig where
  rnf VPCConfig' {..} =
    Prelude.rnf assignPublicIp
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assignPublicIp" Data..=)
              Prelude.<$> assignPublicIp,
            ("securityGroups" Data..=)
              Prelude.<$> securityGroups,
            Prelude.Just ("subnets" Data..= subnets)
          ]
      )

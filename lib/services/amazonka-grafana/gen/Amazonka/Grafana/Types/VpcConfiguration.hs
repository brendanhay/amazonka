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
-- Module      : Amazonka.Grafana.Types.VpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.VpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration settings for an Amazon VPC that contains data sources
-- for your Grafana workspace to connect to.
--
-- Provided @securityGroupIds@ and @subnetIds@ must be part of the same
-- VPC.
--
-- /See:/ 'newVpcConfiguration' smart constructor.
data VpcConfiguration = VpcConfiguration'
  { -- | The list of Amazon EC2 security group IDs attached to the Amazon VPC for
    -- your Grafana workspace to connect. Duplicates not allowed.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The list of Amazon EC2 subnet IDs created in the Amazon VPC for your
    -- Grafana workspace to connect. Duplicates not allowed.
    subnetIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfiguration_securityGroupIds' - The list of Amazon EC2 security group IDs attached to the Amazon VPC for
-- your Grafana workspace to connect. Duplicates not allowed.
--
-- 'subnetIds', 'vpcConfiguration_subnetIds' - The list of Amazon EC2 subnet IDs created in the Amazon VPC for your
-- Grafana workspace to connect. Duplicates not allowed.
newVpcConfiguration ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  VpcConfiguration
newVpcConfiguration pSecurityGroupIds_ pSubnetIds_ =
  VpcConfiguration'
    { securityGroupIds =
        Lens.coerced Lens.# pSecurityGroupIds_,
      subnetIds = Lens.coerced Lens.# pSubnetIds_
    }

-- | The list of Amazon EC2 security group IDs attached to the Amazon VPC for
-- your Grafana workspace to connect. Duplicates not allowed.
vpcConfiguration_securityGroupIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_securityGroupIds = Lens.lens (\VpcConfiguration' {securityGroupIds} -> securityGroupIds) (\s@VpcConfiguration' {} a -> s {securityGroupIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

-- | The list of Amazon EC2 subnet IDs created in the Amazon VPC for your
-- Grafana workspace to connect. Duplicates not allowed.
vpcConfiguration_subnetIds :: Lens.Lens' VpcConfiguration (Prelude.NonEmpty Prelude.Text)
vpcConfiguration_subnetIds = Lens.lens (\VpcConfiguration' {subnetIds} -> subnetIds) (\s@VpcConfiguration' {} a -> s {subnetIds = a} :: VpcConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON VpcConfiguration where
  parseJSON =
    Data.withObject
      "VpcConfiguration"
      ( \x ->
          VpcConfiguration'
            Prelude.<$> (x Data..: "securityGroupIds")
            Prelude.<*> (x Data..: "subnetIds")
      )

instance Prelude.Hashable VpcConfiguration where
  hashWithSalt _salt VpcConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VpcConfiguration where
  rnf VpcConfiguration' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON VpcConfiguration where
  toJSON VpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("securityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("subnetIds" Data..= subnetIds)
          ]
      )

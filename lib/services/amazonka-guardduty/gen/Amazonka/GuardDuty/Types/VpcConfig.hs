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
-- Module      : Amazonka.GuardDuty.Types.VpcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.VpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.SecurityGroup
import qualified Amazonka.Prelude as Prelude

-- | Amazon Virtual Private Cloud configuration details associated with your
-- Lambda function.
--
-- /See:/ 'newVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { -- | The identifier of the security group attached to the Lambda function.
    securityGroups :: Prelude.Maybe [SecurityGroup],
    -- | The identifiers of the subnets that are associated with your Lambda
    -- function.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the Amazon Virtual Private Cloud.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroups', 'vpcConfig_securityGroups' - The identifier of the security group attached to the Lambda function.
--
-- 'subnetIds', 'vpcConfig_subnetIds' - The identifiers of the subnets that are associated with your Lambda
-- function.
--
-- 'vpcId', 'vpcConfig_vpcId' - The identifier of the Amazon Virtual Private Cloud.
newVpcConfig ::
  VpcConfig
newVpcConfig =
  VpcConfig'
    { securityGroups = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The identifier of the security group attached to the Lambda function.
vpcConfig_securityGroups :: Lens.Lens' VpcConfig (Prelude.Maybe [SecurityGroup])
vpcConfig_securityGroups = Lens.lens (\VpcConfig' {securityGroups} -> securityGroups) (\s@VpcConfig' {} a -> s {securityGroups = a} :: VpcConfig) Prelude.. Lens.mapping Lens.coerced

-- | The identifiers of the subnets that are associated with your Lambda
-- function.
vpcConfig_subnetIds :: Lens.Lens' VpcConfig (Prelude.Maybe [Prelude.Text])
vpcConfig_subnetIds = Lens.lens (\VpcConfig' {subnetIds} -> subnetIds) (\s@VpcConfig' {} a -> s {subnetIds = a} :: VpcConfig) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the Amazon Virtual Private Cloud.
vpcConfig_vpcId :: Lens.Lens' VpcConfig (Prelude.Maybe Prelude.Text)
vpcConfig_vpcId = Lens.lens (\VpcConfig' {vpcId} -> vpcId) (\s@VpcConfig' {} a -> s {vpcId = a} :: VpcConfig)

instance Data.FromJSON VpcConfig where
  parseJSON =
    Data.withObject
      "VpcConfig"
      ( \x ->
          VpcConfig'
            Prelude.<$> (x Data..:? "securityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "subnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable VpcConfig where
  hashWithSalt _salt VpcConfig' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcConfig where
  rnf VpcConfig' {..} =
    Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId

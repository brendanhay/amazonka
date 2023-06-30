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
-- Module      : Amazonka.Comprehend.Types.VpcConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.VpcConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration parameters for an optional private Virtual Private Cloud
-- (VPC) containing the resources you are using for the job. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/what-is-amazon-vpc.html Amazon VPC>.
--
-- /See:/ 'newVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { -- | The ID number for a security group on an instance of your private VPC.
    -- Security groups on your VPC function serve as a virtual firewall to
    -- control inbound and outbound traffic and provides security for the
    -- resources that you’ll be accessing on the VPC. This ID number is
    -- preceded by \"sg-\", for instance: \"sg-03b388029b0a285ea\". For more
    -- information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC>.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The ID for each subnet being used in your private VPC. This subnet is a
    -- subset of the a range of IPv4 addresses used by the VPC and is specific
    -- to a given availability zone in the VPC’s region. This ID number is
    -- preceded by \"subnet-\", for instance: \"subnet-04ccf456919e69055\". For
    -- more information, see
    -- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>.
    subnets :: Prelude.NonEmpty Prelude.Text
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
-- 'securityGroupIds', 'vpcConfig_securityGroupIds' - The ID number for a security group on an instance of your private VPC.
-- Security groups on your VPC function serve as a virtual firewall to
-- control inbound and outbound traffic and provides security for the
-- resources that you’ll be accessing on the VPC. This ID number is
-- preceded by \"sg-\", for instance: \"sg-03b388029b0a285ea\". For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC>.
--
-- 'subnets', 'vpcConfig_subnets' - The ID for each subnet being used in your private VPC. This subnet is a
-- subset of the a range of IPv4 addresses used by the VPC and is specific
-- to a given availability zone in the VPC’s region. This ID number is
-- preceded by \"subnet-\", for instance: \"subnet-04ccf456919e69055\". For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>.
newVpcConfig ::
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'subnets'
  Prelude.NonEmpty Prelude.Text ->
  VpcConfig
newVpcConfig pSecurityGroupIds_ pSubnets_ =
  VpcConfig'
    { securityGroupIds =
        Lens.coerced Lens.# pSecurityGroupIds_,
      subnets = Lens.coerced Lens.# pSubnets_
    }

-- | The ID number for a security group on an instance of your private VPC.
-- Security groups on your VPC function serve as a virtual firewall to
-- control inbound and outbound traffic and provides security for the
-- resources that you’ll be accessing on the VPC. This ID number is
-- preceded by \"sg-\", for instance: \"sg-03b388029b0a285ea\". For more
-- information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_SecurityGroups.html Security Groups for your VPC>.
vpcConfig_securityGroupIds :: Lens.Lens' VpcConfig (Prelude.NonEmpty Prelude.Text)
vpcConfig_securityGroupIds = Lens.lens (\VpcConfig' {securityGroupIds} -> securityGroupIds) (\s@VpcConfig' {} a -> s {securityGroupIds = a} :: VpcConfig) Prelude.. Lens.coerced

-- | The ID for each subnet being used in your private VPC. This subnet is a
-- subset of the a range of IPv4 addresses used by the VPC and is specific
-- to a given availability zone in the VPC’s region. This ID number is
-- preceded by \"subnet-\", for instance: \"subnet-04ccf456919e69055\". For
-- more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html VPCs and Subnets>.
vpcConfig_subnets :: Lens.Lens' VpcConfig (Prelude.NonEmpty Prelude.Text)
vpcConfig_subnets = Lens.lens (\VpcConfig' {subnets} -> subnets) (\s@VpcConfig' {} a -> s {subnets = a} :: VpcConfig) Prelude.. Lens.coerced

instance Data.FromJSON VpcConfig where
  parseJSON =
    Data.withObject
      "VpcConfig"
      ( \x ->
          VpcConfig'
            Prelude.<$> (x Data..: "SecurityGroupIds")
            Prelude.<*> (x Data..: "Subnets")
      )

instance Prelude.Hashable VpcConfig where
  hashWithSalt _salt VpcConfig' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnets

instance Prelude.NFData VpcConfig where
  rnf VpcConfig' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnets

instance Data.ToJSON VpcConfig where
  toJSON VpcConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("Subnets" Data..= subnets)
          ]
      )

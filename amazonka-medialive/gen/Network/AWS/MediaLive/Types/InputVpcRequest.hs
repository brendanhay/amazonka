{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.InputVpcRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputVpcRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for a private VPC Input. When this property is specified, the
-- input destination addresses will be created in a VPC rather than with
-- public Internet addresses. This property requires setting the roleArn
-- property on Input creation. Not compatible with the inputSecurityGroups
-- property.
--
-- /See:/ 'newInputVpcRequest' smart constructor.
data InputVpcRequest = InputVpcRequest'
  { -- | A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC
    -- network interfaces. Requires subnetIds. If none are specified then the
    -- VPC default security group will be used.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of 2 VPC subnet IDs from the same VPC. Subnet IDs must be mapped
    -- to two unique availability zones (AZ).
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputVpcRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'inputVpcRequest_securityGroupIds' - A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC
-- network interfaces. Requires subnetIds. If none are specified then the
-- VPC default security group will be used.
--
-- 'subnetIds', 'inputVpcRequest_subnetIds' - A list of 2 VPC subnet IDs from the same VPC. Subnet IDs must be mapped
-- to two unique availability zones (AZ).
newInputVpcRequest ::
  InputVpcRequest
newInputVpcRequest =
  InputVpcRequest'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.mempty
    }

-- | A list of up to 5 EC2 VPC security group IDs to attach to the Input VPC
-- network interfaces. Requires subnetIds. If none are specified then the
-- VPC default security group will be used.
inputVpcRequest_securityGroupIds :: Lens.Lens' InputVpcRequest (Prelude.Maybe [Prelude.Text])
inputVpcRequest_securityGroupIds = Lens.lens (\InputVpcRequest' {securityGroupIds} -> securityGroupIds) (\s@InputVpcRequest' {} a -> s {securityGroupIds = a} :: InputVpcRequest) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of 2 VPC subnet IDs from the same VPC. Subnet IDs must be mapped
-- to two unique availability zones (AZ).
inputVpcRequest_subnetIds :: Lens.Lens' InputVpcRequest [Prelude.Text]
inputVpcRequest_subnetIds = Lens.lens (\InputVpcRequest' {subnetIds} -> subnetIds) (\s@InputVpcRequest' {} a -> s {subnetIds = a} :: InputVpcRequest) Prelude.. Prelude._Coerce

instance Prelude.Hashable InputVpcRequest

instance Prelude.NFData InputVpcRequest

instance Prelude.ToJSON InputVpcRequest where
  toJSON InputVpcRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Prelude..=)
              Prelude.<$> securityGroupIds,
            Prelude.Just ("subnetIds" Prelude..= subnetIds)
          ]
      )

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
-- Module      : Network.AWS.AppStream.Types.VpcConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.VpcConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes VPC configuration information for fleets and image builders.
--
-- /See:/ 'newVpcConfig' smart constructor.
data VpcConfig = VpcConfig'
  { -- | The identifiers of the security groups for the fleet or image builder.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifiers of the subnets to which a network interface is attached
    -- from the fleet instance or image builder instance. Fleet instances use
    -- one or more subnets. Image builder instances use one subnet.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcConfig_securityGroupIds' - The identifiers of the security groups for the fleet or image builder.
--
-- 'subnetIds', 'vpcConfig_subnetIds' - The identifiers of the subnets to which a network interface is attached
-- from the fleet instance or image builder instance. Fleet instances use
-- one or more subnets. Image builder instances use one subnet.
newVpcConfig ::
  VpcConfig
newVpcConfig =
  VpcConfig'
    { securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | The identifiers of the security groups for the fleet or image builder.
vpcConfig_securityGroupIds :: Lens.Lens' VpcConfig (Prelude.Maybe [Prelude.Text])
vpcConfig_securityGroupIds = Lens.lens (\VpcConfig' {securityGroupIds} -> securityGroupIds) (\s@VpcConfig' {} a -> s {securityGroupIds = a} :: VpcConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifiers of the subnets to which a network interface is attached
-- from the fleet instance or image builder instance. Fleet instances use
-- one or more subnets. Image builder instances use one subnet.
vpcConfig_subnetIds :: Lens.Lens' VpcConfig (Prelude.Maybe [Prelude.Text])
vpcConfig_subnetIds = Lens.lens (\VpcConfig' {subnetIds} -> subnetIds) (\s@VpcConfig' {} a -> s {subnetIds = a} :: VpcConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON VpcConfig where
  parseJSON =
    Prelude.withObject
      "VpcConfig"
      ( \x ->
          VpcConfig'
            Prelude.<$> ( x Prelude..:? "SecurityGroupIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "SubnetIds"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VpcConfig

instance Prelude.NFData VpcConfig

instance Prelude.ToJSON VpcConfig where
  toJSON VpcConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecurityGroupIds" Prelude..=)
              Prelude.<$> securityGroupIds,
            ("SubnetIds" Prelude..=) Prelude.<$> subnetIds
          ]
      )

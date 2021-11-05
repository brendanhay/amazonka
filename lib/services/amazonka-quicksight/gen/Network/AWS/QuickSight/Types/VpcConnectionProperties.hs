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
-- Module      : Network.AWS.QuickSight.Types.VpcConnectionProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.VpcConnectionProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | VPC connection properties.
--
-- /See:/ 'newVpcConnectionProperties' smart constructor.
data VpcConnectionProperties = VpcConnectionProperties'
  { -- | The Amazon Resource Name (ARN) for the VPC connection.
    vpcConnectionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectionProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConnectionArn', 'vpcConnectionProperties_vpcConnectionArn' - The Amazon Resource Name (ARN) for the VPC connection.
newVpcConnectionProperties ::
  -- | 'vpcConnectionArn'
  Prelude.Text ->
  VpcConnectionProperties
newVpcConnectionProperties pVpcConnectionArn_ =
  VpcConnectionProperties'
    { vpcConnectionArn =
        pVpcConnectionArn_
    }

-- | The Amazon Resource Name (ARN) for the VPC connection.
vpcConnectionProperties_vpcConnectionArn :: Lens.Lens' VpcConnectionProperties Prelude.Text
vpcConnectionProperties_vpcConnectionArn = Lens.lens (\VpcConnectionProperties' {vpcConnectionArn} -> vpcConnectionArn) (\s@VpcConnectionProperties' {} a -> s {vpcConnectionArn = a} :: VpcConnectionProperties)

instance Core.FromJSON VpcConnectionProperties where
  parseJSON =
    Core.withObject
      "VpcConnectionProperties"
      ( \x ->
          VpcConnectionProperties'
            Prelude.<$> (x Core..: "VpcConnectionArn")
      )

instance Prelude.Hashable VpcConnectionProperties

instance Prelude.NFData VpcConnectionProperties

instance Core.ToJSON VpcConnectionProperties where
  toJSON VpcConnectionProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VpcConnectionArn" Core..= vpcConnectionArn)
          ]
      )

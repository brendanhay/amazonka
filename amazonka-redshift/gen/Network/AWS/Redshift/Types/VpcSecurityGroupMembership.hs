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
-- Module      : Network.AWS.Redshift.Types.VpcSecurityGroupMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.VpcSecurityGroupMembership where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes the members of a VPC security group.
--
-- /See:/ 'newVpcSecurityGroupMembership' smart constructor.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership'
  { -- | The status of the VPC security group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC security group.
    vpcSecurityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcSecurityGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'vpcSecurityGroupMembership_status' - The status of the VPC security group.
--
-- 'vpcSecurityGroupId', 'vpcSecurityGroupMembership_vpcSecurityGroupId' - The identifier of the VPC security group.
newVpcSecurityGroupMembership ::
  VpcSecurityGroupMembership
newVpcSecurityGroupMembership =
  VpcSecurityGroupMembership'
    { status =
        Prelude.Nothing,
      vpcSecurityGroupId = Prelude.Nothing
    }

-- | The status of the VPC security group.
vpcSecurityGroupMembership_status :: Lens.Lens' VpcSecurityGroupMembership (Prelude.Maybe Prelude.Text)
vpcSecurityGroupMembership_status = Lens.lens (\VpcSecurityGroupMembership' {status} -> status) (\s@VpcSecurityGroupMembership' {} a -> s {status = a} :: VpcSecurityGroupMembership)

-- | The identifier of the VPC security group.
vpcSecurityGroupMembership_vpcSecurityGroupId :: Lens.Lens' VpcSecurityGroupMembership (Prelude.Maybe Prelude.Text)
vpcSecurityGroupMembership_vpcSecurityGroupId = Lens.lens (\VpcSecurityGroupMembership' {vpcSecurityGroupId} -> vpcSecurityGroupId) (\s@VpcSecurityGroupMembership' {} a -> s {vpcSecurityGroupId = a} :: VpcSecurityGroupMembership)

instance Core.FromXML VpcSecurityGroupMembership where
  parseXML x =
    VpcSecurityGroupMembership'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "VpcSecurityGroupId")

instance Prelude.Hashable VpcSecurityGroupMembership

instance Prelude.NFData VpcSecurityGroupMembership

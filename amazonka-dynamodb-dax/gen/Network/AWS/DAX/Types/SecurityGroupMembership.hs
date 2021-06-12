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
-- Module      : Network.AWS.DAX.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SecurityGroupMembership where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An individual VPC security group and its status.
--
-- /See:/ 'newSecurityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { -- | The status of this security group.
    status :: Core.Maybe Core.Text,
    -- | The unique ID for this security group.
    securityGroupIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SecurityGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'securityGroupMembership_status' - The status of this security group.
--
-- 'securityGroupIdentifier', 'securityGroupMembership_securityGroupIdentifier' - The unique ID for this security group.
newSecurityGroupMembership ::
  SecurityGroupMembership
newSecurityGroupMembership =
  SecurityGroupMembership'
    { status = Core.Nothing,
      securityGroupIdentifier = Core.Nothing
    }

-- | The status of this security group.
securityGroupMembership_status :: Lens.Lens' SecurityGroupMembership (Core.Maybe Core.Text)
securityGroupMembership_status = Lens.lens (\SecurityGroupMembership' {status} -> status) (\s@SecurityGroupMembership' {} a -> s {status = a} :: SecurityGroupMembership)

-- | The unique ID for this security group.
securityGroupMembership_securityGroupIdentifier :: Lens.Lens' SecurityGroupMembership (Core.Maybe Core.Text)
securityGroupMembership_securityGroupIdentifier = Lens.lens (\SecurityGroupMembership' {securityGroupIdentifier} -> securityGroupIdentifier) (\s@SecurityGroupMembership' {} a -> s {securityGroupIdentifier = a} :: SecurityGroupMembership)

instance Core.FromJSON SecurityGroupMembership where
  parseJSON =
    Core.withObject
      "SecurityGroupMembership"
      ( \x ->
          SecurityGroupMembership'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "SecurityGroupIdentifier")
      )

instance Core.Hashable SecurityGroupMembership

instance Core.NFData SecurityGroupMembership

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
-- Module      : Network.AWS.ElastiCache.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SecurityGroupMembership where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a single cache security group and its status.
--
-- /See:/ 'newSecurityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { -- | The status of the cache security group membership. The status changes
    -- whenever a cache security group is modified, or when the cache security
    -- groups assigned to a cluster are modified.
    status :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the cache security group.
    securityGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'securityGroupMembership_status' - The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cluster are modified.
--
-- 'securityGroupId', 'securityGroupMembership_securityGroupId' - The identifier of the cache security group.
newSecurityGroupMembership ::
  SecurityGroupMembership
newSecurityGroupMembership =
  SecurityGroupMembership'
    { status = Prelude.Nothing,
      securityGroupId = Prelude.Nothing
    }

-- | The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cluster are modified.
securityGroupMembership_status :: Lens.Lens' SecurityGroupMembership (Prelude.Maybe Prelude.Text)
securityGroupMembership_status = Lens.lens (\SecurityGroupMembership' {status} -> status) (\s@SecurityGroupMembership' {} a -> s {status = a} :: SecurityGroupMembership)

-- | The identifier of the cache security group.
securityGroupMembership_securityGroupId :: Lens.Lens' SecurityGroupMembership (Prelude.Maybe Prelude.Text)
securityGroupMembership_securityGroupId = Lens.lens (\SecurityGroupMembership' {securityGroupId} -> securityGroupId) (\s@SecurityGroupMembership' {} a -> s {securityGroupId = a} :: SecurityGroupMembership)

instance Prelude.FromXML SecurityGroupMembership where
  parseXML x =
    SecurityGroupMembership'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "SecurityGroupId")

instance Prelude.Hashable SecurityGroupMembership

instance Prelude.NFData SecurityGroupMembership

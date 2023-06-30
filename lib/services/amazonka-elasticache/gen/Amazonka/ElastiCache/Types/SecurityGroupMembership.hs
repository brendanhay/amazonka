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
-- Module      : Amazonka.ElastiCache.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.SecurityGroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a single cache security group and its status.
--
-- /See:/ 'newSecurityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { -- | The identifier of the cache security group.
    securityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The status of the cache security group membership. The status changes
    -- whenever a cache security group is modified, or when the cache security
    -- groups assigned to a cluster are modified.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupId', 'securityGroupMembership_securityGroupId' - The identifier of the cache security group.
--
-- 'status', 'securityGroupMembership_status' - The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cluster are modified.
newSecurityGroupMembership ::
  SecurityGroupMembership
newSecurityGroupMembership =
  SecurityGroupMembership'
    { securityGroupId =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The identifier of the cache security group.
securityGroupMembership_securityGroupId :: Lens.Lens' SecurityGroupMembership (Prelude.Maybe Prelude.Text)
securityGroupMembership_securityGroupId = Lens.lens (\SecurityGroupMembership' {securityGroupId} -> securityGroupId) (\s@SecurityGroupMembership' {} a -> s {securityGroupId = a} :: SecurityGroupMembership)

-- | The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cluster are modified.
securityGroupMembership_status :: Lens.Lens' SecurityGroupMembership (Prelude.Maybe Prelude.Text)
securityGroupMembership_status = Lens.lens (\SecurityGroupMembership' {status} -> status) (\s@SecurityGroupMembership' {} a -> s {status = a} :: SecurityGroupMembership)

instance Data.FromXML SecurityGroupMembership where
  parseXML x =
    SecurityGroupMembership'
      Prelude.<$> (x Data..@? "SecurityGroupId")
      Prelude.<*> (x Data..@? "Status")

instance Prelude.Hashable SecurityGroupMembership where
  hashWithSalt _salt SecurityGroupMembership' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupId
      `Prelude.hashWithSalt` status

instance Prelude.NFData SecurityGroupMembership where
  rnf SecurityGroupMembership' {..} =
    Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf status

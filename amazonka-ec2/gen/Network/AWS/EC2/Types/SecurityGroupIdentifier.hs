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
-- Module      : Network.AWS.EC2.Types.SecurityGroupIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupIdentifier where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a security group.
--
-- /See:/ 'newSecurityGroupIdentifier' smart constructor.
data SecurityGroupIdentifier = SecurityGroupIdentifier'
  { -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
    groupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'securityGroupIdentifier_groupName' - The name of the security group.
--
-- 'groupId', 'securityGroupIdentifier_groupId' - The ID of the security group.
newSecurityGroupIdentifier ::
  SecurityGroupIdentifier
newSecurityGroupIdentifier =
  SecurityGroupIdentifier'
    { groupName =
        Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | The name of the security group.
securityGroupIdentifier_groupName :: Lens.Lens' SecurityGroupIdentifier (Prelude.Maybe Prelude.Text)
securityGroupIdentifier_groupName = Lens.lens (\SecurityGroupIdentifier' {groupName} -> groupName) (\s@SecurityGroupIdentifier' {} a -> s {groupName = a} :: SecurityGroupIdentifier)

-- | The ID of the security group.
securityGroupIdentifier_groupId :: Lens.Lens' SecurityGroupIdentifier (Prelude.Maybe Prelude.Text)
securityGroupIdentifier_groupId = Lens.lens (\SecurityGroupIdentifier' {groupId} -> groupId) (\s@SecurityGroupIdentifier' {} a -> s {groupId = a} :: SecurityGroupIdentifier)

instance Prelude.FromXML SecurityGroupIdentifier where
  parseXML x =
    SecurityGroupIdentifier'
      Prelude.<$> (x Prelude..@? "groupName")
      Prelude.<*> (x Prelude..@? "groupId")

instance Prelude.Hashable SecurityGroupIdentifier

instance Prelude.NFData SecurityGroupIdentifier

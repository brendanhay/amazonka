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
-- Module      : Network.AWS.GuardDuty.Types.SecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.SecurityGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the security groups associated with the EC2
-- instance.
--
-- /See:/ 'newSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { -- | The security group name of the EC2 instance.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The security group ID of the EC2 instance.
    groupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'securityGroup_groupName' - The security group name of the EC2 instance.
--
-- 'groupId', 'securityGroup_groupId' - The security group ID of the EC2 instance.
newSecurityGroup ::
  SecurityGroup
newSecurityGroup =
  SecurityGroup'
    { groupName = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | The security group name of the EC2 instance.
securityGroup_groupName :: Lens.Lens' SecurityGroup (Prelude.Maybe Prelude.Text)
securityGroup_groupName = Lens.lens (\SecurityGroup' {groupName} -> groupName) (\s@SecurityGroup' {} a -> s {groupName = a} :: SecurityGroup)

-- | The security group ID of the EC2 instance.
securityGroup_groupId :: Lens.Lens' SecurityGroup (Prelude.Maybe Prelude.Text)
securityGroup_groupId = Lens.lens (\SecurityGroup' {groupId} -> groupId) (\s@SecurityGroup' {} a -> s {groupId = a} :: SecurityGroup)

instance Prelude.FromJSON SecurityGroup where
  parseJSON =
    Prelude.withObject
      "SecurityGroup"
      ( \x ->
          SecurityGroup'
            Prelude.<$> (x Prelude..:? "groupName")
            Prelude.<*> (x Prelude..:? "groupId")
      )

instance Prelude.Hashable SecurityGroup

instance Prelude.NFData SecurityGroup

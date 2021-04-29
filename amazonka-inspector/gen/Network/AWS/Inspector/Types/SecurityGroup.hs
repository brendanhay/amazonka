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
-- Module      : Network.AWS.Inspector.Types.SecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.SecurityGroup where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a security group associated with a network
-- interface. This data type is used as one of the elements of the
-- NetworkInterface data type.
--
-- /See:/ 'newSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { -- | The name of the security group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group.
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
-- 'groupName', 'securityGroup_groupName' - The name of the security group.
--
-- 'groupId', 'securityGroup_groupId' - The ID of the security group.
newSecurityGroup ::
  SecurityGroup
newSecurityGroup =
  SecurityGroup'
    { groupName = Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | The name of the security group.
securityGroup_groupName :: Lens.Lens' SecurityGroup (Prelude.Maybe Prelude.Text)
securityGroup_groupName = Lens.lens (\SecurityGroup' {groupName} -> groupName) (\s@SecurityGroup' {} a -> s {groupName = a} :: SecurityGroup)

-- | The ID of the security group.
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

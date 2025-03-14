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
-- Module      : Amazonka.GuardDuty.Types.SecurityGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.SecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the security groups associated with the EC2
-- instance.
--
-- /See:/ 'newSecurityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { -- | The security group ID of the EC2 instance.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The security group name of the EC2 instance.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'securityGroup_groupId' - The security group ID of the EC2 instance.
--
-- 'groupName', 'securityGroup_groupName' - The security group name of the EC2 instance.
newSecurityGroup ::
  SecurityGroup
newSecurityGroup =
  SecurityGroup'
    { groupId = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The security group ID of the EC2 instance.
securityGroup_groupId :: Lens.Lens' SecurityGroup (Prelude.Maybe Prelude.Text)
securityGroup_groupId = Lens.lens (\SecurityGroup' {groupId} -> groupId) (\s@SecurityGroup' {} a -> s {groupId = a} :: SecurityGroup)

-- | The security group name of the EC2 instance.
securityGroup_groupName :: Lens.Lens' SecurityGroup (Prelude.Maybe Prelude.Text)
securityGroup_groupName = Lens.lens (\SecurityGroup' {groupName} -> groupName) (\s@SecurityGroup' {} a -> s {groupName = a} :: SecurityGroup)

instance Data.FromJSON SecurityGroup where
  parseJSON =
    Data.withObject
      "SecurityGroup"
      ( \x ->
          SecurityGroup'
            Prelude.<$> (x Data..:? "groupId")
            Prelude.<*> (x Data..:? "groupName")
      )

instance Prelude.Hashable SecurityGroup where
  hashWithSalt _salt SecurityGroup' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData SecurityGroup where
  rnf SecurityGroup' {..} =
    Prelude.rnf groupId `Prelude.seq`
      Prelude.rnf groupName

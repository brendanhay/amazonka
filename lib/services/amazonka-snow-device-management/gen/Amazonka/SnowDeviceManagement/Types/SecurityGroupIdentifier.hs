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
-- Module      : Amazonka.SnowDeviceManagement.Types.SecurityGroupIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.SecurityGroupIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the device\'s security group.
--
-- /See:/ 'newSecurityGroupIdentifier' smart constructor.
data SecurityGroupIdentifier = SecurityGroupIdentifier'
  { -- | The security group name.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The security group ID.
    groupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'securityGroupIdentifier_groupName' - The security group name.
--
-- 'groupId', 'securityGroupIdentifier_groupId' - The security group ID.
newSecurityGroupIdentifier ::
  SecurityGroupIdentifier
newSecurityGroupIdentifier =
  SecurityGroupIdentifier'
    { groupName =
        Prelude.Nothing,
      groupId = Prelude.Nothing
    }

-- | The security group name.
securityGroupIdentifier_groupName :: Lens.Lens' SecurityGroupIdentifier (Prelude.Maybe Prelude.Text)
securityGroupIdentifier_groupName = Lens.lens (\SecurityGroupIdentifier' {groupName} -> groupName) (\s@SecurityGroupIdentifier' {} a -> s {groupName = a} :: SecurityGroupIdentifier)

-- | The security group ID.
securityGroupIdentifier_groupId :: Lens.Lens' SecurityGroupIdentifier (Prelude.Maybe Prelude.Text)
securityGroupIdentifier_groupId = Lens.lens (\SecurityGroupIdentifier' {groupId} -> groupId) (\s@SecurityGroupIdentifier' {} a -> s {groupId = a} :: SecurityGroupIdentifier)

instance Core.FromJSON SecurityGroupIdentifier where
  parseJSON =
    Core.withObject
      "SecurityGroupIdentifier"
      ( \x ->
          SecurityGroupIdentifier'
            Prelude.<$> (x Core..:? "groupName")
            Prelude.<*> (x Core..:? "groupId")
      )

instance Prelude.Hashable SecurityGroupIdentifier where
  hashWithSalt _salt SecurityGroupIdentifier' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData SecurityGroupIdentifier where
  rnf SecurityGroupIdentifier' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupId

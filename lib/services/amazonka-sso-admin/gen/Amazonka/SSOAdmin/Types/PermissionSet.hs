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
-- Module      : Amazonka.SSOAdmin.Types.PermissionSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.PermissionSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An entity that contains IAM policies.
--
-- /See:/ 'newPermissionSet' smart constructor.
data PermissionSet = PermissionSet'
  { -- | Used to redirect users within the application during the federation
    -- authentication process.
    relayState :: Prelude.Maybe Prelude.Text,
    -- | The length of time that the application user sessions are valid for in
    -- the ISO-8601 standard.
    sessionDuration :: Prelude.Maybe Prelude.Text,
    -- | The date that the permission set was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the permission set. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
    permissionSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the permission set.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the PermissionSet.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PermissionSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relayState', 'permissionSet_relayState' - Used to redirect users within the application during the federation
-- authentication process.
--
-- 'sessionDuration', 'permissionSet_sessionDuration' - The length of time that the application user sessions are valid for in
-- the ISO-8601 standard.
--
-- 'createdDate', 'permissionSet_createdDate' - The date that the permission set was created.
--
-- 'permissionSetArn', 'permissionSet_permissionSetArn' - The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
--
-- 'name', 'permissionSet_name' - The name of the permission set.
--
-- 'description', 'permissionSet_description' - The description of the PermissionSet.
newPermissionSet ::
  PermissionSet
newPermissionSet =
  PermissionSet'
    { relayState = Prelude.Nothing,
      sessionDuration = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      permissionSetArn = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Used to redirect users within the application during the federation
-- authentication process.
permissionSet_relayState :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_relayState = Lens.lens (\PermissionSet' {relayState} -> relayState) (\s@PermissionSet' {} a -> s {relayState = a} :: PermissionSet)

-- | The length of time that the application user sessions are valid for in
-- the ISO-8601 standard.
permissionSet_sessionDuration :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_sessionDuration = Lens.lens (\PermissionSet' {sessionDuration} -> sessionDuration) (\s@PermissionSet' {} a -> s {sessionDuration = a} :: PermissionSet)

-- | The date that the permission set was created.
permissionSet_createdDate :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.UTCTime)
permissionSet_createdDate = Lens.lens (\PermissionSet' {createdDate} -> createdDate) (\s@PermissionSet' {} a -> s {createdDate = a} :: PermissionSet) Prelude.. Lens.mapping Core._Time

-- | The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
permissionSet_permissionSetArn :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_permissionSetArn = Lens.lens (\PermissionSet' {permissionSetArn} -> permissionSetArn) (\s@PermissionSet' {} a -> s {permissionSetArn = a} :: PermissionSet)

-- | The name of the permission set.
permissionSet_name :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_name = Lens.lens (\PermissionSet' {name} -> name) (\s@PermissionSet' {} a -> s {name = a} :: PermissionSet)

-- | The description of the PermissionSet.
permissionSet_description :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_description = Lens.lens (\PermissionSet' {description} -> description) (\s@PermissionSet' {} a -> s {description = a} :: PermissionSet)

instance Core.FromJSON PermissionSet where
  parseJSON =
    Core.withObject
      "PermissionSet"
      ( \x ->
          PermissionSet'
            Prelude.<$> (x Core..:? "RelayState")
            Prelude.<*> (x Core..:? "SessionDuration")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "PermissionSetArn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable PermissionSet

instance Prelude.NFData PermissionSet

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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.PermissionSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An entity that contains IAM policies.
--
-- /See:/ 'newPermissionSet' smart constructor.
data PermissionSet = PermissionSet'
  { -- | The date that the permission set was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The description of the PermissionSet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the permission set.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the permission set. For more information about ARNs, see
    -- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
    -- in the /AWS General Reference/.
    permissionSetArn :: Prelude.Maybe Prelude.Text,
    -- | Used to redirect users within the application during the federation
    -- authentication process.
    relayState :: Prelude.Maybe Prelude.Text,
    -- | The length of time that the application user sessions are valid for in
    -- the ISO-8601 standard.
    sessionDuration :: Prelude.Maybe Prelude.Text
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
-- 'createdDate', 'permissionSet_createdDate' - The date that the permission set was created.
--
-- 'description', 'permissionSet_description' - The description of the PermissionSet.
--
-- 'name', 'permissionSet_name' - The name of the permission set.
--
-- 'permissionSetArn', 'permissionSet_permissionSetArn' - The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
--
-- 'relayState', 'permissionSet_relayState' - Used to redirect users within the application during the federation
-- authentication process.
--
-- 'sessionDuration', 'permissionSet_sessionDuration' - The length of time that the application user sessions are valid for in
-- the ISO-8601 standard.
newPermissionSet ::
  PermissionSet
newPermissionSet =
  PermissionSet'
    { createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      permissionSetArn = Prelude.Nothing,
      relayState = Prelude.Nothing,
      sessionDuration = Prelude.Nothing
    }

-- | The date that the permission set was created.
permissionSet_createdDate :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.UTCTime)
permissionSet_createdDate = Lens.lens (\PermissionSet' {createdDate} -> createdDate) (\s@PermissionSet' {} a -> s {createdDate = a} :: PermissionSet) Prelude.. Lens.mapping Data._Time

-- | The description of the PermissionSet.
permissionSet_description :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_description = Lens.lens (\PermissionSet' {description} -> description) (\s@PermissionSet' {} a -> s {description = a} :: PermissionSet)

-- | The name of the permission set.
permissionSet_name :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_name = Lens.lens (\PermissionSet' {name} -> name) (\s@PermissionSet' {} a -> s {name = a} :: PermissionSet)

-- | The ARN of the permission set. For more information about ARNs, see
-- </general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>
-- in the /AWS General Reference/.
permissionSet_permissionSetArn :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_permissionSetArn = Lens.lens (\PermissionSet' {permissionSetArn} -> permissionSetArn) (\s@PermissionSet' {} a -> s {permissionSetArn = a} :: PermissionSet)

-- | Used to redirect users within the application during the federation
-- authentication process.
permissionSet_relayState :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_relayState = Lens.lens (\PermissionSet' {relayState} -> relayState) (\s@PermissionSet' {} a -> s {relayState = a} :: PermissionSet)

-- | The length of time that the application user sessions are valid for in
-- the ISO-8601 standard.
permissionSet_sessionDuration :: Lens.Lens' PermissionSet (Prelude.Maybe Prelude.Text)
permissionSet_sessionDuration = Lens.lens (\PermissionSet' {sessionDuration} -> sessionDuration) (\s@PermissionSet' {} a -> s {sessionDuration = a} :: PermissionSet)

instance Data.FromJSON PermissionSet where
  parseJSON =
    Data.withObject
      "PermissionSet"
      ( \x ->
          PermissionSet'
            Prelude.<$> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PermissionSetArn")
            Prelude.<*> (x Data..:? "RelayState")
            Prelude.<*> (x Data..:? "SessionDuration")
      )

instance Prelude.Hashable PermissionSet where
  hashWithSalt _salt PermissionSet' {..} =
    _salt `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permissionSetArn
      `Prelude.hashWithSalt` relayState
      `Prelude.hashWithSalt` sessionDuration

instance Prelude.NFData PermissionSet where
  rnf PermissionSet' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf permissionSetArn
      `Prelude.seq` Prelude.rnf relayState
      `Prelude.seq` Prelude.rnf sessionDuration

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
-- Module      : Amazonka.OpsWorks.Types.UserProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.UserProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a user\'s SSH information.
--
-- /See:/ 'newUserProfile' smart constructor.
data UserProfile = UserProfile'
  { -- | The user\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The user\'s IAM ARN.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | The user\'s SSH public key.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The user\'s SSH user name.
    sshUsername :: Prelude.Maybe Prelude.Text,
    -- | Whether users can specify their own SSH public key through the My
    -- Settings page. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
    allowSelfManagement :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'userProfile_name' - The user\'s name.
--
-- 'iamUserArn', 'userProfile_iamUserArn' - The user\'s IAM ARN.
--
-- 'sshPublicKey', 'userProfile_sshPublicKey' - The user\'s SSH public key.
--
-- 'sshUsername', 'userProfile_sshUsername' - The user\'s SSH user name.
--
-- 'allowSelfManagement', 'userProfile_allowSelfManagement' - Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
newUserProfile ::
  UserProfile
newUserProfile =
  UserProfile'
    { name = Prelude.Nothing,
      iamUserArn = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      sshUsername = Prelude.Nothing,
      allowSelfManagement = Prelude.Nothing
    }

-- | The user\'s name.
userProfile_name :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_name = Lens.lens (\UserProfile' {name} -> name) (\s@UserProfile' {} a -> s {name = a} :: UserProfile)

-- | The user\'s IAM ARN.
userProfile_iamUserArn :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_iamUserArn = Lens.lens (\UserProfile' {iamUserArn} -> iamUserArn) (\s@UserProfile' {} a -> s {iamUserArn = a} :: UserProfile)

-- | The user\'s SSH public key.
userProfile_sshPublicKey :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_sshPublicKey = Lens.lens (\UserProfile' {sshPublicKey} -> sshPublicKey) (\s@UserProfile' {} a -> s {sshPublicKey = a} :: UserProfile)

-- | The user\'s SSH user name.
userProfile_sshUsername :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_sshUsername = Lens.lens (\UserProfile' {sshUsername} -> sshUsername) (\s@UserProfile' {} a -> s {sshUsername = a} :: UserProfile)

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
userProfile_allowSelfManagement :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Bool)
userProfile_allowSelfManagement = Lens.lens (\UserProfile' {allowSelfManagement} -> allowSelfManagement) (\s@UserProfile' {} a -> s {allowSelfManagement = a} :: UserProfile)

instance Data.FromJSON UserProfile where
  parseJSON =
    Data.withObject
      "UserProfile"
      ( \x ->
          UserProfile'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "IamUserArn")
            Prelude.<*> (x Data..:? "SshPublicKey")
            Prelude.<*> (x Data..:? "SshUsername")
            Prelude.<*> (x Data..:? "AllowSelfManagement")
      )

instance Prelude.Hashable UserProfile where
  hashWithSalt _salt UserProfile' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` iamUserArn
      `Prelude.hashWithSalt` sshPublicKey
      `Prelude.hashWithSalt` sshUsername
      `Prelude.hashWithSalt` allowSelfManagement

instance Prelude.NFData UserProfile where
  rnf UserProfile' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf iamUserArn
      `Prelude.seq` Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf sshUsername
      `Prelude.seq` Prelude.rnf allowSelfManagement

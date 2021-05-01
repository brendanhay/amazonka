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
-- Module      : Network.AWS.OpsWorks.Types.UserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.UserProfile where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a user\'s SSH information.
--
-- /See:/ 'newUserProfile' smart constructor.
data UserProfile = UserProfile'
  { -- | The user\'s IAM ARN.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | Whether users can specify their own SSH public key through the My
    -- Settings page. For more information, see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
    allowSelfManagement :: Prelude.Maybe Prelude.Bool,
    -- | The user\'s SSH user name.
    sshUsername :: Prelude.Maybe Prelude.Text,
    -- | The user\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The user\'s SSH public key.
    sshPublicKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iamUserArn', 'userProfile_iamUserArn' - The user\'s IAM ARN.
--
-- 'allowSelfManagement', 'userProfile_allowSelfManagement' - Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
--
-- 'sshUsername', 'userProfile_sshUsername' - The user\'s SSH user name.
--
-- 'name', 'userProfile_name' - The user\'s name.
--
-- 'sshPublicKey', 'userProfile_sshPublicKey' - The user\'s SSH public key.
newUserProfile ::
  UserProfile
newUserProfile =
  UserProfile'
    { iamUserArn = Prelude.Nothing,
      allowSelfManagement = Prelude.Nothing,
      sshUsername = Prelude.Nothing,
      name = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing
    }

-- | The user\'s IAM ARN.
userProfile_iamUserArn :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_iamUserArn = Lens.lens (\UserProfile' {iamUserArn} -> iamUserArn) (\s@UserProfile' {} a -> s {iamUserArn = a} :: UserProfile)

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
userProfile_allowSelfManagement :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Bool)
userProfile_allowSelfManagement = Lens.lens (\UserProfile' {allowSelfManagement} -> allowSelfManagement) (\s@UserProfile' {} a -> s {allowSelfManagement = a} :: UserProfile)

-- | The user\'s SSH user name.
userProfile_sshUsername :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_sshUsername = Lens.lens (\UserProfile' {sshUsername} -> sshUsername) (\s@UserProfile' {} a -> s {sshUsername = a} :: UserProfile)

-- | The user\'s name.
userProfile_name :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_name = Lens.lens (\UserProfile' {name} -> name) (\s@UserProfile' {} a -> s {name = a} :: UserProfile)

-- | The user\'s SSH public key.
userProfile_sshPublicKey :: Lens.Lens' UserProfile (Prelude.Maybe Prelude.Text)
userProfile_sshPublicKey = Lens.lens (\UserProfile' {sshPublicKey} -> sshPublicKey) (\s@UserProfile' {} a -> s {sshPublicKey = a} :: UserProfile)

instance Prelude.FromJSON UserProfile where
  parseJSON =
    Prelude.withObject
      "UserProfile"
      ( \x ->
          UserProfile'
            Prelude.<$> (x Prelude..:? "IamUserArn")
            Prelude.<*> (x Prelude..:? "AllowSelfManagement")
            Prelude.<*> (x Prelude..:? "SshUsername")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "SshPublicKey")
      )

instance Prelude.Hashable UserProfile

instance Prelude.NFData UserProfile

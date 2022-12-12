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
-- Module      : Amazonka.CodeStar.Types.UserProfileSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Types.UserProfileSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a user\'s profile in AWS CodeStar.
--
-- /See:/ 'newUserProfileSummary' smart constructor.
data UserProfileSummary = UserProfileSummary'
  { -- | The display name of a user in AWS CodeStar. For example, this could be
    -- set to both first and last name (\"Mary Major\") or a single name
    -- (\"Mary\"). The display name is also used to generate the initial icon
    -- associated with the user in AWS CodeStar projects. If spaces are
    -- included in the display name, the first character that appears after the
    -- space will be used as the second character in the user initial icon. The
    -- initial icon displays a maximum of two characters, so a display name
    -- with more than one space (for example \"Mary Jane Major\") would
    -- generate an initial icon using the first character and the first
    -- character after the space (\"MJ\", not \"MM\").
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The email address associated with the user.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The SSH public key associated with the user in AWS CodeStar. If a
    -- project owner allows the user remote access to project resources, this
    -- public key will be used along with the user\'s private key for SSH
    -- access.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'userProfileSummary_displayName' - The display name of a user in AWS CodeStar. For example, this could be
-- set to both first and last name (\"Mary Major\") or a single name
-- (\"Mary\"). The display name is also used to generate the initial icon
-- associated with the user in AWS CodeStar projects. If spaces are
-- included in the display name, the first character that appears after the
-- space will be used as the second character in the user initial icon. The
-- initial icon displays a maximum of two characters, so a display name
-- with more than one space (for example \"Mary Jane Major\") would
-- generate an initial icon using the first character and the first
-- character after the space (\"MJ\", not \"MM\").
--
-- 'emailAddress', 'userProfileSummary_emailAddress' - The email address associated with the user.
--
-- 'sshPublicKey', 'userProfileSummary_sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
--
-- 'userArn', 'userProfileSummary_userArn' - The Amazon Resource Name (ARN) of the user in IAM.
newUserProfileSummary ::
  UserProfileSummary
newUserProfileSummary =
  UserProfileSummary'
    { displayName = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      sshPublicKey = Prelude.Nothing,
      userArn = Prelude.Nothing
    }

-- | The display name of a user in AWS CodeStar. For example, this could be
-- set to both first and last name (\"Mary Major\") or a single name
-- (\"Mary\"). The display name is also used to generate the initial icon
-- associated with the user in AWS CodeStar projects. If spaces are
-- included in the display name, the first character that appears after the
-- space will be used as the second character in the user initial icon. The
-- initial icon displays a maximum of two characters, so a display name
-- with more than one space (for example \"Mary Jane Major\") would
-- generate an initial icon using the first character and the first
-- character after the space (\"MJ\", not \"MM\").
userProfileSummary_displayName :: Lens.Lens' UserProfileSummary (Prelude.Maybe Prelude.Text)
userProfileSummary_displayName = Lens.lens (\UserProfileSummary' {displayName} -> displayName) (\s@UserProfileSummary' {} a -> s {displayName = a} :: UserProfileSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The email address associated with the user.
userProfileSummary_emailAddress :: Lens.Lens' UserProfileSummary (Prelude.Maybe Prelude.Text)
userProfileSummary_emailAddress = Lens.lens (\UserProfileSummary' {emailAddress} -> emailAddress) (\s@UserProfileSummary' {} a -> s {emailAddress = a} :: UserProfileSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The SSH public key associated with the user in AWS CodeStar. If a
-- project owner allows the user remote access to project resources, this
-- public key will be used along with the user\'s private key for SSH
-- access.
userProfileSummary_sshPublicKey :: Lens.Lens' UserProfileSummary (Prelude.Maybe Prelude.Text)
userProfileSummary_sshPublicKey = Lens.lens (\UserProfileSummary' {sshPublicKey} -> sshPublicKey) (\s@UserProfileSummary' {} a -> s {sshPublicKey = a} :: UserProfileSummary)

-- | The Amazon Resource Name (ARN) of the user in IAM.
userProfileSummary_userArn :: Lens.Lens' UserProfileSummary (Prelude.Maybe Prelude.Text)
userProfileSummary_userArn = Lens.lens (\UserProfileSummary' {userArn} -> userArn) (\s@UserProfileSummary' {} a -> s {userArn = a} :: UserProfileSummary)

instance Data.FromJSON UserProfileSummary where
  parseJSON =
    Data.withObject
      "UserProfileSummary"
      ( \x ->
          UserProfileSummary'
            Prelude.<$> (x Data..:? "displayName")
            Prelude.<*> (x Data..:? "emailAddress")
            Prelude.<*> (x Data..:? "sshPublicKey")
            Prelude.<*> (x Data..:? "userArn")
      )

instance Prelude.Hashable UserProfileSummary where
  hashWithSalt _salt UserProfileSummary' {..} =
    _salt `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` sshPublicKey
      `Prelude.hashWithSalt` userArn

instance Prelude.NFData UserProfileSummary where
  rnf UserProfileSummary' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf sshPublicKey
      `Prelude.seq` Prelude.rnf userArn

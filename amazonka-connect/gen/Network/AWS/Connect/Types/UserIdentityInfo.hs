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
-- Module      : Network.AWS.Connect.Types.UserIdentityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserIdentityInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the identity of a user.
--
-- /See:/ 'newUserIdentityInfo' smart constructor.
data UserIdentityInfo = UserIdentityInfo'
  { -- | The email address. If you are using SAML for identity management and
    -- include this parameter, an error is returned.
    email :: Prelude.Maybe Prelude.Text,
    -- | The first name. This is required if you are using Amazon Connect or SAML
    -- for identity management.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The last name. This is required if you are using Amazon Connect or SAML
    -- for identity management.
    lastName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserIdentityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'email', 'userIdentityInfo_email' - The email address. If you are using SAML for identity management and
-- include this parameter, an error is returned.
--
-- 'firstName', 'userIdentityInfo_firstName' - The first name. This is required if you are using Amazon Connect or SAML
-- for identity management.
--
-- 'lastName', 'userIdentityInfo_lastName' - The last name. This is required if you are using Amazon Connect or SAML
-- for identity management.
newUserIdentityInfo ::
  UserIdentityInfo
newUserIdentityInfo =
  UserIdentityInfo'
    { email = Prelude.Nothing,
      firstName = Prelude.Nothing,
      lastName = Prelude.Nothing
    }

-- | The email address. If you are using SAML for identity management and
-- include this parameter, an error is returned.
userIdentityInfo_email :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_email = Lens.lens (\UserIdentityInfo' {email} -> email) (\s@UserIdentityInfo' {} a -> s {email = a} :: UserIdentityInfo)

-- | The first name. This is required if you are using Amazon Connect or SAML
-- for identity management.
userIdentityInfo_firstName :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_firstName = Lens.lens (\UserIdentityInfo' {firstName} -> firstName) (\s@UserIdentityInfo' {} a -> s {firstName = a} :: UserIdentityInfo)

-- | The last name. This is required if you are using Amazon Connect or SAML
-- for identity management.
userIdentityInfo_lastName :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_lastName = Lens.lens (\UserIdentityInfo' {lastName} -> lastName) (\s@UserIdentityInfo' {} a -> s {lastName = a} :: UserIdentityInfo)

instance Prelude.FromJSON UserIdentityInfo where
  parseJSON =
    Prelude.withObject
      "UserIdentityInfo"
      ( \x ->
          UserIdentityInfo'
            Prelude.<$> (x Prelude..:? "Email")
            Prelude.<*> (x Prelude..:? "FirstName")
            Prelude.<*> (x Prelude..:? "LastName")
      )

instance Prelude.Hashable UserIdentityInfo

instance Prelude.NFData UserIdentityInfo

instance Prelude.ToJSON UserIdentityInfo where
  toJSON UserIdentityInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Email" Prelude..=) Prelude.<$> email,
            ("FirstName" Prelude..=) Prelude.<$> firstName,
            ("LastName" Prelude..=) Prelude.<$> lastName
          ]
      )

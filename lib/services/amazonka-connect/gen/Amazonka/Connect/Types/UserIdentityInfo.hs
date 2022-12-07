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
-- Module      : Amazonka.Connect.Types.UserIdentityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserIdentityInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the identity of a user.
--
-- /See:/ 'newUserIdentityInfo' smart constructor.
data UserIdentityInfo = UserIdentityInfo'
  { -- | The first name. This is required if you are using Amazon Connect or SAML
    -- for identity management.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The email address. If you are using SAML for identity management and
    -- include this parameter, an error is returned.
    email :: Prelude.Maybe Prelude.Text,
    -- | The user\'s secondary email address. If you provide a secondary email,
    -- the user receives email notifications - other than password reset
    -- notifications - to this email address instead of to their primary email
    -- address.
    --
    -- Pattern:
    -- @(?=^.{0,265}$)[a-zA-Z0-9._%+-]+\@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}@
    secondaryEmail :: Prelude.Maybe Prelude.Text,
    -- | The last name. This is required if you are using Amazon Connect or SAML
    -- for identity management.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The user\'s mobile number.
    mobile :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserIdentityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firstName', 'userIdentityInfo_firstName' - The first name. This is required if you are using Amazon Connect or SAML
-- for identity management.
--
-- 'email', 'userIdentityInfo_email' - The email address. If you are using SAML for identity management and
-- include this parameter, an error is returned.
--
-- 'secondaryEmail', 'userIdentityInfo_secondaryEmail' - The user\'s secondary email address. If you provide a secondary email,
-- the user receives email notifications - other than password reset
-- notifications - to this email address instead of to their primary email
-- address.
--
-- Pattern:
-- @(?=^.{0,265}$)[a-zA-Z0-9._%+-]+\@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}@
--
-- 'lastName', 'userIdentityInfo_lastName' - The last name. This is required if you are using Amazon Connect or SAML
-- for identity management.
--
-- 'mobile', 'userIdentityInfo_mobile' - The user\'s mobile number.
newUserIdentityInfo ::
  UserIdentityInfo
newUserIdentityInfo =
  UserIdentityInfo'
    { firstName = Prelude.Nothing,
      email = Prelude.Nothing,
      secondaryEmail = Prelude.Nothing,
      lastName = Prelude.Nothing,
      mobile = Prelude.Nothing
    }

-- | The first name. This is required if you are using Amazon Connect or SAML
-- for identity management.
userIdentityInfo_firstName :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_firstName = Lens.lens (\UserIdentityInfo' {firstName} -> firstName) (\s@UserIdentityInfo' {} a -> s {firstName = a} :: UserIdentityInfo)

-- | The email address. If you are using SAML for identity management and
-- include this parameter, an error is returned.
userIdentityInfo_email :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_email = Lens.lens (\UserIdentityInfo' {email} -> email) (\s@UserIdentityInfo' {} a -> s {email = a} :: UserIdentityInfo)

-- | The user\'s secondary email address. If you provide a secondary email,
-- the user receives email notifications - other than password reset
-- notifications - to this email address instead of to their primary email
-- address.
--
-- Pattern:
-- @(?=^.{0,265}$)[a-zA-Z0-9._%+-]+\@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,63}@
userIdentityInfo_secondaryEmail :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_secondaryEmail = Lens.lens (\UserIdentityInfo' {secondaryEmail} -> secondaryEmail) (\s@UserIdentityInfo' {} a -> s {secondaryEmail = a} :: UserIdentityInfo)

-- | The last name. This is required if you are using Amazon Connect or SAML
-- for identity management.
userIdentityInfo_lastName :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_lastName = Lens.lens (\UserIdentityInfo' {lastName} -> lastName) (\s@UserIdentityInfo' {} a -> s {lastName = a} :: UserIdentityInfo)

-- | The user\'s mobile number.
userIdentityInfo_mobile :: Lens.Lens' UserIdentityInfo (Prelude.Maybe Prelude.Text)
userIdentityInfo_mobile = Lens.lens (\UserIdentityInfo' {mobile} -> mobile) (\s@UserIdentityInfo' {} a -> s {mobile = a} :: UserIdentityInfo)

instance Data.FromJSON UserIdentityInfo where
  parseJSON =
    Data.withObject
      "UserIdentityInfo"
      ( \x ->
          UserIdentityInfo'
            Prelude.<$> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "SecondaryEmail")
            Prelude.<*> (x Data..:? "LastName")
            Prelude.<*> (x Data..:? "Mobile")
      )

instance Prelude.Hashable UserIdentityInfo where
  hashWithSalt _salt UserIdentityInfo' {..} =
    _salt `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` secondaryEmail
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` mobile

instance Prelude.NFData UserIdentityInfo where
  rnf UserIdentityInfo' {..} =
    Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf secondaryEmail
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf mobile

instance Data.ToJSON UserIdentityInfo where
  toJSON UserIdentityInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FirstName" Data..=) Prelude.<$> firstName,
            ("Email" Data..=) Prelude.<$> email,
            ("SecondaryEmail" Data..=)
              Prelude.<$> secondaryEmail,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("Mobile" Data..=) Prelude.<$> mobile
          ]
      )

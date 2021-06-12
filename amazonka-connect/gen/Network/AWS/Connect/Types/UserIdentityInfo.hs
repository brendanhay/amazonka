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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the identity of a user.
--
-- /See:/ 'newUserIdentityInfo' smart constructor.
data UserIdentityInfo = UserIdentityInfo'
  { -- | The email address. If you are using SAML for identity management and
    -- include this parameter, an error is returned.
    email :: Core.Maybe Core.Text,
    -- | The first name. This is required if you are using Amazon Connect or SAML
    -- for identity management.
    firstName :: Core.Maybe Core.Text,
    -- | The last name. This is required if you are using Amazon Connect or SAML
    -- for identity management.
    lastName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { email = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing
    }

-- | The email address. If you are using SAML for identity management and
-- include this parameter, an error is returned.
userIdentityInfo_email :: Lens.Lens' UserIdentityInfo (Core.Maybe Core.Text)
userIdentityInfo_email = Lens.lens (\UserIdentityInfo' {email} -> email) (\s@UserIdentityInfo' {} a -> s {email = a} :: UserIdentityInfo)

-- | The first name. This is required if you are using Amazon Connect or SAML
-- for identity management.
userIdentityInfo_firstName :: Lens.Lens' UserIdentityInfo (Core.Maybe Core.Text)
userIdentityInfo_firstName = Lens.lens (\UserIdentityInfo' {firstName} -> firstName) (\s@UserIdentityInfo' {} a -> s {firstName = a} :: UserIdentityInfo)

-- | The last name. This is required if you are using Amazon Connect or SAML
-- for identity management.
userIdentityInfo_lastName :: Lens.Lens' UserIdentityInfo (Core.Maybe Core.Text)
userIdentityInfo_lastName = Lens.lens (\UserIdentityInfo' {lastName} -> lastName) (\s@UserIdentityInfo' {} a -> s {lastName = a} :: UserIdentityInfo)

instance Core.FromJSON UserIdentityInfo where
  parseJSON =
    Core.withObject
      "UserIdentityInfo"
      ( \x ->
          UserIdentityInfo'
            Core.<$> (x Core..:? "Email")
            Core.<*> (x Core..:? "FirstName")
            Core.<*> (x Core..:? "LastName")
      )

instance Core.Hashable UserIdentityInfo

instance Core.NFData UserIdentityInfo

instance Core.ToJSON UserIdentityInfo where
  toJSON UserIdentityInfo' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Email" Core..=) Core.<$> email,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName
          ]
      )

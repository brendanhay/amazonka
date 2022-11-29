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
-- Module      : Amazonka.Grafana.Types.AssertionAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.AssertionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that defines which attributes in the IdP assertion are to be
-- used to define information about the users authenticated by the IdP to
-- use the workspace.
--
-- /See:/ 'newAssertionAttributes' smart constructor.
data AssertionAttributes = AssertionAttributes'
  { -- | The name of the attribute within the SAML assertion to use as the user
    -- full \"friendly\" names for SAML users.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute within the SAML assertion to use as the email
    -- names for SAML users.
    email :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute within the SAML assertion to use as the login
    -- names for SAML users.
    login :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute within the SAML assertion to use as the user
    -- full \"friendly\" names for the users\' organizations.
    org :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute within the SAML assertion to use as the user
    -- roles.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute within the SAML assertion to use as the user
    -- full \"friendly\" names for user groups.
    groups :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssertionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'assertionAttributes_name' - The name of the attribute within the SAML assertion to use as the user
-- full \"friendly\" names for SAML users.
--
-- 'email', 'assertionAttributes_email' - The name of the attribute within the SAML assertion to use as the email
-- names for SAML users.
--
-- 'login', 'assertionAttributes_login' - The name of the attribute within the SAML assertion to use as the login
-- names for SAML users.
--
-- 'org', 'assertionAttributes_org' - The name of the attribute within the SAML assertion to use as the user
-- full \"friendly\" names for the users\' organizations.
--
-- 'role'', 'assertionAttributes_role' - The name of the attribute within the SAML assertion to use as the user
-- roles.
--
-- 'groups', 'assertionAttributes_groups' - The name of the attribute within the SAML assertion to use as the user
-- full \"friendly\" names for user groups.
newAssertionAttributes ::
  AssertionAttributes
newAssertionAttributes =
  AssertionAttributes'
    { name = Prelude.Nothing,
      email = Prelude.Nothing,
      login = Prelude.Nothing,
      org = Prelude.Nothing,
      role' = Prelude.Nothing,
      groups = Prelude.Nothing
    }

-- | The name of the attribute within the SAML assertion to use as the user
-- full \"friendly\" names for SAML users.
assertionAttributes_name :: Lens.Lens' AssertionAttributes (Prelude.Maybe Prelude.Text)
assertionAttributes_name = Lens.lens (\AssertionAttributes' {name} -> name) (\s@AssertionAttributes' {} a -> s {name = a} :: AssertionAttributes)

-- | The name of the attribute within the SAML assertion to use as the email
-- names for SAML users.
assertionAttributes_email :: Lens.Lens' AssertionAttributes (Prelude.Maybe Prelude.Text)
assertionAttributes_email = Lens.lens (\AssertionAttributes' {email} -> email) (\s@AssertionAttributes' {} a -> s {email = a} :: AssertionAttributes)

-- | The name of the attribute within the SAML assertion to use as the login
-- names for SAML users.
assertionAttributes_login :: Lens.Lens' AssertionAttributes (Prelude.Maybe Prelude.Text)
assertionAttributes_login = Lens.lens (\AssertionAttributes' {login} -> login) (\s@AssertionAttributes' {} a -> s {login = a} :: AssertionAttributes)

-- | The name of the attribute within the SAML assertion to use as the user
-- full \"friendly\" names for the users\' organizations.
assertionAttributes_org :: Lens.Lens' AssertionAttributes (Prelude.Maybe Prelude.Text)
assertionAttributes_org = Lens.lens (\AssertionAttributes' {org} -> org) (\s@AssertionAttributes' {} a -> s {org = a} :: AssertionAttributes)

-- | The name of the attribute within the SAML assertion to use as the user
-- roles.
assertionAttributes_role :: Lens.Lens' AssertionAttributes (Prelude.Maybe Prelude.Text)
assertionAttributes_role = Lens.lens (\AssertionAttributes' {role'} -> role') (\s@AssertionAttributes' {} a -> s {role' = a} :: AssertionAttributes)

-- | The name of the attribute within the SAML assertion to use as the user
-- full \"friendly\" names for user groups.
assertionAttributes_groups :: Lens.Lens' AssertionAttributes (Prelude.Maybe Prelude.Text)
assertionAttributes_groups = Lens.lens (\AssertionAttributes' {groups} -> groups) (\s@AssertionAttributes' {} a -> s {groups = a} :: AssertionAttributes)

instance Core.FromJSON AssertionAttributes where
  parseJSON =
    Core.withObject
      "AssertionAttributes"
      ( \x ->
          AssertionAttributes'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "email")
            Prelude.<*> (x Core..:? "login")
            Prelude.<*> (x Core..:? "org")
            Prelude.<*> (x Core..:? "role")
            Prelude.<*> (x Core..:? "groups")
      )

instance Prelude.Hashable AssertionAttributes where
  hashWithSalt _salt AssertionAttributes' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` login
      `Prelude.hashWithSalt` org
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` groups

instance Prelude.NFData AssertionAttributes where
  rnf AssertionAttributes' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf login
      `Prelude.seq` Prelude.rnf org
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf groups

instance Core.ToJSON AssertionAttributes where
  toJSON AssertionAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("email" Core..=) Prelude.<$> email,
            ("login" Core..=) Prelude.<$> login,
            ("org" Core..=) Prelude.<$> org,
            ("role" Core..=) Prelude.<$> role',
            ("groups" Core..=) Prelude.<$> groups
          ]
      )

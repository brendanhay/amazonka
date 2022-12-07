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
-- Module      : Amazonka.OpenSearch.Types.SAMLOptionsInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.SAMLOptionsInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.SAMLIdp
import qualified Amazonka.Prelude as Prelude

-- | The SAML authentication configuration for an Amazon OpenSearch Service
-- domain.
--
-- /See:/ 'newSAMLOptionsInput' smart constructor.
data SAMLOptionsInput = SAMLOptionsInput'
  { -- | The SAML master user name, which is stored in the domain\'s internal
    -- user database.
    masterUserName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | True to enable SAML authentication for a domain.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The duration, in minutes, after which a user session becomes inactive.
    -- Acceptable values are between 1 and 1440, and the default value is 60.
    sessionTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | Element of the SAML assertion to use for backend roles. Default is
    -- @roles@.
    rolesKey :: Prelude.Maybe Prelude.Text,
    -- | The backend role that the SAML master user is mapped to.
    masterBackendRole :: Prelude.Maybe Prelude.Text,
    -- | The SAML Identity Provider\'s information.
    idp :: Prelude.Maybe SAMLIdp,
    -- | Element of the SAML assertion to use for the user name. Default is
    -- @NameID@.
    subjectKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAMLOptionsInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterUserName', 'sAMLOptionsInput_masterUserName' - The SAML master user name, which is stored in the domain\'s internal
-- user database.
--
-- 'enabled', 'sAMLOptionsInput_enabled' - True to enable SAML authentication for a domain.
--
-- 'sessionTimeoutMinutes', 'sAMLOptionsInput_sessionTimeoutMinutes' - The duration, in minutes, after which a user session becomes inactive.
-- Acceptable values are between 1 and 1440, and the default value is 60.
--
-- 'rolesKey', 'sAMLOptionsInput_rolesKey' - Element of the SAML assertion to use for backend roles. Default is
-- @roles@.
--
-- 'masterBackendRole', 'sAMLOptionsInput_masterBackendRole' - The backend role that the SAML master user is mapped to.
--
-- 'idp', 'sAMLOptionsInput_idp' - The SAML Identity Provider\'s information.
--
-- 'subjectKey', 'sAMLOptionsInput_subjectKey' - Element of the SAML assertion to use for the user name. Default is
-- @NameID@.
newSAMLOptionsInput ::
  SAMLOptionsInput
newSAMLOptionsInput =
  SAMLOptionsInput'
    { masterUserName = Prelude.Nothing,
      enabled = Prelude.Nothing,
      sessionTimeoutMinutes = Prelude.Nothing,
      rolesKey = Prelude.Nothing,
      masterBackendRole = Prelude.Nothing,
      idp = Prelude.Nothing,
      subjectKey = Prelude.Nothing
    }

-- | The SAML master user name, which is stored in the domain\'s internal
-- user database.
sAMLOptionsInput_masterUserName :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_masterUserName = Lens.lens (\SAMLOptionsInput' {masterUserName} -> masterUserName) (\s@SAMLOptionsInput' {} a -> s {masterUserName = a} :: SAMLOptionsInput) Prelude.. Lens.mapping Data._Sensitive

-- | True to enable SAML authentication for a domain.
sAMLOptionsInput_enabled :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Bool)
sAMLOptionsInput_enabled = Lens.lens (\SAMLOptionsInput' {enabled} -> enabled) (\s@SAMLOptionsInput' {} a -> s {enabled = a} :: SAMLOptionsInput)

-- | The duration, in minutes, after which a user session becomes inactive.
-- Acceptable values are between 1 and 1440, and the default value is 60.
sAMLOptionsInput_sessionTimeoutMinutes :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Int)
sAMLOptionsInput_sessionTimeoutMinutes = Lens.lens (\SAMLOptionsInput' {sessionTimeoutMinutes} -> sessionTimeoutMinutes) (\s@SAMLOptionsInput' {} a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsInput)

-- | Element of the SAML assertion to use for backend roles. Default is
-- @roles@.
sAMLOptionsInput_rolesKey :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_rolesKey = Lens.lens (\SAMLOptionsInput' {rolesKey} -> rolesKey) (\s@SAMLOptionsInput' {} a -> s {rolesKey = a} :: SAMLOptionsInput)

-- | The backend role that the SAML master user is mapped to.
sAMLOptionsInput_masterBackendRole :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_masterBackendRole = Lens.lens (\SAMLOptionsInput' {masterBackendRole} -> masterBackendRole) (\s@SAMLOptionsInput' {} a -> s {masterBackendRole = a} :: SAMLOptionsInput)

-- | The SAML Identity Provider\'s information.
sAMLOptionsInput_idp :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe SAMLIdp)
sAMLOptionsInput_idp = Lens.lens (\SAMLOptionsInput' {idp} -> idp) (\s@SAMLOptionsInput' {} a -> s {idp = a} :: SAMLOptionsInput)

-- | Element of the SAML assertion to use for the user name. Default is
-- @NameID@.
sAMLOptionsInput_subjectKey :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_subjectKey = Lens.lens (\SAMLOptionsInput' {subjectKey} -> subjectKey) (\s@SAMLOptionsInput' {} a -> s {subjectKey = a} :: SAMLOptionsInput)

instance Prelude.Hashable SAMLOptionsInput where
  hashWithSalt _salt SAMLOptionsInput' {..} =
    _salt `Prelude.hashWithSalt` masterUserName
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` sessionTimeoutMinutes
      `Prelude.hashWithSalt` rolesKey
      `Prelude.hashWithSalt` masterBackendRole
      `Prelude.hashWithSalt` idp
      `Prelude.hashWithSalt` subjectKey

instance Prelude.NFData SAMLOptionsInput where
  rnf SAMLOptionsInput' {..} =
    Prelude.rnf masterUserName
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf sessionTimeoutMinutes
      `Prelude.seq` Prelude.rnf rolesKey
      `Prelude.seq` Prelude.rnf masterBackendRole
      `Prelude.seq` Prelude.rnf idp
      `Prelude.seq` Prelude.rnf subjectKey

instance Data.ToJSON SAMLOptionsInput where
  toJSON SAMLOptionsInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MasterUserName" Data..=)
              Prelude.<$> masterUserName,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("SessionTimeoutMinutes" Data..=)
              Prelude.<$> sessionTimeoutMinutes,
            ("RolesKey" Data..=) Prelude.<$> rolesKey,
            ("MasterBackendRole" Data..=)
              Prelude.<$> masterBackendRole,
            ("Idp" Data..=) Prelude.<$> idp,
            ("SubjectKey" Data..=) Prelude.<$> subjectKey
          ]
      )

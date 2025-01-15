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
-- Module      : Amazonka.ElasticSearch.Types.SAMLOptionsInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.SAMLOptionsInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.SAMLIdp
import qualified Amazonka.Prelude as Prelude

-- | Specifies the SAML application configuration for the domain.
--
-- /See:/ 'newSAMLOptionsInput' smart constructor.
data SAMLOptionsInput = SAMLOptionsInput'
  { -- | True if SAML is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the SAML Identity Provider\'s information.
    idp :: Prelude.Maybe SAMLIdp,
    -- | The backend role to which the SAML master user is mapped to.
    masterBackendRole :: Prelude.Maybe Prelude.Text,
    -- | The SAML master username, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The key to use for matching the SAML Roles attribute.
    rolesKey :: Prelude.Maybe Prelude.Text,
    -- | The duration, in minutes, after which a user session becomes inactive.
    -- Acceptable values are between 1 and 1440, and the default value is 60.
    sessionTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The key to use for matching the SAML Subject attribute.
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
-- 'enabled', 'sAMLOptionsInput_enabled' - True if SAML is enabled.
--
-- 'idp', 'sAMLOptionsInput_idp' - Specifies the SAML Identity Provider\'s information.
--
-- 'masterBackendRole', 'sAMLOptionsInput_masterBackendRole' - The backend role to which the SAML master user is mapped to.
--
-- 'masterUserName', 'sAMLOptionsInput_masterUserName' - The SAML master username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
--
-- 'rolesKey', 'sAMLOptionsInput_rolesKey' - The key to use for matching the SAML Roles attribute.
--
-- 'sessionTimeoutMinutes', 'sAMLOptionsInput_sessionTimeoutMinutes' - The duration, in minutes, after which a user session becomes inactive.
-- Acceptable values are between 1 and 1440, and the default value is 60.
--
-- 'subjectKey', 'sAMLOptionsInput_subjectKey' - The key to use for matching the SAML Subject attribute.
newSAMLOptionsInput ::
  SAMLOptionsInput
newSAMLOptionsInput =
  SAMLOptionsInput'
    { enabled = Prelude.Nothing,
      idp = Prelude.Nothing,
      masterBackendRole = Prelude.Nothing,
      masterUserName = Prelude.Nothing,
      rolesKey = Prelude.Nothing,
      sessionTimeoutMinutes = Prelude.Nothing,
      subjectKey = Prelude.Nothing
    }

-- | True if SAML is enabled.
sAMLOptionsInput_enabled :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Bool)
sAMLOptionsInput_enabled = Lens.lens (\SAMLOptionsInput' {enabled} -> enabled) (\s@SAMLOptionsInput' {} a -> s {enabled = a} :: SAMLOptionsInput)

-- | Specifies the SAML Identity Provider\'s information.
sAMLOptionsInput_idp :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe SAMLIdp)
sAMLOptionsInput_idp = Lens.lens (\SAMLOptionsInput' {idp} -> idp) (\s@SAMLOptionsInput' {} a -> s {idp = a} :: SAMLOptionsInput)

-- | The backend role to which the SAML master user is mapped to.
sAMLOptionsInput_masterBackendRole :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_masterBackendRole = Lens.lens (\SAMLOptionsInput' {masterBackendRole} -> masterBackendRole) (\s@SAMLOptionsInput' {} a -> s {masterBackendRole = a} :: SAMLOptionsInput)

-- | The SAML master username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
sAMLOptionsInput_masterUserName :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_masterUserName = Lens.lens (\SAMLOptionsInput' {masterUserName} -> masterUserName) (\s@SAMLOptionsInput' {} a -> s {masterUserName = a} :: SAMLOptionsInput) Prelude.. Lens.mapping Data._Sensitive

-- | The key to use for matching the SAML Roles attribute.
sAMLOptionsInput_rolesKey :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_rolesKey = Lens.lens (\SAMLOptionsInput' {rolesKey} -> rolesKey) (\s@SAMLOptionsInput' {} a -> s {rolesKey = a} :: SAMLOptionsInput)

-- | The duration, in minutes, after which a user session becomes inactive.
-- Acceptable values are between 1 and 1440, and the default value is 60.
sAMLOptionsInput_sessionTimeoutMinutes :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Int)
sAMLOptionsInput_sessionTimeoutMinutes = Lens.lens (\SAMLOptionsInput' {sessionTimeoutMinutes} -> sessionTimeoutMinutes) (\s@SAMLOptionsInput' {} a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsInput)

-- | The key to use for matching the SAML Subject attribute.
sAMLOptionsInput_subjectKey :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_subjectKey = Lens.lens (\SAMLOptionsInput' {subjectKey} -> subjectKey) (\s@SAMLOptionsInput' {} a -> s {subjectKey = a} :: SAMLOptionsInput)

instance Prelude.Hashable SAMLOptionsInput where
  hashWithSalt _salt SAMLOptionsInput' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` idp
      `Prelude.hashWithSalt` masterBackendRole
      `Prelude.hashWithSalt` masterUserName
      `Prelude.hashWithSalt` rolesKey
      `Prelude.hashWithSalt` sessionTimeoutMinutes
      `Prelude.hashWithSalt` subjectKey

instance Prelude.NFData SAMLOptionsInput where
  rnf SAMLOptionsInput' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf idp `Prelude.seq`
        Prelude.rnf masterBackendRole `Prelude.seq`
          Prelude.rnf masterUserName `Prelude.seq`
            Prelude.rnf rolesKey `Prelude.seq`
              Prelude.rnf sessionTimeoutMinutes `Prelude.seq`
                Prelude.rnf subjectKey

instance Data.ToJSON SAMLOptionsInput where
  toJSON SAMLOptionsInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("Idp" Data..=) Prelude.<$> idp,
            ("MasterBackendRole" Data..=)
              Prelude.<$> masterBackendRole,
            ("MasterUserName" Data..=)
              Prelude.<$> masterUserName,
            ("RolesKey" Data..=) Prelude.<$> rolesKey,
            ("SessionTimeoutMinutes" Data..=)
              Prelude.<$> sessionTimeoutMinutes,
            ("SubjectKey" Data..=) Prelude.<$> subjectKey
          ]
      )

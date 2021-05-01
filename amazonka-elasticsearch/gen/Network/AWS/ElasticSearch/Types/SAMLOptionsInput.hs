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
-- Module      : Network.AWS.ElasticSearch.Types.SAMLOptionsInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLOptionsInput where

import Network.AWS.ElasticSearch.Types.SAMLIdp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the SAML application configuration for the domain.
--
-- /See:/ 'newSAMLOptionsInput' smart constructor.
data SAMLOptionsInput = SAMLOptionsInput'
  { -- | The backend role to which the SAML master user is mapped to.
    masterBackendRole :: Prelude.Maybe Prelude.Text,
    -- | The key to use for matching the SAML Roles attribute.
    rolesKey :: Prelude.Maybe Prelude.Text,
    -- | The duration, in minutes, after which a user session becomes inactive.
    -- Acceptable values are between 1 and 1440, and the default value is 60.
    sessionTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | Specifies the SAML Identity Provider\'s information.
    idp :: Prelude.Maybe SAMLIdp,
    -- | True if SAML is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The SAML master username, which is stored in the Amazon Elasticsearch
    -- Service domain\'s internal database.
    masterUserName :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The key to use for matching the SAML Subject attribute.
    subjectKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SAMLOptionsInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterBackendRole', 'sAMLOptionsInput_masterBackendRole' - The backend role to which the SAML master user is mapped to.
--
-- 'rolesKey', 'sAMLOptionsInput_rolesKey' - The key to use for matching the SAML Roles attribute.
--
-- 'sessionTimeoutMinutes', 'sAMLOptionsInput_sessionTimeoutMinutes' - The duration, in minutes, after which a user session becomes inactive.
-- Acceptable values are between 1 and 1440, and the default value is 60.
--
-- 'idp', 'sAMLOptionsInput_idp' - Specifies the SAML Identity Provider\'s information.
--
-- 'enabled', 'sAMLOptionsInput_enabled' - True if SAML is enabled.
--
-- 'masterUserName', 'sAMLOptionsInput_masterUserName' - The SAML master username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
--
-- 'subjectKey', 'sAMLOptionsInput_subjectKey' - The key to use for matching the SAML Subject attribute.
newSAMLOptionsInput ::
  SAMLOptionsInput
newSAMLOptionsInput =
  SAMLOptionsInput'
    { masterBackendRole =
        Prelude.Nothing,
      rolesKey = Prelude.Nothing,
      sessionTimeoutMinutes = Prelude.Nothing,
      idp = Prelude.Nothing,
      enabled = Prelude.Nothing,
      masterUserName = Prelude.Nothing,
      subjectKey = Prelude.Nothing
    }

-- | The backend role to which the SAML master user is mapped to.
sAMLOptionsInput_masterBackendRole :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_masterBackendRole = Lens.lens (\SAMLOptionsInput' {masterBackendRole} -> masterBackendRole) (\s@SAMLOptionsInput' {} a -> s {masterBackendRole = a} :: SAMLOptionsInput)

-- | The key to use for matching the SAML Roles attribute.
sAMLOptionsInput_rolesKey :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_rolesKey = Lens.lens (\SAMLOptionsInput' {rolesKey} -> rolesKey) (\s@SAMLOptionsInput' {} a -> s {rolesKey = a} :: SAMLOptionsInput)

-- | The duration, in minutes, after which a user session becomes inactive.
-- Acceptable values are between 1 and 1440, and the default value is 60.
sAMLOptionsInput_sessionTimeoutMinutes :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Int)
sAMLOptionsInput_sessionTimeoutMinutes = Lens.lens (\SAMLOptionsInput' {sessionTimeoutMinutes} -> sessionTimeoutMinutes) (\s@SAMLOptionsInput' {} a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsInput)

-- | Specifies the SAML Identity Provider\'s information.
sAMLOptionsInput_idp :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe SAMLIdp)
sAMLOptionsInput_idp = Lens.lens (\SAMLOptionsInput' {idp} -> idp) (\s@SAMLOptionsInput' {} a -> s {idp = a} :: SAMLOptionsInput)

-- | True if SAML is enabled.
sAMLOptionsInput_enabled :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Bool)
sAMLOptionsInput_enabled = Lens.lens (\SAMLOptionsInput' {enabled} -> enabled) (\s@SAMLOptionsInput' {} a -> s {enabled = a} :: SAMLOptionsInput)

-- | The SAML master username, which is stored in the Amazon Elasticsearch
-- Service domain\'s internal database.
sAMLOptionsInput_masterUserName :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_masterUserName = Lens.lens (\SAMLOptionsInput' {masterUserName} -> masterUserName) (\s@SAMLOptionsInput' {} a -> s {masterUserName = a} :: SAMLOptionsInput) Prelude.. Lens.mapping Prelude._Sensitive

-- | The key to use for matching the SAML Subject attribute.
sAMLOptionsInput_subjectKey :: Lens.Lens' SAMLOptionsInput (Prelude.Maybe Prelude.Text)
sAMLOptionsInput_subjectKey = Lens.lens (\SAMLOptionsInput' {subjectKey} -> subjectKey) (\s@SAMLOptionsInput' {} a -> s {subjectKey = a} :: SAMLOptionsInput)

instance Prelude.Hashable SAMLOptionsInput

instance Prelude.NFData SAMLOptionsInput

instance Prelude.ToJSON SAMLOptionsInput where
  toJSON SAMLOptionsInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MasterBackendRole" Prelude..=)
              Prelude.<$> masterBackendRole,
            ("RolesKey" Prelude..=) Prelude.<$> rolesKey,
            ("SessionTimeoutMinutes" Prelude..=)
              Prelude.<$> sessionTimeoutMinutes,
            ("Idp" Prelude..=) Prelude.<$> idp,
            ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("MasterUserName" Prelude..=)
              Prelude.<$> masterUserName,
            ("SubjectKey" Prelude..=) Prelude.<$> subjectKey
          ]
      )

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
-- Module      : Amazonka.ElasticSearch.Types.SAMLOptionsOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.SAMLOptionsOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.SAMLIdp
import qualified Amazonka.Prelude as Prelude

-- | Describes the SAML application configured for the domain.
--
-- /See:/ 'newSAMLOptionsOutput' smart constructor.
data SAMLOptionsOutput = SAMLOptionsOutput'
  { -- | True if SAML is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Describes the SAML Identity Provider\'s information.
    idp :: Prelude.Maybe SAMLIdp,
    -- | The key used for matching the SAML Roles attribute.
    rolesKey :: Prelude.Maybe Prelude.Text,
    -- | The duration, in minutes, after which a user session becomes inactive.
    sessionTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The key used for matching the SAML Subject attribute.
    subjectKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAMLOptionsOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'sAMLOptionsOutput_enabled' - True if SAML is enabled.
--
-- 'idp', 'sAMLOptionsOutput_idp' - Describes the SAML Identity Provider\'s information.
--
-- 'rolesKey', 'sAMLOptionsOutput_rolesKey' - The key used for matching the SAML Roles attribute.
--
-- 'sessionTimeoutMinutes', 'sAMLOptionsOutput_sessionTimeoutMinutes' - The duration, in minutes, after which a user session becomes inactive.
--
-- 'subjectKey', 'sAMLOptionsOutput_subjectKey' - The key used for matching the SAML Subject attribute.
newSAMLOptionsOutput ::
  SAMLOptionsOutput
newSAMLOptionsOutput =
  SAMLOptionsOutput'
    { enabled = Prelude.Nothing,
      idp = Prelude.Nothing,
      rolesKey = Prelude.Nothing,
      sessionTimeoutMinutes = Prelude.Nothing,
      subjectKey = Prelude.Nothing
    }

-- | True if SAML is enabled.
sAMLOptionsOutput_enabled :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Bool)
sAMLOptionsOutput_enabled = Lens.lens (\SAMLOptionsOutput' {enabled} -> enabled) (\s@SAMLOptionsOutput' {} a -> s {enabled = a} :: SAMLOptionsOutput)

-- | Describes the SAML Identity Provider\'s information.
sAMLOptionsOutput_idp :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe SAMLIdp)
sAMLOptionsOutput_idp = Lens.lens (\SAMLOptionsOutput' {idp} -> idp) (\s@SAMLOptionsOutput' {} a -> s {idp = a} :: SAMLOptionsOutput)

-- | The key used for matching the SAML Roles attribute.
sAMLOptionsOutput_rolesKey :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Text)
sAMLOptionsOutput_rolesKey = Lens.lens (\SAMLOptionsOutput' {rolesKey} -> rolesKey) (\s@SAMLOptionsOutput' {} a -> s {rolesKey = a} :: SAMLOptionsOutput)

-- | The duration, in minutes, after which a user session becomes inactive.
sAMLOptionsOutput_sessionTimeoutMinutes :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Int)
sAMLOptionsOutput_sessionTimeoutMinutes = Lens.lens (\SAMLOptionsOutput' {sessionTimeoutMinutes} -> sessionTimeoutMinutes) (\s@SAMLOptionsOutput' {} a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsOutput)

-- | The key used for matching the SAML Subject attribute.
sAMLOptionsOutput_subjectKey :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Text)
sAMLOptionsOutput_subjectKey = Lens.lens (\SAMLOptionsOutput' {subjectKey} -> subjectKey) (\s@SAMLOptionsOutput' {} a -> s {subjectKey = a} :: SAMLOptionsOutput)

instance Data.FromJSON SAMLOptionsOutput where
  parseJSON =
    Data.withObject
      "SAMLOptionsOutput"
      ( \x ->
          SAMLOptionsOutput'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "Idp")
            Prelude.<*> (x Data..:? "RolesKey")
            Prelude.<*> (x Data..:? "SessionTimeoutMinutes")
            Prelude.<*> (x Data..:? "SubjectKey")
      )

instance Prelude.Hashable SAMLOptionsOutput where
  hashWithSalt _salt SAMLOptionsOutput' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` idp
      `Prelude.hashWithSalt` rolesKey
      `Prelude.hashWithSalt` sessionTimeoutMinutes
      `Prelude.hashWithSalt` subjectKey

instance Prelude.NFData SAMLOptionsOutput where
  rnf SAMLOptionsOutput' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf idp `Prelude.seq`
        Prelude.rnf rolesKey `Prelude.seq`
          Prelude.rnf sessionTimeoutMinutes `Prelude.seq`
            Prelude.rnf subjectKey

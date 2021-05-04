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
-- Module      : Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLOptionsOutput where

import Network.AWS.ElasticSearch.Types.SAMLIdp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the SAML application configured for the domain.
--
-- /See:/ 'newSAMLOptionsOutput' smart constructor.
data SAMLOptionsOutput = SAMLOptionsOutput'
  { -- | The key used for matching the SAML Roles attribute.
    rolesKey :: Prelude.Maybe Prelude.Text,
    -- | The duration, in minutes, after which a user session becomes inactive.
    sessionTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | Describes the SAML Identity Provider\'s information.
    idp :: Prelude.Maybe SAMLIdp,
    -- | True if SAML is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The key used for matching the SAML Subject attribute.
    subjectKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SAMLOptionsOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rolesKey', 'sAMLOptionsOutput_rolesKey' - The key used for matching the SAML Roles attribute.
--
-- 'sessionTimeoutMinutes', 'sAMLOptionsOutput_sessionTimeoutMinutes' - The duration, in minutes, after which a user session becomes inactive.
--
-- 'idp', 'sAMLOptionsOutput_idp' - Describes the SAML Identity Provider\'s information.
--
-- 'enabled', 'sAMLOptionsOutput_enabled' - True if SAML is enabled.
--
-- 'subjectKey', 'sAMLOptionsOutput_subjectKey' - The key used for matching the SAML Subject attribute.
newSAMLOptionsOutput ::
  SAMLOptionsOutput
newSAMLOptionsOutput =
  SAMLOptionsOutput'
    { rolesKey = Prelude.Nothing,
      sessionTimeoutMinutes = Prelude.Nothing,
      idp = Prelude.Nothing,
      enabled = Prelude.Nothing,
      subjectKey = Prelude.Nothing
    }

-- | The key used for matching the SAML Roles attribute.
sAMLOptionsOutput_rolesKey :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Text)
sAMLOptionsOutput_rolesKey = Lens.lens (\SAMLOptionsOutput' {rolesKey} -> rolesKey) (\s@SAMLOptionsOutput' {} a -> s {rolesKey = a} :: SAMLOptionsOutput)

-- | The duration, in minutes, after which a user session becomes inactive.
sAMLOptionsOutput_sessionTimeoutMinutes :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Int)
sAMLOptionsOutput_sessionTimeoutMinutes = Lens.lens (\SAMLOptionsOutput' {sessionTimeoutMinutes} -> sessionTimeoutMinutes) (\s@SAMLOptionsOutput' {} a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsOutput)

-- | Describes the SAML Identity Provider\'s information.
sAMLOptionsOutput_idp :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe SAMLIdp)
sAMLOptionsOutput_idp = Lens.lens (\SAMLOptionsOutput' {idp} -> idp) (\s@SAMLOptionsOutput' {} a -> s {idp = a} :: SAMLOptionsOutput)

-- | True if SAML is enabled.
sAMLOptionsOutput_enabled :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Bool)
sAMLOptionsOutput_enabled = Lens.lens (\SAMLOptionsOutput' {enabled} -> enabled) (\s@SAMLOptionsOutput' {} a -> s {enabled = a} :: SAMLOptionsOutput)

-- | The key used for matching the SAML Subject attribute.
sAMLOptionsOutput_subjectKey :: Lens.Lens' SAMLOptionsOutput (Prelude.Maybe Prelude.Text)
sAMLOptionsOutput_subjectKey = Lens.lens (\SAMLOptionsOutput' {subjectKey} -> subjectKey) (\s@SAMLOptionsOutput' {} a -> s {subjectKey = a} :: SAMLOptionsOutput)

instance Prelude.FromJSON SAMLOptionsOutput where
  parseJSON =
    Prelude.withObject
      "SAMLOptionsOutput"
      ( \x ->
          SAMLOptionsOutput'
            Prelude.<$> (x Prelude..:? "RolesKey")
            Prelude.<*> (x Prelude..:? "SessionTimeoutMinutes")
            Prelude.<*> (x Prelude..:? "Idp")
            Prelude.<*> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "SubjectKey")
      )

instance Prelude.Hashable SAMLOptionsOutput

instance Prelude.NFData SAMLOptionsOutput

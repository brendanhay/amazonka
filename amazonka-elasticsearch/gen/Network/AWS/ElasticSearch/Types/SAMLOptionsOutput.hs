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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.SAMLIdp
import qualified Network.AWS.Lens as Lens

-- | Describes the SAML application configured for the domain.
--
-- /See:/ 'newSAMLOptionsOutput' smart constructor.
data SAMLOptionsOutput = SAMLOptionsOutput'
  { -- | The key used for matching the SAML Roles attribute.
    rolesKey :: Core.Maybe Core.Text,
    -- | The duration, in minutes, after which a user session becomes inactive.
    sessionTimeoutMinutes :: Core.Maybe Core.Int,
    -- | Describes the SAML Identity Provider\'s information.
    idp :: Core.Maybe SAMLIdp,
    -- | True if SAML is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | The key used for matching the SAML Subject attribute.
    subjectKey :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { rolesKey = Core.Nothing,
      sessionTimeoutMinutes = Core.Nothing,
      idp = Core.Nothing,
      enabled = Core.Nothing,
      subjectKey = Core.Nothing
    }

-- | The key used for matching the SAML Roles attribute.
sAMLOptionsOutput_rolesKey :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Core.Text)
sAMLOptionsOutput_rolesKey = Lens.lens (\SAMLOptionsOutput' {rolesKey} -> rolesKey) (\s@SAMLOptionsOutput' {} a -> s {rolesKey = a} :: SAMLOptionsOutput)

-- | The duration, in minutes, after which a user session becomes inactive.
sAMLOptionsOutput_sessionTimeoutMinutes :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Core.Int)
sAMLOptionsOutput_sessionTimeoutMinutes = Lens.lens (\SAMLOptionsOutput' {sessionTimeoutMinutes} -> sessionTimeoutMinutes) (\s@SAMLOptionsOutput' {} a -> s {sessionTimeoutMinutes = a} :: SAMLOptionsOutput)

-- | Describes the SAML Identity Provider\'s information.
sAMLOptionsOutput_idp :: Lens.Lens' SAMLOptionsOutput (Core.Maybe SAMLIdp)
sAMLOptionsOutput_idp = Lens.lens (\SAMLOptionsOutput' {idp} -> idp) (\s@SAMLOptionsOutput' {} a -> s {idp = a} :: SAMLOptionsOutput)

-- | True if SAML is enabled.
sAMLOptionsOutput_enabled :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Core.Bool)
sAMLOptionsOutput_enabled = Lens.lens (\SAMLOptionsOutput' {enabled} -> enabled) (\s@SAMLOptionsOutput' {} a -> s {enabled = a} :: SAMLOptionsOutput)

-- | The key used for matching the SAML Subject attribute.
sAMLOptionsOutput_subjectKey :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Core.Text)
sAMLOptionsOutput_subjectKey = Lens.lens (\SAMLOptionsOutput' {subjectKey} -> subjectKey) (\s@SAMLOptionsOutput' {} a -> s {subjectKey = a} :: SAMLOptionsOutput)

instance Core.FromJSON SAMLOptionsOutput where
  parseJSON =
    Core.withObject
      "SAMLOptionsOutput"
      ( \x ->
          SAMLOptionsOutput'
            Core.<$> (x Core..:? "RolesKey")
            Core.<*> (x Core..:? "SessionTimeoutMinutes")
            Core.<*> (x Core..:? "Idp")
            Core.<*> (x Core..:? "Enabled")
            Core.<*> (x Core..:? "SubjectKey")
      )

instance Core.Hashable SAMLOptionsOutput

instance Core.NFData SAMLOptionsOutput

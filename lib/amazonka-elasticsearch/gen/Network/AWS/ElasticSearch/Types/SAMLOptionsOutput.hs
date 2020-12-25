{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
  ( SAMLOptionsOutput (..),

    -- * Smart constructor
    mkSAMLOptionsOutput,

    -- * Lenses
    samlooEnabled,
    samlooIdp,
    samlooRolesKey,
    samlooSessionTimeoutMinutes,
    samlooSubjectKey,
  )
where

import qualified Network.AWS.ElasticSearch.Types.RolesKey as Types
import qualified Network.AWS.ElasticSearch.Types.SAMLIdp as Types
import qualified Network.AWS.ElasticSearch.Types.SubjectKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the SAML application configured for the domain.
--
-- /See:/ 'mkSAMLOptionsOutput' smart constructor.
data SAMLOptionsOutput = SAMLOptionsOutput'
  { -- | True if SAML is enabled.
    enabled :: Core.Maybe Core.Bool,
    -- | Describes the SAML Identity Provider's information.
    idp :: Core.Maybe Types.SAMLIdp,
    -- | The key used for matching the SAML Roles attribute.
    rolesKey :: Core.Maybe Types.RolesKey,
    -- | The duration, in minutes, after which a user session becomes inactive.
    sessionTimeoutMinutes :: Core.Maybe Core.Int,
    -- | The key used for matching the SAML Subject attribute.
    subjectKey :: Core.Maybe Types.SubjectKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SAMLOptionsOutput' value with any optional fields omitted.
mkSAMLOptionsOutput ::
  SAMLOptionsOutput
mkSAMLOptionsOutput =
  SAMLOptionsOutput'
    { enabled = Core.Nothing,
      idp = Core.Nothing,
      rolesKey = Core.Nothing,
      sessionTimeoutMinutes = Core.Nothing,
      subjectKey = Core.Nothing
    }

-- | True if SAML is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooEnabled :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Core.Bool)
samlooEnabled = Lens.field @"enabled"
{-# DEPRECATED samlooEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Describes the SAML Identity Provider's information.
--
-- /Note:/ Consider using 'idp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooIdp :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Types.SAMLIdp)
samlooIdp = Lens.field @"idp"
{-# DEPRECATED samlooIdp "Use generic-lens or generic-optics with 'idp' instead." #-}

-- | The key used for matching the SAML Roles attribute.
--
-- /Note:/ Consider using 'rolesKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooRolesKey :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Types.RolesKey)
samlooRolesKey = Lens.field @"rolesKey"
{-# DEPRECATED samlooRolesKey "Use generic-lens or generic-optics with 'rolesKey' instead." #-}

-- | The duration, in minutes, after which a user session becomes inactive.
--
-- /Note:/ Consider using 'sessionTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooSessionTimeoutMinutes :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Core.Int)
samlooSessionTimeoutMinutes = Lens.field @"sessionTimeoutMinutes"
{-# DEPRECATED samlooSessionTimeoutMinutes "Use generic-lens or generic-optics with 'sessionTimeoutMinutes' instead." #-}

-- | The key used for matching the SAML Subject attribute.
--
-- /Note:/ Consider using 'subjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samlooSubjectKey :: Lens.Lens' SAMLOptionsOutput (Core.Maybe Types.SubjectKey)
samlooSubjectKey = Lens.field @"subjectKey"
{-# DEPRECATED samlooSubjectKey "Use generic-lens or generic-optics with 'subjectKey' instead." #-}

instance Core.FromJSON SAMLOptionsOutput where
  parseJSON =
    Core.withObject "SAMLOptionsOutput" Core.$
      \x ->
        SAMLOptionsOutput'
          Core.<$> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "Idp")
          Core.<*> (x Core..:? "RolesKey")
          Core.<*> (x Core..:? "SessionTimeoutMinutes")
          Core.<*> (x Core..:? "SubjectKey")

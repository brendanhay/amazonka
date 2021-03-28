{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.SAMLOptionsInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.SAMLOptionsInput
  ( SAMLOptionsInput (..)
  -- * Smart constructor
  , mkSAMLOptionsInput
  -- * Lenses
  , samloiEnabled
  , samloiIdp
  , samloiMasterBackendRole
  , samloiMasterUserName
  , samloiRolesKey
  , samloiSessionTimeoutMinutes
  , samloiSubjectKey
  ) where

import qualified Network.AWS.ElasticSearch.Types.BackendRole as Types
import qualified Network.AWS.ElasticSearch.Types.SAMLIdp as Types
import qualified Network.AWS.ElasticSearch.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the SAML application configuration for the domain.
--
-- /See:/ 'mkSAMLOptionsInput' smart constructor.
data SAMLOptionsInput = SAMLOptionsInput'
  { enabled :: Core.Maybe Core.Bool
    -- ^ True if SAML is enabled.
  , idp :: Core.Maybe Types.SAMLIdp
    -- ^ Specifies the SAML Identity Provider's information.
  , masterBackendRole :: Core.Maybe Types.BackendRole
    -- ^ The backend role to which the SAML master user is mapped to.
  , masterUserName :: Core.Maybe Types.Username
    -- ^ The SAML master username, which is stored in the Amazon Elasticsearch Service domain's internal database.
  , rolesKey :: Core.Maybe Core.Text
    -- ^ The key to use for matching the SAML Roles attribute.
  , sessionTimeoutMinutes :: Core.Maybe Core.Int
    -- ^ The duration, in minutes, after which a user session becomes inactive. Acceptable values are between 1 and 1440, and the default value is 60.
  , subjectKey :: Core.Maybe Core.Text
    -- ^ The key to use for matching the SAML Subject attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SAMLOptionsInput' value with any optional fields omitted.
mkSAMLOptionsInput
    :: SAMLOptionsInput
mkSAMLOptionsInput
  = SAMLOptionsInput'{enabled = Core.Nothing, idp = Core.Nothing,
                      masterBackendRole = Core.Nothing, masterUserName = Core.Nothing,
                      rolesKey = Core.Nothing, sessionTimeoutMinutes = Core.Nothing,
                      subjectKey = Core.Nothing}

-- | True if SAML is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiEnabled :: Lens.Lens' SAMLOptionsInput (Core.Maybe Core.Bool)
samloiEnabled = Lens.field @"enabled"
{-# INLINEABLE samloiEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | Specifies the SAML Identity Provider's information.
--
-- /Note:/ Consider using 'idp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiIdp :: Lens.Lens' SAMLOptionsInput (Core.Maybe Types.SAMLIdp)
samloiIdp = Lens.field @"idp"
{-# INLINEABLE samloiIdp #-}
{-# DEPRECATED idp "Use generic-lens or generic-optics with 'idp' instead"  #-}

-- | The backend role to which the SAML master user is mapped to.
--
-- /Note:/ Consider using 'masterBackendRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiMasterBackendRole :: Lens.Lens' SAMLOptionsInput (Core.Maybe Types.BackendRole)
samloiMasterBackendRole = Lens.field @"masterBackendRole"
{-# INLINEABLE samloiMasterBackendRole #-}
{-# DEPRECATED masterBackendRole "Use generic-lens or generic-optics with 'masterBackendRole' instead"  #-}

-- | The SAML master username, which is stored in the Amazon Elasticsearch Service domain's internal database.
--
-- /Note:/ Consider using 'masterUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiMasterUserName :: Lens.Lens' SAMLOptionsInput (Core.Maybe Types.Username)
samloiMasterUserName = Lens.field @"masterUserName"
{-# INLINEABLE samloiMasterUserName #-}
{-# DEPRECATED masterUserName "Use generic-lens or generic-optics with 'masterUserName' instead"  #-}

-- | The key to use for matching the SAML Roles attribute.
--
-- /Note:/ Consider using 'rolesKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiRolesKey :: Lens.Lens' SAMLOptionsInput (Core.Maybe Core.Text)
samloiRolesKey = Lens.field @"rolesKey"
{-# INLINEABLE samloiRolesKey #-}
{-# DEPRECATED rolesKey "Use generic-lens or generic-optics with 'rolesKey' instead"  #-}

-- | The duration, in minutes, after which a user session becomes inactive. Acceptable values are between 1 and 1440, and the default value is 60.
--
-- /Note:/ Consider using 'sessionTimeoutMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiSessionTimeoutMinutes :: Lens.Lens' SAMLOptionsInput (Core.Maybe Core.Int)
samloiSessionTimeoutMinutes = Lens.field @"sessionTimeoutMinutes"
{-# INLINEABLE samloiSessionTimeoutMinutes #-}
{-# DEPRECATED sessionTimeoutMinutes "Use generic-lens or generic-optics with 'sessionTimeoutMinutes' instead"  #-}

-- | The key to use for matching the SAML Subject attribute.
--
-- /Note:/ Consider using 'subjectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
samloiSubjectKey :: Lens.Lens' SAMLOptionsInput (Core.Maybe Core.Text)
samloiSubjectKey = Lens.field @"subjectKey"
{-# INLINEABLE samloiSubjectKey #-}
{-# DEPRECATED subjectKey "Use generic-lens or generic-optics with 'subjectKey' instead"  #-}

instance Core.FromJSON SAMLOptionsInput where
        toJSON SAMLOptionsInput{..}
          = Core.object
              (Core.catMaybes
                 [("Enabled" Core..=) Core.<$> enabled,
                  ("Idp" Core..=) Core.<$> idp,
                  ("MasterBackendRole" Core..=) Core.<$> masterBackendRole,
                  ("MasterUserName" Core..=) Core.<$> masterUserName,
                  ("RolesKey" Core..=) Core.<$> rolesKey,
                  ("SessionTimeoutMinutes" Core..=) Core.<$> sessionTimeoutMinutes,
                  ("SubjectKey" Core..=) Core.<$> subjectKey])

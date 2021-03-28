{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.RoleMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.RoleMapping
  ( RoleMapping (..)
  -- * Smart constructor
  , mkRoleMapping
  -- * Lenses
  , rmType
  , rmAmbiguousRoleResolution
  , rmRulesConfiguration
  ) where

import qualified Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType as Types
import qualified Network.AWS.CognitoIdentity.Types.RoleMappingType as Types
import qualified Network.AWS.CognitoIdentity.Types.RulesConfigurationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A role mapping.
--
-- /See:/ 'mkRoleMapping' smart constructor.
data RoleMapping = RoleMapping'
  { type' :: Types.RoleMappingType
    -- ^ The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
  , ambiguousRoleResolution :: Core.Maybe Types.AmbiguousRoleResolutionType
    -- ^ If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required.
--
-- Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
  , rulesConfiguration :: Core.Maybe Types.RulesConfigurationType
    -- ^ The rules to be used for mapping users to roles.
--
-- If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoleMapping' value with any optional fields omitted.
mkRoleMapping
    :: Types.RoleMappingType -- ^ 'type\''
    -> RoleMapping
mkRoleMapping type'
  = RoleMapping'{type', ambiguousRoleResolution = Core.Nothing,
                 rulesConfiguration = Core.Nothing}

-- | The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmType :: Lens.Lens' RoleMapping Types.RoleMappingType
rmType = Lens.field @"type'"
{-# INLINEABLE rmType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required.
--
-- Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
--
-- /Note:/ Consider using 'ambiguousRoleResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmAmbiguousRoleResolution :: Lens.Lens' RoleMapping (Core.Maybe Types.AmbiguousRoleResolutionType)
rmAmbiguousRoleResolution = Lens.field @"ambiguousRoleResolution"
{-# INLINEABLE rmAmbiguousRoleResolution #-}
{-# DEPRECATED ambiguousRoleResolution "Use generic-lens or generic-optics with 'ambiguousRoleResolution' instead"  #-}

-- | The rules to be used for mapping users to roles.
--
-- If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
--
-- /Note:/ Consider using 'rulesConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRulesConfiguration :: Lens.Lens' RoleMapping (Core.Maybe Types.RulesConfigurationType)
rmRulesConfiguration = Lens.field @"rulesConfiguration"
{-# INLINEABLE rmRulesConfiguration #-}
{-# DEPRECATED rulesConfiguration "Use generic-lens or generic-optics with 'rulesConfiguration' instead"  #-}

instance Core.FromJSON RoleMapping where
        toJSON RoleMapping{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  ("AmbiguousRoleResolution" Core..=) Core.<$>
                    ambiguousRoleResolution,
                  ("RulesConfiguration" Core..=) Core.<$> rulesConfiguration])

instance Core.FromJSON RoleMapping where
        parseJSON
          = Core.withObject "RoleMapping" Core.$
              \ x ->
                RoleMapping' Core.<$>
                  (x Core..: "Type") Core.<*> x Core..:? "AmbiguousRoleResolution"
                    Core.<*> x Core..:? "RulesConfiguration"

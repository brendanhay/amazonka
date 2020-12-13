{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.RoleMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.RoleMapping
  ( RoleMapping (..),

    -- * Smart constructor
    mkRoleMapping,

    -- * Lenses
    rmRulesConfiguration,
    rmType,
    rmAmbiguousRoleResolution,
  )
where

import Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
import Network.AWS.CognitoIdentity.Types.RoleMappingType
import Network.AWS.CognitoIdentity.Types.RulesConfigurationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A role mapping.
--
-- /See:/ 'mkRoleMapping' smart constructor.
data RoleMapping = RoleMapping'
  { -- | The rules to be used for mapping users to roles.
    --
    -- If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
    rulesConfiguration :: Lude.Maybe RulesConfigurationType,
    -- | The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
    type' :: RoleMappingType,
    -- | If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required.
    --
    -- Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
    ambiguousRoleResolution :: Lude.Maybe AmbiguousRoleResolutionType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoleMapping' with the minimum fields required to make a request.
--
-- * 'rulesConfiguration' - The rules to be used for mapping users to roles.
--
-- If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
-- * 'type'' - The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
-- * 'ambiguousRoleResolution' - If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required.
--
-- Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
mkRoleMapping ::
  -- | 'type''
  RoleMappingType ->
  RoleMapping
mkRoleMapping pType_ =
  RoleMapping'
    { rulesConfiguration = Lude.Nothing,
      type' = pType_,
      ambiguousRoleResolution = Lude.Nothing
    }

-- | The rules to be used for mapping users to roles.
--
-- If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
--
-- /Note:/ Consider using 'rulesConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmRulesConfiguration :: Lens.Lens' RoleMapping (Lude.Maybe RulesConfigurationType)
rmRulesConfiguration = Lens.lens (rulesConfiguration :: RoleMapping -> Lude.Maybe RulesConfigurationType) (\s a -> s {rulesConfiguration = a} :: RoleMapping)
{-# DEPRECATED rmRulesConfiguration "Use generic-lens or generic-optics with 'rulesConfiguration' instead." #-}

-- | The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmType :: Lens.Lens' RoleMapping RoleMappingType
rmType = Lens.lens (type' :: RoleMapping -> RoleMappingType) (\s a -> s {type' = a} :: RoleMapping)
{-# DEPRECATED rmType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required.
--
-- Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
--
-- /Note:/ Consider using 'ambiguousRoleResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmAmbiguousRoleResolution :: Lens.Lens' RoleMapping (Lude.Maybe AmbiguousRoleResolutionType)
rmAmbiguousRoleResolution = Lens.lens (ambiguousRoleResolution :: RoleMapping -> Lude.Maybe AmbiguousRoleResolutionType) (\s a -> s {ambiguousRoleResolution = a} :: RoleMapping)
{-# DEPRECATED rmAmbiguousRoleResolution "Use generic-lens or generic-optics with 'ambiguousRoleResolution' instead." #-}

instance Lude.FromJSON RoleMapping where
  parseJSON =
    Lude.withObject
      "RoleMapping"
      ( \x ->
          RoleMapping'
            Lude.<$> (x Lude..:? "RulesConfiguration")
            Lude.<*> (x Lude..: "Type")
            Lude.<*> (x Lude..:? "AmbiguousRoleResolution")
      )

instance Lude.ToJSON RoleMapping where
  toJSON RoleMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RulesConfiguration" Lude..=) Lude.<$> rulesConfiguration,
            Lude.Just ("Type" Lude..= type'),
            ("AmbiguousRoleResolution" Lude..=)
              Lude.<$> ambiguousRoleResolution
          ]
      )

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
-- Module      : Network.AWS.CognitoIdentity.Types.RoleMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.RoleMapping where

import Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
import Network.AWS.CognitoIdentity.Types.RoleMappingType
import Network.AWS.CognitoIdentity.Types.RulesConfigurationType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A role mapping.
--
-- /See:/ 'newRoleMapping' smart constructor.
data RoleMapping = RoleMapping'
  { -- | If you specify Token or Rules as the @Type@, @AmbiguousRoleResolution@
    -- is required.
    --
    -- Specifies the action to be taken if either no rules match the claim
    -- value for the @Rules@ type, or there is no @cognito:preferred_role@
    -- claim and there are multiple @cognito:roles@ matches for the @Token@
    -- type.
    ambiguousRoleResolution :: Core.Maybe AmbiguousRoleResolutionType,
    -- | The rules to be used for mapping users to roles.
    --
    -- If you specify Rules as the role mapping type, @RulesConfiguration@ is
    -- required.
    rulesConfiguration :: Core.Maybe RulesConfigurationType,
    -- | The role mapping type. Token will use @cognito:roles@ and
    -- @cognito:preferred_role@ claims from the Cognito identity provider token
    -- to map groups to roles. Rules will attempt to match claims from the
    -- token to map to a role.
    type' :: RoleMappingType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RoleMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ambiguousRoleResolution', 'roleMapping_ambiguousRoleResolution' - If you specify Token or Rules as the @Type@, @AmbiguousRoleResolution@
-- is required.
--
-- Specifies the action to be taken if either no rules match the claim
-- value for the @Rules@ type, or there is no @cognito:preferred_role@
-- claim and there are multiple @cognito:roles@ matches for the @Token@
-- type.
--
-- 'rulesConfiguration', 'roleMapping_rulesConfiguration' - The rules to be used for mapping users to roles.
--
-- If you specify Rules as the role mapping type, @RulesConfiguration@ is
-- required.
--
-- 'type'', 'roleMapping_type' - The role mapping type. Token will use @cognito:roles@ and
-- @cognito:preferred_role@ claims from the Cognito identity provider token
-- to map groups to roles. Rules will attempt to match claims from the
-- token to map to a role.
newRoleMapping ::
  -- | 'type''
  RoleMappingType ->
  RoleMapping
newRoleMapping pType_ =
  RoleMapping'
    { ambiguousRoleResolution =
        Core.Nothing,
      rulesConfiguration = Core.Nothing,
      type' = pType_
    }

-- | If you specify Token or Rules as the @Type@, @AmbiguousRoleResolution@
-- is required.
--
-- Specifies the action to be taken if either no rules match the claim
-- value for the @Rules@ type, or there is no @cognito:preferred_role@
-- claim and there are multiple @cognito:roles@ matches for the @Token@
-- type.
roleMapping_ambiguousRoleResolution :: Lens.Lens' RoleMapping (Core.Maybe AmbiguousRoleResolutionType)
roleMapping_ambiguousRoleResolution = Lens.lens (\RoleMapping' {ambiguousRoleResolution} -> ambiguousRoleResolution) (\s@RoleMapping' {} a -> s {ambiguousRoleResolution = a} :: RoleMapping)

-- | The rules to be used for mapping users to roles.
--
-- If you specify Rules as the role mapping type, @RulesConfiguration@ is
-- required.
roleMapping_rulesConfiguration :: Lens.Lens' RoleMapping (Core.Maybe RulesConfigurationType)
roleMapping_rulesConfiguration = Lens.lens (\RoleMapping' {rulesConfiguration} -> rulesConfiguration) (\s@RoleMapping' {} a -> s {rulesConfiguration = a} :: RoleMapping)

-- | The role mapping type. Token will use @cognito:roles@ and
-- @cognito:preferred_role@ claims from the Cognito identity provider token
-- to map groups to roles. Rules will attempt to match claims from the
-- token to map to a role.
roleMapping_type :: Lens.Lens' RoleMapping RoleMappingType
roleMapping_type = Lens.lens (\RoleMapping' {type'} -> type') (\s@RoleMapping' {} a -> s {type' = a} :: RoleMapping)

instance Core.FromJSON RoleMapping where
  parseJSON =
    Core.withObject
      "RoleMapping"
      ( \x ->
          RoleMapping'
            Core.<$> (x Core..:? "AmbiguousRoleResolution")
            Core.<*> (x Core..:? "RulesConfiguration")
            Core.<*> (x Core..: "Type")
      )

instance Core.Hashable RoleMapping

instance Core.NFData RoleMapping

instance Core.ToJSON RoleMapping where
  toJSON RoleMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AmbiguousRoleResolution" Core..=)
              Core.<$> ambiguousRoleResolution,
            ("RulesConfiguration" Core..=)
              Core.<$> rulesConfiguration,
            Core.Just ("Type" Core..= type')
          ]
      )

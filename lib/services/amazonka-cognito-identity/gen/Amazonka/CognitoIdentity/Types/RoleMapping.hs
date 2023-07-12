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
-- Module      : Amazonka.CognitoIdentity.Types.RoleMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.RoleMapping where

import Amazonka.CognitoIdentity.Types.AmbiguousRoleResolutionType
import Amazonka.CognitoIdentity.Types.RoleMappingType
import Amazonka.CognitoIdentity.Types.RulesConfigurationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    ambiguousRoleResolution :: Prelude.Maybe AmbiguousRoleResolutionType,
    -- | The rules to be used for mapping users to roles.
    --
    -- If you specify Rules as the role mapping type, @RulesConfiguration@ is
    -- required.
    rulesConfiguration :: Prelude.Maybe RulesConfigurationType,
    -- | The role mapping type. Token will use @cognito:roles@ and
    -- @cognito:preferred_role@ claims from the Cognito identity provider token
    -- to map groups to roles. Rules will attempt to match claims from the
    -- token to map to a role.
    type' :: RoleMappingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      rulesConfiguration = Prelude.Nothing,
      type' = pType_
    }

-- | If you specify Token or Rules as the @Type@, @AmbiguousRoleResolution@
-- is required.
--
-- Specifies the action to be taken if either no rules match the claim
-- value for the @Rules@ type, or there is no @cognito:preferred_role@
-- claim and there are multiple @cognito:roles@ matches for the @Token@
-- type.
roleMapping_ambiguousRoleResolution :: Lens.Lens' RoleMapping (Prelude.Maybe AmbiguousRoleResolutionType)
roleMapping_ambiguousRoleResolution = Lens.lens (\RoleMapping' {ambiguousRoleResolution} -> ambiguousRoleResolution) (\s@RoleMapping' {} a -> s {ambiguousRoleResolution = a} :: RoleMapping)

-- | The rules to be used for mapping users to roles.
--
-- If you specify Rules as the role mapping type, @RulesConfiguration@ is
-- required.
roleMapping_rulesConfiguration :: Lens.Lens' RoleMapping (Prelude.Maybe RulesConfigurationType)
roleMapping_rulesConfiguration = Lens.lens (\RoleMapping' {rulesConfiguration} -> rulesConfiguration) (\s@RoleMapping' {} a -> s {rulesConfiguration = a} :: RoleMapping)

-- | The role mapping type. Token will use @cognito:roles@ and
-- @cognito:preferred_role@ claims from the Cognito identity provider token
-- to map groups to roles. Rules will attempt to match claims from the
-- token to map to a role.
roleMapping_type :: Lens.Lens' RoleMapping RoleMappingType
roleMapping_type = Lens.lens (\RoleMapping' {type'} -> type') (\s@RoleMapping' {} a -> s {type' = a} :: RoleMapping)

instance Data.FromJSON RoleMapping where
  parseJSON =
    Data.withObject
      "RoleMapping"
      ( \x ->
          RoleMapping'
            Prelude.<$> (x Data..:? "AmbiguousRoleResolution")
            Prelude.<*> (x Data..:? "RulesConfiguration")
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable RoleMapping where
  hashWithSalt _salt RoleMapping' {..} =
    _salt
      `Prelude.hashWithSalt` ambiguousRoleResolution
      `Prelude.hashWithSalt` rulesConfiguration
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RoleMapping where
  rnf RoleMapping' {..} =
    Prelude.rnf ambiguousRoleResolution
      `Prelude.seq` Prelude.rnf rulesConfiguration
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON RoleMapping where
  toJSON RoleMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AmbiguousRoleResolution" Data..=)
              Prelude.<$> ambiguousRoleResolution,
            ("RulesConfiguration" Data..=)
              Prelude.<$> rulesConfiguration,
            Prelude.Just ("Type" Data..= type')
          ]
      )

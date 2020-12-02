{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.RoleMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.RoleMapping where

import Network.AWS.CognitoIdentity.Types.AmbiguousRoleResolutionType
import Network.AWS.CognitoIdentity.Types.RoleMappingType
import Network.AWS.CognitoIdentity.Types.RulesConfigurationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A role mapping.
--
--
--
-- /See:/ 'roleMapping' smart constructor.
data RoleMapping = RoleMapping'
  { _rmRulesConfiguration ::
      !(Maybe RulesConfigurationType),
    _rmAmbiguousRoleResolution :: !(Maybe AmbiguousRoleResolutionType),
    _rmType :: !RoleMappingType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoleMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmRulesConfiguration' - The rules to be used for mapping users to roles. If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
--
-- * 'rmAmbiguousRoleResolution' - If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required. Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
--
-- * 'rmType' - The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
roleMapping ::
  -- | 'rmType'
  RoleMappingType ->
  RoleMapping
roleMapping pType_ =
  RoleMapping'
    { _rmRulesConfiguration = Nothing,
      _rmAmbiguousRoleResolution = Nothing,
      _rmType = pType_
    }

-- | The rules to be used for mapping users to roles. If you specify Rules as the role mapping type, @RulesConfiguration@ is required.
rmRulesConfiguration :: Lens' RoleMapping (Maybe RulesConfigurationType)
rmRulesConfiguration = lens _rmRulesConfiguration (\s a -> s {_rmRulesConfiguration = a})

-- | If you specify Token or Rules as the @Type@ , @AmbiguousRoleResolution@ is required. Specifies the action to be taken if either no rules match the claim value for the @Rules@ type, or there is no @cognito:preferred_role@ claim and there are multiple @cognito:roles@ matches for the @Token@ type.
rmAmbiguousRoleResolution :: Lens' RoleMapping (Maybe AmbiguousRoleResolutionType)
rmAmbiguousRoleResolution = lens _rmAmbiguousRoleResolution (\s a -> s {_rmAmbiguousRoleResolution = a})

-- | The role mapping type. Token will use @cognito:roles@ and @cognito:preferred_role@ claims from the Cognito identity provider token to map groups to roles. Rules will attempt to match claims from the token to map to a role.
rmType :: Lens' RoleMapping RoleMappingType
rmType = lens _rmType (\s a -> s {_rmType = a})

instance FromJSON RoleMapping where
  parseJSON =
    withObject
      "RoleMapping"
      ( \x ->
          RoleMapping'
            <$> (x .:? "RulesConfiguration")
            <*> (x .:? "AmbiguousRoleResolution")
            <*> (x .: "Type")
      )

instance Hashable RoleMapping

instance NFData RoleMapping

instance ToJSON RoleMapping where
  toJSON RoleMapping' {..} =
    object
      ( catMaybes
          [ ("RulesConfiguration" .=) <$> _rmRulesConfiguration,
            ("AmbiguousRoleResolution" .=) <$> _rmAmbiguousRoleResolution,
            Just ("Type" .= _rmType)
          ]
      )

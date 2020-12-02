{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.ParameterConstraints

-- | Information about a parameter used to provision a product.
--
--
--
-- /See:/ 'provisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { _pIsNoEcho ::
      !(Maybe Bool),
    _pParameterKey :: !(Maybe Text),
    _pParameterType ::
      !(Maybe Text),
    _pParameterConstraints ::
      !(Maybe ParameterConstraints),
    _pDefaultValue :: !(Maybe Text),
    _pDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningArtifactParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pIsNoEcho' - If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
--
-- * 'pParameterKey' - The parameter key.
--
-- * 'pParameterType' - The parameter type.
--
-- * 'pParameterConstraints' - Constraints that the administrator has put on a parameter.
--
-- * 'pDefaultValue' - The default value.
--
-- * 'pDescription' - The description of the parameter.
provisioningArtifactParameter ::
  ProvisioningArtifactParameter
provisioningArtifactParameter =
  ProvisioningArtifactParameter'
    { _pIsNoEcho = Nothing,
      _pParameterKey = Nothing,
      _pParameterType = Nothing,
      _pParameterConstraints = Nothing,
      _pDefaultValue = Nothing,
      _pDescription = Nothing
    }

-- | If this value is true, the value for this parameter is obfuscated from view when the parameter is retrieved. This parameter is used to hide sensitive information.
pIsNoEcho :: Lens' ProvisioningArtifactParameter (Maybe Bool)
pIsNoEcho = lens _pIsNoEcho (\s a -> s {_pIsNoEcho = a})

-- | The parameter key.
pParameterKey :: Lens' ProvisioningArtifactParameter (Maybe Text)
pParameterKey = lens _pParameterKey (\s a -> s {_pParameterKey = a})

-- | The parameter type.
pParameterType :: Lens' ProvisioningArtifactParameter (Maybe Text)
pParameterType = lens _pParameterType (\s a -> s {_pParameterType = a})

-- | Constraints that the administrator has put on a parameter.
pParameterConstraints :: Lens' ProvisioningArtifactParameter (Maybe ParameterConstraints)
pParameterConstraints = lens _pParameterConstraints (\s a -> s {_pParameterConstraints = a})

-- | The default value.
pDefaultValue :: Lens' ProvisioningArtifactParameter (Maybe Text)
pDefaultValue = lens _pDefaultValue (\s a -> s {_pDefaultValue = a})

-- | The description of the parameter.
pDescription :: Lens' ProvisioningArtifactParameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s {_pDescription = a})

instance FromJSON ProvisioningArtifactParameter where
  parseJSON =
    withObject
      "ProvisioningArtifactParameter"
      ( \x ->
          ProvisioningArtifactParameter'
            <$> (x .:? "IsNoEcho")
            <*> (x .:? "ParameterKey")
            <*> (x .:? "ParameterType")
            <*> (x .:? "ParameterConstraints")
            <*> (x .:? "DefaultValue")
            <*> (x .:? "Description")
      )

instance Hashable ProvisioningArtifactParameter

instance NFData ProvisioningArtifactParameter

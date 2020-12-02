{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Parameter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ParameterApplyType

-- | Describes a parameter in a cluster parameter group.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pApplyType ::
      !(Maybe ParameterApplyType),
    _pParameterValue :: !(Maybe Text),
    _pMinimumEngineVersion :: !(Maybe Text),
    _pSource :: !(Maybe Text),
    _pIsModifiable :: !(Maybe Bool),
    _pDataType :: !(Maybe Text),
    _pAllowedValues :: !(Maybe Text),
    _pParameterName :: !(Maybe Text),
    _pDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pApplyType' - Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- * 'pParameterValue' - The value of the parameter.
--
-- * 'pMinimumEngineVersion' - The earliest engine version to which the parameter can apply.
--
-- * 'pSource' - The source of the parameter value, such as "engine-default" or "user".
--
-- * 'pIsModifiable' - If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'pDataType' - The data type of the parameter.
--
-- * 'pAllowedValues' - The valid range of values for the parameter.
--
-- * 'pParameterName' - The name of the parameter.
--
-- * 'pDescription' - A description of the parameter.
parameter ::
  Parameter
parameter =
  Parameter'
    { _pApplyType = Nothing,
      _pParameterValue = Nothing,
      _pMinimumEngineVersion = Nothing,
      _pSource = Nothing,
      _pIsModifiable = Nothing,
      _pDataType = Nothing,
      _pAllowedValues = Nothing,
      _pParameterName = Nothing,
      _pDescription = Nothing
    }

-- | Specifies how to apply the WLM configuration parameter. Some properties can be applied dynamically, while other properties require that any associated clusters be rebooted for the configuration changes to be applied. For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
pApplyType :: Lens' Parameter (Maybe ParameterApplyType)
pApplyType = lens _pApplyType (\s a -> s {_pApplyType = a})

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s {_pParameterValue = a})

-- | The earliest engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\s a -> s {_pMinimumEngineVersion = a})

-- | The source of the parameter value, such as "engine-default" or "user".
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s {_pSource = a})

-- | If @true@ , the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s {_pIsModifiable = a})

-- | The data type of the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s {_pDataType = a})

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s {_pAllowedValues = a})

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s {_pParameterName = a})

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s {_pDescription = a})

instance FromXML Parameter where
  parseXML x =
    Parameter'
      <$> (x .@? "ApplyType")
      <*> (x .@? "ParameterValue")
      <*> (x .@? "MinimumEngineVersion")
      <*> (x .@? "Source")
      <*> (x .@? "IsModifiable")
      <*> (x .@? "DataType")
      <*> (x .@? "AllowedValues")
      <*> (x .@? "ParameterName")
      <*> (x .@? "Description")

instance Hashable Parameter

instance NFData Parameter

instance ToQuery Parameter where
  toQuery Parameter' {..} =
    mconcat
      [ "ApplyType" =: _pApplyType,
        "ParameterValue" =: _pParameterValue,
        "MinimumEngineVersion" =: _pMinimumEngineVersion,
        "Source" =: _pSource,
        "IsModifiable" =: _pIsModifiable,
        "DataType" =: _pDataType,
        "AllowedValues" =: _pAllowedValues,
        "ParameterName" =: _pParameterName,
        "Description" =: _pDescription
      ]

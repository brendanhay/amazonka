{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Parameter where

import Network.AWS.ElastiCache.Types.ChangeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an individual setting that controls some aspect of ElastiCache behavior.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pParameterValue :: !(Maybe Text),
    _pMinimumEngineVersion :: !(Maybe Text),
    _pSource :: !(Maybe Text),
    _pIsModifiable :: !(Maybe Bool),
    _pDataType :: !(Maybe Text),
    _pAllowedValues :: !(Maybe Text),
    _pParameterName :: !(Maybe Text),
    _pDescription :: !(Maybe Text),
    _pChangeType :: !(Maybe ChangeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameterValue' - The value of the parameter.
--
-- * 'pMinimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
--
-- * 'pSource' - The source of the parameter.
--
-- * 'pIsModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'pDataType' - The valid data type for the parameter.
--
-- * 'pAllowedValues' - The valid range of values for the parameter.
--
-- * 'pParameterName' - The name of the parameter.
--
-- * 'pDescription' - A description of the parameter.
--
-- * 'pChangeType' - Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
parameter ::
  Parameter
parameter =
  Parameter'
    { _pParameterValue = Nothing,
      _pMinimumEngineVersion = Nothing,
      _pSource = Nothing,
      _pIsModifiable = Nothing,
      _pDataType = Nothing,
      _pAllowedValues = Nothing,
      _pParameterName = Nothing,
      _pDescription = Nothing,
      _pChangeType = Nothing
    }

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s {_pParameterValue = a})

-- | The earliest cache engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\s a -> s {_pMinimumEngineVersion = a})

-- | The source of the parameter.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s {_pSource = a})

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s {_pIsModifiable = a})

-- | The valid data type for the parameter.
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

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster> .
pChangeType :: Lens' Parameter (Maybe ChangeType)
pChangeType = lens _pChangeType (\s a -> s {_pChangeType = a})

instance FromXML Parameter where
  parseXML x =
    Parameter'
      <$> (x .@? "ParameterValue")
      <*> (x .@? "MinimumEngineVersion")
      <*> (x .@? "Source")
      <*> (x .@? "IsModifiable")
      <*> (x .@? "DataType")
      <*> (x .@? "AllowedValues")
      <*> (x .@? "ParameterName")
      <*> (x .@? "Description")
      <*> (x .@? "ChangeType")

instance Hashable Parameter

instance NFData Parameter

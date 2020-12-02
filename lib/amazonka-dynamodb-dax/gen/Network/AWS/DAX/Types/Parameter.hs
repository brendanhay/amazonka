{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Parameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Parameter where

import Network.AWS.DAX.Types.ChangeType
import Network.AWS.DAX.Types.IsModifiable
import Network.AWS.DAX.Types.NodeTypeSpecificValue
import Network.AWS.DAX.Types.ParameterType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an individual setting that controls some aspect of DAX behavior.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pParameterValue :: !(Maybe Text),
    _pParameterType :: !(Maybe ParameterType),
    _pSource :: !(Maybe Text),
    _pIsModifiable :: !(Maybe IsModifiable),
    _pDataType :: !(Maybe Text),
    _pNodeTypeSpecificValues :: !(Maybe [NodeTypeSpecificValue]),
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
-- * 'pParameterValue' - The value for the parameter.
--
-- * 'pParameterType' - Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
--
-- * 'pSource' - How the parameter is defined. For example, @system@ denotes a system-defined parameter.
--
-- * 'pIsModifiable' - Whether the customer is allowed to modify the parameter.
--
-- * 'pDataType' - The data type of the parameter. For example, @integer@ :
--
-- * 'pNodeTypeSpecificValues' - A list of node types, and specific parameter values for each node.
--
-- * 'pAllowedValues' - A range of values within which the parameter can be set.
--
-- * 'pParameterName' - The name of the parameter.
--
-- * 'pDescription' - A description of the parameter
--
-- * 'pChangeType' - The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
parameter ::
  Parameter
parameter =
  Parameter'
    { _pParameterValue = Nothing,
      _pParameterType = Nothing,
      _pSource = Nothing,
      _pIsModifiable = Nothing,
      _pDataType = Nothing,
      _pNodeTypeSpecificValues = Nothing,
      _pAllowedValues = Nothing,
      _pParameterName = Nothing,
      _pDescription = Nothing,
      _pChangeType = Nothing
    }

-- | The value for the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s {_pParameterValue = a})

-- | Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
pParameterType :: Lens' Parameter (Maybe ParameterType)
pParameterType = lens _pParameterType (\s a -> s {_pParameterType = a})

-- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s {_pSource = a})

-- | Whether the customer is allowed to modify the parameter.
pIsModifiable :: Lens' Parameter (Maybe IsModifiable)
pIsModifiable = lens _pIsModifiable (\s a -> s {_pIsModifiable = a})

-- | The data type of the parameter. For example, @integer@ :
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s {_pDataType = a})

-- | A list of node types, and specific parameter values for each node.
pNodeTypeSpecificValues :: Lens' Parameter [NodeTypeSpecificValue]
pNodeTypeSpecificValues = lens _pNodeTypeSpecificValues (\s a -> s {_pNodeTypeSpecificValues = a}) . _Default . _Coerce

-- | A range of values within which the parameter can be set.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s {_pAllowedValues = a})

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s {_pParameterName = a})

-- | A description of the parameter
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s {_pDescription = a})

-- | The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
pChangeType :: Lens' Parameter (Maybe ChangeType)
pChangeType = lens _pChangeType (\s a -> s {_pChangeType = a})

instance FromJSON Parameter where
  parseJSON =
    withObject
      "Parameter"
      ( \x ->
          Parameter'
            <$> (x .:? "ParameterValue")
            <*> (x .:? "ParameterType")
            <*> (x .:? "Source")
            <*> (x .:? "IsModifiable")
            <*> (x .:? "DataType")
            <*> (x .:? "NodeTypeSpecificValues" .!= mempty)
            <*> (x .:? "AllowedValues")
            <*> (x .:? "ParameterName")
            <*> (x .:? "Description")
            <*> (x .:? "ChangeType")
      )

instance Hashable Parameter

instance NFData Parameter

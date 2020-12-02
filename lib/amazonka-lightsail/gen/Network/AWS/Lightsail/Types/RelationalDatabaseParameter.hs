{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the parameters of a database.
--
--
--
-- /See:/ 'relationalDatabaseParameter' smart constructor.
data RelationalDatabaseParameter = RelationalDatabaseParameter'
  { _rdpApplyType ::
      !(Maybe Text),
    _rdpParameterValue :: !(Maybe Text),
    _rdpApplyMethod :: !(Maybe Text),
    _rdpDataType :: !(Maybe Text),
    _rdpIsModifiable :: !(Maybe Bool),
    _rdpAllowedValues :: !(Maybe Text),
    _rdpParameterName :: !(Maybe Text),
    _rdpDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RelationalDatabaseParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdpApplyType' - Specifies the engine-specific parameter type.
--
-- * 'rdpParameterValue' - Specifies the value of the parameter.
--
-- * 'rdpApplyMethod' - Indicates when parameter updates are applied. Can be @immediate@ or @pending-reboot@ .
--
-- * 'rdpDataType' - Specifies the valid data type for the parameter.
--
-- * 'rdpIsModifiable' - A Boolean value indicating whether the parameter can be modified.
--
-- * 'rdpAllowedValues' - Specifies the valid range of values for the parameter.
--
-- * 'rdpParameterName' - Specifies the name of the parameter.
--
-- * 'rdpDescription' - Provides a description of the parameter.
relationalDatabaseParameter ::
  RelationalDatabaseParameter
relationalDatabaseParameter =
  RelationalDatabaseParameter'
    { _rdpApplyType = Nothing,
      _rdpParameterValue = Nothing,
      _rdpApplyMethod = Nothing,
      _rdpDataType = Nothing,
      _rdpIsModifiable = Nothing,
      _rdpAllowedValues = Nothing,
      _rdpParameterName = Nothing,
      _rdpDescription = Nothing
    }

-- | Specifies the engine-specific parameter type.
rdpApplyType :: Lens' RelationalDatabaseParameter (Maybe Text)
rdpApplyType = lens _rdpApplyType (\s a -> s {_rdpApplyType = a})

-- | Specifies the value of the parameter.
rdpParameterValue :: Lens' RelationalDatabaseParameter (Maybe Text)
rdpParameterValue = lens _rdpParameterValue (\s a -> s {_rdpParameterValue = a})

-- | Indicates when parameter updates are applied. Can be @immediate@ or @pending-reboot@ .
rdpApplyMethod :: Lens' RelationalDatabaseParameter (Maybe Text)
rdpApplyMethod = lens _rdpApplyMethod (\s a -> s {_rdpApplyMethod = a})

-- | Specifies the valid data type for the parameter.
rdpDataType :: Lens' RelationalDatabaseParameter (Maybe Text)
rdpDataType = lens _rdpDataType (\s a -> s {_rdpDataType = a})

-- | A Boolean value indicating whether the parameter can be modified.
rdpIsModifiable :: Lens' RelationalDatabaseParameter (Maybe Bool)
rdpIsModifiable = lens _rdpIsModifiable (\s a -> s {_rdpIsModifiable = a})

-- | Specifies the valid range of values for the parameter.
rdpAllowedValues :: Lens' RelationalDatabaseParameter (Maybe Text)
rdpAllowedValues = lens _rdpAllowedValues (\s a -> s {_rdpAllowedValues = a})

-- | Specifies the name of the parameter.
rdpParameterName :: Lens' RelationalDatabaseParameter (Maybe Text)
rdpParameterName = lens _rdpParameterName (\s a -> s {_rdpParameterName = a})

-- | Provides a description of the parameter.
rdpDescription :: Lens' RelationalDatabaseParameter (Maybe Text)
rdpDescription = lens _rdpDescription (\s a -> s {_rdpDescription = a})

instance FromJSON RelationalDatabaseParameter where
  parseJSON =
    withObject
      "RelationalDatabaseParameter"
      ( \x ->
          RelationalDatabaseParameter'
            <$> (x .:? "applyType")
            <*> (x .:? "parameterValue")
            <*> (x .:? "applyMethod")
            <*> (x .:? "dataType")
            <*> (x .:? "isModifiable")
            <*> (x .:? "allowedValues")
            <*> (x .:? "parameterName")
            <*> (x .:? "description")
      )

instance Hashable RelationalDatabaseParameter

instance NFData RelationalDatabaseParameter

instance ToJSON RelationalDatabaseParameter where
  toJSON RelationalDatabaseParameter' {..} =
    object
      ( catMaybes
          [ ("applyType" .=) <$> _rdpApplyType,
            ("parameterValue" .=) <$> _rdpParameterValue,
            ("applyMethod" .=) <$> _rdpApplyMethod,
            ("dataType" .=) <$> _rdpDataType,
            ("isModifiable" .=) <$> _rdpIsModifiable,
            ("allowedValues" .=) <$> _rdpAllowedValues,
            ("parameterName" .=) <$> _rdpParameterName,
            ("description" .=) <$> _rdpDescription
          ]
      )

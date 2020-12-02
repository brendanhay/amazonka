{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentParameter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.DocumentParameterType

-- | Parameters specified in a System Manager document that run on the server when the command is run.
--
--
--
-- /See:/ 'documentParameter' smart constructor.
data DocumentParameter = DocumentParameter'
  { _dpName ::
      !(Maybe Text),
    _dpDefaultValue :: !(Maybe Text),
    _dpType :: !(Maybe DocumentParameterType),
    _dpDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpName' - The name of the parameter.
--
-- * 'dpDefaultValue' - If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
--
-- * 'dpType' - The type of parameter. The type can be either String or StringList.
--
-- * 'dpDescription' - A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
documentParameter ::
  DocumentParameter
documentParameter =
  DocumentParameter'
    { _dpName = Nothing,
      _dpDefaultValue = Nothing,
      _dpType = Nothing,
      _dpDescription = Nothing
    }

-- | The name of the parameter.
dpName :: Lens' DocumentParameter (Maybe Text)
dpName = lens _dpName (\s a -> s {_dpName = a})

-- | If specified, the default values for the parameters. Parameters without a default value are required. Parameters with a default value are optional.
dpDefaultValue :: Lens' DocumentParameter (Maybe Text)
dpDefaultValue = lens _dpDefaultValue (\s a -> s {_dpDefaultValue = a})

-- | The type of parameter. The type can be either String or StringList.
dpType :: Lens' DocumentParameter (Maybe DocumentParameterType)
dpType = lens _dpType (\s a -> s {_dpType = a})

-- | A description of what the parameter does, how to use it, the default value, and whether or not the parameter is optional.
dpDescription :: Lens' DocumentParameter (Maybe Text)
dpDescription = lens _dpDescription (\s a -> s {_dpDescription = a})

instance FromJSON DocumentParameter where
  parseJSON =
    withObject
      "DocumentParameter"
      ( \x ->
          DocumentParameter'
            <$> (x .:? "Name")
            <*> (x .:? "DefaultValue")
            <*> (x .:? "Type")
            <*> (x .:? "Description")
      )

instance Hashable DocumentParameter

instance NFData DocumentParameter

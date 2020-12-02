{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A value or list of parameter values.
--
--
--
-- /See:/ 'parameterValue' smart constructor.
data ParameterValue = ParameterValue'
  { _pvId :: !Text,
    _pvStringValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvId' - The ID of the parameter value.
--
-- * 'pvStringValue' - The field value, expressed as a String.
parameterValue ::
  -- | 'pvId'
  Text ->
  -- | 'pvStringValue'
  Text ->
  ParameterValue
parameterValue pId_ pStringValue_ =
  ParameterValue' {_pvId = pId_, _pvStringValue = pStringValue_}

-- | The ID of the parameter value.
pvId :: Lens' ParameterValue Text
pvId = lens _pvId (\s a -> s {_pvId = a})

-- | The field value, expressed as a String.
pvStringValue :: Lens' ParameterValue Text
pvStringValue = lens _pvStringValue (\s a -> s {_pvStringValue = a})

instance FromJSON ParameterValue where
  parseJSON =
    withObject
      "ParameterValue"
      (\x -> ParameterValue' <$> (x .: "id") <*> (x .: "stringValue"))

instance Hashable ParameterValue

instance NFData ParameterValue

instance ToJSON ParameterValue where
  toJSON ParameterValue' {..} =
    object
      ( catMaybes
          [Just ("id" .= _pvId), Just ("stringValue" .= _pvStringValue)]
      )

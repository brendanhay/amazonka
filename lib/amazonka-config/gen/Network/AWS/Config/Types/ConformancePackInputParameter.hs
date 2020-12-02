{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackInputParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackInputParameter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Input parameters in the form of key-value pairs for the conformance pack, both of which you define. Keys can have a maximum character length of 255 characters, and values can have a maximum length of 4096 characters.
--
--
--
-- /See:/ 'conformancePackInputParameter' smart constructor.
data ConformancePackInputParameter = ConformancePackInputParameter'
  { _cpipParameterName ::
      !Text,
    _cpipParameterValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConformancePackInputParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpipParameterName' - One part of a key-value pair.
--
-- * 'cpipParameterValue' - Another part of the key-value pair.
conformancePackInputParameter ::
  -- | 'cpipParameterName'
  Text ->
  -- | 'cpipParameterValue'
  Text ->
  ConformancePackInputParameter
conformancePackInputParameter pParameterName_ pParameterValue_ =
  ConformancePackInputParameter'
    { _cpipParameterName =
        pParameterName_,
      _cpipParameterValue = pParameterValue_
    }

-- | One part of a key-value pair.
cpipParameterName :: Lens' ConformancePackInputParameter Text
cpipParameterName = lens _cpipParameterName (\s a -> s {_cpipParameterName = a})

-- | Another part of the key-value pair.
cpipParameterValue :: Lens' ConformancePackInputParameter Text
cpipParameterValue = lens _cpipParameterValue (\s a -> s {_cpipParameterValue = a})

instance FromJSON ConformancePackInputParameter where
  parseJSON =
    withObject
      "ConformancePackInputParameter"
      ( \x ->
          ConformancePackInputParameter'
            <$> (x .: "ParameterName") <*> (x .: "ParameterValue")
      )

instance Hashable ConformancePackInputParameter

instance NFData ConformancePackInputParameter

instance ToJSON ConformancePackInputParameter where
  toJSON ConformancePackInputParameter' {..} =
    object
      ( catMaybes
          [ Just ("ParameterName" .= _cpipParameterName),
            Just ("ParameterValue" .= _cpipParameterValue)
          ]
      )

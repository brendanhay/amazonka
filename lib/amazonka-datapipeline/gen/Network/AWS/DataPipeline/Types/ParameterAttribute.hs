{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.ParameterAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.ParameterAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The attributes allowed or specified with a parameter object.
--
--
--
-- /See:/ 'parameterAttribute' smart constructor.
data ParameterAttribute = ParameterAttribute'
  { _paKey :: !Text,
    _paStringValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ParameterAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paKey' - The field identifier.
--
-- * 'paStringValue' - The field value, expressed as a String.
parameterAttribute ::
  -- | 'paKey'
  Text ->
  -- | 'paStringValue'
  Text ->
  ParameterAttribute
parameterAttribute pKey_ pStringValue_ =
  ParameterAttribute'
    { _paKey = pKey_,
      _paStringValue = pStringValue_
    }

-- | The field identifier.
paKey :: Lens' ParameterAttribute Text
paKey = lens _paKey (\s a -> s {_paKey = a})

-- | The field value, expressed as a String.
paStringValue :: Lens' ParameterAttribute Text
paStringValue = lens _paStringValue (\s a -> s {_paStringValue = a})

instance FromJSON ParameterAttribute where
  parseJSON =
    withObject
      "ParameterAttribute"
      ( \x ->
          ParameterAttribute' <$> (x .: "key") <*> (x .: "stringValue")
      )

instance Hashable ParameterAttribute

instance NFData ParameterAttribute

instance ToJSON ParameterAttribute where
  toJSON ParameterAttribute' {..} =
    object
      ( catMaybes
          [Just ("key" .= _paKey), Just ("stringValue" .= _paStringValue)]
      )

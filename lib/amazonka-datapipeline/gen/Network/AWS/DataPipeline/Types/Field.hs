{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.Field
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.Field where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key-value pair that describes a property of a pipeline object. The value is specified as either a string value (@StringValue@ ) or a reference to another object (@RefValue@ ) but not as both.
--
--
--
-- /See:/ 'field' smart constructor.
data Field = Field'
  { _fRefValue :: !(Maybe Text),
    _fStringValue :: !(Maybe Text),
    _fKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Field' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fRefValue' - The field value, expressed as the identifier of another object.
--
-- * 'fStringValue' - The field value, expressed as a String.
--
-- * 'fKey' - The field identifier.
field ::
  -- | 'fKey'
  Text ->
  Field
field pKey_ =
  Field'
    { _fRefValue = Nothing,
      _fStringValue = Nothing,
      _fKey = pKey_
    }

-- | The field value, expressed as the identifier of another object.
fRefValue :: Lens' Field (Maybe Text)
fRefValue = lens _fRefValue (\s a -> s {_fRefValue = a})

-- | The field value, expressed as a String.
fStringValue :: Lens' Field (Maybe Text)
fStringValue = lens _fStringValue (\s a -> s {_fStringValue = a})

-- | The field identifier.
fKey :: Lens' Field Text
fKey = lens _fKey (\s a -> s {_fKey = a})

instance FromJSON Field where
  parseJSON =
    withObject
      "Field"
      ( \x ->
          Field'
            <$> (x .:? "refValue") <*> (x .:? "stringValue") <*> (x .: "key")
      )

instance Hashable Field

instance NFData Field

instance ToJSON Field where
  toJSON Field' {..} =
    object
      ( catMaybes
          [ ("refValue" .=) <$> _fRefValue,
            ("stringValue" .=) <$> _fStringValue,
            Just ("key" .= _fKey)
          ]
      )

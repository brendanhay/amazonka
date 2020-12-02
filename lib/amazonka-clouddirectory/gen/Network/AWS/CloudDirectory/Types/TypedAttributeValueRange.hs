{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedAttributeValueRange where

import Network.AWS.CloudDirectory.Types.RangeMode
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A range of attribute values. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_range_filters.html Range Filters> .
--
--
--
-- /See:/ 'typedAttributeValueRange' smart constructor.
data TypedAttributeValueRange = TypedAttributeValueRange'
  { _tavrEndValue ::
      !(Maybe TypedAttributeValue),
    _tavrStartValue ::
      !(Maybe TypedAttributeValue),
    _tavrStartMode :: !RangeMode,
    _tavrEndMode :: !RangeMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedAttributeValueRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tavrEndValue' - The attribute value to terminate the range at.
--
-- * 'tavrStartValue' - The value to start the range at.
--
-- * 'tavrStartMode' - The inclusive or exclusive range start.
--
-- * 'tavrEndMode' - The inclusive or exclusive range end.
typedAttributeValueRange ::
  -- | 'tavrStartMode'
  RangeMode ->
  -- | 'tavrEndMode'
  RangeMode ->
  TypedAttributeValueRange
typedAttributeValueRange pStartMode_ pEndMode_ =
  TypedAttributeValueRange'
    { _tavrEndValue = Nothing,
      _tavrStartValue = Nothing,
      _tavrStartMode = pStartMode_,
      _tavrEndMode = pEndMode_
    }

-- | The attribute value to terminate the range at.
tavrEndValue :: Lens' TypedAttributeValueRange (Maybe TypedAttributeValue)
tavrEndValue = lens _tavrEndValue (\s a -> s {_tavrEndValue = a})

-- | The value to start the range at.
tavrStartValue :: Lens' TypedAttributeValueRange (Maybe TypedAttributeValue)
tavrStartValue = lens _tavrStartValue (\s a -> s {_tavrStartValue = a})

-- | The inclusive or exclusive range start.
tavrStartMode :: Lens' TypedAttributeValueRange RangeMode
tavrStartMode = lens _tavrStartMode (\s a -> s {_tavrStartMode = a})

-- | The inclusive or exclusive range end.
tavrEndMode :: Lens' TypedAttributeValueRange RangeMode
tavrEndMode = lens _tavrEndMode (\s a -> s {_tavrEndMode = a})

instance Hashable TypedAttributeValueRange

instance NFData TypedAttributeValueRange

instance ToJSON TypedAttributeValueRange where
  toJSON TypedAttributeValueRange' {..} =
    object
      ( catMaybes
          [ ("EndValue" .=) <$> _tavrEndValue,
            ("StartValue" .=) <$> _tavrStartValue,
            Just ("StartMode" .= _tavrStartMode),
            Just ("EndMode" .= _tavrEndMode)
          ]
      )

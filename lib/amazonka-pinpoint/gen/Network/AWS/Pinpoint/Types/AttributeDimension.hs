{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AttributeDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AttributeDimension where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.AttributeType
import Network.AWS.Prelude

-- | Specifies attribute-based criteria for including or excluding endpoints from a segment.
--
--
--
-- /See:/ 'attributeDimension' smart constructor.
data AttributeDimension = AttributeDimension'
  { _adAttributeType ::
      !(Maybe AttributeType),
    _adValues :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributeDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adAttributeType' - The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
--
-- * 'adValues' - The criteria values to use for the segment dimension. Depending on the value of the AttributeType property, endpoints are included or excluded from the segment if their attribute values match the criteria values.
attributeDimension ::
  AttributeDimension
attributeDimension =
  AttributeDimension'
    { _adAttributeType = Nothing,
      _adValues = mempty
    }

-- | The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
adAttributeType :: Lens' AttributeDimension (Maybe AttributeType)
adAttributeType = lens _adAttributeType (\s a -> s {_adAttributeType = a})

-- | The criteria values to use for the segment dimension. Depending on the value of the AttributeType property, endpoints are included or excluded from the segment if their attribute values match the criteria values.
adValues :: Lens' AttributeDimension [Text]
adValues = lens _adValues (\s a -> s {_adValues = a}) . _Coerce

instance FromJSON AttributeDimension where
  parseJSON =
    withObject
      "AttributeDimension"
      ( \x ->
          AttributeDimension'
            <$> (x .:? "AttributeType") <*> (x .:? "Values" .!= mempty)
      )

instance Hashable AttributeDimension

instance NFData AttributeDimension

instance ToJSON AttributeDimension where
  toJSON AttributeDimension' {..} =
    object
      ( catMaybes
          [ ("AttributeType" .=) <$> _adAttributeType,
            Just ("Values" .= _adValues)
          ]
      )

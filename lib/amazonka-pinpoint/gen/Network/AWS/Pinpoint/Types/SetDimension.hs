{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SetDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SetDimension where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.DimensionType
import Network.AWS.Prelude

-- | Specifies the dimension type and values for a segment dimension.
--
--
--
-- /See:/ 'setDimension' smart constructor.
data SetDimension = SetDimension'
  { _sdDimensionType ::
      !(Maybe DimensionType),
    _sdValues :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdDimensionType' - The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
--
-- * 'sdValues' - The criteria values to use for the segment dimension. Depending on the value of the DimensionType property, endpoints are included or excluded from the segment if their values match the criteria values.
setDimension ::
  SetDimension
setDimension =
  SetDimension' {_sdDimensionType = Nothing, _sdValues = mempty}

-- | The type of segment dimension to use. Valid values are: INCLUSIVE, endpoints that match the criteria are included in the segment; and, EXCLUSIVE, endpoints that match the criteria are excluded from the segment.
sdDimensionType :: Lens' SetDimension (Maybe DimensionType)
sdDimensionType = lens _sdDimensionType (\s a -> s {_sdDimensionType = a})

-- | The criteria values to use for the segment dimension. Depending on the value of the DimensionType property, endpoints are included or excluded from the segment if their values match the criteria values.
sdValues :: Lens' SetDimension [Text]
sdValues = lens _sdValues (\s a -> s {_sdValues = a}) . _Coerce

instance FromJSON SetDimension where
  parseJSON =
    withObject
      "SetDimension"
      ( \x ->
          SetDimension'
            <$> (x .:? "DimensionType") <*> (x .:? "Values" .!= mempty)
      )

instance Hashable SetDimension

instance NFData SetDimension

instance ToJSON SetDimension where
  toJSON SetDimension' {..} =
    object
      ( catMaybes
          [ ("DimensionType" .=) <$> _sdDimensionType,
            Just ("Values" .= _sdValues)
          ]
      )

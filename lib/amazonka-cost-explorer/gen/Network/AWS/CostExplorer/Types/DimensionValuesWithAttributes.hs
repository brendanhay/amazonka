{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValuesWithAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata of a specific type that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
--
--
-- /See:/ 'dimensionValuesWithAttributes' smart constructor.
data DimensionValuesWithAttributes = DimensionValuesWithAttributes'
  { _dvwaValue ::
      !(Maybe Text),
    _dvwaAttributes ::
      !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DimensionValuesWithAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvwaValue' - The value of a dimension with a specific attribute.
--
-- * 'dvwaAttributes' - The attribute that applies to a specific @Dimension@ .
dimensionValuesWithAttributes ::
  DimensionValuesWithAttributes
dimensionValuesWithAttributes =
  DimensionValuesWithAttributes'
    { _dvwaValue = Nothing,
      _dvwaAttributes = Nothing
    }

-- | The value of a dimension with a specific attribute.
dvwaValue :: Lens' DimensionValuesWithAttributes (Maybe Text)
dvwaValue = lens _dvwaValue (\s a -> s {_dvwaValue = a})

-- | The attribute that applies to a specific @Dimension@ .
dvwaAttributes :: Lens' DimensionValuesWithAttributes (HashMap Text (Text))
dvwaAttributes = lens _dvwaAttributes (\s a -> s {_dvwaAttributes = a}) . _Default . _Map

instance FromJSON DimensionValuesWithAttributes where
  parseJSON =
    withObject
      "DimensionValuesWithAttributes"
      ( \x ->
          DimensionValuesWithAttributes'
            <$> (x .:? "Value") <*> (x .:? "Attributes" .!= mempty)
      )

instance Hashable DimensionValuesWithAttributes

instance NFData DimensionValuesWithAttributes

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.DimensionValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.DimensionValues where

import Network.AWS.CostExplorer.Types.Dimension
import Network.AWS.CostExplorer.Types.MatchOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
--
--
-- /See:/ 'dimensionValues' smart constructor.
data DimensionValues = DimensionValues'
  { _dvValues ::
      !(Maybe [Text]),
    _dvKey :: !(Maybe Dimension),
    _dvMatchOptions :: !(Maybe [MatchOption])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DimensionValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvValues' - The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
--
-- * 'dvKey' - The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
--
-- * 'dvMatchOptions' - The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
dimensionValues ::
  DimensionValues
dimensionValues =
  DimensionValues'
    { _dvValues = Nothing,
      _dvKey = Nothing,
      _dvMatchOptions = Nothing
    }

-- | The metadata values that you can use to filter and group your results. You can use @GetDimensionValues@ to find specific values.
dvValues :: Lens' DimensionValues [Text]
dvValues = lens _dvValues (\s a -> s {_dvValues = a}) . _Default . _Coerce

-- | The names of the metadata types that you can use to filter and group your results. For example, @AZ@ returns a list of Availability Zones.
dvKey :: Lens' DimensionValues (Maybe Dimension)
dvKey = lens _dvKey (\s a -> s {_dvKey = a})

-- | The match options that you can use to filter your results. @MatchOptions@ is only applicable for actions related to Cost Category. The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@ .
dvMatchOptions :: Lens' DimensionValues [MatchOption]
dvMatchOptions = lens _dvMatchOptions (\s a -> s {_dvMatchOptions = a}) . _Default . _Coerce

instance FromJSON DimensionValues where
  parseJSON =
    withObject
      "DimensionValues"
      ( \x ->
          DimensionValues'
            <$> (x .:? "Values" .!= mempty)
            <*> (x .:? "Key")
            <*> (x .:? "MatchOptions" .!= mempty)
      )

instance Hashable DimensionValues

instance NFData DimensionValues

instance ToJSON DimensionValues where
  toJSON DimensionValues' {..} =
    object
      ( catMaybes
          [ ("Values" .=) <$> _dvValues,
            ("Key" .=) <$> _dvKey,
            ("MatchOptions" .=) <$> _dvMatchOptions
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Group where

import Network.AWS.CostExplorer.Types.MetricValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | One level of grouped data in the results.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gMetrics :: !(Maybe (Map Text (MetricValue))),
    _gKeys :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gMetrics' - The metrics that are included in this group.
--
-- * 'gKeys' - The keys that are included in this group.
group' ::
  Group
group' = Group' {_gMetrics = Nothing, _gKeys = Nothing}

-- | The metrics that are included in this group.
gMetrics :: Lens' Group (HashMap Text (MetricValue))
gMetrics = lens _gMetrics (\s a -> s {_gMetrics = a}) . _Default . _Map

-- | The keys that are included in this group.
gKeys :: Lens' Group [Text]
gKeys = lens _gKeys (\s a -> s {_gKeys = a}) . _Default . _Coerce

instance FromJSON Group where
  parseJSON =
    withObject
      "Group"
      ( \x ->
          Group'
            <$> (x .:? "Metrics" .!= mempty) <*> (x .:? "Keys" .!= mempty)
      )

instance Hashable Group

instance NFData Group

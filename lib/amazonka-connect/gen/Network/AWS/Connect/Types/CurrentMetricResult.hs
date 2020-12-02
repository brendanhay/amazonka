{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricResult where

import Network.AWS.Connect.Types.CurrentMetricData
import Network.AWS.Connect.Types.Dimensions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a set of real-time metrics.
--
--
--
-- /See:/ 'currentMetricResult' smart constructor.
data CurrentMetricResult = CurrentMetricResult'
  { _cmrCollections ::
      !(Maybe [CurrentMetricData]),
    _cmrDimensions :: !(Maybe Dimensions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CurrentMetricResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrCollections' - The set of metrics.
--
-- * 'cmrDimensions' - The dimensions for the metrics.
currentMetricResult ::
  CurrentMetricResult
currentMetricResult =
  CurrentMetricResult'
    { _cmrCollections = Nothing,
      _cmrDimensions = Nothing
    }

-- | The set of metrics.
cmrCollections :: Lens' CurrentMetricResult [CurrentMetricData]
cmrCollections = lens _cmrCollections (\s a -> s {_cmrCollections = a}) . _Default . _Coerce

-- | The dimensions for the metrics.
cmrDimensions :: Lens' CurrentMetricResult (Maybe Dimensions)
cmrDimensions = lens _cmrDimensions (\s a -> s {_cmrDimensions = a})

instance FromJSON CurrentMetricResult where
  parseJSON =
    withObject
      "CurrentMetricResult"
      ( \x ->
          CurrentMetricResult'
            <$> (x .:? "Collections" .!= mempty) <*> (x .:? "Dimensions")
      )

instance Hashable CurrentMetricResult

instance NFData CurrentMetricResult

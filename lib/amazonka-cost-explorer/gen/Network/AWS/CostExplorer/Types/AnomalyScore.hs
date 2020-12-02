{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyScore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyScore where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Quantifies the anomaly. The higher score means that it is more anomalous.
--
--
--
-- /See:/ 'anomalyScore' smart constructor.
data AnomalyScore = AnomalyScore'
  { _asMaxScore :: !Double,
    _asCurrentScore :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnomalyScore' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asMaxScore' - The maximum score observed during the @AnomalyDateInterval@ .
--
-- * 'asCurrentScore' - The last observed score.
anomalyScore ::
  -- | 'asMaxScore'
  Double ->
  -- | 'asCurrentScore'
  Double ->
  AnomalyScore
anomalyScore pMaxScore_ pCurrentScore_ =
  AnomalyScore'
    { _asMaxScore = pMaxScore_,
      _asCurrentScore = pCurrentScore_
    }

-- | The maximum score observed during the @AnomalyDateInterval@ .
asMaxScore :: Lens' AnomalyScore Double
asMaxScore = lens _asMaxScore (\s a -> s {_asMaxScore = a})

-- | The last observed score.
asCurrentScore :: Lens' AnomalyScore Double
asCurrentScore = lens _asCurrentScore (\s a -> s {_asCurrentScore = a})

instance FromJSON AnomalyScore where
  parseJSON =
    withObject
      "AnomalyScore"
      ( \x ->
          AnomalyScore' <$> (x .: "MaxScore") <*> (x .: "CurrentScore")
      )

instance Hashable AnomalyScore

instance NFData AnomalyScore

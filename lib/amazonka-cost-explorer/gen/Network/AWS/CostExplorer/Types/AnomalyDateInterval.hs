{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalyDateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalyDateInterval where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The time period for an anomaly.
--
--
--
-- /See:/ 'anomalyDateInterval' smart constructor.
data AnomalyDateInterval = AnomalyDateInterval'
  { _adiEndDate ::
      !(Maybe Text),
    _adiStartDate :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnomalyDateInterval' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adiEndDate' - The last date an anomaly was observed.
--
-- * 'adiStartDate' - The first date an anomaly was observed.
anomalyDateInterval ::
  -- | 'adiStartDate'
  Text ->
  AnomalyDateInterval
anomalyDateInterval pStartDate_ =
  AnomalyDateInterval'
    { _adiEndDate = Nothing,
      _adiStartDate = pStartDate_
    }

-- | The last date an anomaly was observed.
adiEndDate :: Lens' AnomalyDateInterval (Maybe Text)
adiEndDate = lens _adiEndDate (\s a -> s {_adiEndDate = a})

-- | The first date an anomaly was observed.
adiStartDate :: Lens' AnomalyDateInterval Text
adiStartDate = lens _adiStartDate (\s a -> s {_adiStartDate = a})

instance Hashable AnomalyDateInterval

instance NFData AnomalyDateInterval

instance ToJSON AnomalyDateInterval where
  toJSON AnomalyDateInterval' {..} =
    object
      ( catMaybes
          [ ("EndDate" .=) <$> _adiEndDate,
            Just ("StartDate" .= _adiStartDate)
          ]
      )

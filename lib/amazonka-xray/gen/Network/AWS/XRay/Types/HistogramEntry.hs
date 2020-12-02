{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.HistogramEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.HistogramEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An entry in a histogram for a statistic. A histogram maps the range of observed values on the X axis, and the prevalence of each value on the Y axis.
--
--
--
-- /See:/ 'histogramEntry' smart constructor.
data HistogramEntry = HistogramEntry'
  { _heCount :: !(Maybe Int),
    _heValue :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HistogramEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heCount' - The prevalence of the entry.
--
-- * 'heValue' - The value of the entry.
histogramEntry ::
  HistogramEntry
histogramEntry =
  HistogramEntry' {_heCount = Nothing, _heValue = Nothing}

-- | The prevalence of the entry.
heCount :: Lens' HistogramEntry (Maybe Int)
heCount = lens _heCount (\s a -> s {_heCount = a})

-- | The value of the entry.
heValue :: Lens' HistogramEntry (Maybe Double)
heValue = lens _heValue (\s a -> s {_heValue = a})

instance FromJSON HistogramEntry where
  parseJSON =
    withObject
      "HistogramEntry"
      (\x -> HistogramEntry' <$> (x .:? "Count") <*> (x .:? "Value"))

instance Hashable HistogramEntry

instance NFData HistogramEntry

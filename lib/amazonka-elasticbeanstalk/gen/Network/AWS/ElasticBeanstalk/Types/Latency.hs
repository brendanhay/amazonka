{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Latency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Latency where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds.
--
--
--
-- /See:/ 'latency' smart constructor.
data Latency = Latency'
  { _lP75 :: !(Maybe Double),
    _lP50 :: !(Maybe Double),
    _lP85 :: !(Maybe Double),
    _lP999 :: !(Maybe Double),
    _lP90 :: !(Maybe Double),
    _lP95 :: !(Maybe Double),
    _lP99 :: !(Maybe Double),
    _lP10 :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Latency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lP75' - The average latency for the slowest 25 percent of requests over the last 10 seconds.
--
-- * 'lP50' - The average latency for the slowest 50 percent of requests over the last 10 seconds.
--
-- * 'lP85' - The average latency for the slowest 15 percent of requests over the last 10 seconds.
--
-- * 'lP999' - The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
--
-- * 'lP90' - The average latency for the slowest 10 percent of requests over the last 10 seconds.
--
-- * 'lP95' - The average latency for the slowest 5 percent of requests over the last 10 seconds.
--
-- * 'lP99' - The average latency for the slowest 1 percent of requests over the last 10 seconds.
--
-- * 'lP10' - The average latency for the slowest 90 percent of requests over the last 10 seconds.
latency ::
  Latency
latency =
  Latency'
    { _lP75 = Nothing,
      _lP50 = Nothing,
      _lP85 = Nothing,
      _lP999 = Nothing,
      _lP90 = Nothing,
      _lP95 = Nothing,
      _lP99 = Nothing,
      _lP10 = Nothing
    }

-- | The average latency for the slowest 25 percent of requests over the last 10 seconds.
lP75 :: Lens' Latency (Maybe Double)
lP75 = lens _lP75 (\s a -> s {_lP75 = a})

-- | The average latency for the slowest 50 percent of requests over the last 10 seconds.
lP50 :: Lens' Latency (Maybe Double)
lP50 = lens _lP50 (\s a -> s {_lP50 = a})

-- | The average latency for the slowest 15 percent of requests over the last 10 seconds.
lP85 :: Lens' Latency (Maybe Double)
lP85 = lens _lP85 (\s a -> s {_lP85 = a})

-- | The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
lP999 :: Lens' Latency (Maybe Double)
lP999 = lens _lP999 (\s a -> s {_lP999 = a})

-- | The average latency for the slowest 10 percent of requests over the last 10 seconds.
lP90 :: Lens' Latency (Maybe Double)
lP90 = lens _lP90 (\s a -> s {_lP90 = a})

-- | The average latency for the slowest 5 percent of requests over the last 10 seconds.
lP95 :: Lens' Latency (Maybe Double)
lP95 = lens _lP95 (\s a -> s {_lP95 = a})

-- | The average latency for the slowest 1 percent of requests over the last 10 seconds.
lP99 :: Lens' Latency (Maybe Double)
lP99 = lens _lP99 (\s a -> s {_lP99 = a})

-- | The average latency for the slowest 90 percent of requests over the last 10 seconds.
lP10 :: Lens' Latency (Maybe Double)
lP10 = lens _lP10 (\s a -> s {_lP10 = a})

instance FromXML Latency where
  parseXML x =
    Latency'
      <$> (x .@? "P75")
      <*> (x .@? "P50")
      <*> (x .@? "P85")
      <*> (x .@? "P999")
      <*> (x .@? "P90")
      <*> (x .@? "P95")
      <*> (x .@? "P99")
      <*> (x .@? "P10")

instance Hashable Latency

instance NFData Latency

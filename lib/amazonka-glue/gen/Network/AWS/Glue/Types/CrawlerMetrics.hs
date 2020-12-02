{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerMetrics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metrics for a specified crawler.
--
--
--
-- /See:/ 'crawlerMetrics' smart constructor.
data CrawlerMetrics = CrawlerMetrics'
  { _cmLastRuntimeSeconds ::
      !(Maybe Double),
    _cmTablesCreated :: !(Maybe Nat),
    _cmStillEstimating :: !(Maybe Bool),
    _cmMedianRuntimeSeconds :: !(Maybe Double),
    _cmTimeLeftSeconds :: !(Maybe Double),
    _cmTablesDeleted :: !(Maybe Nat),
    _cmTablesUpdated :: !(Maybe Nat),
    _cmCrawlerName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CrawlerMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmLastRuntimeSeconds' - The duration of the crawler's most recent run, in seconds.
--
-- * 'cmTablesCreated' - The number of tables created by this crawler.
--
-- * 'cmStillEstimating' - True if the crawler is still estimating how long it will take to complete this run.
--
-- * 'cmMedianRuntimeSeconds' - The median duration of this crawler's runs, in seconds.
--
-- * 'cmTimeLeftSeconds' - The estimated time left to complete a running crawl.
--
-- * 'cmTablesDeleted' - The number of tables deleted by this crawler.
--
-- * 'cmTablesUpdated' - The number of tables updated by this crawler.
--
-- * 'cmCrawlerName' - The name of the crawler.
crawlerMetrics ::
  CrawlerMetrics
crawlerMetrics =
  CrawlerMetrics'
    { _cmLastRuntimeSeconds = Nothing,
      _cmTablesCreated = Nothing,
      _cmStillEstimating = Nothing,
      _cmMedianRuntimeSeconds = Nothing,
      _cmTimeLeftSeconds = Nothing,
      _cmTablesDeleted = Nothing,
      _cmTablesUpdated = Nothing,
      _cmCrawlerName = Nothing
    }

-- | The duration of the crawler's most recent run, in seconds.
cmLastRuntimeSeconds :: Lens' CrawlerMetrics (Maybe Double)
cmLastRuntimeSeconds = lens _cmLastRuntimeSeconds (\s a -> s {_cmLastRuntimeSeconds = a})

-- | The number of tables created by this crawler.
cmTablesCreated :: Lens' CrawlerMetrics (Maybe Natural)
cmTablesCreated = lens _cmTablesCreated (\s a -> s {_cmTablesCreated = a}) . mapping _Nat

-- | True if the crawler is still estimating how long it will take to complete this run.
cmStillEstimating :: Lens' CrawlerMetrics (Maybe Bool)
cmStillEstimating = lens _cmStillEstimating (\s a -> s {_cmStillEstimating = a})

-- | The median duration of this crawler's runs, in seconds.
cmMedianRuntimeSeconds :: Lens' CrawlerMetrics (Maybe Double)
cmMedianRuntimeSeconds = lens _cmMedianRuntimeSeconds (\s a -> s {_cmMedianRuntimeSeconds = a})

-- | The estimated time left to complete a running crawl.
cmTimeLeftSeconds :: Lens' CrawlerMetrics (Maybe Double)
cmTimeLeftSeconds = lens _cmTimeLeftSeconds (\s a -> s {_cmTimeLeftSeconds = a})

-- | The number of tables deleted by this crawler.
cmTablesDeleted :: Lens' CrawlerMetrics (Maybe Natural)
cmTablesDeleted = lens _cmTablesDeleted (\s a -> s {_cmTablesDeleted = a}) . mapping _Nat

-- | The number of tables updated by this crawler.
cmTablesUpdated :: Lens' CrawlerMetrics (Maybe Natural)
cmTablesUpdated = lens _cmTablesUpdated (\s a -> s {_cmTablesUpdated = a}) . mapping _Nat

-- | The name of the crawler.
cmCrawlerName :: Lens' CrawlerMetrics (Maybe Text)
cmCrawlerName = lens _cmCrawlerName (\s a -> s {_cmCrawlerName = a})

instance FromJSON CrawlerMetrics where
  parseJSON =
    withObject
      "CrawlerMetrics"
      ( \x ->
          CrawlerMetrics'
            <$> (x .:? "LastRuntimeSeconds")
            <*> (x .:? "TablesCreated")
            <*> (x .:? "StillEstimating")
            <*> (x .:? "MedianRuntimeSeconds")
            <*> (x .:? "TimeLeftSeconds")
            <*> (x .:? "TablesDeleted")
            <*> (x .:? "TablesUpdated")
            <*> (x .:? "CrawlerName")
      )

instance Hashable CrawlerMetrics

instance NFData CrawlerMetrics

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerNodeDetails where

import Network.AWS.Glue.Types.Crawl
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a Crawler node present in the workflow.
--
--
--
-- /See:/ 'crawlerNodeDetails' smart constructor.
newtype CrawlerNodeDetails = CrawlerNodeDetails'
  { _cndCrawls ::
      Maybe [Crawl]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CrawlerNodeDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cndCrawls' - A list of crawls represented by the crawl node.
crawlerNodeDetails ::
  CrawlerNodeDetails
crawlerNodeDetails = CrawlerNodeDetails' {_cndCrawls = Nothing}

-- | A list of crawls represented by the crawl node.
cndCrawls :: Lens' CrawlerNodeDetails [Crawl]
cndCrawls = lens _cndCrawls (\s a -> s {_cndCrawls = a}) . _Default . _Coerce

instance FromJSON CrawlerNodeDetails where
  parseJSON =
    withObject
      "CrawlerNodeDetails"
      (\x -> CrawlerNodeDetails' <$> (x .:? "Crawls" .!= mempty))

instance Hashable CrawlerNodeDetails

instance NFData CrawlerNodeDetails

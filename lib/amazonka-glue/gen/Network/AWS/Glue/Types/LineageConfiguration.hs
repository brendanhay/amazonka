{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LineageConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LineageConfiguration where

import Network.AWS.Glue.Types.CrawlerLineageSettings
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies data lineage configuration settings for the crawler.
--
--
--
-- /See:/ 'lineageConfiguration' smart constructor.
newtype LineageConfiguration = LineageConfiguration'
  { _lcCrawlerLineageSettings ::
      Maybe CrawlerLineageSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LineageConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcCrawlerLineageSettings' - Specifies whether data lineage is enabled for the crawler. Valid values are:     * ENABLE: enables data lineage for the crawler     * DISABLE: disables data lineage for the crawler
lineageConfiguration ::
  LineageConfiguration
lineageConfiguration =
  LineageConfiguration' {_lcCrawlerLineageSettings = Nothing}

-- | Specifies whether data lineage is enabled for the crawler. Valid values are:     * ENABLE: enables data lineage for the crawler     * DISABLE: disables data lineage for the crawler
lcCrawlerLineageSettings :: Lens' LineageConfiguration (Maybe CrawlerLineageSettings)
lcCrawlerLineageSettings = lens _lcCrawlerLineageSettings (\s a -> s {_lcCrawlerLineageSettings = a})

instance FromJSON LineageConfiguration where
  parseJSON =
    withObject
      "LineageConfiguration"
      (\x -> LineageConfiguration' <$> (x .:? "CrawlerLineageSettings"))

instance Hashable LineageConfiguration

instance NFData LineageConfiguration

instance ToJSON LineageConfiguration where
  toJSON LineageConfiguration' {..} =
    object
      ( catMaybes
          [("CrawlerLineageSettings" .=) <$> _lcCrawlerLineageSettings]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RecrawlPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RecrawlPolicy where

import Network.AWS.Glue.Types.RecrawlBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | When crawling an Amazon S3 data source after the first crawl is complete, specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run. For more information, see <https://docs.aws.amazon.com/glue/latest/dg/incremental-crawls.html Incremental Crawls in AWS Glue> in the developer guide.
--
--
--
-- /See:/ 'recrawlPolicy' smart constructor.
newtype RecrawlPolicy = RecrawlPolicy'
  { _rpRecrawlBehavior ::
      Maybe RecrawlBehavior
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RecrawlPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpRecrawlBehavior' - Specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run. A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset again. A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that were added since the last crawler run.
recrawlPolicy ::
  RecrawlPolicy
recrawlPolicy = RecrawlPolicy' {_rpRecrawlBehavior = Nothing}

-- | Specifies whether to crawl the entire dataset again or to crawl only folders that were added since the last crawler run. A value of @CRAWL_EVERYTHING@ specifies crawling the entire dataset again. A value of @CRAWL_NEW_FOLDERS_ONLY@ specifies crawling only folders that were added since the last crawler run.
rpRecrawlBehavior :: Lens' RecrawlPolicy (Maybe RecrawlBehavior)
rpRecrawlBehavior = lens _rpRecrawlBehavior (\s a -> s {_rpRecrawlBehavior = a})

instance FromJSON RecrawlPolicy where
  parseJSON =
    withObject
      "RecrawlPolicy"
      (\x -> RecrawlPolicy' <$> (x .:? "RecrawlBehavior"))

instance Hashable RecrawlPolicy

instance NFData RecrawlPolicy

instance ToJSON RecrawlPolicy where
  toJSON RecrawlPolicy' {..} =
    object
      (catMaybes [("RecrawlBehavior" .=) <$> _rpRecrawlBehavior])

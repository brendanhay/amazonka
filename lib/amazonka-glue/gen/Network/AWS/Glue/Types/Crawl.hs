{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Crawl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Crawl where

import Network.AWS.Glue.Types.CrawlState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a crawl in the workflow.
--
--
--
-- /See:/ 'crawl' smart constructor.
data Crawl = Crawl'
  { _craCompletedOn :: !(Maybe POSIX),
    _craState :: !(Maybe CrawlState),
    _craStartedOn :: !(Maybe POSIX),
    _craLogStream :: !(Maybe Text),
    _craLogGroup :: !(Maybe Text),
    _craErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Crawl' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'craCompletedOn' - The date and time on which the crawl completed.
--
-- * 'craState' - The state of the crawler.
--
-- * 'craStartedOn' - The date and time on which the crawl started.
--
-- * 'craLogStream' - The log stream associated with the crawl.
--
-- * 'craLogGroup' - The log group associated with the crawl.
--
-- * 'craErrorMessage' - The error message associated with the crawl.
crawl ::
  Crawl
crawl =
  Crawl'
    { _craCompletedOn = Nothing,
      _craState = Nothing,
      _craStartedOn = Nothing,
      _craLogStream = Nothing,
      _craLogGroup = Nothing,
      _craErrorMessage = Nothing
    }

-- | The date and time on which the crawl completed.
craCompletedOn :: Lens' Crawl (Maybe UTCTime)
craCompletedOn = lens _craCompletedOn (\s a -> s {_craCompletedOn = a}) . mapping _Time

-- | The state of the crawler.
craState :: Lens' Crawl (Maybe CrawlState)
craState = lens _craState (\s a -> s {_craState = a})

-- | The date and time on which the crawl started.
craStartedOn :: Lens' Crawl (Maybe UTCTime)
craStartedOn = lens _craStartedOn (\s a -> s {_craStartedOn = a}) . mapping _Time

-- | The log stream associated with the crawl.
craLogStream :: Lens' Crawl (Maybe Text)
craLogStream = lens _craLogStream (\s a -> s {_craLogStream = a})

-- | The log group associated with the crawl.
craLogGroup :: Lens' Crawl (Maybe Text)
craLogGroup = lens _craLogGroup (\s a -> s {_craLogGroup = a})

-- | The error message associated with the crawl.
craErrorMessage :: Lens' Crawl (Maybe Text)
craErrorMessage = lens _craErrorMessage (\s a -> s {_craErrorMessage = a})

instance FromJSON Crawl where
  parseJSON =
    withObject
      "Crawl"
      ( \x ->
          Crawl'
            <$> (x .:? "CompletedOn")
            <*> (x .:? "State")
            <*> (x .:? "StartedOn")
            <*> (x .:? "LogStream")
            <*> (x .:? "LogGroup")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable Crawl

instance NFData Crawl

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Condition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Condition where

import Network.AWS.Glue.Types.CrawlState
import Network.AWS.Glue.Types.JobRunState
import Network.AWS.Glue.Types.LogicalOperator
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines a condition under which a trigger fires.
--
--
--
-- /See:/ 'condition' smart constructor.
data Condition = Condition'
  { _cCrawlState :: !(Maybe CrawlState),
    _cState :: !(Maybe JobRunState),
    _cJobName :: !(Maybe Text),
    _cLogicalOperator :: !(Maybe LogicalOperator),
    _cCrawlerName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Condition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cCrawlState' - The state of the crawler to which this condition applies.
--
-- * 'cState' - The condition state. Currently, the only job states that a trigger can listen for are @SUCCEEDED@ , @STOPPED@ , @FAILED@ , and @TIMEOUT@ . The only crawler states that a trigger can listen for are @SUCCEEDED@ , @FAILED@ , and @CANCELLED@ .
--
-- * 'cJobName' - The name of the job whose @JobRuns@ this condition applies to, and on which this trigger waits.
--
-- * 'cLogicalOperator' - A logical operator.
--
-- * 'cCrawlerName' - The name of the crawler to which this condition applies.
condition ::
  Condition
condition =
  Condition'
    { _cCrawlState = Nothing,
      _cState = Nothing,
      _cJobName = Nothing,
      _cLogicalOperator = Nothing,
      _cCrawlerName = Nothing
    }

-- | The state of the crawler to which this condition applies.
cCrawlState :: Lens' Condition (Maybe CrawlState)
cCrawlState = lens _cCrawlState (\s a -> s {_cCrawlState = a})

-- | The condition state. Currently, the only job states that a trigger can listen for are @SUCCEEDED@ , @STOPPED@ , @FAILED@ , and @TIMEOUT@ . The only crawler states that a trigger can listen for are @SUCCEEDED@ , @FAILED@ , and @CANCELLED@ .
cState :: Lens' Condition (Maybe JobRunState)
cState = lens _cState (\s a -> s {_cState = a})

-- | The name of the job whose @JobRuns@ this condition applies to, and on which this trigger waits.
cJobName :: Lens' Condition (Maybe Text)
cJobName = lens _cJobName (\s a -> s {_cJobName = a})

-- | A logical operator.
cLogicalOperator :: Lens' Condition (Maybe LogicalOperator)
cLogicalOperator = lens _cLogicalOperator (\s a -> s {_cLogicalOperator = a})

-- | The name of the crawler to which this condition applies.
cCrawlerName :: Lens' Condition (Maybe Text)
cCrawlerName = lens _cCrawlerName (\s a -> s {_cCrawlerName = a})

instance FromJSON Condition where
  parseJSON =
    withObject
      "Condition"
      ( \x ->
          Condition'
            <$> (x .:? "CrawlState")
            <*> (x .:? "State")
            <*> (x .:? "JobName")
            <*> (x .:? "LogicalOperator")
            <*> (x .:? "CrawlerName")
      )

instance Hashable Condition

instance NFData Condition

instance ToJSON Condition where
  toJSON Condition' {..} =
    object
      ( catMaybes
          [ ("CrawlState" .=) <$> _cCrawlState,
            ("State" .=) <$> _cState,
            ("JobName" .=) <$> _cJobName,
            ("LogicalOperator" .=) <$> _cLogicalOperator,
            ("CrawlerName" .=) <$> _cCrawlerName
          ]
      )

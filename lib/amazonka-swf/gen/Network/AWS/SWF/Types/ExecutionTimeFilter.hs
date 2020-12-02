{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExecutionTimeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExecutionTimeFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to filter the workflow executions in visibility APIs by various time-based rules. Each parameter, if specified, defines a rule that must be satisfied by each returned query result. The parameter values are in the <https://en.wikipedia.org/wiki/Unix_time Unix Time format> . For example: @"oldestDate": 1325376070.@
--
--
--
-- /See:/ 'executionTimeFilter' smart constructor.
data ExecutionTimeFilter = ExecutionTimeFilter'
  { _etfLatestDate ::
      !(Maybe POSIX),
    _etfOldestDate :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionTimeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etfLatestDate' - Specifies the latest start or close date and time to return.
--
-- * 'etfOldestDate' - Specifies the oldest start or close date and time to return.
executionTimeFilter ::
  -- | 'etfOldestDate'
  UTCTime ->
  ExecutionTimeFilter
executionTimeFilter pOldestDate_ =
  ExecutionTimeFilter'
    { _etfLatestDate = Nothing,
      _etfOldestDate = _Time # pOldestDate_
    }

-- | Specifies the latest start or close date and time to return.
etfLatestDate :: Lens' ExecutionTimeFilter (Maybe UTCTime)
etfLatestDate = lens _etfLatestDate (\s a -> s {_etfLatestDate = a}) . mapping _Time

-- | Specifies the oldest start or close date and time to return.
etfOldestDate :: Lens' ExecutionTimeFilter UTCTime
etfOldestDate = lens _etfOldestDate (\s a -> s {_etfOldestDate = a}) . _Time

instance Hashable ExecutionTimeFilter

instance NFData ExecutionTimeFilter

instance ToJSON ExecutionTimeFilter where
  toJSON ExecutionTimeFilter' {..} =
    object
      ( catMaybes
          [ ("latestDate" .=) <$> _etfLatestDate,
            Just ("oldestDate" .= _etfOldestDate)
          ]
      )

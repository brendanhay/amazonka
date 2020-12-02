{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.SearchedLogStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.SearchedLogStream where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the search status of a log stream.
--
--
--
-- /See:/ 'searchedLogStream' smart constructor.
data SearchedLogStream = SearchedLogStream'
  { _slsLogStreamName ::
      !(Maybe Text),
    _slsSearchedCompletely :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchedLogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slsLogStreamName' - The name of the log stream.
--
-- * 'slsSearchedCompletely' - Indicates whether all the events in this log stream were searched.
searchedLogStream ::
  SearchedLogStream
searchedLogStream =
  SearchedLogStream'
    { _slsLogStreamName = Nothing,
      _slsSearchedCompletely = Nothing
    }

-- | The name of the log stream.
slsLogStreamName :: Lens' SearchedLogStream (Maybe Text)
slsLogStreamName = lens _slsLogStreamName (\s a -> s {_slsLogStreamName = a})

-- | Indicates whether all the events in this log stream were searched.
slsSearchedCompletely :: Lens' SearchedLogStream (Maybe Bool)
slsSearchedCompletely = lens _slsSearchedCompletely (\s a -> s {_slsSearchedCompletely = a})

instance FromJSON SearchedLogStream where
  parseJSON =
    withObject
      "SearchedLogStream"
      ( \x ->
          SearchedLogStream'
            <$> (x .:? "logStreamName") <*> (x .:? "searchedCompletely")
      )

instance Hashable SearchedLogStream

instance NFData SearchedLogStream

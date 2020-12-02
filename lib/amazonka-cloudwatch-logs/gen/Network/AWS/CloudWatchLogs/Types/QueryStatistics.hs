{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned.
--
--
--
-- /See:/ 'queryStatistics' smart constructor.
data QueryStatistics = QueryStatistics'
  { _qsRecordsScanned ::
      !(Maybe Double),
    _qsBytesScanned :: !(Maybe Double),
    _qsRecordsMatched :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qsRecordsScanned' - The total number of log events scanned during the query.
--
-- * 'qsBytesScanned' - The total number of bytes in the log events scanned during the query.
--
-- * 'qsRecordsMatched' - The number of log events that matched the query string.
queryStatistics ::
  QueryStatistics
queryStatistics =
  QueryStatistics'
    { _qsRecordsScanned = Nothing,
      _qsBytesScanned = Nothing,
      _qsRecordsMatched = Nothing
    }

-- | The total number of log events scanned during the query.
qsRecordsScanned :: Lens' QueryStatistics (Maybe Double)
qsRecordsScanned = lens _qsRecordsScanned (\s a -> s {_qsRecordsScanned = a})

-- | The total number of bytes in the log events scanned during the query.
qsBytesScanned :: Lens' QueryStatistics (Maybe Double)
qsBytesScanned = lens _qsBytesScanned (\s a -> s {_qsBytesScanned = a})

-- | The number of log events that matched the query string.
qsRecordsMatched :: Lens' QueryStatistics (Maybe Double)
qsRecordsMatched = lens _qsRecordsMatched (\s a -> s {_qsRecordsMatched = a})

instance FromJSON QueryStatistics where
  parseJSON =
    withObject
      "QueryStatistics"
      ( \x ->
          QueryStatistics'
            <$> (x .:? "recordsScanned")
            <*> (x .:? "bytesScanned")
            <*> (x .:? "recordsMatched")
      )

instance Hashable QueryStatistics

instance NFData QueryStatistics

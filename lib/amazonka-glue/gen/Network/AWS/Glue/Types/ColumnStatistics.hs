{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatistics where

import Network.AWS.Glue.Types.ColumnStatisticsData
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the generated column-level statistics for a table or partition.
--
--
--
-- /See:/ 'columnStatistics' smart constructor.
data ColumnStatistics = ColumnStatistics'
  { _csColumnName :: !Text,
    _csColumnType :: !Text,
    _csAnalyzedTime :: !POSIX,
    _csStatisticsData :: !ColumnStatisticsData
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ColumnStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csColumnName' - Name of column which statistics belong to.
--
-- * 'csColumnType' - The data type of the column.
--
-- * 'csAnalyzedTime' - The timestamp of when column statistics were generated.
--
-- * 'csStatisticsData' - A @ColumnStatisticData@ object that contains the statistics data values.
columnStatistics ::
  -- | 'csColumnName'
  Text ->
  -- | 'csColumnType'
  Text ->
  -- | 'csAnalyzedTime'
  UTCTime ->
  -- | 'csStatisticsData'
  ColumnStatisticsData ->
  ColumnStatistics
columnStatistics
  pColumnName_
  pColumnType_
  pAnalyzedTime_
  pStatisticsData_ =
    ColumnStatistics'
      { _csColumnName = pColumnName_,
        _csColumnType = pColumnType_,
        _csAnalyzedTime = _Time # pAnalyzedTime_,
        _csStatisticsData = pStatisticsData_
      }

-- | Name of column which statistics belong to.
csColumnName :: Lens' ColumnStatistics Text
csColumnName = lens _csColumnName (\s a -> s {_csColumnName = a})

-- | The data type of the column.
csColumnType :: Lens' ColumnStatistics Text
csColumnType = lens _csColumnType (\s a -> s {_csColumnType = a})

-- | The timestamp of when column statistics were generated.
csAnalyzedTime :: Lens' ColumnStatistics UTCTime
csAnalyzedTime = lens _csAnalyzedTime (\s a -> s {_csAnalyzedTime = a}) . _Time

-- | A @ColumnStatisticData@ object that contains the statistics data values.
csStatisticsData :: Lens' ColumnStatistics ColumnStatisticsData
csStatisticsData = lens _csStatisticsData (\s a -> s {_csStatisticsData = a})

instance FromJSON ColumnStatistics where
  parseJSON =
    withObject
      "ColumnStatistics"
      ( \x ->
          ColumnStatistics'
            <$> (x .: "ColumnName")
            <*> (x .: "ColumnType")
            <*> (x .: "AnalyzedTime")
            <*> (x .: "StatisticsData")
      )

instance Hashable ColumnStatistics

instance NFData ColumnStatistics

instance ToJSON ColumnStatistics where
  toJSON ColumnStatistics' {..} =
    object
      ( catMaybes
          [ Just ("ColumnName" .= _csColumnName),
            Just ("ColumnType" .= _csColumnType),
            Just ("AnalyzedTime" .= _csAnalyzedTime),
            Just ("StatisticsData" .= _csStatisticsData)
          ]
      )

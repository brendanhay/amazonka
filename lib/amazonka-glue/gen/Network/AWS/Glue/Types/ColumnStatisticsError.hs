{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsError where

import Network.AWS.Glue.Types.ColumnStatistics
import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encapsulates a @ColumnStatistics@ object that failed and the reason for failure.
--
--
--
-- /See:/ 'columnStatisticsError' smart constructor.
data ColumnStatisticsError = ColumnStatisticsError'
  { _cseError ::
      !(Maybe ErrorDetail),
    _cseColumnStatistics ::
      !(Maybe ColumnStatistics)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ColumnStatisticsError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cseError' - An error message with the reason for the failure of an operation.
--
-- * 'cseColumnStatistics' - The @ColumnStatistics@ of the column.
columnStatisticsError ::
  ColumnStatisticsError
columnStatisticsError =
  ColumnStatisticsError'
    { _cseError = Nothing,
      _cseColumnStatistics = Nothing
    }

-- | An error message with the reason for the failure of an operation.
cseError :: Lens' ColumnStatisticsError (Maybe ErrorDetail)
cseError = lens _cseError (\s a -> s {_cseError = a})

-- | The @ColumnStatistics@ of the column.
cseColumnStatistics :: Lens' ColumnStatisticsError (Maybe ColumnStatistics)
cseColumnStatistics = lens _cseColumnStatistics (\s a -> s {_cseColumnStatistics = a})

instance FromJSON ColumnStatisticsError where
  parseJSON =
    withObject
      "ColumnStatisticsError"
      ( \x ->
          ColumnStatisticsError'
            <$> (x .:? "Error") <*> (x .:? "ColumnStatistics")
      )

instance Hashable ColumnStatisticsError

instance NFData ColumnStatisticsError

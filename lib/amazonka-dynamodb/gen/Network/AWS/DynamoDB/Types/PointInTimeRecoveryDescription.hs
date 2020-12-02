{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription where

import Network.AWS.DynamoDB.Types.PointInTimeRecoveryStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description of the point in time settings applied to the table.
--
--
--
-- /See:/ 'pointInTimeRecoveryDescription' smart constructor.
data PointInTimeRecoveryDescription = PointInTimeRecoveryDescription'
  { _pitrdPointInTimeRecoveryStatus ::
      !( Maybe
           PointInTimeRecoveryStatus
       ),
    _pitrdEarliestRestorableDateTime ::
      !(Maybe POSIX),
    _pitrdLatestRestorableDateTime ::
      !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PointInTimeRecoveryDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pitrdPointInTimeRecoveryStatus' - The current state of point in time recovery:     * @ENABLING@ - Point in time recovery is being enabled.     * @ENABLED@ - Point in time recovery is enabled.     * @DISABLED@ - Point in time recovery is disabled.
--
-- * 'pitrdEarliestRestorableDateTime' - Specifies the earliest point in time you can restore your table to. You can restore your table to any point in time during the last 35 days.
--
-- * 'pitrdLatestRestorableDateTime' - @LatestRestorableDateTime@ is typically 5 minutes before the current time.
pointInTimeRecoveryDescription ::
  PointInTimeRecoveryDescription
pointInTimeRecoveryDescription =
  PointInTimeRecoveryDescription'
    { _pitrdPointInTimeRecoveryStatus =
        Nothing,
      _pitrdEarliestRestorableDateTime = Nothing,
      _pitrdLatestRestorableDateTime = Nothing
    }

-- | The current state of point in time recovery:     * @ENABLING@ - Point in time recovery is being enabled.     * @ENABLED@ - Point in time recovery is enabled.     * @DISABLED@ - Point in time recovery is disabled.
pitrdPointInTimeRecoveryStatus :: Lens' PointInTimeRecoveryDescription (Maybe PointInTimeRecoveryStatus)
pitrdPointInTimeRecoveryStatus = lens _pitrdPointInTimeRecoveryStatus (\s a -> s {_pitrdPointInTimeRecoveryStatus = a})

-- | Specifies the earliest point in time you can restore your table to. You can restore your table to any point in time during the last 35 days.
pitrdEarliestRestorableDateTime :: Lens' PointInTimeRecoveryDescription (Maybe UTCTime)
pitrdEarliestRestorableDateTime = lens _pitrdEarliestRestorableDateTime (\s a -> s {_pitrdEarliestRestorableDateTime = a}) . mapping _Time

-- | @LatestRestorableDateTime@ is typically 5 minutes before the current time.
pitrdLatestRestorableDateTime :: Lens' PointInTimeRecoveryDescription (Maybe UTCTime)
pitrdLatestRestorableDateTime = lens _pitrdLatestRestorableDateTime (\s a -> s {_pitrdLatestRestorableDateTime = a}) . mapping _Time

instance FromJSON PointInTimeRecoveryDescription where
  parseJSON =
    withObject
      "PointInTimeRecoveryDescription"
      ( \x ->
          PointInTimeRecoveryDescription'
            <$> (x .:? "PointInTimeRecoveryStatus")
            <*> (x .:? "EarliestRestorableDateTime")
            <*> (x .:? "LatestRestorableDateTime")
      )

instance Hashable PointInTimeRecoveryDescription

instance NFData PointInTimeRecoveryDescription

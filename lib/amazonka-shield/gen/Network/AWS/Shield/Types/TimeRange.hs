{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.TimeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.TimeRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The time range.
--
--
--
-- /See:/ 'timeRange' smart constructor.
data TimeRange = TimeRange'
  { _trFromInclusive :: !(Maybe POSIX),
    _trToExclusive :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trFromInclusive' - The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- * 'trToExclusive' - The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
timeRange ::
  TimeRange
timeRange =
  TimeRange' {_trFromInclusive = Nothing, _trToExclusive = Nothing}

-- | The start time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
trFromInclusive :: Lens' TimeRange (Maybe UTCTime)
trFromInclusive = lens _trFromInclusive (\s a -> s {_trFromInclusive = a}) . mapping _Time

-- | The end time, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
trToExclusive :: Lens' TimeRange (Maybe UTCTime)
trToExclusive = lens _trToExclusive (\s a -> s {_trToExclusive = a}) . mapping _Time

instance FromJSON TimeRange where
  parseJSON =
    withObject
      "TimeRange"
      ( \x ->
          TimeRange' <$> (x .:? "FromInclusive") <*> (x .:? "ToExclusive")
      )

instance Hashable TimeRange

instance NFData TimeRange

instance ToJSON TimeRange where
  toJSON TimeRange' {..} =
    object
      ( catMaybes
          [ ("FromInclusive" .=) <$> _trFromInclusive,
            ("ToExclusive" .=) <$> _trToExclusive
          ]
      )

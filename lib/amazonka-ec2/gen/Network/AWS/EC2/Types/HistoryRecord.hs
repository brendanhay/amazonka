{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HistoryRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HistoryRecord where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EventInformation
import Network.AWS.EC2.Types.EventType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an event in the history of the Spot Fleet request.
--
--
--
-- /See:/ 'historyRecord' smart constructor.
data HistoryRecord = HistoryRecord'
  { _hrEventType ::
      !(Maybe EventType),
    _hrEventInformation :: !(Maybe EventInformation),
    _hrTimestamp :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HistoryRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hrEventType' - The event type.     * @error@ - An error with the Spot Fleet request.     * @fleetRequestChange@ - A change in the status or configuration of the Spot Fleet request.     * @instanceChange@ - An instance was launched or terminated.     * @Information@ - An informational event.
--
-- * 'hrEventInformation' - Information about the event.
--
-- * 'hrTimestamp' - The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
historyRecord ::
  HistoryRecord
historyRecord =
  HistoryRecord'
    { _hrEventType = Nothing,
      _hrEventInformation = Nothing,
      _hrTimestamp = Nothing
    }

-- | The event type.     * @error@ - An error with the Spot Fleet request.     * @fleetRequestChange@ - A change in the status or configuration of the Spot Fleet request.     * @instanceChange@ - An instance was launched or terminated.     * @Information@ - An informational event.
hrEventType :: Lens' HistoryRecord (Maybe EventType)
hrEventType = lens _hrEventType (\s a -> s {_hrEventType = a})

-- | Information about the event.
hrEventInformation :: Lens' HistoryRecord (Maybe EventInformation)
hrEventInformation = lens _hrEventInformation (\s a -> s {_hrEventInformation = a})

-- | The date and time of the event, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
hrTimestamp :: Lens' HistoryRecord (Maybe UTCTime)
hrTimestamp = lens _hrTimestamp (\s a -> s {_hrTimestamp = a}) . mapping _Time

instance FromXML HistoryRecord where
  parseXML x =
    HistoryRecord'
      <$> (x .@? "eventType")
      <*> (x .@? "eventInformation")
      <*> (x .@? "timestamp")

instance Hashable HistoryRecord

instance NFData HistoryRecord

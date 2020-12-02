{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SendDataPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SendDataPoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents sending statistics data. Each @SendDataPoint@ contains statistics for a 15-minute period of sending activity.
--
--
--
-- /See:/ 'sendDataPoint' smart constructor.
data SendDataPoint = SendDataPoint'
  { _sdpRejects ::
      !(Maybe Integer),
    _sdpComplaints :: !(Maybe Integer),
    _sdpDeliveryAttempts :: !(Maybe Integer),
    _sdpBounces :: !(Maybe Integer),
    _sdpTimestamp :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendDataPoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdpRejects' - Number of emails rejected by Amazon SES.
--
-- * 'sdpComplaints' - Number of unwanted emails that were rejected by recipients.
--
-- * 'sdpDeliveryAttempts' - Number of emails that have been sent.
--
-- * 'sdpBounces' - Number of emails that have bounced.
--
-- * 'sdpTimestamp' - Time of the data point.
sendDataPoint ::
  SendDataPoint
sendDataPoint =
  SendDataPoint'
    { _sdpRejects = Nothing,
      _sdpComplaints = Nothing,
      _sdpDeliveryAttempts = Nothing,
      _sdpBounces = Nothing,
      _sdpTimestamp = Nothing
    }

-- | Number of emails rejected by Amazon SES.
sdpRejects :: Lens' SendDataPoint (Maybe Integer)
sdpRejects = lens _sdpRejects (\s a -> s {_sdpRejects = a})

-- | Number of unwanted emails that were rejected by recipients.
sdpComplaints :: Lens' SendDataPoint (Maybe Integer)
sdpComplaints = lens _sdpComplaints (\s a -> s {_sdpComplaints = a})

-- | Number of emails that have been sent.
sdpDeliveryAttempts :: Lens' SendDataPoint (Maybe Integer)
sdpDeliveryAttempts = lens _sdpDeliveryAttempts (\s a -> s {_sdpDeliveryAttempts = a})

-- | Number of emails that have bounced.
sdpBounces :: Lens' SendDataPoint (Maybe Integer)
sdpBounces = lens _sdpBounces (\s a -> s {_sdpBounces = a})

-- | Time of the data point.
sdpTimestamp :: Lens' SendDataPoint (Maybe UTCTime)
sdpTimestamp = lens _sdpTimestamp (\s a -> s {_sdpTimestamp = a}) . mapping _Time

instance FromXML SendDataPoint where
  parseXML x =
    SendDataPoint'
      <$> (x .@? "Rejects")
      <*> (x .@? "Complaints")
      <*> (x .@? "DeliveryAttempts")
      <*> (x .@? "Bounces")
      <*> (x .@? "Timestamp")

instance Hashable SendDataPoint

instance NFData SendDataPoint

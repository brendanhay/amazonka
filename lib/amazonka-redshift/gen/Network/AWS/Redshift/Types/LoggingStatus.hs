{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.LoggingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.LoggingStatus where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes the status of logging for a cluster.
--
--
--
-- /See:/ 'loggingStatus' smart constructor.
data LoggingStatus = LoggingStatus'
  { _lsLastFailureTime ::
      !(Maybe ISO8601),
    _lsLastSuccessfulDeliveryTime :: !(Maybe ISO8601),
    _lsS3KeyPrefix :: !(Maybe Text),
    _lsBucketName :: !(Maybe Text),
    _lsLoggingEnabled :: !(Maybe Bool),
    _lsLastFailureMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsLastFailureTime' - The last time when logs failed to be delivered.
--
-- * 'lsLastSuccessfulDeliveryTime' - The last time that logs were delivered.
--
-- * 'lsS3KeyPrefix' - The prefix applied to the log file names.
--
-- * 'lsBucketName' - The name of the S3 bucket where the log files are stored.
--
-- * 'lsLoggingEnabled' - @true@ if logging is on, @false@ if logging is off.
--
-- * 'lsLastFailureMessage' - The message indicating that logs failed to be delivered.
loggingStatus ::
  LoggingStatus
loggingStatus =
  LoggingStatus'
    { _lsLastFailureTime = Nothing,
      _lsLastSuccessfulDeliveryTime = Nothing,
      _lsS3KeyPrefix = Nothing,
      _lsBucketName = Nothing,
      _lsLoggingEnabled = Nothing,
      _lsLastFailureMessage = Nothing
    }

-- | The last time when logs failed to be delivered.
lsLastFailureTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastFailureTime = lens _lsLastFailureTime (\s a -> s {_lsLastFailureTime = a}) . mapping _Time

-- | The last time that logs were delivered.
lsLastSuccessfulDeliveryTime :: Lens' LoggingStatus (Maybe UTCTime)
lsLastSuccessfulDeliveryTime = lens _lsLastSuccessfulDeliveryTime (\s a -> s {_lsLastSuccessfulDeliveryTime = a}) . mapping _Time

-- | The prefix applied to the log file names.
lsS3KeyPrefix :: Lens' LoggingStatus (Maybe Text)
lsS3KeyPrefix = lens _lsS3KeyPrefix (\s a -> s {_lsS3KeyPrefix = a})

-- | The name of the S3 bucket where the log files are stored.
lsBucketName :: Lens' LoggingStatus (Maybe Text)
lsBucketName = lens _lsBucketName (\s a -> s {_lsBucketName = a})

-- | @true@ if logging is on, @false@ if logging is off.
lsLoggingEnabled :: Lens' LoggingStatus (Maybe Bool)
lsLoggingEnabled = lens _lsLoggingEnabled (\s a -> s {_lsLoggingEnabled = a})

-- | The message indicating that logs failed to be delivered.
lsLastFailureMessage :: Lens' LoggingStatus (Maybe Text)
lsLastFailureMessage = lens _lsLastFailureMessage (\s a -> s {_lsLastFailureMessage = a})

instance FromXML LoggingStatus where
  parseXML x =
    LoggingStatus'
      <$> (x .@? "LastFailureTime")
      <*> (x .@? "LastSuccessfulDeliveryTime")
      <*> (x .@? "S3KeyPrefix")
      <*> (x .@? "BucketName")
      <*> (x .@? "LoggingEnabled")
      <*> (x .@? "LastFailureMessage")

instance Hashable LoggingStatus

instance NFData LoggingStatus

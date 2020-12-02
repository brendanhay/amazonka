{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationTime where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ReplicationTimeStatus
import Network.AWS.S3.Types.ReplicationTimeValue

-- | A container specifying S3 Replication Time Control (S3 RTC) related information, including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
--
--
-- /See:/ 'replicationTime' smart constructor.
data ReplicationTime = ReplicationTime'
  { _rtStatus ::
      !ReplicationTimeStatus,
    _rtTime :: !ReplicationTimeValue
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtStatus' - Specifies whether the replication time is enabled.
--
-- * 'rtTime' - A container specifying the time by which replication should be complete for all objects and operations on objects.
replicationTime ::
  -- | 'rtStatus'
  ReplicationTimeStatus ->
  -- | 'rtTime'
  ReplicationTimeValue ->
  ReplicationTime
replicationTime pStatus_ pTime_ =
  ReplicationTime' {_rtStatus = pStatus_, _rtTime = pTime_}

-- | Specifies whether the replication time is enabled.
rtStatus :: Lens' ReplicationTime ReplicationTimeStatus
rtStatus = lens _rtStatus (\s a -> s {_rtStatus = a})

-- | A container specifying the time by which replication should be complete for all objects and operations on objects.
rtTime :: Lens' ReplicationTime ReplicationTimeValue
rtTime = lens _rtTime (\s a -> s {_rtTime = a})

instance FromXML ReplicationTime where
  parseXML x = ReplicationTime' <$> (x .@ "Status") <*> (x .@ "Time")

instance Hashable ReplicationTime

instance NFData ReplicationTime

instance ToXML ReplicationTime where
  toXML ReplicationTime' {..} =
    mconcat ["Status" @= _rtStatus, "Time" @= _rtTime]

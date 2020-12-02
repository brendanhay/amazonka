{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationTimeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationTimeValue where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | A container specifying the time value for S3 Replication Time Control (S3 RTC) and replication metrics @EventThreshold@ .
--
--
--
-- /See:/ 'replicationTimeValue' smart constructor.
newtype ReplicationTimeValue = ReplicationTimeValue'
  { _rtvMinutes ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTimeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtvMinutes' - Contains an integer specifying time in minutes.  Valid values: 15 minutes.
replicationTimeValue ::
  ReplicationTimeValue
replicationTimeValue = ReplicationTimeValue' {_rtvMinutes = Nothing}

-- | Contains an integer specifying time in minutes.  Valid values: 15 minutes.
rtvMinutes :: Lens' ReplicationTimeValue (Maybe Int)
rtvMinutes = lens _rtvMinutes (\s a -> s {_rtvMinutes = a})

instance FromXML ReplicationTimeValue where
  parseXML x = ReplicationTimeValue' <$> (x .@? "Minutes")

instance Hashable ReplicationTimeValue

instance NFData ReplicationTimeValue

instance ToXML ReplicationTimeValue where
  toXML ReplicationTimeValue' {..} =
    mconcat ["Minutes" @= _rtvMinutes]

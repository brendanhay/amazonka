{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Metrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Metrics where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsStatus
import Network.AWS.S3.Types.ReplicationTimeValue

-- | A container specifying replication metrics-related settings enabling replication metrics and events.
--
--
--
-- /See:/ 'metrics' smart constructor.
data Metrics = Metrics'
  { _mEventThreshold ::
      !(Maybe ReplicationTimeValue),
    _mStatus :: !MetricsStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Metrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mEventThreshold' - A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event.
--
-- * 'mStatus' - Specifies whether the replication metrics are enabled.
metrics ::
  -- | 'mStatus'
  MetricsStatus ->
  Metrics
metrics pStatus_ =
  Metrics' {_mEventThreshold = Nothing, _mStatus = pStatus_}

-- | A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event.
mEventThreshold :: Lens' Metrics (Maybe ReplicationTimeValue)
mEventThreshold = lens _mEventThreshold (\s a -> s {_mEventThreshold = a})

-- | Specifies whether the replication metrics are enabled.
mStatus :: Lens' Metrics MetricsStatus
mStatus = lens _mStatus (\s a -> s {_mStatus = a})

instance FromXML Metrics where
  parseXML x =
    Metrics' <$> (x .@? "EventThreshold") <*> (x .@ "Status")

instance Hashable Metrics

instance NFData Metrics

instance ToXML Metrics where
  toXML Metrics' {..} =
    mconcat
      ["EventThreshold" @= _mEventThreshold, "Status" @= _mStatus]

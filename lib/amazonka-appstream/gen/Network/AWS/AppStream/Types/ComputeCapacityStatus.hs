{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ComputeCapacityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ComputeCapacityStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the capacity status for a fleet.
--
--
--
-- /See:/ 'computeCapacityStatus' smart constructor.
data ComputeCapacityStatus = ComputeCapacityStatus'
  { _ccsInUse ::
      !(Maybe Int),
    _ccsRunning :: !(Maybe Int),
    _ccsAvailable :: !(Maybe Int),
    _ccsDesired :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComputeCapacityStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsInUse' - The number of instances in use for streaming.
--
-- * 'ccsRunning' - The total number of simultaneous streaming instances that are running.
--
-- * 'ccsAvailable' - The number of currently available instances that can be used to stream sessions.
--
-- * 'ccsDesired' - The desired number of streaming instances.
computeCapacityStatus ::
  -- | 'ccsDesired'
  Int ->
  ComputeCapacityStatus
computeCapacityStatus pDesired_ =
  ComputeCapacityStatus'
    { _ccsInUse = Nothing,
      _ccsRunning = Nothing,
      _ccsAvailable = Nothing,
      _ccsDesired = pDesired_
    }

-- | The number of instances in use for streaming.
ccsInUse :: Lens' ComputeCapacityStatus (Maybe Int)
ccsInUse = lens _ccsInUse (\s a -> s {_ccsInUse = a})

-- | The total number of simultaneous streaming instances that are running.
ccsRunning :: Lens' ComputeCapacityStatus (Maybe Int)
ccsRunning = lens _ccsRunning (\s a -> s {_ccsRunning = a})

-- | The number of currently available instances that can be used to stream sessions.
ccsAvailable :: Lens' ComputeCapacityStatus (Maybe Int)
ccsAvailable = lens _ccsAvailable (\s a -> s {_ccsAvailable = a})

-- | The desired number of streaming instances.
ccsDesired :: Lens' ComputeCapacityStatus Int
ccsDesired = lens _ccsDesired (\s a -> s {_ccsDesired = a})

instance FromJSON ComputeCapacityStatus where
  parseJSON =
    withObject
      "ComputeCapacityStatus"
      ( \x ->
          ComputeCapacityStatus'
            <$> (x .:? "InUse")
            <*> (x .:? "Running")
            <*> (x .:? "Available")
            <*> (x .: "Desired")
      )

instance Hashable ComputeCapacityStatus

instance NFData ComputeCapacityStatus

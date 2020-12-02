{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesMonitoring where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes whether monitoring is enabled for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesMonitoring' smart constructor.
newtype ScheduledInstancesMonitoring = ScheduledInstancesMonitoring'
  { _simEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesMonitoring' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simEnabled' - Indicates whether monitoring is enabled.
scheduledInstancesMonitoring ::
  ScheduledInstancesMonitoring
scheduledInstancesMonitoring =
  ScheduledInstancesMonitoring' {_simEnabled = Nothing}

-- | Indicates whether monitoring is enabled.
simEnabled :: Lens' ScheduledInstancesMonitoring (Maybe Bool)
simEnabled = lens _simEnabled (\s a -> s {_simEnabled = a})

instance Hashable ScheduledInstancesMonitoring

instance NFData ScheduledInstancesMonitoring

instance ToQuery ScheduledInstancesMonitoring where
  toQuery ScheduledInstancesMonitoring' {..} =
    mconcat ["Enabled" =: _simEnabled]

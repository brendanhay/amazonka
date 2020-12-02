{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RunInstancesMonitoringEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.RunInstancesMonitoringEnabled where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the monitoring of an instance.
--
--
--
-- /See:/ 'runInstancesMonitoringEnabled' smart constructor.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled'
  { _rimeEnabled ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RunInstancesMonitoringEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rimeEnabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
runInstancesMonitoringEnabled ::
  -- | 'rimeEnabled'
  Bool ->
  RunInstancesMonitoringEnabled
runInstancesMonitoringEnabled pEnabled_ =
  RunInstancesMonitoringEnabled' {_rimeEnabled = pEnabled_}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
rimeEnabled :: Lens' RunInstancesMonitoringEnabled Bool
rimeEnabled = lens _rimeEnabled (\s a -> s {_rimeEnabled = a})

instance FromXML RunInstancesMonitoringEnabled where
  parseXML x = RunInstancesMonitoringEnabled' <$> (x .@ "enabled")

instance Hashable RunInstancesMonitoringEnabled

instance NFData RunInstancesMonitoringEnabled

instance ToQuery RunInstancesMonitoringEnabled where
  toQuery RunInstancesMonitoringEnabled' {..} =
    mconcat ["Enabled" =: _rimeEnabled]

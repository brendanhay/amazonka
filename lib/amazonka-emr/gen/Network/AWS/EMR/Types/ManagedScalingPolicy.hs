{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ManagedScalingPolicy where

import Network.AWS.EMR.Types.ComputeLimits
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Managed scaling policy for an Amazon EMR cluster. The policy specifies the limits for resources that can be added or terminated from a cluster. The policy only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
--
--
--
-- /See:/ 'managedScalingPolicy' smart constructor.
newtype ManagedScalingPolicy = ManagedScalingPolicy'
  { _mspComputeLimits ::
      Maybe ComputeLimits
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ManagedScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mspComputeLimits' - The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster is not allowed to go above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
managedScalingPolicy ::
  ManagedScalingPolicy
managedScalingPolicy =
  ManagedScalingPolicy' {_mspComputeLimits = Nothing}

-- | The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster is not allowed to go above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
mspComputeLimits :: Lens' ManagedScalingPolicy (Maybe ComputeLimits)
mspComputeLimits = lens _mspComputeLimits (\s a -> s {_mspComputeLimits = a})

instance FromJSON ManagedScalingPolicy where
  parseJSON =
    withObject
      "ManagedScalingPolicy"
      (\x -> ManagedScalingPolicy' <$> (x .:? "ComputeLimits"))

instance Hashable ManagedScalingPolicy

instance NFData ManagedScalingPolicy

instance ToJSON ManagedScalingPolicy where
  toJSON ManagedScalingPolicy' {..} =
    object (catMaybes [("ComputeLimits" .=) <$> _mspComputeLimits])

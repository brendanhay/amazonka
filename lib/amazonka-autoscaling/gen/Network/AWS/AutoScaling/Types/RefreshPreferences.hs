{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.RefreshPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.RefreshPreferences where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes information used to start an instance refresh.
--
--
--
-- /See:/ 'refreshPreferences' smart constructor.
data RefreshPreferences = RefreshPreferences'
  { _rpMinHealthyPercentage ::
      !(Maybe Nat),
    _rpInstanceWarmup :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RefreshPreferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpMinHealthyPercentage' - The amount of capacity in the Auto Scaling group that must remain healthy during an instance refresh to allow the operation to continue, as a percentage of the desired capacity of the Auto Scaling group (rounded up to the nearest integer). The default is @90@ .
--
-- * 'rpInstanceWarmup' - The number of seconds until a newly launched instance is configured and ready to use. During this time, Amazon EC2 Auto Scaling does not immediately move on to the next replacement. The default is to use the value for the health check grace period defined for the group.
refreshPreferences ::
  RefreshPreferences
refreshPreferences =
  RefreshPreferences'
    { _rpMinHealthyPercentage = Nothing,
      _rpInstanceWarmup = Nothing
    }

-- | The amount of capacity in the Auto Scaling group that must remain healthy during an instance refresh to allow the operation to continue, as a percentage of the desired capacity of the Auto Scaling group (rounded up to the nearest integer). The default is @90@ .
rpMinHealthyPercentage :: Lens' RefreshPreferences (Maybe Natural)
rpMinHealthyPercentage = lens _rpMinHealthyPercentage (\s a -> s {_rpMinHealthyPercentage = a}) . mapping _Nat

-- | The number of seconds until a newly launched instance is configured and ready to use. During this time, Amazon EC2 Auto Scaling does not immediately move on to the next replacement. The default is to use the value for the health check grace period defined for the group.
rpInstanceWarmup :: Lens' RefreshPreferences (Maybe Natural)
rpInstanceWarmup = lens _rpInstanceWarmup (\s a -> s {_rpInstanceWarmup = a}) . mapping _Nat

instance Hashable RefreshPreferences

instance NFData RefreshPreferences

instance ToQuery RefreshPreferences where
  toQuery RefreshPreferences' {..} =
    mconcat
      [ "MinHealthyPercentage" =: _rpMinHealthyPercentage,
        "InstanceWarmup" =: _rpInstanceWarmup
      ]

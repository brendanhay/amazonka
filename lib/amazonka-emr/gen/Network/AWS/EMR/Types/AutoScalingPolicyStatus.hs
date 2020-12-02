{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStatus where

import Network.AWS.EMR.Types.AutoScalingPolicyState
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of an automatic scaling policy.
--
--
--
-- /See:/ 'autoScalingPolicyStatus' smart constructor.
data AutoScalingPolicyStatus = AutoScalingPolicyStatus'
  { _aspsState ::
      !(Maybe AutoScalingPolicyState),
    _aspsStateChangeReason ::
      !(Maybe AutoScalingPolicyStateChangeReason)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingPolicyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspsState' - Indicates the status of the automatic scaling policy.
--
-- * 'aspsStateChangeReason' - The reason for a change in status.
autoScalingPolicyStatus ::
  AutoScalingPolicyStatus
autoScalingPolicyStatus =
  AutoScalingPolicyStatus'
    { _aspsState = Nothing,
      _aspsStateChangeReason = Nothing
    }

-- | Indicates the status of the automatic scaling policy.
aspsState :: Lens' AutoScalingPolicyStatus (Maybe AutoScalingPolicyState)
aspsState = lens _aspsState (\s a -> s {_aspsState = a})

-- | The reason for a change in status.
aspsStateChangeReason :: Lens' AutoScalingPolicyStatus (Maybe AutoScalingPolicyStateChangeReason)
aspsStateChangeReason = lens _aspsStateChangeReason (\s a -> s {_aspsStateChangeReason = a})

instance FromJSON AutoScalingPolicyStatus where
  parseJSON =
    withObject
      "AutoScalingPolicyStatus"
      ( \x ->
          AutoScalingPolicyStatus'
            <$> (x .:? "State") <*> (x .:? "StateChangeReason")
      )

instance Hashable AutoScalingPolicyStatus

instance NFData AutoScalingPolicyStatus

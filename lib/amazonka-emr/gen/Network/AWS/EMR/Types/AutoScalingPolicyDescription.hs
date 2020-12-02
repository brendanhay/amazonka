{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyDescription where

import Network.AWS.EMR.Types.AutoScalingPolicyStatus
import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
--
--
-- /See:/ 'autoScalingPolicyDescription' smart constructor.
data AutoScalingPolicyDescription = AutoScalingPolicyDescription'
  { _aspdStatus ::
      !(Maybe AutoScalingPolicyStatus),
    _aspdRules ::
      !(Maybe [ScalingRule]),
    _aspdConstraints ::
      !(Maybe ScalingConstraints)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingPolicyDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspdStatus' - The status of an automatic scaling policy.
--
-- * 'aspdRules' - The scale-in and scale-out rules that comprise the automatic scaling policy.
--
-- * 'aspdConstraints' - The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
autoScalingPolicyDescription ::
  AutoScalingPolicyDescription
autoScalingPolicyDescription =
  AutoScalingPolicyDescription'
    { _aspdStatus = Nothing,
      _aspdRules = Nothing,
      _aspdConstraints = Nothing
    }

-- | The status of an automatic scaling policy.
aspdStatus :: Lens' AutoScalingPolicyDescription (Maybe AutoScalingPolicyStatus)
aspdStatus = lens _aspdStatus (\s a -> s {_aspdStatus = a})

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
aspdRules :: Lens' AutoScalingPolicyDescription [ScalingRule]
aspdRules = lens _aspdRules (\s a -> s {_aspdRules = a}) . _Default . _Coerce

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
aspdConstraints :: Lens' AutoScalingPolicyDescription (Maybe ScalingConstraints)
aspdConstraints = lens _aspdConstraints (\s a -> s {_aspdConstraints = a})

instance FromJSON AutoScalingPolicyDescription where
  parseJSON =
    withObject
      "AutoScalingPolicyDescription"
      ( \x ->
          AutoScalingPolicyDescription'
            <$> (x .:? "Status")
            <*> (x .:? "Rules" .!= mempty)
            <*> (x .:? "Constraints")
      )

instance Hashable AutoScalingPolicyDescription

instance NFData AutoScalingPolicyDescription

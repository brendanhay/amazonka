{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicy where

import Network.AWS.EMR.Types.ScalingConstraints
import Network.AWS.EMR.Types.ScalingRule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. An automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See 'PutAutoScalingPolicy' .
--
--
--
-- /See:/ 'autoScalingPolicy' smart constructor.
data AutoScalingPolicy = AutoScalingPolicy'
  { _aspConstraints ::
      !ScalingConstraints,
    _aspRules :: ![ScalingRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspConstraints' - The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
--
-- * 'aspRules' - The scale-in and scale-out rules that comprise the automatic scaling policy.
autoScalingPolicy ::
  -- | 'aspConstraints'
  ScalingConstraints ->
  AutoScalingPolicy
autoScalingPolicy pConstraints_ =
  AutoScalingPolicy'
    { _aspConstraints = pConstraints_,
      _aspRules = mempty
    }

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activity will not cause an instance group to grow above or below these limits.
aspConstraints :: Lens' AutoScalingPolicy ScalingConstraints
aspConstraints = lens _aspConstraints (\s a -> s {_aspConstraints = a})

-- | The scale-in and scale-out rules that comprise the automatic scaling policy.
aspRules :: Lens' AutoScalingPolicy [ScalingRule]
aspRules = lens _aspRules (\s a -> s {_aspRules = a}) . _Coerce

instance Hashable AutoScalingPolicy

instance NFData AutoScalingPolicy

instance ToJSON AutoScalingPolicy where
  toJSON AutoScalingPolicy' {..} =
    object
      ( catMaybes
          [ Just ("Constraints" .= _aspConstraints),
            Just ("Rules" .= _aspRules)
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason where

import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The reason for an 'AutoScalingPolicyStatus' change.
--
--
--
-- /See:/ 'autoScalingPolicyStateChangeReason' smart constructor.
data AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason'
  { _aspscrCode ::
      !( Maybe
           AutoScalingPolicyStateChangeReasonCode
       ),
    _aspscrMessage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoScalingPolicyStateChangeReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aspscrCode' - The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
--
-- * 'aspscrMessage' - A friendly, more verbose message that accompanies an automatic scaling policy state change.
autoScalingPolicyStateChangeReason ::
  AutoScalingPolicyStateChangeReason
autoScalingPolicyStateChangeReason =
  AutoScalingPolicyStateChangeReason'
    { _aspscrCode = Nothing,
      _aspscrMessage = Nothing
    }

-- | The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
aspscrCode :: Lens' AutoScalingPolicyStateChangeReason (Maybe AutoScalingPolicyStateChangeReasonCode)
aspscrCode = lens _aspscrCode (\s a -> s {_aspscrCode = a})

-- | A friendly, more verbose message that accompanies an automatic scaling policy state change.
aspscrMessage :: Lens' AutoScalingPolicyStateChangeReason (Maybe Text)
aspscrMessage = lens _aspscrMessage (\s a -> s {_aspscrMessage = a})

instance FromJSON AutoScalingPolicyStateChangeReason where
  parseJSON =
    withObject
      "AutoScalingPolicyStateChangeReason"
      ( \x ->
          AutoScalingPolicyStateChangeReason'
            <$> (x .:? "Code") <*> (x .:? "Message")
      )

instance Hashable AutoScalingPolicyStateChangeReason

instance NFData AutoScalingPolicyStateChangeReason

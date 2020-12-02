{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StateReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StateReason where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a state change.
--
--
--
-- /See:/ 'stateReason' smart constructor.
data StateReason = StateReason'
  { _srCode :: !(Maybe Text),
    _srMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StateReason' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srCode' - The reason code for the state change.
--
-- * 'srMessage' - The message for the state change.     * @Server.InsufficientInstanceCapacity@ : There was insufficient capacity available to satisfy the launch request.     * @Server.InternalError@ : An internal error caused the instance to terminate during launch.     * @Server.ScheduledStop@ : The instance was stopped due to a scheduled retirement.     * @Server.SpotInstanceShutdown@ : The instance was stopped because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.     * @Server.SpotInstanceTermination@ : The instance was terminated because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.     * @Client.InstanceInitiatedShutdown@ : The instance was shut down using the @shutdown -h@ command from the instance.     * @Client.InstanceTerminated@ : The instance was terminated or rebooted during AMI creation.     * @Client.InternalError@ : A client error caused the instance to terminate during launch.     * @Client.InvalidSnapshot.NotFound@ : The specified snapshot was not found.     * @Client.UserInitiatedHibernate@ : Hibernation was initiated on the instance.     * @Client.UserInitiatedShutdown@ : The instance was shut down using the Amazon EC2 API.     * @Client.VolumeLimitExceeded@ : The limit on the number of EBS volumes or total storage was exceeded. Decrease usage or request an increase in your account limits.
stateReason ::
  StateReason
stateReason = StateReason' {_srCode = Nothing, _srMessage = Nothing}

-- | The reason code for the state change.
srCode :: Lens' StateReason (Maybe Text)
srCode = lens _srCode (\s a -> s {_srCode = a})

-- | The message for the state change.     * @Server.InsufficientInstanceCapacity@ : There was insufficient capacity available to satisfy the launch request.     * @Server.InternalError@ : An internal error caused the instance to terminate during launch.     * @Server.ScheduledStop@ : The instance was stopped due to a scheduled retirement.     * @Server.SpotInstanceShutdown@ : The instance was stopped because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.     * @Server.SpotInstanceTermination@ : The instance was terminated because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.     * @Client.InstanceInitiatedShutdown@ : The instance was shut down using the @shutdown -h@ command from the instance.     * @Client.InstanceTerminated@ : The instance was terminated or rebooted during AMI creation.     * @Client.InternalError@ : A client error caused the instance to terminate during launch.     * @Client.InvalidSnapshot.NotFound@ : The specified snapshot was not found.     * @Client.UserInitiatedHibernate@ : Hibernation was initiated on the instance.     * @Client.UserInitiatedShutdown@ : The instance was shut down using the Amazon EC2 API.     * @Client.VolumeLimitExceeded@ : The limit on the number of EBS volumes or total storage was exceeded. Decrease usage or request an increase in your account limits.
srMessage :: Lens' StateReason (Maybe Text)
srMessage = lens _srMessage (\s a -> s {_srMessage = a})

instance FromXML StateReason where
  parseXML x = StateReason' <$> (x .@? "code") <*> (x .@? "message")

instance Hashable StateReason

instance NFData StateReason

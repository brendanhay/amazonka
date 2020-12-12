{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StateReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StateReason
  ( StateReason (..),

    -- * Smart constructor
    mkStateReason,

    -- * Lenses
    srCode,
    srMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a state change.
--
-- /See:/ 'mkStateReason' smart constructor.
data StateReason = StateReason'
  { code :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StateReason' with the minimum fields required to make a request.
--
-- * 'code' - The reason code for the state change.
-- * 'message' - The message for the state change.
--
--
--     * @Server.InsufficientInstanceCapacity@ : There was insufficient capacity available to satisfy the launch request.
--
--
--     * @Server.InternalError@ : An internal error caused the instance to terminate during launch.
--
--
--     * @Server.ScheduledStop@ : The instance was stopped due to a scheduled retirement.
--
--
--     * @Server.SpotInstanceShutdown@ : The instance was stopped because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.
--
--
--     * @Server.SpotInstanceTermination@ : The instance was terminated because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.
--
--
--     * @Client.InstanceInitiatedShutdown@ : The instance was shut down using the @shutdown -h@ command from the instance.
--
--
--     * @Client.InstanceTerminated@ : The instance was terminated or rebooted during AMI creation.
--
--
--     * @Client.InternalError@ : A client error caused the instance to terminate during launch.
--
--
--     * @Client.InvalidSnapshot.NotFound@ : The specified snapshot was not found.
--
--
--     * @Client.UserInitiatedHibernate@ : Hibernation was initiated on the instance.
--
--
--     * @Client.UserInitiatedShutdown@ : The instance was shut down using the Amazon EC2 API.
--
--
--     * @Client.VolumeLimitExceeded@ : The limit on the number of EBS volumes or total storage was exceeded. Decrease usage or request an increase in your account limits.
mkStateReason ::
  StateReason
mkStateReason =
  StateReason' {code = Lude.Nothing, message = Lude.Nothing}

-- | The reason code for the state change.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srCode :: Lens.Lens' StateReason (Lude.Maybe Lude.Text)
srCode = Lens.lens (code :: StateReason -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: StateReason)
{-# DEPRECATED srCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The message for the state change.
--
--
--     * @Server.InsufficientInstanceCapacity@ : There was insufficient capacity available to satisfy the launch request.
--
--
--     * @Server.InternalError@ : An internal error caused the instance to terminate during launch.
--
--
--     * @Server.ScheduledStop@ : The instance was stopped due to a scheduled retirement.
--
--
--     * @Server.SpotInstanceShutdown@ : The instance was stopped because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.
--
--
--     * @Server.SpotInstanceTermination@ : The instance was terminated because the number of Spot requests with a maximum price equal to or higher than the Spot price exceeded available capacity or because of an increase in the Spot price.
--
--
--     * @Client.InstanceInitiatedShutdown@ : The instance was shut down using the @shutdown -h@ command from the instance.
--
--
--     * @Client.InstanceTerminated@ : The instance was terminated or rebooted during AMI creation.
--
--
--     * @Client.InternalError@ : A client error caused the instance to terminate during launch.
--
--
--     * @Client.InvalidSnapshot.NotFound@ : The specified snapshot was not found.
--
--
--     * @Client.UserInitiatedHibernate@ : Hibernation was initiated on the instance.
--
--
--     * @Client.UserInitiatedShutdown@ : The instance was shut down using the Amazon EC2 API.
--
--
--     * @Client.VolumeLimitExceeded@ : The limit on the number of EBS volumes or total storage was exceeded. Decrease usage or request an increase in your account limits.
--
--
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srMessage :: Lens.Lens' StateReason (Lude.Maybe Lude.Text)
srMessage = Lens.lens (message :: StateReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: StateReason)
{-# DEPRECATED srMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML StateReason where
  parseXML x =
    StateReason'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StateReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.StateReason
  ( StateReason (..)
  -- * Smart constructor
  , mkStateReason
  -- * Lenses
  , srCode
  , srMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a state change.
--
-- /See:/ 'mkStateReason' smart constructor.
data StateReason = StateReason'
  { code :: Core.Maybe Core.Text
    -- ^ The reason code for the state change.
  , message :: Core.Maybe Core.Text
    -- ^ The message for the state change.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StateReason' value with any optional fields omitted.
mkStateReason
    :: StateReason
mkStateReason
  = StateReason'{code = Core.Nothing, message = Core.Nothing}

-- | The reason code for the state change.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srCode :: Lens.Lens' StateReason (Core.Maybe Core.Text)
srCode = Lens.field @"code"
{-# INLINEABLE srCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

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
srMessage :: Lens.Lens' StateReason (Core.Maybe Core.Text)
srMessage = Lens.field @"message"
{-# INLINEABLE srMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML StateReason where
        parseXML x
          = StateReason' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"

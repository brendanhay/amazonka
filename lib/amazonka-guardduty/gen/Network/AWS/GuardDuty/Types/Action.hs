{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Action
  ( Action (..),

    -- * Smart constructor
    mkAction,

    -- * Lenses
    aActionType,
    aAwsApiCallAction,
    aDnsRequestAction,
    aNetworkConnectionAction,
    aPortProbeAction,
  )
where

import qualified Network.AWS.GuardDuty.Types.AwsApiCallAction as Types
import qualified Network.AWS.GuardDuty.Types.DnsRequestAction as Types
import qualified Network.AWS.GuardDuty.Types.NetworkConnectionAction as Types
import qualified Network.AWS.GuardDuty.Types.PortProbeAction as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about actions.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { -- | The GuardDuty finding activity type.
    actionType :: Core.Maybe Types.String,
    -- | Information about the AWS_API_CALL action described in this finding.
    awsApiCallAction :: Core.Maybe Types.AwsApiCallAction,
    -- | Information about the DNS_REQUEST action described in this finding.
    dnsRequestAction :: Core.Maybe Types.DnsRequestAction,
    -- | Information about the NETWORK_CONNECTION action described in this finding.
    networkConnectionAction :: Core.Maybe Types.NetworkConnectionAction,
    -- | Information about the PORT_PROBE action described in this finding.
    portProbeAction :: Core.Maybe Types.PortProbeAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Action' value with any optional fields omitted.
mkAction ::
  Action
mkAction =
  Action'
    { actionType = Core.Nothing,
      awsApiCallAction = Core.Nothing,
      dnsRequestAction = Core.Nothing,
      networkConnectionAction = Core.Nothing,
      portProbeAction = Core.Nothing
    }

-- | The GuardDuty finding activity type.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionType :: Lens.Lens' Action (Core.Maybe Types.String)
aActionType = Lens.field @"actionType"
{-# DEPRECATED aActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | Information about the AWS_API_CALL action described in this finding.
--
-- /Note:/ Consider using 'awsApiCallAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAwsApiCallAction :: Lens.Lens' Action (Core.Maybe Types.AwsApiCallAction)
aAwsApiCallAction = Lens.field @"awsApiCallAction"
{-# DEPRECATED aAwsApiCallAction "Use generic-lens or generic-optics with 'awsApiCallAction' instead." #-}

-- | Information about the DNS_REQUEST action described in this finding.
--
-- /Note:/ Consider using 'dnsRequestAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDnsRequestAction :: Lens.Lens' Action (Core.Maybe Types.DnsRequestAction)
aDnsRequestAction = Lens.field @"dnsRequestAction"
{-# DEPRECATED aDnsRequestAction "Use generic-lens or generic-optics with 'dnsRequestAction' instead." #-}

-- | Information about the NETWORK_CONNECTION action described in this finding.
--
-- /Note:/ Consider using 'networkConnectionAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkConnectionAction :: Lens.Lens' Action (Core.Maybe Types.NetworkConnectionAction)
aNetworkConnectionAction = Lens.field @"networkConnectionAction"
{-# DEPRECATED aNetworkConnectionAction "Use generic-lens or generic-optics with 'networkConnectionAction' instead." #-}

-- | Information about the PORT_PROBE action described in this finding.
--
-- /Note:/ Consider using 'portProbeAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPortProbeAction :: Lens.Lens' Action (Core.Maybe Types.PortProbeAction)
aPortProbeAction = Lens.field @"portProbeAction"
{-# DEPRECATED aPortProbeAction "Use generic-lens or generic-optics with 'portProbeAction' instead." #-}

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject "Action" Core.$
      \x ->
        Action'
          Core.<$> (x Core..:? "actionType")
          Core.<*> (x Core..:? "awsApiCallAction")
          Core.<*> (x Core..:? "dnsRequestAction")
          Core.<*> (x Core..:? "networkConnectionAction")
          Core.<*> (x Core..:? "portProbeAction")

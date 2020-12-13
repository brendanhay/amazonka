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
    aNetworkConnectionAction,
    aPortProbeAction,
    aActionType,
    aDNSRequestAction,
    aAWSAPICallAction,
  )
where

import Network.AWS.GuardDuty.Types.AWSAPICallAction
import Network.AWS.GuardDuty.Types.DNSRequestAction
import Network.AWS.GuardDuty.Types.NetworkConnectionAction
import Network.AWS.GuardDuty.Types.PortProbeAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about actions.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { -- | Information about the NETWORK_CONNECTION action described in this finding.
    networkConnectionAction :: Lude.Maybe NetworkConnectionAction,
    -- | Information about the PORT_PROBE action described in this finding.
    portProbeAction :: Lude.Maybe PortProbeAction,
    -- | The GuardDuty finding activity type.
    actionType :: Lude.Maybe Lude.Text,
    -- | Information about the DNS_REQUEST action described in this finding.
    dnsRequestAction :: Lude.Maybe DNSRequestAction,
    -- | Information about the AWS_API_CALL action described in this finding.
    awsAPICallAction :: Lude.Maybe AWSAPICallAction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- * 'networkConnectionAction' - Information about the NETWORK_CONNECTION action described in this finding.
-- * 'portProbeAction' - Information about the PORT_PROBE action described in this finding.
-- * 'actionType' - The GuardDuty finding activity type.
-- * 'dnsRequestAction' - Information about the DNS_REQUEST action described in this finding.
-- * 'awsAPICallAction' - Information about the AWS_API_CALL action described in this finding.
mkAction ::
  Action
mkAction =
  Action'
    { networkConnectionAction = Lude.Nothing,
      portProbeAction = Lude.Nothing,
      actionType = Lude.Nothing,
      dnsRequestAction = Lude.Nothing,
      awsAPICallAction = Lude.Nothing
    }

-- | Information about the NETWORK_CONNECTION action described in this finding.
--
-- /Note:/ Consider using 'networkConnectionAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkConnectionAction :: Lens.Lens' Action (Lude.Maybe NetworkConnectionAction)
aNetworkConnectionAction = Lens.lens (networkConnectionAction :: Action -> Lude.Maybe NetworkConnectionAction) (\s a -> s {networkConnectionAction = a} :: Action)
{-# DEPRECATED aNetworkConnectionAction "Use generic-lens or generic-optics with 'networkConnectionAction' instead." #-}

-- | Information about the PORT_PROBE action described in this finding.
--
-- /Note:/ Consider using 'portProbeAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPortProbeAction :: Lens.Lens' Action (Lude.Maybe PortProbeAction)
aPortProbeAction = Lens.lens (portProbeAction :: Action -> Lude.Maybe PortProbeAction) (\s a -> s {portProbeAction = a} :: Action)
{-# DEPRECATED aPortProbeAction "Use generic-lens or generic-optics with 'portProbeAction' instead." #-}

-- | The GuardDuty finding activity type.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aActionType :: Lens.Lens' Action (Lude.Maybe Lude.Text)
aActionType = Lens.lens (actionType :: Action -> Lude.Maybe Lude.Text) (\s a -> s {actionType = a} :: Action)
{-# DEPRECATED aActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | Information about the DNS_REQUEST action described in this finding.
--
-- /Note:/ Consider using 'dnsRequestAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDNSRequestAction :: Lens.Lens' Action (Lude.Maybe DNSRequestAction)
aDNSRequestAction = Lens.lens (dnsRequestAction :: Action -> Lude.Maybe DNSRequestAction) (\s a -> s {dnsRequestAction = a} :: Action)
{-# DEPRECATED aDNSRequestAction "Use generic-lens or generic-optics with 'dnsRequestAction' instead." #-}

-- | Information about the AWS_API_CALL action described in this finding.
--
-- /Note:/ Consider using 'awsAPICallAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAWSAPICallAction :: Lens.Lens' Action (Lude.Maybe AWSAPICallAction)
aAWSAPICallAction = Lens.lens (awsAPICallAction :: Action -> Lude.Maybe AWSAPICallAction) (\s a -> s {awsAPICallAction = a} :: Action)
{-# DEPRECATED aAWSAPICallAction "Use generic-lens or generic-optics with 'awsAPICallAction' instead." #-}

instance Lude.FromJSON Action where
  parseJSON =
    Lude.withObject
      "Action"
      ( \x ->
          Action'
            Lude.<$> (x Lude..:? "networkConnectionAction")
            Lude.<*> (x Lude..:? "portProbeAction")
            Lude.<*> (x Lude..:? "actionType")
            Lude.<*> (x Lude..:? "dnsRequestAction")
            Lude.<*> (x Lude..:? "awsApiCallAction")
      )

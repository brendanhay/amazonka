{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Action where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types.AwsApiCallAction
import Network.AWS.GuardDuty.Types.DnsRequestAction
import Network.AWS.GuardDuty.Types.NetworkConnectionAction
import Network.AWS.GuardDuty.Types.PortProbeAction
import qualified Network.AWS.Lens as Lens

-- | Contains information about actions.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | The GuardDuty finding activity type.
    actionType :: Core.Maybe Core.Text,
    -- | Information about the DNS_REQUEST action described in this finding.
    dnsRequestAction :: Core.Maybe DnsRequestAction,
    -- | Information about the NETWORK_CONNECTION action described in this
    -- finding.
    networkConnectionAction :: Core.Maybe NetworkConnectionAction,
    -- | Information about the AWS_API_CALL action described in this finding.
    awsApiCallAction :: Core.Maybe AwsApiCallAction,
    -- | Information about the PORT_PROBE action described in this finding.
    portProbeAction :: Core.Maybe PortProbeAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'action_actionType' - The GuardDuty finding activity type.
--
-- 'dnsRequestAction', 'action_dnsRequestAction' - Information about the DNS_REQUEST action described in this finding.
--
-- 'networkConnectionAction', 'action_networkConnectionAction' - Information about the NETWORK_CONNECTION action described in this
-- finding.
--
-- 'awsApiCallAction', 'action_awsApiCallAction' - Information about the AWS_API_CALL action described in this finding.
--
-- 'portProbeAction', 'action_portProbeAction' - Information about the PORT_PROBE action described in this finding.
newAction ::
  Action
newAction =
  Action'
    { actionType = Core.Nothing,
      dnsRequestAction = Core.Nothing,
      networkConnectionAction = Core.Nothing,
      awsApiCallAction = Core.Nothing,
      portProbeAction = Core.Nothing
    }

-- | The GuardDuty finding activity type.
action_actionType :: Lens.Lens' Action (Core.Maybe Core.Text)
action_actionType = Lens.lens (\Action' {actionType} -> actionType) (\s@Action' {} a -> s {actionType = a} :: Action)

-- | Information about the DNS_REQUEST action described in this finding.
action_dnsRequestAction :: Lens.Lens' Action (Core.Maybe DnsRequestAction)
action_dnsRequestAction = Lens.lens (\Action' {dnsRequestAction} -> dnsRequestAction) (\s@Action' {} a -> s {dnsRequestAction = a} :: Action)

-- | Information about the NETWORK_CONNECTION action described in this
-- finding.
action_networkConnectionAction :: Lens.Lens' Action (Core.Maybe NetworkConnectionAction)
action_networkConnectionAction = Lens.lens (\Action' {networkConnectionAction} -> networkConnectionAction) (\s@Action' {} a -> s {networkConnectionAction = a} :: Action)

-- | Information about the AWS_API_CALL action described in this finding.
action_awsApiCallAction :: Lens.Lens' Action (Core.Maybe AwsApiCallAction)
action_awsApiCallAction = Lens.lens (\Action' {awsApiCallAction} -> awsApiCallAction) (\s@Action' {} a -> s {awsApiCallAction = a} :: Action)

-- | Information about the PORT_PROBE action described in this finding.
action_portProbeAction :: Lens.Lens' Action (Core.Maybe PortProbeAction)
action_portProbeAction = Lens.lens (\Action' {portProbeAction} -> portProbeAction) (\s@Action' {} a -> s {portProbeAction = a} :: Action)

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action'
            Core.<$> (x Core..:? "actionType")
            Core.<*> (x Core..:? "dnsRequestAction")
            Core.<*> (x Core..:? "networkConnectionAction")
            Core.<*> (x Core..:? "awsApiCallAction")
            Core.<*> (x Core..:? "portProbeAction")
      )

instance Core.Hashable Action

instance Core.NFData Action

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
-- Module      : Amazonka.SecurityHub.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsApiCallAction
import Amazonka.SecurityHub.Types.DnsRequestAction
import Amazonka.SecurityHub.Types.NetworkConnectionAction
import Amazonka.SecurityHub.Types.PortProbeAction

-- | Provides details about one of the following actions that affects or that
-- was taken on a resource:
--
-- -   A remote IP address issued an Amazon Web Services API call
--
-- -   A DNS request was received
--
-- -   A remote IP address attempted to connect to an EC2 instance
--
-- -   A remote IP address attempted a port probe on an EC2 instance
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Included if @ActionType@ is @NETWORK_CONNECTION@. Provides details about
    -- the network connection that was detected.
    networkConnectionAction :: Prelude.Maybe NetworkConnectionAction,
    -- | Included if @ActionType@ is @PORT_PROBE@. Provides details about the
    -- port probe that was detected.
    portProbeAction :: Prelude.Maybe PortProbeAction,
    -- | The type of action that was detected. The possible action types are:
    --
    -- -   @NETWORK_CONNECTION@
    --
    -- -   @AWS_API_CALL@
    --
    -- -   @DNS_REQUEST@
    --
    -- -   @PORT_PROBE@
    actionType :: Prelude.Maybe Prelude.Text,
    -- | Included if @ActionType@ is @DNS_REQUEST@. Provides details about the
    -- DNS request that was detected.
    dnsRequestAction :: Prelude.Maybe DnsRequestAction,
    -- | Included if @ActionType@ is @AWS_API_CALL@. Provides details about the
    -- API call that was detected.
    awsApiCallAction :: Prelude.Maybe AwsApiCallAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkConnectionAction', 'action_networkConnectionAction' - Included if @ActionType@ is @NETWORK_CONNECTION@. Provides details about
-- the network connection that was detected.
--
-- 'portProbeAction', 'action_portProbeAction' - Included if @ActionType@ is @PORT_PROBE@. Provides details about the
-- port probe that was detected.
--
-- 'actionType', 'action_actionType' - The type of action that was detected. The possible action types are:
--
-- -   @NETWORK_CONNECTION@
--
-- -   @AWS_API_CALL@
--
-- -   @DNS_REQUEST@
--
-- -   @PORT_PROBE@
--
-- 'dnsRequestAction', 'action_dnsRequestAction' - Included if @ActionType@ is @DNS_REQUEST@. Provides details about the
-- DNS request that was detected.
--
-- 'awsApiCallAction', 'action_awsApiCallAction' - Included if @ActionType@ is @AWS_API_CALL@. Provides details about the
-- API call that was detected.
newAction ::
  Action
newAction =
  Action'
    { networkConnectionAction = Prelude.Nothing,
      portProbeAction = Prelude.Nothing,
      actionType = Prelude.Nothing,
      dnsRequestAction = Prelude.Nothing,
      awsApiCallAction = Prelude.Nothing
    }

-- | Included if @ActionType@ is @NETWORK_CONNECTION@. Provides details about
-- the network connection that was detected.
action_networkConnectionAction :: Lens.Lens' Action (Prelude.Maybe NetworkConnectionAction)
action_networkConnectionAction = Lens.lens (\Action' {networkConnectionAction} -> networkConnectionAction) (\s@Action' {} a -> s {networkConnectionAction = a} :: Action)

-- | Included if @ActionType@ is @PORT_PROBE@. Provides details about the
-- port probe that was detected.
action_portProbeAction :: Lens.Lens' Action (Prelude.Maybe PortProbeAction)
action_portProbeAction = Lens.lens (\Action' {portProbeAction} -> portProbeAction) (\s@Action' {} a -> s {portProbeAction = a} :: Action)

-- | The type of action that was detected. The possible action types are:
--
-- -   @NETWORK_CONNECTION@
--
-- -   @AWS_API_CALL@
--
-- -   @DNS_REQUEST@
--
-- -   @PORT_PROBE@
action_actionType :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_actionType = Lens.lens (\Action' {actionType} -> actionType) (\s@Action' {} a -> s {actionType = a} :: Action)

-- | Included if @ActionType@ is @DNS_REQUEST@. Provides details about the
-- DNS request that was detected.
action_dnsRequestAction :: Lens.Lens' Action (Prelude.Maybe DnsRequestAction)
action_dnsRequestAction = Lens.lens (\Action' {dnsRequestAction} -> dnsRequestAction) (\s@Action' {} a -> s {dnsRequestAction = a} :: Action)

-- | Included if @ActionType@ is @AWS_API_CALL@. Provides details about the
-- API call that was detected.
action_awsApiCallAction :: Lens.Lens' Action (Prelude.Maybe AwsApiCallAction)
action_awsApiCallAction = Lens.lens (\Action' {awsApiCallAction} -> awsApiCallAction) (\s@Action' {} a -> s {awsApiCallAction = a} :: Action)

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Core..:? "NetworkConnectionAction")
            Prelude.<*> (x Core..:? "PortProbeAction")
            Prelude.<*> (x Core..:? "ActionType")
            Prelude.<*> (x Core..:? "DnsRequestAction")
            Prelude.<*> (x Core..:? "AwsApiCallAction")
      )

instance Prelude.Hashable Action

instance Prelude.NFData Action

instance Core.ToJSON Action where
  toJSON Action' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NetworkConnectionAction" Core..=)
              Prelude.<$> networkConnectionAction,
            ("PortProbeAction" Core..=)
              Prelude.<$> portProbeAction,
            ("ActionType" Core..=) Prelude.<$> actionType,
            ("DnsRequestAction" Core..=)
              Prelude.<$> dnsRequestAction,
            ("AwsApiCallAction" Core..=)
              Prelude.<$> awsApiCallAction
          ]
      )

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
-- Module      : Amazonka.GuardDuty.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Action where

import qualified Amazonka.Core as Core
import Amazonka.GuardDuty.Types.AwsApiCallAction
import Amazonka.GuardDuty.Types.DnsRequestAction
import Amazonka.GuardDuty.Types.NetworkConnectionAction
import Amazonka.GuardDuty.Types.PortProbeAction
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about actions.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | Information about the NETWORK_CONNECTION action described in this
    -- finding.
    networkConnectionAction :: Prelude.Maybe NetworkConnectionAction,
    -- | Information about the PORT_PROBE action described in this finding.
    portProbeAction :: Prelude.Maybe PortProbeAction,
    -- | The GuardDuty finding activity type.
    actionType :: Prelude.Maybe Prelude.Text,
    -- | Information about the DNS_REQUEST action described in this finding.
    dnsRequestAction :: Prelude.Maybe DnsRequestAction,
    -- | Information about the AWS_API_CALL action described in this finding.
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
-- 'networkConnectionAction', 'action_networkConnectionAction' - Information about the NETWORK_CONNECTION action described in this
-- finding.
--
-- 'portProbeAction', 'action_portProbeAction' - Information about the PORT_PROBE action described in this finding.
--
-- 'actionType', 'action_actionType' - The GuardDuty finding activity type.
--
-- 'dnsRequestAction', 'action_dnsRequestAction' - Information about the DNS_REQUEST action described in this finding.
--
-- 'awsApiCallAction', 'action_awsApiCallAction' - Information about the AWS_API_CALL action described in this finding.
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

-- | Information about the NETWORK_CONNECTION action described in this
-- finding.
action_networkConnectionAction :: Lens.Lens' Action (Prelude.Maybe NetworkConnectionAction)
action_networkConnectionAction = Lens.lens (\Action' {networkConnectionAction} -> networkConnectionAction) (\s@Action' {} a -> s {networkConnectionAction = a} :: Action)

-- | Information about the PORT_PROBE action described in this finding.
action_portProbeAction :: Lens.Lens' Action (Prelude.Maybe PortProbeAction)
action_portProbeAction = Lens.lens (\Action' {portProbeAction} -> portProbeAction) (\s@Action' {} a -> s {portProbeAction = a} :: Action)

-- | The GuardDuty finding activity type.
action_actionType :: Lens.Lens' Action (Prelude.Maybe Prelude.Text)
action_actionType = Lens.lens (\Action' {actionType} -> actionType) (\s@Action' {} a -> s {actionType = a} :: Action)

-- | Information about the DNS_REQUEST action described in this finding.
action_dnsRequestAction :: Lens.Lens' Action (Prelude.Maybe DnsRequestAction)
action_dnsRequestAction = Lens.lens (\Action' {dnsRequestAction} -> dnsRequestAction) (\s@Action' {} a -> s {dnsRequestAction = a} :: Action)

-- | Information about the AWS_API_CALL action described in this finding.
action_awsApiCallAction :: Lens.Lens' Action (Prelude.Maybe AwsApiCallAction)
action_awsApiCallAction = Lens.lens (\Action' {awsApiCallAction} -> awsApiCallAction) (\s@Action' {} a -> s {awsApiCallAction = a} :: Action)

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action'
            Prelude.<$> (x Core..:? "networkConnectionAction")
            Prelude.<*> (x Core..:? "portProbeAction")
            Prelude.<*> (x Core..:? "actionType")
            Prelude.<*> (x Core..:? "dnsRequestAction")
            Prelude.<*> (x Core..:? "awsApiCallAction")
      )

instance Prelude.Hashable Action where
  hashWithSalt salt' Action' {..} =
    salt' `Prelude.hashWithSalt` awsApiCallAction
      `Prelude.hashWithSalt` dnsRequestAction
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` portProbeAction
      `Prelude.hashWithSalt` networkConnectionAction

instance Prelude.NFData Action where
  rnf Action' {..} =
    Prelude.rnf networkConnectionAction
      `Prelude.seq` Prelude.rnf awsApiCallAction
      `Prelude.seq` Prelude.rnf dnsRequestAction
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf portProbeAction

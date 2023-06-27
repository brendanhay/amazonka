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
-- Module      : Amazonka.GroundStation.Types.AwsGroundStationAgentEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.AwsGroundStationAgentEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.AgentStatus
import Amazonka.GroundStation.Types.AuditResults
import Amazonka.GroundStation.Types.ConnectionDetails
import Amazonka.GroundStation.Types.RangedConnectionDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about AwsGroundStationAgentEndpoint.
--
-- /See:/ 'newAwsGroundStationAgentEndpoint' smart constructor.
data AwsGroundStationAgentEndpoint = AwsGroundStationAgentEndpoint'
  { -- | The status of AgentEndpoint.
    agentStatus :: Prelude.Maybe AgentStatus,
    -- | The results of the audit.
    auditResults :: Prelude.Maybe AuditResults,
    -- | The egress address of AgentEndpoint.
    egressAddress :: ConnectionDetails,
    -- | The ingress address of AgentEndpoint.
    ingressAddress :: RangedConnectionDetails,
    -- | Name string associated with AgentEndpoint. Used as a human-readable
    -- identifier for AgentEndpoint.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGroundStationAgentEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentStatus', 'awsGroundStationAgentEndpoint_agentStatus' - The status of AgentEndpoint.
--
-- 'auditResults', 'awsGroundStationAgentEndpoint_auditResults' - The results of the audit.
--
-- 'egressAddress', 'awsGroundStationAgentEndpoint_egressAddress' - The egress address of AgentEndpoint.
--
-- 'ingressAddress', 'awsGroundStationAgentEndpoint_ingressAddress' - The ingress address of AgentEndpoint.
--
-- 'name', 'awsGroundStationAgentEndpoint_name' - Name string associated with AgentEndpoint. Used as a human-readable
-- identifier for AgentEndpoint.
newAwsGroundStationAgentEndpoint ::
  -- | 'egressAddress'
  ConnectionDetails ->
  -- | 'ingressAddress'
  RangedConnectionDetails ->
  -- | 'name'
  Prelude.Text ->
  AwsGroundStationAgentEndpoint
newAwsGroundStationAgentEndpoint
  pEgressAddress_
  pIngressAddress_
  pName_ =
    AwsGroundStationAgentEndpoint'
      { agentStatus =
          Prelude.Nothing,
        auditResults = Prelude.Nothing,
        egressAddress = pEgressAddress_,
        ingressAddress = pIngressAddress_,
        name = pName_
      }

-- | The status of AgentEndpoint.
awsGroundStationAgentEndpoint_agentStatus :: Lens.Lens' AwsGroundStationAgentEndpoint (Prelude.Maybe AgentStatus)
awsGroundStationAgentEndpoint_agentStatus = Lens.lens (\AwsGroundStationAgentEndpoint' {agentStatus} -> agentStatus) (\s@AwsGroundStationAgentEndpoint' {} a -> s {agentStatus = a} :: AwsGroundStationAgentEndpoint)

-- | The results of the audit.
awsGroundStationAgentEndpoint_auditResults :: Lens.Lens' AwsGroundStationAgentEndpoint (Prelude.Maybe AuditResults)
awsGroundStationAgentEndpoint_auditResults = Lens.lens (\AwsGroundStationAgentEndpoint' {auditResults} -> auditResults) (\s@AwsGroundStationAgentEndpoint' {} a -> s {auditResults = a} :: AwsGroundStationAgentEndpoint)

-- | The egress address of AgentEndpoint.
awsGroundStationAgentEndpoint_egressAddress :: Lens.Lens' AwsGroundStationAgentEndpoint ConnectionDetails
awsGroundStationAgentEndpoint_egressAddress = Lens.lens (\AwsGroundStationAgentEndpoint' {egressAddress} -> egressAddress) (\s@AwsGroundStationAgentEndpoint' {} a -> s {egressAddress = a} :: AwsGroundStationAgentEndpoint)

-- | The ingress address of AgentEndpoint.
awsGroundStationAgentEndpoint_ingressAddress :: Lens.Lens' AwsGroundStationAgentEndpoint RangedConnectionDetails
awsGroundStationAgentEndpoint_ingressAddress = Lens.lens (\AwsGroundStationAgentEndpoint' {ingressAddress} -> ingressAddress) (\s@AwsGroundStationAgentEndpoint' {} a -> s {ingressAddress = a} :: AwsGroundStationAgentEndpoint)

-- | Name string associated with AgentEndpoint. Used as a human-readable
-- identifier for AgentEndpoint.
awsGroundStationAgentEndpoint_name :: Lens.Lens' AwsGroundStationAgentEndpoint Prelude.Text
awsGroundStationAgentEndpoint_name = Lens.lens (\AwsGroundStationAgentEndpoint' {name} -> name) (\s@AwsGroundStationAgentEndpoint' {} a -> s {name = a} :: AwsGroundStationAgentEndpoint)

instance Data.FromJSON AwsGroundStationAgentEndpoint where
  parseJSON =
    Data.withObject
      "AwsGroundStationAgentEndpoint"
      ( \x ->
          AwsGroundStationAgentEndpoint'
            Prelude.<$> (x Data..:? "agentStatus")
            Prelude.<*> (x Data..:? "auditResults")
            Prelude.<*> (x Data..: "egressAddress")
            Prelude.<*> (x Data..: "ingressAddress")
            Prelude.<*> (x Data..: "name")
      )

instance
  Prelude.Hashable
    AwsGroundStationAgentEndpoint
  where
  hashWithSalt _salt AwsGroundStationAgentEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` agentStatus
      `Prelude.hashWithSalt` auditResults
      `Prelude.hashWithSalt` egressAddress
      `Prelude.hashWithSalt` ingressAddress
      `Prelude.hashWithSalt` name

instance Prelude.NFData AwsGroundStationAgentEndpoint where
  rnf AwsGroundStationAgentEndpoint' {..} =
    Prelude.rnf agentStatus
      `Prelude.seq` Prelude.rnf auditResults
      `Prelude.seq` Prelude.rnf egressAddress
      `Prelude.seq` Prelude.rnf ingressAddress
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AwsGroundStationAgentEndpoint where
  toJSON AwsGroundStationAgentEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("agentStatus" Data..=) Prelude.<$> agentStatus,
            ("auditResults" Data..=) Prelude.<$> auditResults,
            Prelude.Just ("egressAddress" Data..= egressAddress),
            Prelude.Just
              ("ingressAddress" Data..= ingressAddress),
            Prelude.Just ("name" Data..= name)
          ]
      )

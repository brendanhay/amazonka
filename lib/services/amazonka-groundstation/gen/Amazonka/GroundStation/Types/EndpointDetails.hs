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
-- Module      : Amazonka.GroundStation.Types.EndpointDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EndpointDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.AwsGroundStationAgentEndpoint
import Amazonka.GroundStation.Types.CapabilityHealth
import Amazonka.GroundStation.Types.CapabilityHealthReason
import Amazonka.GroundStation.Types.DataflowEndpoint
import Amazonka.GroundStation.Types.SecurityDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the endpoint details.
--
-- /See:/ 'newEndpointDetails' smart constructor.
data EndpointDetails = EndpointDetails'
  { -- | An agent endpoint.
    awsGroundStationAgentEndpoint :: Prelude.Maybe AwsGroundStationAgentEndpoint,
    -- | A dataflow endpoint.
    endpoint :: Prelude.Maybe DataflowEndpoint,
    -- | Health reasons for a dataflow endpoint. This field is ignored when
    -- calling @CreateDataflowEndpointGroup@.
    healthReasons :: Prelude.Maybe [CapabilityHealthReason],
    -- | A dataflow endpoint health status. This field is ignored when calling
    -- @CreateDataflowEndpointGroup@.
    healthStatus :: Prelude.Maybe CapabilityHealth,
    -- | Endpoint security details including a list of subnets, a list of
    -- security groups and a role to connect streams to instances.
    securityDetails :: Prelude.Maybe SecurityDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsGroundStationAgentEndpoint', 'endpointDetails_awsGroundStationAgentEndpoint' - An agent endpoint.
--
-- 'endpoint', 'endpointDetails_endpoint' - A dataflow endpoint.
--
-- 'healthReasons', 'endpointDetails_healthReasons' - Health reasons for a dataflow endpoint. This field is ignored when
-- calling @CreateDataflowEndpointGroup@.
--
-- 'healthStatus', 'endpointDetails_healthStatus' - A dataflow endpoint health status. This field is ignored when calling
-- @CreateDataflowEndpointGroup@.
--
-- 'securityDetails', 'endpointDetails_securityDetails' - Endpoint security details including a list of subnets, a list of
-- security groups and a role to connect streams to instances.
newEndpointDetails ::
  EndpointDetails
newEndpointDetails =
  EndpointDetails'
    { awsGroundStationAgentEndpoint =
        Prelude.Nothing,
      endpoint = Prelude.Nothing,
      healthReasons = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      securityDetails = Prelude.Nothing
    }

-- | An agent endpoint.
endpointDetails_awsGroundStationAgentEndpoint :: Lens.Lens' EndpointDetails (Prelude.Maybe AwsGroundStationAgentEndpoint)
endpointDetails_awsGroundStationAgentEndpoint = Lens.lens (\EndpointDetails' {awsGroundStationAgentEndpoint} -> awsGroundStationAgentEndpoint) (\s@EndpointDetails' {} a -> s {awsGroundStationAgentEndpoint = a} :: EndpointDetails)

-- | A dataflow endpoint.
endpointDetails_endpoint :: Lens.Lens' EndpointDetails (Prelude.Maybe DataflowEndpoint)
endpointDetails_endpoint = Lens.lens (\EndpointDetails' {endpoint} -> endpoint) (\s@EndpointDetails' {} a -> s {endpoint = a} :: EndpointDetails)

-- | Health reasons for a dataflow endpoint. This field is ignored when
-- calling @CreateDataflowEndpointGroup@.
endpointDetails_healthReasons :: Lens.Lens' EndpointDetails (Prelude.Maybe [CapabilityHealthReason])
endpointDetails_healthReasons = Lens.lens (\EndpointDetails' {healthReasons} -> healthReasons) (\s@EndpointDetails' {} a -> s {healthReasons = a} :: EndpointDetails) Prelude.. Lens.mapping Lens.coerced

-- | A dataflow endpoint health status. This field is ignored when calling
-- @CreateDataflowEndpointGroup@.
endpointDetails_healthStatus :: Lens.Lens' EndpointDetails (Prelude.Maybe CapabilityHealth)
endpointDetails_healthStatus = Lens.lens (\EndpointDetails' {healthStatus} -> healthStatus) (\s@EndpointDetails' {} a -> s {healthStatus = a} :: EndpointDetails)

-- | Endpoint security details including a list of subnets, a list of
-- security groups and a role to connect streams to instances.
endpointDetails_securityDetails :: Lens.Lens' EndpointDetails (Prelude.Maybe SecurityDetails)
endpointDetails_securityDetails = Lens.lens (\EndpointDetails' {securityDetails} -> securityDetails) (\s@EndpointDetails' {} a -> s {securityDetails = a} :: EndpointDetails)

instance Data.FromJSON EndpointDetails where
  parseJSON =
    Data.withObject
      "EndpointDetails"
      ( \x ->
          EndpointDetails'
            Prelude.<$> (x Data..:? "awsGroundStationAgentEndpoint")
            Prelude.<*> (x Data..:? "endpoint")
            Prelude.<*> (x Data..:? "healthReasons" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "healthStatus")
            Prelude.<*> (x Data..:? "securityDetails")
      )

instance Prelude.Hashable EndpointDetails where
  hashWithSalt _salt EndpointDetails' {..} =
    _salt
      `Prelude.hashWithSalt` awsGroundStationAgentEndpoint
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` healthReasons
      `Prelude.hashWithSalt` healthStatus
      `Prelude.hashWithSalt` securityDetails

instance Prelude.NFData EndpointDetails where
  rnf EndpointDetails' {..} =
    Prelude.rnf awsGroundStationAgentEndpoint
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf healthReasons
      `Prelude.seq` Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf securityDetails

instance Data.ToJSON EndpointDetails where
  toJSON EndpointDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsGroundStationAgentEndpoint" Data..=)
              Prelude.<$> awsGroundStationAgentEndpoint,
            ("endpoint" Data..=) Prelude.<$> endpoint,
            ("healthReasons" Data..=) Prelude.<$> healthReasons,
            ("healthStatus" Data..=) Prelude.<$> healthStatus,
            ("securityDetails" Data..=)
              Prelude.<$> securityDetails
          ]
      )

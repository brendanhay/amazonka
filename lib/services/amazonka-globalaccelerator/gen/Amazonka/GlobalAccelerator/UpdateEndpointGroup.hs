{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GlobalAccelerator.UpdateEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an endpoint group. A resource must be valid and active when you
-- add it as an endpoint.
module Amazonka.GlobalAccelerator.UpdateEndpointGroup
  ( -- * Creating a Request
    UpdateEndpointGroup (..),
    newUpdateEndpointGroup,

    -- * Request Lenses
    updateEndpointGroup_endpointConfigurations,
    updateEndpointGroup_healthCheckIntervalSeconds,
    updateEndpointGroup_healthCheckPath,
    updateEndpointGroup_healthCheckPort,
    updateEndpointGroup_healthCheckProtocol,
    updateEndpointGroup_portOverrides,
    updateEndpointGroup_thresholdCount,
    updateEndpointGroup_trafficDialPercentage,
    updateEndpointGroup_endpointGroupArn,

    -- * Destructuring the Response
    UpdateEndpointGroupResponse (..),
    newUpdateEndpointGroupResponse,

    -- * Response Lenses
    updateEndpointGroupResponse_endpointGroup,
    updateEndpointGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEndpointGroup' smart constructor.
data UpdateEndpointGroup = UpdateEndpointGroup'
  { -- | The list of endpoint objects. A resource must be valid and active when
    -- you add it as an endpoint.
    endpointConfigurations :: Prelude.Maybe [EndpointConfiguration],
    -- | The time—10 seconds or 30 seconds—between each health check for an
    -- endpoint. The default value is 30.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | If the protocol is HTTP\/S, then this specifies the path that is the
    -- destination for health check targets. The default value is slash (\/).
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The port that Global Accelerator uses to check the health of endpoints
    -- that are part of this endpoint group. The default port is the listener
    -- port that this endpoint group is associated with. If the listener port
    -- is a list of ports, Global Accelerator uses the first port in the list.
    healthCheckPort :: Prelude.Maybe Prelude.Natural,
    -- | The protocol that Global Accelerator uses to check the health of
    -- endpoints that are part of this endpoint group. The default value is
    -- TCP.
    healthCheckProtocol :: Prelude.Maybe HealthCheckProtocol,
    -- | Override specific listener ports used to route traffic to endpoints that
    -- are part of this endpoint group. For example, you can create a port
    -- override in which the listener receives user traffic on ports 80 and
    -- 443, but your accelerator routes that traffic to ports 1080 and 1443,
    -- respectively, on the endpoints.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoint-groups-port-override.html Overriding listener ports>
    -- in the /Global Accelerator Developer Guide/.
    portOverrides :: Prelude.Maybe [PortOverride],
    -- | The number of consecutive health checks required to set the state of a
    -- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
    -- healthy. The default value is 3.
    thresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of traffic to send to an Amazon Web Services Region.
    -- Additional traffic is distributed to other endpoint groups for this
    -- listener.
    --
    -- Use this action to increase (dial up) or decrease (dial down) traffic to
    -- a specific Region. The percentage is applied to the traffic that would
    -- otherwise have been routed to the Region based on optimal routing.
    --
    -- The default value is 100.
    trafficDialPercentage :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointConfigurations', 'updateEndpointGroup_endpointConfigurations' - The list of endpoint objects. A resource must be valid and active when
-- you add it as an endpoint.
--
-- 'healthCheckIntervalSeconds', 'updateEndpointGroup_healthCheckIntervalSeconds' - The time—10 seconds or 30 seconds—between each health check for an
-- endpoint. The default value is 30.
--
-- 'healthCheckPath', 'updateEndpointGroup_healthCheckPath' - If the protocol is HTTP\/S, then this specifies the path that is the
-- destination for health check targets. The default value is slash (\/).
--
-- 'healthCheckPort', 'updateEndpointGroup_healthCheckPort' - The port that Global Accelerator uses to check the health of endpoints
-- that are part of this endpoint group. The default port is the listener
-- port that this endpoint group is associated with. If the listener port
-- is a list of ports, Global Accelerator uses the first port in the list.
--
-- 'healthCheckProtocol', 'updateEndpointGroup_healthCheckProtocol' - The protocol that Global Accelerator uses to check the health of
-- endpoints that are part of this endpoint group. The default value is
-- TCP.
--
-- 'portOverrides', 'updateEndpointGroup_portOverrides' - Override specific listener ports used to route traffic to endpoints that
-- are part of this endpoint group. For example, you can create a port
-- override in which the listener receives user traffic on ports 80 and
-- 443, but your accelerator routes that traffic to ports 1080 and 1443,
-- respectively, on the endpoints.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoint-groups-port-override.html Overriding listener ports>
-- in the /Global Accelerator Developer Guide/.
--
-- 'thresholdCount', 'updateEndpointGroup_thresholdCount' - The number of consecutive health checks required to set the state of a
-- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
-- healthy. The default value is 3.
--
-- 'trafficDialPercentage', 'updateEndpointGroup_trafficDialPercentage' - The percentage of traffic to send to an Amazon Web Services Region.
-- Additional traffic is distributed to other endpoint groups for this
-- listener.
--
-- Use this action to increase (dial up) or decrease (dial down) traffic to
-- a specific Region. The percentage is applied to the traffic that would
-- otherwise have been routed to the Region based on optimal routing.
--
-- The default value is 100.
--
-- 'endpointGroupArn', 'updateEndpointGroup_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
newUpdateEndpointGroup ::
  -- | 'endpointGroupArn'
  Prelude.Text ->
  UpdateEndpointGroup
newUpdateEndpointGroup pEndpointGroupArn_ =
  UpdateEndpointGroup'
    { endpointConfigurations =
        Prelude.Nothing,
      healthCheckIntervalSeconds = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing,
      healthCheckProtocol = Prelude.Nothing,
      portOverrides = Prelude.Nothing,
      thresholdCount = Prelude.Nothing,
      trafficDialPercentage = Prelude.Nothing,
      endpointGroupArn = pEndpointGroupArn_
    }

-- | The list of endpoint objects. A resource must be valid and active when
-- you add it as an endpoint.
updateEndpointGroup_endpointConfigurations :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe [EndpointConfiguration])
updateEndpointGroup_endpointConfigurations = Lens.lens (\UpdateEndpointGroup' {endpointConfigurations} -> endpointConfigurations) (\s@UpdateEndpointGroup' {} a -> s {endpointConfigurations = a} :: UpdateEndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | The time—10 seconds or 30 seconds—between each health check for an
-- endpoint. The default value is 30.
updateEndpointGroup_healthCheckIntervalSeconds :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe Prelude.Natural)
updateEndpointGroup_healthCheckIntervalSeconds = Lens.lens (\UpdateEndpointGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@UpdateEndpointGroup' {} a -> s {healthCheckIntervalSeconds = a} :: UpdateEndpointGroup)

-- | If the protocol is HTTP\/S, then this specifies the path that is the
-- destination for health check targets. The default value is slash (\/).
updateEndpointGroup_healthCheckPath :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe Prelude.Text)
updateEndpointGroup_healthCheckPath = Lens.lens (\UpdateEndpointGroup' {healthCheckPath} -> healthCheckPath) (\s@UpdateEndpointGroup' {} a -> s {healthCheckPath = a} :: UpdateEndpointGroup)

-- | The port that Global Accelerator uses to check the health of endpoints
-- that are part of this endpoint group. The default port is the listener
-- port that this endpoint group is associated with. If the listener port
-- is a list of ports, Global Accelerator uses the first port in the list.
updateEndpointGroup_healthCheckPort :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe Prelude.Natural)
updateEndpointGroup_healthCheckPort = Lens.lens (\UpdateEndpointGroup' {healthCheckPort} -> healthCheckPort) (\s@UpdateEndpointGroup' {} a -> s {healthCheckPort = a} :: UpdateEndpointGroup)

-- | The protocol that Global Accelerator uses to check the health of
-- endpoints that are part of this endpoint group. The default value is
-- TCP.
updateEndpointGroup_healthCheckProtocol :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe HealthCheckProtocol)
updateEndpointGroup_healthCheckProtocol = Lens.lens (\UpdateEndpointGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@UpdateEndpointGroup' {} a -> s {healthCheckProtocol = a} :: UpdateEndpointGroup)

-- | Override specific listener ports used to route traffic to endpoints that
-- are part of this endpoint group. For example, you can create a port
-- override in which the listener receives user traffic on ports 80 and
-- 443, but your accelerator routes that traffic to ports 1080 and 1443,
-- respectively, on the endpoints.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoint-groups-port-override.html Overriding listener ports>
-- in the /Global Accelerator Developer Guide/.
updateEndpointGroup_portOverrides :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe [PortOverride])
updateEndpointGroup_portOverrides = Lens.lens (\UpdateEndpointGroup' {portOverrides} -> portOverrides) (\s@UpdateEndpointGroup' {} a -> s {portOverrides = a} :: UpdateEndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | The number of consecutive health checks required to set the state of a
-- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
-- healthy. The default value is 3.
updateEndpointGroup_thresholdCount :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe Prelude.Natural)
updateEndpointGroup_thresholdCount = Lens.lens (\UpdateEndpointGroup' {thresholdCount} -> thresholdCount) (\s@UpdateEndpointGroup' {} a -> s {thresholdCount = a} :: UpdateEndpointGroup)

-- | The percentage of traffic to send to an Amazon Web Services Region.
-- Additional traffic is distributed to other endpoint groups for this
-- listener.
--
-- Use this action to increase (dial up) or decrease (dial down) traffic to
-- a specific Region. The percentage is applied to the traffic that would
-- otherwise have been routed to the Region based on optimal routing.
--
-- The default value is 100.
updateEndpointGroup_trafficDialPercentage :: Lens.Lens' UpdateEndpointGroup (Prelude.Maybe Prelude.Double)
updateEndpointGroup_trafficDialPercentage = Lens.lens (\UpdateEndpointGroup' {trafficDialPercentage} -> trafficDialPercentage) (\s@UpdateEndpointGroup' {} a -> s {trafficDialPercentage = a} :: UpdateEndpointGroup)

-- | The Amazon Resource Name (ARN) of the endpoint group.
updateEndpointGroup_endpointGroupArn :: Lens.Lens' UpdateEndpointGroup Prelude.Text
updateEndpointGroup_endpointGroupArn = Lens.lens (\UpdateEndpointGroup' {endpointGroupArn} -> endpointGroupArn) (\s@UpdateEndpointGroup' {} a -> s {endpointGroupArn = a} :: UpdateEndpointGroup)

instance Core.AWSRequest UpdateEndpointGroup where
  type
    AWSResponse UpdateEndpointGroup =
      UpdateEndpointGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointGroupResponse'
            Prelude.<$> (x Data..?> "EndpointGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEndpointGroup where
  hashWithSalt _salt UpdateEndpointGroup' {..} =
    _salt
      `Prelude.hashWithSalt` endpointConfigurations
      `Prelude.hashWithSalt` healthCheckIntervalSeconds
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` healthCheckPort
      `Prelude.hashWithSalt` healthCheckProtocol
      `Prelude.hashWithSalt` portOverrides
      `Prelude.hashWithSalt` thresholdCount
      `Prelude.hashWithSalt` trafficDialPercentage
      `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData UpdateEndpointGroup where
  rnf UpdateEndpointGroup' {..} =
    Prelude.rnf endpointConfigurations
      `Prelude.seq` Prelude.rnf healthCheckIntervalSeconds
      `Prelude.seq` Prelude.rnf healthCheckPath
      `Prelude.seq` Prelude.rnf healthCheckPort
      `Prelude.seq` Prelude.rnf healthCheckProtocol
      `Prelude.seq` Prelude.rnf portOverrides
      `Prelude.seq` Prelude.rnf thresholdCount
      `Prelude.seq` Prelude.rnf trafficDialPercentage
      `Prelude.seq` Prelude.rnf endpointGroupArn

instance Data.ToHeaders UpdateEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.UpdateEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEndpointGroup where
  toJSON UpdateEndpointGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndpointConfigurations" Data..=)
              Prelude.<$> endpointConfigurations,
            ("HealthCheckIntervalSeconds" Data..=)
              Prelude.<$> healthCheckIntervalSeconds,
            ("HealthCheckPath" Data..=)
              Prelude.<$> healthCheckPath,
            ("HealthCheckPort" Data..=)
              Prelude.<$> healthCheckPort,
            ("HealthCheckProtocol" Data..=)
              Prelude.<$> healthCheckProtocol,
            ("PortOverrides" Data..=) Prelude.<$> portOverrides,
            ("ThresholdCount" Data..=)
              Prelude.<$> thresholdCount,
            ("TrafficDialPercentage" Data..=)
              Prelude.<$> trafficDialPercentage,
            Prelude.Just
              ("EndpointGroupArn" Data..= endpointGroupArn)
          ]
      )

instance Data.ToPath UpdateEndpointGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEndpointGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointGroupResponse' smart constructor.
data UpdateEndpointGroupResponse = UpdateEndpointGroupResponse'
  { -- | The information about the endpoint group that was updated.
    endpointGroup :: Prelude.Maybe EndpointGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroup', 'updateEndpointGroupResponse_endpointGroup' - The information about the endpoint group that was updated.
--
-- 'httpStatus', 'updateEndpointGroupResponse_httpStatus' - The response's http status code.
newUpdateEndpointGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEndpointGroupResponse
newUpdateEndpointGroupResponse pHttpStatus_ =
  UpdateEndpointGroupResponse'
    { endpointGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information about the endpoint group that was updated.
updateEndpointGroupResponse_endpointGroup :: Lens.Lens' UpdateEndpointGroupResponse (Prelude.Maybe EndpointGroup)
updateEndpointGroupResponse_endpointGroup = Lens.lens (\UpdateEndpointGroupResponse' {endpointGroup} -> endpointGroup) (\s@UpdateEndpointGroupResponse' {} a -> s {endpointGroup = a} :: UpdateEndpointGroupResponse)

-- | The response's http status code.
updateEndpointGroupResponse_httpStatus :: Lens.Lens' UpdateEndpointGroupResponse Prelude.Int
updateEndpointGroupResponse_httpStatus = Lens.lens (\UpdateEndpointGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointGroupResponse' {} a -> s {httpStatus = a} :: UpdateEndpointGroupResponse)

instance Prelude.NFData UpdateEndpointGroupResponse where
  rnf UpdateEndpointGroupResponse' {..} =
    Prelude.rnf endpointGroup
      `Prelude.seq` Prelude.rnf httpStatus

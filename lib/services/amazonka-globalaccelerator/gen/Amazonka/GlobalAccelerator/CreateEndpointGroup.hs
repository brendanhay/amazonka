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
-- Module      : Amazonka.GlobalAccelerator.CreateEndpointGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an endpoint group for the specified listener. An endpoint group
-- is a collection of endpoints in one AWS Region. A resource must be valid
-- and active when you add it as an endpoint.
module Amazonka.GlobalAccelerator.CreateEndpointGroup
  ( -- * Creating a Request
    CreateEndpointGroup (..),
    newCreateEndpointGroup,

    -- * Request Lenses
    createEndpointGroup_thresholdCount,
    createEndpointGroup_healthCheckPath,
    createEndpointGroup_healthCheckIntervalSeconds,
    createEndpointGroup_healthCheckProtocol,
    createEndpointGroup_trafficDialPercentage,
    createEndpointGroup_endpointConfigurations,
    createEndpointGroup_healthCheckPort,
    createEndpointGroup_portOverrides,
    createEndpointGroup_listenerArn,
    createEndpointGroup_endpointGroupRegion,
    createEndpointGroup_idempotencyToken,

    -- * Destructuring the Response
    CreateEndpointGroupResponse (..),
    newCreateEndpointGroupResponse,

    -- * Response Lenses
    createEndpointGroupResponse_endpointGroup,
    createEndpointGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEndpointGroup' smart constructor.
data CreateEndpointGroup = CreateEndpointGroup'
  { -- | The number of consecutive health checks required to set the state of a
    -- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
    -- healthy. The default value is 3.
    thresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | If the protocol is HTTP\/S, then this specifies the path that is the
    -- destination for health check targets. The default value is slash (\/).
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The time—10 seconds or 30 seconds—between each health check for an
    -- endpoint. The default value is 30.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The protocol that AWS Global Accelerator uses to check the health of
    -- endpoints that are part of this endpoint group. The default value is
    -- TCP.
    healthCheckProtocol :: Prelude.Maybe HealthCheckProtocol,
    -- | The percentage of traffic to send to an AWS Region. Additional traffic
    -- is distributed to other endpoint groups for this listener.
    --
    -- Use this action to increase (dial up) or decrease (dial down) traffic to
    -- a specific Region. The percentage is applied to the traffic that would
    -- otherwise have been routed to the Region based on optimal routing.
    --
    -- The default value is 100.
    trafficDialPercentage :: Prelude.Maybe Prelude.Double,
    -- | The list of endpoint objects.
    endpointConfigurations :: Prelude.Maybe [EndpointConfiguration],
    -- | The port that AWS Global Accelerator uses to check the health of
    -- endpoints that are part of this endpoint group. The default port is the
    -- listener port that this endpoint group is associated with. If listener
    -- port is a list of ports, Global Accelerator uses the first port in the
    -- list.
    healthCheckPort :: Prelude.Maybe Prelude.Natural,
    -- | Override specific listener ports used to route traffic to endpoints that
    -- are part of this endpoint group. For example, you can create a port
    -- override in which the listener receives user traffic on ports 80 and
    -- 443, but your accelerator routes that traffic to ports 1080 and 1443,
    -- respectively, on the endpoints.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoint-groups-port-override.html Port overrides>
    -- in the /AWS Global Accelerator Developer Guide/.
    portOverrides :: Prelude.Maybe [PortOverride],
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Text,
    -- | The AWS Region where the endpoint group is located. A listener can have
    -- only one endpoint group in a specific Region.
    endpointGroupRegion :: Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency—that is, the uniqueness—of the request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thresholdCount', 'createEndpointGroup_thresholdCount' - The number of consecutive health checks required to set the state of a
-- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
-- healthy. The default value is 3.
--
-- 'healthCheckPath', 'createEndpointGroup_healthCheckPath' - If the protocol is HTTP\/S, then this specifies the path that is the
-- destination for health check targets. The default value is slash (\/).
--
-- 'healthCheckIntervalSeconds', 'createEndpointGroup_healthCheckIntervalSeconds' - The time—10 seconds or 30 seconds—between each health check for an
-- endpoint. The default value is 30.
--
-- 'healthCheckProtocol', 'createEndpointGroup_healthCheckProtocol' - The protocol that AWS Global Accelerator uses to check the health of
-- endpoints that are part of this endpoint group. The default value is
-- TCP.
--
-- 'trafficDialPercentage', 'createEndpointGroup_trafficDialPercentage' - The percentage of traffic to send to an AWS Region. Additional traffic
-- is distributed to other endpoint groups for this listener.
--
-- Use this action to increase (dial up) or decrease (dial down) traffic to
-- a specific Region. The percentage is applied to the traffic that would
-- otherwise have been routed to the Region based on optimal routing.
--
-- The default value is 100.
--
-- 'endpointConfigurations', 'createEndpointGroup_endpointConfigurations' - The list of endpoint objects.
--
-- 'healthCheckPort', 'createEndpointGroup_healthCheckPort' - The port that AWS Global Accelerator uses to check the health of
-- endpoints that are part of this endpoint group. The default port is the
-- listener port that this endpoint group is associated with. If listener
-- port is a list of ports, Global Accelerator uses the first port in the
-- list.
--
-- 'portOverrides', 'createEndpointGroup_portOverrides' - Override specific listener ports used to route traffic to endpoints that
-- are part of this endpoint group. For example, you can create a port
-- override in which the listener receives user traffic on ports 80 and
-- 443, but your accelerator routes that traffic to ports 1080 and 1443,
-- respectively, on the endpoints.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoint-groups-port-override.html Port overrides>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- 'listenerArn', 'createEndpointGroup_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'endpointGroupRegion', 'createEndpointGroup_endpointGroupRegion' - The AWS Region where the endpoint group is located. A listener can have
-- only one endpoint group in a specific Region.
--
-- 'idempotencyToken', 'createEndpointGroup_idempotencyToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
newCreateEndpointGroup ::
  -- | 'listenerArn'
  Prelude.Text ->
  -- | 'endpointGroupRegion'
  Prelude.Text ->
  -- | 'idempotencyToken'
  Prelude.Text ->
  CreateEndpointGroup
newCreateEndpointGroup
  pListenerArn_
  pEndpointGroupRegion_
  pIdempotencyToken_ =
    CreateEndpointGroup'
      { thresholdCount =
          Prelude.Nothing,
        healthCheckPath = Prelude.Nothing,
        healthCheckIntervalSeconds = Prelude.Nothing,
        healthCheckProtocol = Prelude.Nothing,
        trafficDialPercentage = Prelude.Nothing,
        endpointConfigurations = Prelude.Nothing,
        healthCheckPort = Prelude.Nothing,
        portOverrides = Prelude.Nothing,
        listenerArn = pListenerArn_,
        endpointGroupRegion = pEndpointGroupRegion_,
        idempotencyToken = pIdempotencyToken_
      }

-- | The number of consecutive health checks required to set the state of a
-- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
-- healthy. The default value is 3.
createEndpointGroup_thresholdCount :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe Prelude.Natural)
createEndpointGroup_thresholdCount = Lens.lens (\CreateEndpointGroup' {thresholdCount} -> thresholdCount) (\s@CreateEndpointGroup' {} a -> s {thresholdCount = a} :: CreateEndpointGroup)

-- | If the protocol is HTTP\/S, then this specifies the path that is the
-- destination for health check targets. The default value is slash (\/).
createEndpointGroup_healthCheckPath :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe Prelude.Text)
createEndpointGroup_healthCheckPath = Lens.lens (\CreateEndpointGroup' {healthCheckPath} -> healthCheckPath) (\s@CreateEndpointGroup' {} a -> s {healthCheckPath = a} :: CreateEndpointGroup)

-- | The time—10 seconds or 30 seconds—between each health check for an
-- endpoint. The default value is 30.
createEndpointGroup_healthCheckIntervalSeconds :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe Prelude.Natural)
createEndpointGroup_healthCheckIntervalSeconds = Lens.lens (\CreateEndpointGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@CreateEndpointGroup' {} a -> s {healthCheckIntervalSeconds = a} :: CreateEndpointGroup)

-- | The protocol that AWS Global Accelerator uses to check the health of
-- endpoints that are part of this endpoint group. The default value is
-- TCP.
createEndpointGroup_healthCheckProtocol :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe HealthCheckProtocol)
createEndpointGroup_healthCheckProtocol = Lens.lens (\CreateEndpointGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@CreateEndpointGroup' {} a -> s {healthCheckProtocol = a} :: CreateEndpointGroup)

-- | The percentage of traffic to send to an AWS Region. Additional traffic
-- is distributed to other endpoint groups for this listener.
--
-- Use this action to increase (dial up) or decrease (dial down) traffic to
-- a specific Region. The percentage is applied to the traffic that would
-- otherwise have been routed to the Region based on optimal routing.
--
-- The default value is 100.
createEndpointGroup_trafficDialPercentage :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe Prelude.Double)
createEndpointGroup_trafficDialPercentage = Lens.lens (\CreateEndpointGroup' {trafficDialPercentage} -> trafficDialPercentage) (\s@CreateEndpointGroup' {} a -> s {trafficDialPercentage = a} :: CreateEndpointGroup)

-- | The list of endpoint objects.
createEndpointGroup_endpointConfigurations :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe [EndpointConfiguration])
createEndpointGroup_endpointConfigurations = Lens.lens (\CreateEndpointGroup' {endpointConfigurations} -> endpointConfigurations) (\s@CreateEndpointGroup' {} a -> s {endpointConfigurations = a} :: CreateEndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | The port that AWS Global Accelerator uses to check the health of
-- endpoints that are part of this endpoint group. The default port is the
-- listener port that this endpoint group is associated with. If listener
-- port is a list of ports, Global Accelerator uses the first port in the
-- list.
createEndpointGroup_healthCheckPort :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe Prelude.Natural)
createEndpointGroup_healthCheckPort = Lens.lens (\CreateEndpointGroup' {healthCheckPort} -> healthCheckPort) (\s@CreateEndpointGroup' {} a -> s {healthCheckPort = a} :: CreateEndpointGroup)

-- | Override specific listener ports used to route traffic to endpoints that
-- are part of this endpoint group. For example, you can create a port
-- override in which the listener receives user traffic on ports 80 and
-- 443, but your accelerator routes that traffic to ports 1080 and 1443,
-- respectively, on the endpoints.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoint-groups-port-override.html Port overrides>
-- in the /AWS Global Accelerator Developer Guide/.
createEndpointGroup_portOverrides :: Lens.Lens' CreateEndpointGroup (Prelude.Maybe [PortOverride])
createEndpointGroup_portOverrides = Lens.lens (\CreateEndpointGroup' {portOverrides} -> portOverrides) (\s@CreateEndpointGroup' {} a -> s {portOverrides = a} :: CreateEndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the listener.
createEndpointGroup_listenerArn :: Lens.Lens' CreateEndpointGroup Prelude.Text
createEndpointGroup_listenerArn = Lens.lens (\CreateEndpointGroup' {listenerArn} -> listenerArn) (\s@CreateEndpointGroup' {} a -> s {listenerArn = a} :: CreateEndpointGroup)

-- | The AWS Region where the endpoint group is located. A listener can have
-- only one endpoint group in a specific Region.
createEndpointGroup_endpointGroupRegion :: Lens.Lens' CreateEndpointGroup Prelude.Text
createEndpointGroup_endpointGroupRegion = Lens.lens (\CreateEndpointGroup' {endpointGroupRegion} -> endpointGroupRegion) (\s@CreateEndpointGroup' {} a -> s {endpointGroupRegion = a} :: CreateEndpointGroup)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency—that is, the uniqueness—of the request.
createEndpointGroup_idempotencyToken :: Lens.Lens' CreateEndpointGroup Prelude.Text
createEndpointGroup_idempotencyToken = Lens.lens (\CreateEndpointGroup' {idempotencyToken} -> idempotencyToken) (\s@CreateEndpointGroup' {} a -> s {idempotencyToken = a} :: CreateEndpointGroup)

instance Core.AWSRequest CreateEndpointGroup where
  type
    AWSResponse CreateEndpointGroup =
      CreateEndpointGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointGroupResponse'
            Prelude.<$> (x Core..?> "EndpointGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpointGroup where
  hashWithSalt _salt CreateEndpointGroup' {..} =
    _salt `Prelude.hashWithSalt` thresholdCount
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` healthCheckIntervalSeconds
      `Prelude.hashWithSalt` healthCheckProtocol
      `Prelude.hashWithSalt` trafficDialPercentage
      `Prelude.hashWithSalt` endpointConfigurations
      `Prelude.hashWithSalt` healthCheckPort
      `Prelude.hashWithSalt` portOverrides
      `Prelude.hashWithSalt` listenerArn
      `Prelude.hashWithSalt` endpointGroupRegion
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData CreateEndpointGroup where
  rnf CreateEndpointGroup' {..} =
    Prelude.rnf thresholdCount
      `Prelude.seq` Prelude.rnf healthCheckPath
      `Prelude.seq` Prelude.rnf healthCheckIntervalSeconds
      `Prelude.seq` Prelude.rnf healthCheckProtocol
      `Prelude.seq` Prelude.rnf trafficDialPercentage
      `Prelude.seq` Prelude.rnf endpointConfigurations
      `Prelude.seq` Prelude.rnf healthCheckPort
      `Prelude.seq` Prelude.rnf portOverrides
      `Prelude.seq` Prelude.rnf listenerArn
      `Prelude.seq` Prelude.rnf endpointGroupRegion
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Core.ToHeaders CreateEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GlobalAccelerator_V20180706.CreateEndpointGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEndpointGroup where
  toJSON CreateEndpointGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ThresholdCount" Core..=)
              Prelude.<$> thresholdCount,
            ("HealthCheckPath" Core..=)
              Prelude.<$> healthCheckPath,
            ("HealthCheckIntervalSeconds" Core..=)
              Prelude.<$> healthCheckIntervalSeconds,
            ("HealthCheckProtocol" Core..=)
              Prelude.<$> healthCheckProtocol,
            ("TrafficDialPercentage" Core..=)
              Prelude.<$> trafficDialPercentage,
            ("EndpointConfigurations" Core..=)
              Prelude.<$> endpointConfigurations,
            ("HealthCheckPort" Core..=)
              Prelude.<$> healthCheckPort,
            ("PortOverrides" Core..=) Prelude.<$> portOverrides,
            Prelude.Just ("ListenerArn" Core..= listenerArn),
            Prelude.Just
              ("EndpointGroupRegion" Core..= endpointGroupRegion),
            Prelude.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath CreateEndpointGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEndpointGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointGroupResponse' smart constructor.
data CreateEndpointGroupResponse = CreateEndpointGroupResponse'
  { -- | The information about the endpoint group that was created.
    endpointGroup :: Prelude.Maybe EndpointGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroup', 'createEndpointGroupResponse_endpointGroup' - The information about the endpoint group that was created.
--
-- 'httpStatus', 'createEndpointGroupResponse_httpStatus' - The response's http status code.
newCreateEndpointGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEndpointGroupResponse
newCreateEndpointGroupResponse pHttpStatus_ =
  CreateEndpointGroupResponse'
    { endpointGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information about the endpoint group that was created.
createEndpointGroupResponse_endpointGroup :: Lens.Lens' CreateEndpointGroupResponse (Prelude.Maybe EndpointGroup)
createEndpointGroupResponse_endpointGroup = Lens.lens (\CreateEndpointGroupResponse' {endpointGroup} -> endpointGroup) (\s@CreateEndpointGroupResponse' {} a -> s {endpointGroup = a} :: CreateEndpointGroupResponse)

-- | The response's http status code.
createEndpointGroupResponse_httpStatus :: Lens.Lens' CreateEndpointGroupResponse Prelude.Int
createEndpointGroupResponse_httpStatus = Lens.lens (\CreateEndpointGroupResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointGroupResponse' {} a -> s {httpStatus = a} :: CreateEndpointGroupResponse)

instance Prelude.NFData CreateEndpointGroupResponse where
  rnf CreateEndpointGroupResponse' {..} =
    Prelude.rnf endpointGroup
      `Prelude.seq` Prelude.rnf httpStatus

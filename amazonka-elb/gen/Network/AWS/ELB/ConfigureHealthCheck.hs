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
-- Module      : Network.AWS.ELB.ConfigureHealthCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the health check settings to use when evaluating the health
-- state of your EC2 instances.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-healthchecks.html Configure Health Checks for Your Load Balancer>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.ConfigureHealthCheck
  ( -- * Creating a Request
    ConfigureHealthCheck (..),
    newConfigureHealthCheck,

    -- * Request Lenses
    configureHealthCheck_loadBalancerName,
    configureHealthCheck_healthCheck,

    -- * Destructuring the Response
    ConfigureHealthCheckResponse (..),
    newConfigureHealthCheckResponse,

    -- * Response Lenses
    configureHealthCheckResponse_healthCheck,
    configureHealthCheckResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ConfigureHealthCheck.
--
-- /See:/ 'newConfigureHealthCheck' smart constructor.
data ConfigureHealthCheck = ConfigureHealthCheck'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The configuration information.
    healthCheck :: HealthCheck
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigureHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'configureHealthCheck_loadBalancerName' - The name of the load balancer.
--
-- 'healthCheck', 'configureHealthCheck_healthCheck' - The configuration information.
newConfigureHealthCheck ::
  -- | 'loadBalancerName'
  Core.Text ->
  -- | 'healthCheck'
  HealthCheck ->
  ConfigureHealthCheck
newConfigureHealthCheck
  pLoadBalancerName_
  pHealthCheck_ =
    ConfigureHealthCheck'
      { loadBalancerName =
          pLoadBalancerName_,
        healthCheck = pHealthCheck_
      }

-- | The name of the load balancer.
configureHealthCheck_loadBalancerName :: Lens.Lens' ConfigureHealthCheck Core.Text
configureHealthCheck_loadBalancerName = Lens.lens (\ConfigureHealthCheck' {loadBalancerName} -> loadBalancerName) (\s@ConfigureHealthCheck' {} a -> s {loadBalancerName = a} :: ConfigureHealthCheck)

-- | The configuration information.
configureHealthCheck_healthCheck :: Lens.Lens' ConfigureHealthCheck HealthCheck
configureHealthCheck_healthCheck = Lens.lens (\ConfigureHealthCheck' {healthCheck} -> healthCheck) (\s@ConfigureHealthCheck' {} a -> s {healthCheck = a} :: ConfigureHealthCheck)

instance Core.AWSRequest ConfigureHealthCheck where
  type
    AWSResponse ConfigureHealthCheck =
      ConfigureHealthCheckResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ConfigureHealthCheckResult"
      ( \s h x ->
          ConfigureHealthCheckResponse'
            Core.<$> (x Core..@? "HealthCheck")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ConfigureHealthCheck

instance Core.NFData ConfigureHealthCheck

instance Core.ToHeaders ConfigureHealthCheck where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ConfigureHealthCheck where
  toPath = Core.const "/"

instance Core.ToQuery ConfigureHealthCheck where
  toQuery ConfigureHealthCheck' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ConfigureHealthCheck" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "HealthCheck" Core.=: healthCheck
      ]

-- | Contains the output of ConfigureHealthCheck.
--
-- /See:/ 'newConfigureHealthCheckResponse' smart constructor.
data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse'
  { -- | The updated health check.
    healthCheck :: Core.Maybe HealthCheck,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigureHealthCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheck', 'configureHealthCheckResponse_healthCheck' - The updated health check.
--
-- 'httpStatus', 'configureHealthCheckResponse_httpStatus' - The response's http status code.
newConfigureHealthCheckResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ConfigureHealthCheckResponse
newConfigureHealthCheckResponse pHttpStatus_ =
  ConfigureHealthCheckResponse'
    { healthCheck =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated health check.
configureHealthCheckResponse_healthCheck :: Lens.Lens' ConfigureHealthCheckResponse (Core.Maybe HealthCheck)
configureHealthCheckResponse_healthCheck = Lens.lens (\ConfigureHealthCheckResponse' {healthCheck} -> healthCheck) (\s@ConfigureHealthCheckResponse' {} a -> s {healthCheck = a} :: ConfigureHealthCheckResponse)

-- | The response's http status code.
configureHealthCheckResponse_httpStatus :: Lens.Lens' ConfigureHealthCheckResponse Core.Int
configureHealthCheckResponse_httpStatus = Lens.lens (\ConfigureHealthCheckResponse' {httpStatus} -> httpStatus) (\s@ConfigureHealthCheckResponse' {} a -> s {httpStatus = a} :: ConfigureHealthCheckResponse)

instance Core.NFData ConfigureHealthCheckResponse

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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the overall health of the specified
-- environment. The __DescribeEnvironmentHealth__ operation is only
-- available with AWS Elastic Beanstalk Enhanced Health.
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentHealth
  ( -- * Creating a Request
    DescribeEnvironmentHealth (..),
    newDescribeEnvironmentHealth,

    -- * Request Lenses
    describeEnvironmentHealth_environmentId,
    describeEnvironmentHealth_environmentName,
    describeEnvironmentHealth_attributeNames,

    -- * Destructuring the Response
    DescribeEnvironmentHealthResponse (..),
    newDescribeEnvironmentHealthResponse,

    -- * Response Lenses
    describeEnvironmentHealthResponse_status,
    describeEnvironmentHealthResponse_refreshedAt,
    describeEnvironmentHealthResponse_color,
    describeEnvironmentHealthResponse_causes,
    describeEnvironmentHealthResponse_environmentName,
    describeEnvironmentHealthResponse_instancesHealth,
    describeEnvironmentHealthResponse_healthStatus,
    describeEnvironmentHealthResponse_applicationMetrics,
    describeEnvironmentHealthResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | See the example below to learn how to create a request body.
--
-- /See:/ 'newDescribeEnvironmentHealth' smart constructor.
data DescribeEnvironmentHealth = DescribeEnvironmentHealth'
  { -- | Specify the environment by ID.
    --
    -- You must specify either this or an EnvironmentName, or both.
    environmentId :: Core.Maybe Core.Text,
    -- | Specify the environment by name.
    --
    -- You must specify either this or an EnvironmentName, or both.
    environmentName :: Core.Maybe Core.Text,
    -- | Specify the response elements to return. To retrieve all attributes, set
    -- to @All@. If no attribute names are specified, returns the name of the
    -- environment.
    attributeNames :: Core.Maybe [EnvironmentHealthAttribute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEnvironmentHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'describeEnvironmentHealth_environmentId' - Specify the environment by ID.
--
-- You must specify either this or an EnvironmentName, or both.
--
-- 'environmentName', 'describeEnvironmentHealth_environmentName' - Specify the environment by name.
--
-- You must specify either this or an EnvironmentName, or both.
--
-- 'attributeNames', 'describeEnvironmentHealth_attributeNames' - Specify the response elements to return. To retrieve all attributes, set
-- to @All@. If no attribute names are specified, returns the name of the
-- environment.
newDescribeEnvironmentHealth ::
  DescribeEnvironmentHealth
newDescribeEnvironmentHealth =
  DescribeEnvironmentHealth'
    { environmentId =
        Core.Nothing,
      environmentName = Core.Nothing,
      attributeNames = Core.Nothing
    }

-- | Specify the environment by ID.
--
-- You must specify either this or an EnvironmentName, or both.
describeEnvironmentHealth_environmentId :: Lens.Lens' DescribeEnvironmentHealth (Core.Maybe Core.Text)
describeEnvironmentHealth_environmentId = Lens.lens (\DescribeEnvironmentHealth' {environmentId} -> environmentId) (\s@DescribeEnvironmentHealth' {} a -> s {environmentId = a} :: DescribeEnvironmentHealth)

-- | Specify the environment by name.
--
-- You must specify either this or an EnvironmentName, or both.
describeEnvironmentHealth_environmentName :: Lens.Lens' DescribeEnvironmentHealth (Core.Maybe Core.Text)
describeEnvironmentHealth_environmentName = Lens.lens (\DescribeEnvironmentHealth' {environmentName} -> environmentName) (\s@DescribeEnvironmentHealth' {} a -> s {environmentName = a} :: DescribeEnvironmentHealth)

-- | Specify the response elements to return. To retrieve all attributes, set
-- to @All@. If no attribute names are specified, returns the name of the
-- environment.
describeEnvironmentHealth_attributeNames :: Lens.Lens' DescribeEnvironmentHealth (Core.Maybe [EnvironmentHealthAttribute])
describeEnvironmentHealth_attributeNames = Lens.lens (\DescribeEnvironmentHealth' {attributeNames} -> attributeNames) (\s@DescribeEnvironmentHealth' {} a -> s {attributeNames = a} :: DescribeEnvironmentHealth) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeEnvironmentHealth where
  type
    AWSResponse DescribeEnvironmentHealth =
      DescribeEnvironmentHealthResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentHealthResult"
      ( \s h x ->
          DescribeEnvironmentHealthResponse'
            Core.<$> (x Core..@? "Status")
            Core.<*> (x Core..@? "RefreshedAt")
            Core.<*> (x Core..@? "Color")
            Core.<*> ( x Core..@? "Causes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "EnvironmentName")
            Core.<*> (x Core..@? "InstancesHealth")
            Core.<*> (x Core..@? "HealthStatus")
            Core.<*> (x Core..@? "ApplicationMetrics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEnvironmentHealth

instance Core.NFData DescribeEnvironmentHealth

instance Core.ToHeaders DescribeEnvironmentHealth where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeEnvironmentHealth where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEnvironmentHealth where
  toQuery DescribeEnvironmentHealth' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeEnvironmentHealth" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "EnvironmentId" Core.=: environmentId,
        "EnvironmentName" Core.=: environmentName,
        "AttributeNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> attributeNames)
      ]

-- | Health details for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'newDescribeEnvironmentHealthResponse' smart constructor.
data DescribeEnvironmentHealthResponse = DescribeEnvironmentHealthResponse'
  { -- | The environment\'s operational status. @Ready@, @Launching@, @Updating@,
    -- @Terminating@, or @Terminated@.
    status :: Core.Maybe EnvironmentHealth,
    -- | The date and time that the health information was retrieved.
    refreshedAt :: Core.Maybe Core.ISO8601,
    -- | The
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color>
    -- of the environment.
    color :: Core.Maybe Core.Text,
    -- | Descriptions of the data that contributed to the environment\'s current
    -- health status.
    causes :: Core.Maybe [Core.Text],
    -- | The environment\'s name.
    environmentName :: Core.Maybe Core.Text,
    -- | Summary health information for the instances in the environment.
    instancesHealth :: Core.Maybe InstanceHealthSummary,
    -- | The
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status>
    -- of the environment. For example, @Ok@.
    healthStatus :: Core.Maybe Core.Text,
    -- | Application request metrics for the environment.
    applicationMetrics :: Core.Maybe ApplicationMetrics,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEnvironmentHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeEnvironmentHealthResponse_status' - The environment\'s operational status. @Ready@, @Launching@, @Updating@,
-- @Terminating@, or @Terminated@.
--
-- 'refreshedAt', 'describeEnvironmentHealthResponse_refreshedAt' - The date and time that the health information was retrieved.
--
-- 'color', 'describeEnvironmentHealthResponse_color' - The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color>
-- of the environment.
--
-- 'causes', 'describeEnvironmentHealthResponse_causes' - Descriptions of the data that contributed to the environment\'s current
-- health status.
--
-- 'environmentName', 'describeEnvironmentHealthResponse_environmentName' - The environment\'s name.
--
-- 'instancesHealth', 'describeEnvironmentHealthResponse_instancesHealth' - Summary health information for the instances in the environment.
--
-- 'healthStatus', 'describeEnvironmentHealthResponse_healthStatus' - The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status>
-- of the environment. For example, @Ok@.
--
-- 'applicationMetrics', 'describeEnvironmentHealthResponse_applicationMetrics' - Application request metrics for the environment.
--
-- 'httpStatus', 'describeEnvironmentHealthResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentHealthResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEnvironmentHealthResponse
newDescribeEnvironmentHealthResponse pHttpStatus_ =
  DescribeEnvironmentHealthResponse'
    { status =
        Core.Nothing,
      refreshedAt = Core.Nothing,
      color = Core.Nothing,
      causes = Core.Nothing,
      environmentName = Core.Nothing,
      instancesHealth = Core.Nothing,
      healthStatus = Core.Nothing,
      applicationMetrics = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The environment\'s operational status. @Ready@, @Launching@, @Updating@,
-- @Terminating@, or @Terminated@.
describeEnvironmentHealthResponse_status :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe EnvironmentHealth)
describeEnvironmentHealthResponse_status = Lens.lens (\DescribeEnvironmentHealthResponse' {status} -> status) (\s@DescribeEnvironmentHealthResponse' {} a -> s {status = a} :: DescribeEnvironmentHealthResponse)

-- | The date and time that the health information was retrieved.
describeEnvironmentHealthResponse_refreshedAt :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Core.UTCTime)
describeEnvironmentHealthResponse_refreshedAt = Lens.lens (\DescribeEnvironmentHealthResponse' {refreshedAt} -> refreshedAt) (\s@DescribeEnvironmentHealthResponse' {} a -> s {refreshedAt = a} :: DescribeEnvironmentHealthResponse) Core.. Lens.mapping Core._Time

-- | The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color>
-- of the environment.
describeEnvironmentHealthResponse_color :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Core.Text)
describeEnvironmentHealthResponse_color = Lens.lens (\DescribeEnvironmentHealthResponse' {color} -> color) (\s@DescribeEnvironmentHealthResponse' {} a -> s {color = a} :: DescribeEnvironmentHealthResponse)

-- | Descriptions of the data that contributed to the environment\'s current
-- health status.
describeEnvironmentHealthResponse_causes :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe [Core.Text])
describeEnvironmentHealthResponse_causes = Lens.lens (\DescribeEnvironmentHealthResponse' {causes} -> causes) (\s@DescribeEnvironmentHealthResponse' {} a -> s {causes = a} :: DescribeEnvironmentHealthResponse) Core.. Lens.mapping Lens._Coerce

-- | The environment\'s name.
describeEnvironmentHealthResponse_environmentName :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Core.Text)
describeEnvironmentHealthResponse_environmentName = Lens.lens (\DescribeEnvironmentHealthResponse' {environmentName} -> environmentName) (\s@DescribeEnvironmentHealthResponse' {} a -> s {environmentName = a} :: DescribeEnvironmentHealthResponse)

-- | Summary health information for the instances in the environment.
describeEnvironmentHealthResponse_instancesHealth :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe InstanceHealthSummary)
describeEnvironmentHealthResponse_instancesHealth = Lens.lens (\DescribeEnvironmentHealthResponse' {instancesHealth} -> instancesHealth) (\s@DescribeEnvironmentHealthResponse' {} a -> s {instancesHealth = a} :: DescribeEnvironmentHealthResponse)

-- | The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status>
-- of the environment. For example, @Ok@.
describeEnvironmentHealthResponse_healthStatus :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe Core.Text)
describeEnvironmentHealthResponse_healthStatus = Lens.lens (\DescribeEnvironmentHealthResponse' {healthStatus} -> healthStatus) (\s@DescribeEnvironmentHealthResponse' {} a -> s {healthStatus = a} :: DescribeEnvironmentHealthResponse)

-- | Application request metrics for the environment.
describeEnvironmentHealthResponse_applicationMetrics :: Lens.Lens' DescribeEnvironmentHealthResponse (Core.Maybe ApplicationMetrics)
describeEnvironmentHealthResponse_applicationMetrics = Lens.lens (\DescribeEnvironmentHealthResponse' {applicationMetrics} -> applicationMetrics) (\s@DescribeEnvironmentHealthResponse' {} a -> s {applicationMetrics = a} :: DescribeEnvironmentHealthResponse)

-- | The response's http status code.
describeEnvironmentHealthResponse_httpStatus :: Lens.Lens' DescribeEnvironmentHealthResponse Core.Int
describeEnvironmentHealthResponse_httpStatus = Lens.lens (\DescribeEnvironmentHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentHealthResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentHealthResponse)

instance
  Core.NFData
    DescribeEnvironmentHealthResponse

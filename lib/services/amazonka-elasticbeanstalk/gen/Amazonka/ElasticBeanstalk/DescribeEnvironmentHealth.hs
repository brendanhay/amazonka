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
-- Module      : Amazonka.ElasticBeanstalk.DescribeEnvironmentHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the overall health of the specified
-- environment. The __DescribeEnvironmentHealth__ operation is only
-- available with AWS Elastic Beanstalk Enhanced Health.
module Amazonka.ElasticBeanstalk.DescribeEnvironmentHealth
  ( -- * Creating a Request
    DescribeEnvironmentHealth (..),
    newDescribeEnvironmentHealth,

    -- * Request Lenses
    describeEnvironmentHealth_attributeNames,
    describeEnvironmentHealth_environmentId,
    describeEnvironmentHealth_environmentName,

    -- * Destructuring the Response
    DescribeEnvironmentHealthResponse (..),
    newDescribeEnvironmentHealthResponse,

    -- * Response Lenses
    describeEnvironmentHealthResponse_applicationMetrics,
    describeEnvironmentHealthResponse_causes,
    describeEnvironmentHealthResponse_color,
    describeEnvironmentHealthResponse_environmentName,
    describeEnvironmentHealthResponse_healthStatus,
    describeEnvironmentHealthResponse_instancesHealth,
    describeEnvironmentHealthResponse_refreshedAt,
    describeEnvironmentHealthResponse_status,
    describeEnvironmentHealthResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | See the example below to learn how to create a request body.
--
-- /See:/ 'newDescribeEnvironmentHealth' smart constructor.
data DescribeEnvironmentHealth = DescribeEnvironmentHealth'
  { -- | Specify the response elements to return. To retrieve all attributes, set
    -- to @All@. If no attribute names are specified, returns the name of the
    -- environment.
    attributeNames :: Prelude.Maybe [EnvironmentHealthAttribute],
    -- | Specify the environment by ID.
    --
    -- You must specify either this or an EnvironmentName, or both.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Specify the environment by name.
    --
    -- You must specify either this or an EnvironmentName, or both.
    environmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'describeEnvironmentHealth_attributeNames' - Specify the response elements to return. To retrieve all attributes, set
-- to @All@. If no attribute names are specified, returns the name of the
-- environment.
--
-- 'environmentId', 'describeEnvironmentHealth_environmentId' - Specify the environment by ID.
--
-- You must specify either this or an EnvironmentName, or both.
--
-- 'environmentName', 'describeEnvironmentHealth_environmentName' - Specify the environment by name.
--
-- You must specify either this or an EnvironmentName, or both.
newDescribeEnvironmentHealth ::
  DescribeEnvironmentHealth
newDescribeEnvironmentHealth =
  DescribeEnvironmentHealth'
    { attributeNames =
        Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentName = Prelude.Nothing
    }

-- | Specify the response elements to return. To retrieve all attributes, set
-- to @All@. If no attribute names are specified, returns the name of the
-- environment.
describeEnvironmentHealth_attributeNames :: Lens.Lens' DescribeEnvironmentHealth (Prelude.Maybe [EnvironmentHealthAttribute])
describeEnvironmentHealth_attributeNames = Lens.lens (\DescribeEnvironmentHealth' {attributeNames} -> attributeNames) (\s@DescribeEnvironmentHealth' {} a -> s {attributeNames = a} :: DescribeEnvironmentHealth) Prelude.. Lens.mapping Lens.coerced

-- | Specify the environment by ID.
--
-- You must specify either this or an EnvironmentName, or both.
describeEnvironmentHealth_environmentId :: Lens.Lens' DescribeEnvironmentHealth (Prelude.Maybe Prelude.Text)
describeEnvironmentHealth_environmentId = Lens.lens (\DescribeEnvironmentHealth' {environmentId} -> environmentId) (\s@DescribeEnvironmentHealth' {} a -> s {environmentId = a} :: DescribeEnvironmentHealth)

-- | Specify the environment by name.
--
-- You must specify either this or an EnvironmentName, or both.
describeEnvironmentHealth_environmentName :: Lens.Lens' DescribeEnvironmentHealth (Prelude.Maybe Prelude.Text)
describeEnvironmentHealth_environmentName = Lens.lens (\DescribeEnvironmentHealth' {environmentName} -> environmentName) (\s@DescribeEnvironmentHealth' {} a -> s {environmentName = a} :: DescribeEnvironmentHealth)

instance Core.AWSRequest DescribeEnvironmentHealth where
  type
    AWSResponse DescribeEnvironmentHealth =
      DescribeEnvironmentHealthResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEnvironmentHealthResult"
      ( \s h x ->
          DescribeEnvironmentHealthResponse'
            Prelude.<$> (x Data..@? "ApplicationMetrics")
            Prelude.<*> ( x Data..@? "Causes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "Color")
            Prelude.<*> (x Data..@? "EnvironmentName")
            Prelude.<*> (x Data..@? "HealthStatus")
            Prelude.<*> (x Data..@? "InstancesHealth")
            Prelude.<*> (x Data..@? "RefreshedAt")
            Prelude.<*> (x Data..@? "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEnvironmentHealth where
  hashWithSalt _salt DescribeEnvironmentHealth' {..} =
    _salt `Prelude.hashWithSalt` attributeNames
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData DescribeEnvironmentHealth where
  rnf DescribeEnvironmentHealth' {..} =
    Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders DescribeEnvironmentHealth where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeEnvironmentHealth where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEnvironmentHealth where
  toQuery DescribeEnvironmentHealth' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeEnvironmentHealth" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "AttributeNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> attributeNames
            ),
        "EnvironmentId" Data.=: environmentId,
        "EnvironmentName" Data.=: environmentName
      ]

-- | Health details for an AWS Elastic Beanstalk environment.
--
-- /See:/ 'newDescribeEnvironmentHealthResponse' smart constructor.
data DescribeEnvironmentHealthResponse = DescribeEnvironmentHealthResponse'
  { -- | Application request metrics for the environment.
    applicationMetrics :: Prelude.Maybe ApplicationMetrics,
    -- | Descriptions of the data that contributed to the environment\'s current
    -- health status.
    causes :: Prelude.Maybe [Prelude.Text],
    -- | The
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color>
    -- of the environment.
    color :: Prelude.Maybe Prelude.Text,
    -- | The environment\'s name.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status>
    -- of the environment. For example, @Ok@.
    healthStatus :: Prelude.Maybe Prelude.Text,
    -- | Summary health information for the instances in the environment.
    instancesHealth :: Prelude.Maybe InstanceHealthSummary,
    -- | The date and time that the health information was retrieved.
    refreshedAt :: Prelude.Maybe Data.ISO8601,
    -- | The environment\'s operational status. @Ready@, @Launching@, @Updating@,
    -- @Terminating@, or @Terminated@.
    status :: Prelude.Maybe EnvironmentHealth,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEnvironmentHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationMetrics', 'describeEnvironmentHealthResponse_applicationMetrics' - Application request metrics for the environment.
--
-- 'causes', 'describeEnvironmentHealthResponse_causes' - Descriptions of the data that contributed to the environment\'s current
-- health status.
--
-- 'color', 'describeEnvironmentHealthResponse_color' - The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color>
-- of the environment.
--
-- 'environmentName', 'describeEnvironmentHealthResponse_environmentName' - The environment\'s name.
--
-- 'healthStatus', 'describeEnvironmentHealthResponse_healthStatus' - The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status>
-- of the environment. For example, @Ok@.
--
-- 'instancesHealth', 'describeEnvironmentHealthResponse_instancesHealth' - Summary health information for the instances in the environment.
--
-- 'refreshedAt', 'describeEnvironmentHealthResponse_refreshedAt' - The date and time that the health information was retrieved.
--
-- 'status', 'describeEnvironmentHealthResponse_status' - The environment\'s operational status. @Ready@, @Launching@, @Updating@,
-- @Terminating@, or @Terminated@.
--
-- 'httpStatus', 'describeEnvironmentHealthResponse_httpStatus' - The response's http status code.
newDescribeEnvironmentHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEnvironmentHealthResponse
newDescribeEnvironmentHealthResponse pHttpStatus_ =
  DescribeEnvironmentHealthResponse'
    { applicationMetrics =
        Prelude.Nothing,
      causes = Prelude.Nothing,
      color = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      healthStatus = Prelude.Nothing,
      instancesHealth = Prelude.Nothing,
      refreshedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Application request metrics for the environment.
describeEnvironmentHealthResponse_applicationMetrics :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe ApplicationMetrics)
describeEnvironmentHealthResponse_applicationMetrics = Lens.lens (\DescribeEnvironmentHealthResponse' {applicationMetrics} -> applicationMetrics) (\s@DescribeEnvironmentHealthResponse' {} a -> s {applicationMetrics = a} :: DescribeEnvironmentHealthResponse)

-- | Descriptions of the data that contributed to the environment\'s current
-- health status.
describeEnvironmentHealthResponse_causes :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe [Prelude.Text])
describeEnvironmentHealthResponse_causes = Lens.lens (\DescribeEnvironmentHealthResponse' {causes} -> causes) (\s@DescribeEnvironmentHealthResponse' {} a -> s {causes = a} :: DescribeEnvironmentHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health color>
-- of the environment.
describeEnvironmentHealthResponse_color :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe Prelude.Text)
describeEnvironmentHealthResponse_color = Lens.lens (\DescribeEnvironmentHealthResponse' {color} -> color) (\s@DescribeEnvironmentHealthResponse' {} a -> s {color = a} :: DescribeEnvironmentHealthResponse)

-- | The environment\'s name.
describeEnvironmentHealthResponse_environmentName :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe Prelude.Text)
describeEnvironmentHealthResponse_environmentName = Lens.lens (\DescribeEnvironmentHealthResponse' {environmentName} -> environmentName) (\s@DescribeEnvironmentHealthResponse' {} a -> s {environmentName = a} :: DescribeEnvironmentHealthResponse)

-- | The
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html health status>
-- of the environment. For example, @Ok@.
describeEnvironmentHealthResponse_healthStatus :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe Prelude.Text)
describeEnvironmentHealthResponse_healthStatus = Lens.lens (\DescribeEnvironmentHealthResponse' {healthStatus} -> healthStatus) (\s@DescribeEnvironmentHealthResponse' {} a -> s {healthStatus = a} :: DescribeEnvironmentHealthResponse)

-- | Summary health information for the instances in the environment.
describeEnvironmentHealthResponse_instancesHealth :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe InstanceHealthSummary)
describeEnvironmentHealthResponse_instancesHealth = Lens.lens (\DescribeEnvironmentHealthResponse' {instancesHealth} -> instancesHealth) (\s@DescribeEnvironmentHealthResponse' {} a -> s {instancesHealth = a} :: DescribeEnvironmentHealthResponse)

-- | The date and time that the health information was retrieved.
describeEnvironmentHealthResponse_refreshedAt :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe Prelude.UTCTime)
describeEnvironmentHealthResponse_refreshedAt = Lens.lens (\DescribeEnvironmentHealthResponse' {refreshedAt} -> refreshedAt) (\s@DescribeEnvironmentHealthResponse' {} a -> s {refreshedAt = a} :: DescribeEnvironmentHealthResponse) Prelude.. Lens.mapping Data._Time

-- | The environment\'s operational status. @Ready@, @Launching@, @Updating@,
-- @Terminating@, or @Terminated@.
describeEnvironmentHealthResponse_status :: Lens.Lens' DescribeEnvironmentHealthResponse (Prelude.Maybe EnvironmentHealth)
describeEnvironmentHealthResponse_status = Lens.lens (\DescribeEnvironmentHealthResponse' {status} -> status) (\s@DescribeEnvironmentHealthResponse' {} a -> s {status = a} :: DescribeEnvironmentHealthResponse)

-- | The response's http status code.
describeEnvironmentHealthResponse_httpStatus :: Lens.Lens' DescribeEnvironmentHealthResponse Prelude.Int
describeEnvironmentHealthResponse_httpStatus = Lens.lens (\DescribeEnvironmentHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeEnvironmentHealthResponse' {} a -> s {httpStatus = a} :: DescribeEnvironmentHealthResponse)

instance
  Prelude.NFData
    DescribeEnvironmentHealthResponse
  where
  rnf DescribeEnvironmentHealthResponse' {..} =
    Prelude.rnf applicationMetrics
      `Prelude.seq` Prelude.rnf causes
      `Prelude.seq` Prelude.rnf color
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf healthStatus
      `Prelude.seq` Prelude.rnf instancesHealth
      `Prelude.seq` Prelude.rnf refreshedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus

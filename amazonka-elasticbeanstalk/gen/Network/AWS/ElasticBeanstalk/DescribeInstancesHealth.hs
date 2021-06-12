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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information about the health of instances in your AWS
-- Elastic Beanstalk. This operation requires
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html enhanced health reporting>.
module Network.AWS.ElasticBeanstalk.DescribeInstancesHealth
  ( -- * Creating a Request
    DescribeInstancesHealth (..),
    newDescribeInstancesHealth,

    -- * Request Lenses
    describeInstancesHealth_nextToken,
    describeInstancesHealth_environmentId,
    describeInstancesHealth_environmentName,
    describeInstancesHealth_attributeNames,

    -- * Destructuring the Response
    DescribeInstancesHealthResponse (..),
    newDescribeInstancesHealthResponse,

    -- * Response Lenses
    describeInstancesHealthResponse_instanceHealthList,
    describeInstancesHealthResponse_nextToken,
    describeInstancesHealthResponse_refreshedAt,
    describeInstancesHealthResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Parameters for a call to @DescribeInstancesHealth@.
--
-- /See:/ 'newDescribeInstancesHealth' smart constructor.
data DescribeInstancesHealth = DescribeInstancesHealth'
  { -- | Specify the pagination token returned by a previous call.
    nextToken :: Core.Maybe Core.Text,
    -- | Specify the AWS Elastic Beanstalk environment by ID.
    environmentId :: Core.Maybe Core.Text,
    -- | Specify the AWS Elastic Beanstalk environment by name.
    environmentName :: Core.Maybe Core.Text,
    -- | Specifies the response elements you wish to receive. To retrieve all
    -- attributes, set to @All@. If no attribute names are specified, returns a
    -- list of instances.
    attributeNames :: Core.Maybe [InstancesHealthAttribute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancesHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInstancesHealth_nextToken' - Specify the pagination token returned by a previous call.
--
-- 'environmentId', 'describeInstancesHealth_environmentId' - Specify the AWS Elastic Beanstalk environment by ID.
--
-- 'environmentName', 'describeInstancesHealth_environmentName' - Specify the AWS Elastic Beanstalk environment by name.
--
-- 'attributeNames', 'describeInstancesHealth_attributeNames' - Specifies the response elements you wish to receive. To retrieve all
-- attributes, set to @All@. If no attribute names are specified, returns a
-- list of instances.
newDescribeInstancesHealth ::
  DescribeInstancesHealth
newDescribeInstancesHealth =
  DescribeInstancesHealth'
    { nextToken = Core.Nothing,
      environmentId = Core.Nothing,
      environmentName = Core.Nothing,
      attributeNames = Core.Nothing
    }

-- | Specify the pagination token returned by a previous call.
describeInstancesHealth_nextToken :: Lens.Lens' DescribeInstancesHealth (Core.Maybe Core.Text)
describeInstancesHealth_nextToken = Lens.lens (\DescribeInstancesHealth' {nextToken} -> nextToken) (\s@DescribeInstancesHealth' {} a -> s {nextToken = a} :: DescribeInstancesHealth)

-- | Specify the AWS Elastic Beanstalk environment by ID.
describeInstancesHealth_environmentId :: Lens.Lens' DescribeInstancesHealth (Core.Maybe Core.Text)
describeInstancesHealth_environmentId = Lens.lens (\DescribeInstancesHealth' {environmentId} -> environmentId) (\s@DescribeInstancesHealth' {} a -> s {environmentId = a} :: DescribeInstancesHealth)

-- | Specify the AWS Elastic Beanstalk environment by name.
describeInstancesHealth_environmentName :: Lens.Lens' DescribeInstancesHealth (Core.Maybe Core.Text)
describeInstancesHealth_environmentName = Lens.lens (\DescribeInstancesHealth' {environmentName} -> environmentName) (\s@DescribeInstancesHealth' {} a -> s {environmentName = a} :: DescribeInstancesHealth)

-- | Specifies the response elements you wish to receive. To retrieve all
-- attributes, set to @All@. If no attribute names are specified, returns a
-- list of instances.
describeInstancesHealth_attributeNames :: Lens.Lens' DescribeInstancesHealth (Core.Maybe [InstancesHealthAttribute])
describeInstancesHealth_attributeNames = Lens.lens (\DescribeInstancesHealth' {attributeNames} -> attributeNames) (\s@DescribeInstancesHealth' {} a -> s {attributeNames = a} :: DescribeInstancesHealth) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeInstancesHealth where
  type
    AWSResponse DescribeInstancesHealth =
      DescribeInstancesHealthResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeInstancesHealthResult"
      ( \s h x ->
          DescribeInstancesHealthResponse'
            Core.<$> ( x Core..@? "InstanceHealthList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "RefreshedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInstancesHealth

instance Core.NFData DescribeInstancesHealth

instance Core.ToHeaders DescribeInstancesHealth where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeInstancesHealth where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInstancesHealth where
  toQuery DescribeInstancesHealth' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeInstancesHealth" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "EnvironmentId" Core.=: environmentId,
        "EnvironmentName" Core.=: environmentName,
        "AttributeNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> attributeNames)
      ]

-- | Detailed health information about the Amazon EC2 instances in an AWS
-- Elastic Beanstalk environment.
--
-- /See:/ 'newDescribeInstancesHealthResponse' smart constructor.
data DescribeInstancesHealthResponse = DescribeInstancesHealthResponse'
  { -- | Detailed health information about each instance.
    --
    -- The output differs slightly between Linux and Windows environments.
    -- There is a difference in the members that are supported under the
    -- @\<CPUUtilization>@ type.
    instanceHealthList :: Core.Maybe [SingleInstanceHealth],
    -- | Pagination token for the next page of results, if available.
    nextToken :: Core.Maybe Core.Text,
    -- | The date and time that the health information was retrieved.
    refreshedAt :: Core.Maybe Core.ISO8601,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstancesHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceHealthList', 'describeInstancesHealthResponse_instanceHealthList' - Detailed health information about each instance.
--
-- The output differs slightly between Linux and Windows environments.
-- There is a difference in the members that are supported under the
-- @\<CPUUtilization>@ type.
--
-- 'nextToken', 'describeInstancesHealthResponse_nextToken' - Pagination token for the next page of results, if available.
--
-- 'refreshedAt', 'describeInstancesHealthResponse_refreshedAt' - The date and time that the health information was retrieved.
--
-- 'httpStatus', 'describeInstancesHealthResponse_httpStatus' - The response's http status code.
newDescribeInstancesHealthResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstancesHealthResponse
newDescribeInstancesHealthResponse pHttpStatus_ =
  DescribeInstancesHealthResponse'
    { instanceHealthList =
        Core.Nothing,
      nextToken = Core.Nothing,
      refreshedAt = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed health information about each instance.
--
-- The output differs slightly between Linux and Windows environments.
-- There is a difference in the members that are supported under the
-- @\<CPUUtilization>@ type.
describeInstancesHealthResponse_instanceHealthList :: Lens.Lens' DescribeInstancesHealthResponse (Core.Maybe [SingleInstanceHealth])
describeInstancesHealthResponse_instanceHealthList = Lens.lens (\DescribeInstancesHealthResponse' {instanceHealthList} -> instanceHealthList) (\s@DescribeInstancesHealthResponse' {} a -> s {instanceHealthList = a} :: DescribeInstancesHealthResponse) Core.. Lens.mapping Lens._Coerce

-- | Pagination token for the next page of results, if available.
describeInstancesHealthResponse_nextToken :: Lens.Lens' DescribeInstancesHealthResponse (Core.Maybe Core.Text)
describeInstancesHealthResponse_nextToken = Lens.lens (\DescribeInstancesHealthResponse' {nextToken} -> nextToken) (\s@DescribeInstancesHealthResponse' {} a -> s {nextToken = a} :: DescribeInstancesHealthResponse)

-- | The date and time that the health information was retrieved.
describeInstancesHealthResponse_refreshedAt :: Lens.Lens' DescribeInstancesHealthResponse (Core.Maybe Core.UTCTime)
describeInstancesHealthResponse_refreshedAt = Lens.lens (\DescribeInstancesHealthResponse' {refreshedAt} -> refreshedAt) (\s@DescribeInstancesHealthResponse' {} a -> s {refreshedAt = a} :: DescribeInstancesHealthResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeInstancesHealthResponse_httpStatus :: Lens.Lens' DescribeInstancesHealthResponse Core.Int
describeInstancesHealthResponse_httpStatus = Lens.lens (\DescribeInstancesHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancesHealthResponse' {} a -> s {httpStatus = a} :: DescribeInstancesHealthResponse)

instance Core.NFData DescribeInstancesHealthResponse

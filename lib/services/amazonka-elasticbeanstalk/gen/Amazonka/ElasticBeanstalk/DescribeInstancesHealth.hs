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
-- Module      : Amazonka.ElasticBeanstalk.DescribeInstancesHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information about the health of instances in your AWS
-- Elastic Beanstalk. This operation requires
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html enhanced health reporting>.
module Amazonka.ElasticBeanstalk.DescribeInstancesHealth
  ( -- * Creating a Request
    DescribeInstancesHealth (..),
    newDescribeInstancesHealth,

    -- * Request Lenses
    describeInstancesHealth_attributeNames,
    describeInstancesHealth_environmentId,
    describeInstancesHealth_environmentName,
    describeInstancesHealth_nextToken,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Parameters for a call to @DescribeInstancesHealth@.
--
-- /See:/ 'newDescribeInstancesHealth' smart constructor.
data DescribeInstancesHealth = DescribeInstancesHealth'
  { -- | Specifies the response elements you wish to receive. To retrieve all
    -- attributes, set to @All@. If no attribute names are specified, returns a
    -- list of instances.
    attributeNames :: Prelude.Maybe [InstancesHealthAttribute],
    -- | Specify the AWS Elastic Beanstalk environment by ID.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | Specify the AWS Elastic Beanstalk environment by name.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | Specify the pagination token returned by a previous call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancesHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'describeInstancesHealth_attributeNames' - Specifies the response elements you wish to receive. To retrieve all
-- attributes, set to @All@. If no attribute names are specified, returns a
-- list of instances.
--
-- 'environmentId', 'describeInstancesHealth_environmentId' - Specify the AWS Elastic Beanstalk environment by ID.
--
-- 'environmentName', 'describeInstancesHealth_environmentName' - Specify the AWS Elastic Beanstalk environment by name.
--
-- 'nextToken', 'describeInstancesHealth_nextToken' - Specify the pagination token returned by a previous call.
newDescribeInstancesHealth ::
  DescribeInstancesHealth
newDescribeInstancesHealth =
  DescribeInstancesHealth'
    { attributeNames =
        Prelude.Nothing,
      environmentId = Prelude.Nothing,
      environmentName = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifies the response elements you wish to receive. To retrieve all
-- attributes, set to @All@. If no attribute names are specified, returns a
-- list of instances.
describeInstancesHealth_attributeNames :: Lens.Lens' DescribeInstancesHealth (Prelude.Maybe [InstancesHealthAttribute])
describeInstancesHealth_attributeNames = Lens.lens (\DescribeInstancesHealth' {attributeNames} -> attributeNames) (\s@DescribeInstancesHealth' {} a -> s {attributeNames = a} :: DescribeInstancesHealth) Prelude.. Lens.mapping Lens.coerced

-- | Specify the AWS Elastic Beanstalk environment by ID.
describeInstancesHealth_environmentId :: Lens.Lens' DescribeInstancesHealth (Prelude.Maybe Prelude.Text)
describeInstancesHealth_environmentId = Lens.lens (\DescribeInstancesHealth' {environmentId} -> environmentId) (\s@DescribeInstancesHealth' {} a -> s {environmentId = a} :: DescribeInstancesHealth)

-- | Specify the AWS Elastic Beanstalk environment by name.
describeInstancesHealth_environmentName :: Lens.Lens' DescribeInstancesHealth (Prelude.Maybe Prelude.Text)
describeInstancesHealth_environmentName = Lens.lens (\DescribeInstancesHealth' {environmentName} -> environmentName) (\s@DescribeInstancesHealth' {} a -> s {environmentName = a} :: DescribeInstancesHealth)

-- | Specify the pagination token returned by a previous call.
describeInstancesHealth_nextToken :: Lens.Lens' DescribeInstancesHealth (Prelude.Maybe Prelude.Text)
describeInstancesHealth_nextToken = Lens.lens (\DescribeInstancesHealth' {nextToken} -> nextToken) (\s@DescribeInstancesHealth' {} a -> s {nextToken = a} :: DescribeInstancesHealth)

instance Core.AWSRequest DescribeInstancesHealth where
  type
    AWSResponse DescribeInstancesHealth =
      DescribeInstancesHealthResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeInstancesHealthResult"
      ( \s h x ->
          DescribeInstancesHealthResponse'
            Prelude.<$> ( x Data..@? "InstanceHealthList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (x Data..@? "RefreshedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstancesHealth where
  hashWithSalt _salt DescribeInstancesHealth' {..} =
    _salt `Prelude.hashWithSalt` attributeNames
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeInstancesHealth where
  rnf DescribeInstancesHealth' {..} =
    Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeInstancesHealth where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeInstancesHealth where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeInstancesHealth where
  toQuery DescribeInstancesHealth' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeInstancesHealth" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "AttributeNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> attributeNames
            ),
        "EnvironmentId" Data.=: environmentId,
        "EnvironmentName" Data.=: environmentName,
        "NextToken" Data.=: nextToken
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
    instanceHealthList :: Prelude.Maybe [SingleInstanceHealth],
    -- | Pagination token for the next page of results, if available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the health information was retrieved.
    refreshedAt :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeInstancesHealthResponse
newDescribeInstancesHealthResponse pHttpStatus_ =
  DescribeInstancesHealthResponse'
    { instanceHealthList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      refreshedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed health information about each instance.
--
-- The output differs slightly between Linux and Windows environments.
-- There is a difference in the members that are supported under the
-- @\<CPUUtilization>@ type.
describeInstancesHealthResponse_instanceHealthList :: Lens.Lens' DescribeInstancesHealthResponse (Prelude.Maybe [SingleInstanceHealth])
describeInstancesHealthResponse_instanceHealthList = Lens.lens (\DescribeInstancesHealthResponse' {instanceHealthList} -> instanceHealthList) (\s@DescribeInstancesHealthResponse' {} a -> s {instanceHealthList = a} :: DescribeInstancesHealthResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token for the next page of results, if available.
describeInstancesHealthResponse_nextToken :: Lens.Lens' DescribeInstancesHealthResponse (Prelude.Maybe Prelude.Text)
describeInstancesHealthResponse_nextToken = Lens.lens (\DescribeInstancesHealthResponse' {nextToken} -> nextToken) (\s@DescribeInstancesHealthResponse' {} a -> s {nextToken = a} :: DescribeInstancesHealthResponse)

-- | The date and time that the health information was retrieved.
describeInstancesHealthResponse_refreshedAt :: Lens.Lens' DescribeInstancesHealthResponse (Prelude.Maybe Prelude.UTCTime)
describeInstancesHealthResponse_refreshedAt = Lens.lens (\DescribeInstancesHealthResponse' {refreshedAt} -> refreshedAt) (\s@DescribeInstancesHealthResponse' {} a -> s {refreshedAt = a} :: DescribeInstancesHealthResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeInstancesHealthResponse_httpStatus :: Lens.Lens' DescribeInstancesHealthResponse Prelude.Int
describeInstancesHealthResponse_httpStatus = Lens.lens (\DescribeInstancesHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancesHealthResponse' {} a -> s {httpStatus = a} :: DescribeInstancesHealthResponse)

instance
  Prelude.NFData
    DescribeInstancesHealthResponse
  where
  rnf DescribeInstancesHealthResponse' {..} =
    Prelude.rnf instanceHealthList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf refreshedAt
      `Prelude.seq` Prelude.rnf httpStatus

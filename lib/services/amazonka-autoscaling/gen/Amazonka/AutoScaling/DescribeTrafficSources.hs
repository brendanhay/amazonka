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
-- Module      : Amazonka.AutoScaling.DescribeTrafficSources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic sources for the specified Auto
-- Scaling group.
--
-- You can optionally provide a traffic source type. If you provide a
-- traffic source type, then the results only include that traffic source
-- type.
--
-- If you do not provide a traffic source type, then the results include
-- all the traffic sources for the specified Auto Scaling group.
module Amazonka.AutoScaling.DescribeTrafficSources
  ( -- * Creating a Request
    DescribeTrafficSources (..),
    newDescribeTrafficSources,

    -- * Request Lenses
    describeTrafficSources_maxRecords,
    describeTrafficSources_nextToken,
    describeTrafficSources_trafficSourceType,
    describeTrafficSources_autoScalingGroupName,

    -- * Destructuring the Response
    DescribeTrafficSourcesResponse (..),
    newDescribeTrafficSourcesResponse,

    -- * Response Lenses
    describeTrafficSourcesResponse_nextToken,
    describeTrafficSourcesResponse_trafficSources,
    describeTrafficSourcesResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTrafficSources' smart constructor.
data DescribeTrafficSources = DescribeTrafficSources'
  { -- | The maximum number of items to return with this call. The maximum value
    -- is @50@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The traffic source type that you want to describe.
    --
    -- The following lists the valid values:
    --
    -- -   @elb@ if the traffic source is a Classic Load Balancer.
    --
    -- -   @elbv2@ if the traffic source is a Application Load Balancer,
    --     Gateway Load Balancer, or Network Load Balancer.
    --
    -- -   @vpc-lattice@ if the traffic source is VPC Lattice.
    trafficSourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficSources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRecords', 'describeTrafficSources_maxRecords' - The maximum number of items to return with this call. The maximum value
-- is @50@.
--
-- 'nextToken', 'describeTrafficSources_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'trafficSourceType', 'describeTrafficSources_trafficSourceType' - The traffic source type that you want to describe.
--
-- The following lists the valid values:
--
-- -   @elb@ if the traffic source is a Classic Load Balancer.
--
-- -   @elbv2@ if the traffic source is a Application Load Balancer,
--     Gateway Load Balancer, or Network Load Balancer.
--
-- -   @vpc-lattice@ if the traffic source is VPC Lattice.
--
-- 'autoScalingGroupName', 'describeTrafficSources_autoScalingGroupName' - The name of the Auto Scaling group.
newDescribeTrafficSources ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DescribeTrafficSources
newDescribeTrafficSources pAutoScalingGroupName_ =
  DescribeTrafficSources'
    { maxRecords =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      trafficSourceType = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The maximum number of items to return with this call. The maximum value
-- is @50@.
describeTrafficSources_maxRecords :: Lens.Lens' DescribeTrafficSources (Prelude.Maybe Prelude.Int)
describeTrafficSources_maxRecords = Lens.lens (\DescribeTrafficSources' {maxRecords} -> maxRecords) (\s@DescribeTrafficSources' {} a -> s {maxRecords = a} :: DescribeTrafficSources)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeTrafficSources_nextToken :: Lens.Lens' DescribeTrafficSources (Prelude.Maybe Prelude.Text)
describeTrafficSources_nextToken = Lens.lens (\DescribeTrafficSources' {nextToken} -> nextToken) (\s@DescribeTrafficSources' {} a -> s {nextToken = a} :: DescribeTrafficSources)

-- | The traffic source type that you want to describe.
--
-- The following lists the valid values:
--
-- -   @elb@ if the traffic source is a Classic Load Balancer.
--
-- -   @elbv2@ if the traffic source is a Application Load Balancer,
--     Gateway Load Balancer, or Network Load Balancer.
--
-- -   @vpc-lattice@ if the traffic source is VPC Lattice.
describeTrafficSources_trafficSourceType :: Lens.Lens' DescribeTrafficSources (Prelude.Maybe Prelude.Text)
describeTrafficSources_trafficSourceType = Lens.lens (\DescribeTrafficSources' {trafficSourceType} -> trafficSourceType) (\s@DescribeTrafficSources' {} a -> s {trafficSourceType = a} :: DescribeTrafficSources)

-- | The name of the Auto Scaling group.
describeTrafficSources_autoScalingGroupName :: Lens.Lens' DescribeTrafficSources Prelude.Text
describeTrafficSources_autoScalingGroupName = Lens.lens (\DescribeTrafficSources' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeTrafficSources' {} a -> s {autoScalingGroupName = a} :: DescribeTrafficSources)

instance Core.AWSRequest DescribeTrafficSources where
  type
    AWSResponse DescribeTrafficSources =
      DescribeTrafficSourcesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTrafficSourcesResult"
      ( \s h x ->
          DescribeTrafficSourcesResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x
                            Data..@? "TrafficSources"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrafficSources where
  hashWithSalt _salt DescribeTrafficSources' {..} =
    _salt
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` trafficSourceType
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DescribeTrafficSources where
  rnf DescribeTrafficSources' {..} =
    Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficSourceType
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders DescribeTrafficSources where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeTrafficSources where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTrafficSources where
  toQuery DescribeTrafficSources' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeTrafficSources" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken,
        "TrafficSourceType" Data.=: trafficSourceType,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeTrafficSourcesResponse' smart constructor.
data DescribeTrafficSourcesResponse = DescribeTrafficSourcesResponse'
  { -- | This string indicates that the response contains more items than can be
    -- returned in a single response. To receive additional items, specify this
    -- string for the @NextToken@ value when requesting the next set of items.
    -- This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the traffic sources.
    trafficSources :: Prelude.Maybe [TrafficSourceState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrafficSourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTrafficSourcesResponse_nextToken' - This string indicates that the response contains more items than can be
-- returned in a single response. To receive additional items, specify this
-- string for the @NextToken@ value when requesting the next set of items.
-- This value is null when there are no more items to return.
--
-- 'trafficSources', 'describeTrafficSourcesResponse_trafficSources' - Information about the traffic sources.
--
-- 'httpStatus', 'describeTrafficSourcesResponse_httpStatus' - The response's http status code.
newDescribeTrafficSourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrafficSourcesResponse
newDescribeTrafficSourcesResponse pHttpStatus_ =
  DescribeTrafficSourcesResponse'
    { nextToken =
        Prelude.Nothing,
      trafficSources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This string indicates that the response contains more items than can be
-- returned in a single response. To receive additional items, specify this
-- string for the @NextToken@ value when requesting the next set of items.
-- This value is null when there are no more items to return.
describeTrafficSourcesResponse_nextToken :: Lens.Lens' DescribeTrafficSourcesResponse (Prelude.Maybe Prelude.Text)
describeTrafficSourcesResponse_nextToken = Lens.lens (\DescribeTrafficSourcesResponse' {nextToken} -> nextToken) (\s@DescribeTrafficSourcesResponse' {} a -> s {nextToken = a} :: DescribeTrafficSourcesResponse)

-- | Information about the traffic sources.
describeTrafficSourcesResponse_trafficSources :: Lens.Lens' DescribeTrafficSourcesResponse (Prelude.Maybe [TrafficSourceState])
describeTrafficSourcesResponse_trafficSources = Lens.lens (\DescribeTrafficSourcesResponse' {trafficSources} -> trafficSources) (\s@DescribeTrafficSourcesResponse' {} a -> s {trafficSources = a} :: DescribeTrafficSourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrafficSourcesResponse_httpStatus :: Lens.Lens' DescribeTrafficSourcesResponse Prelude.Int
describeTrafficSourcesResponse_httpStatus = Lens.lens (\DescribeTrafficSourcesResponse' {httpStatus} -> httpStatus) (\s@DescribeTrafficSourcesResponse' {} a -> s {httpStatus = a} :: DescribeTrafficSourcesResponse)

instance
  Prelude.NFData
    DescribeTrafficSourcesResponse
  where
  rnf DescribeTrafficSourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trafficSources
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.AutoScaling.DescribeWarmPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a warm pool and its instances.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-warm-pools.html Warm pools for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.DescribeWarmPool
  ( -- * Creating a Request
    DescribeWarmPool (..),
    newDescribeWarmPool,

    -- * Request Lenses
    describeWarmPool_maxRecords,
    describeWarmPool_nextToken,
    describeWarmPool_autoScalingGroupName,

    -- * Destructuring the Response
    DescribeWarmPoolResponse (..),
    newDescribeWarmPoolResponse,

    -- * Response Lenses
    describeWarmPoolResponse_instances,
    describeWarmPoolResponse_nextToken,
    describeWarmPoolResponse_warmPoolConfiguration,
    describeWarmPoolResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeWarmPool' smart constructor.
data DescribeWarmPool = DescribeWarmPool'
  { -- | The maximum number of instances to return with this call. The maximum
    -- value is @50@.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of instances to return. (You received this
    -- token from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWarmPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxRecords', 'describeWarmPool_maxRecords' - The maximum number of instances to return with this call. The maximum
-- value is @50@.
--
-- 'nextToken', 'describeWarmPool_nextToken' - The token for the next set of instances to return. (You received this
-- token from a previous call.)
--
-- 'autoScalingGroupName', 'describeWarmPool_autoScalingGroupName' - The name of the Auto Scaling group.
newDescribeWarmPool ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DescribeWarmPool
newDescribeWarmPool pAutoScalingGroupName_ =
  DescribeWarmPool'
    { maxRecords = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The maximum number of instances to return with this call. The maximum
-- value is @50@.
describeWarmPool_maxRecords :: Lens.Lens' DescribeWarmPool (Prelude.Maybe Prelude.Int)
describeWarmPool_maxRecords = Lens.lens (\DescribeWarmPool' {maxRecords} -> maxRecords) (\s@DescribeWarmPool' {} a -> s {maxRecords = a} :: DescribeWarmPool)

-- | The token for the next set of instances to return. (You received this
-- token from a previous call.)
describeWarmPool_nextToken :: Lens.Lens' DescribeWarmPool (Prelude.Maybe Prelude.Text)
describeWarmPool_nextToken = Lens.lens (\DescribeWarmPool' {nextToken} -> nextToken) (\s@DescribeWarmPool' {} a -> s {nextToken = a} :: DescribeWarmPool)

-- | The name of the Auto Scaling group.
describeWarmPool_autoScalingGroupName :: Lens.Lens' DescribeWarmPool Prelude.Text
describeWarmPool_autoScalingGroupName = Lens.lens (\DescribeWarmPool' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeWarmPool' {} a -> s {autoScalingGroupName = a} :: DescribeWarmPool)

instance Core.AWSRequest DescribeWarmPool where
  type
    AWSResponse DescribeWarmPool =
      DescribeWarmPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeWarmPoolResult"
      ( \s h x ->
          DescribeWarmPoolResponse'
            Prelude.<$> ( x
                            Data..@? "Instances"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (x Data..@? "WarmPoolConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWarmPool where
  hashWithSalt _salt DescribeWarmPool' {..} =
    _salt
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DescribeWarmPool where
  rnf DescribeWarmPool' {..} =
    Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders DescribeWarmPool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeWarmPool where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWarmPool where
  toQuery DescribeWarmPool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeWarmPool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "MaxRecords" Data.=: maxRecords,
        "NextToken" Data.=: nextToken,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeWarmPoolResponse' smart constructor.
data DescribeWarmPoolResponse = DescribeWarmPoolResponse'
  { -- | The instances that are currently in the warm pool.
    instances :: Prelude.Maybe [Instance],
    -- | This string indicates that the response contains more items than can be
    -- returned in a single response. To receive additional items, specify this
    -- string for the @NextToken@ value when requesting the next set of items.
    -- This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The warm pool configuration details.
    warmPoolConfiguration :: Prelude.Maybe WarmPoolConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWarmPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'describeWarmPoolResponse_instances' - The instances that are currently in the warm pool.
--
-- 'nextToken', 'describeWarmPoolResponse_nextToken' - This string indicates that the response contains more items than can be
-- returned in a single response. To receive additional items, specify this
-- string for the @NextToken@ value when requesting the next set of items.
-- This value is null when there are no more items to return.
--
-- 'warmPoolConfiguration', 'describeWarmPoolResponse_warmPoolConfiguration' - The warm pool configuration details.
--
-- 'httpStatus', 'describeWarmPoolResponse_httpStatus' - The response's http status code.
newDescribeWarmPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWarmPoolResponse
newDescribeWarmPoolResponse pHttpStatus_ =
  DescribeWarmPoolResponse'
    { instances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      warmPoolConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The instances that are currently in the warm pool.
describeWarmPoolResponse_instances :: Lens.Lens' DescribeWarmPoolResponse (Prelude.Maybe [Instance])
describeWarmPoolResponse_instances = Lens.lens (\DescribeWarmPoolResponse' {instances} -> instances) (\s@DescribeWarmPoolResponse' {} a -> s {instances = a} :: DescribeWarmPoolResponse) Prelude.. Lens.mapping Lens.coerced

-- | This string indicates that the response contains more items than can be
-- returned in a single response. To receive additional items, specify this
-- string for the @NextToken@ value when requesting the next set of items.
-- This value is null when there are no more items to return.
describeWarmPoolResponse_nextToken :: Lens.Lens' DescribeWarmPoolResponse (Prelude.Maybe Prelude.Text)
describeWarmPoolResponse_nextToken = Lens.lens (\DescribeWarmPoolResponse' {nextToken} -> nextToken) (\s@DescribeWarmPoolResponse' {} a -> s {nextToken = a} :: DescribeWarmPoolResponse)

-- | The warm pool configuration details.
describeWarmPoolResponse_warmPoolConfiguration :: Lens.Lens' DescribeWarmPoolResponse (Prelude.Maybe WarmPoolConfiguration)
describeWarmPoolResponse_warmPoolConfiguration = Lens.lens (\DescribeWarmPoolResponse' {warmPoolConfiguration} -> warmPoolConfiguration) (\s@DescribeWarmPoolResponse' {} a -> s {warmPoolConfiguration = a} :: DescribeWarmPoolResponse)

-- | The response's http status code.
describeWarmPoolResponse_httpStatus :: Lens.Lens' DescribeWarmPoolResponse Prelude.Int
describeWarmPoolResponse_httpStatus = Lens.lens (\DescribeWarmPoolResponse' {httpStatus} -> httpStatus) (\s@DescribeWarmPoolResponse' {} a -> s {httpStatus = a} :: DescribeWarmPoolResponse)

instance Prelude.NFData DescribeWarmPoolResponse where
  rnf DescribeWarmPoolResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf warmPoolConfiguration
      `Prelude.seq` Prelude.rnf httpStatus

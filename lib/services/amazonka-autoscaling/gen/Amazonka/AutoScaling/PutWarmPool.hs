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
-- Module      : Amazonka.AutoScaling.PutWarmPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a warm pool for the specified Auto Scaling group. A
-- warm pool is a pool of pre-initialized EC2 instances that sits alongside
-- the Auto Scaling group. Whenever your application needs to scale out,
-- the Auto Scaling group can draw on the warm pool to meet its new desired
-- capacity. For more information and example configurations, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-warm-pools.html Warm pools for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This operation must be called from the Region in which the Auto Scaling
-- group was created. This operation cannot be called on an Auto Scaling
-- group that has a mixed instances policy or a launch template or launch
-- configuration that requests Spot Instances.
--
-- You can view the instances in the warm pool using the DescribeWarmPool
-- API call. If you are no longer using a warm pool, you can delete it by
-- calling the DeleteWarmPool API.
module Amazonka.AutoScaling.PutWarmPool
  ( -- * Creating a Request
    PutWarmPool (..),
    newPutWarmPool,

    -- * Request Lenses
    putWarmPool_poolState,
    putWarmPool_minSize,
    putWarmPool_instanceReusePolicy,
    putWarmPool_maxGroupPreparedCapacity,
    putWarmPool_autoScalingGroupName,

    -- * Destructuring the Response
    PutWarmPoolResponse (..),
    newPutWarmPoolResponse,

    -- * Response Lenses
    putWarmPoolResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutWarmPool' smart constructor.
data PutWarmPool = PutWarmPool'
  { -- | Sets the instance state to transition to after the lifecycle actions are
    -- complete. Default is @Stopped@.
    poolState :: Prelude.Maybe WarmPoolState,
    -- | Specifies the minimum number of instances to maintain in the warm pool.
    -- This helps you to ensure that there is always a certain number of warmed
    -- instances available to handle traffic spikes. Defaults to 0 if not
    -- specified.
    minSize :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether instances in the Auto Scaling group can be returned to
    -- the warm pool on scale in. The default is to terminate instances in the
    -- Auto Scaling group when the group scales in.
    instanceReusePolicy :: Prelude.Maybe InstanceReusePolicy,
    -- | Specifies the maximum number of instances that are allowed to be in the
    -- warm pool or in any state except @Terminated@ for the Auto Scaling
    -- group. This is an optional property. Specify it only if you do not want
    -- the warm pool size to be determined by the difference between the
    -- group\'s maximum capacity and its desired capacity.
    --
    -- If a value for @MaxGroupPreparedCapacity@ is not specified, Amazon EC2
    -- Auto Scaling launches and maintains the difference between the group\'s
    -- maximum capacity and its desired capacity. If you specify a value for
    -- @MaxGroupPreparedCapacity@, Amazon EC2 Auto Scaling uses the difference
    -- between the @MaxGroupPreparedCapacity@ and the desired capacity instead.
    --
    -- The size of the warm pool is dynamic. Only when
    -- @MaxGroupPreparedCapacity@ and @MinSize@ are set to the same value does
    -- the warm pool have an absolute size.
    --
    -- If the desired capacity of the Auto Scaling group is higher than the
    -- @MaxGroupPreparedCapacity@, the capacity of the warm pool is 0, unless
    -- you specify a value for @MinSize@. To remove a value that you previously
    -- set, include the property but specify -1 for the value.
    maxGroupPreparedCapacity :: Prelude.Maybe Prelude.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutWarmPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolState', 'putWarmPool_poolState' - Sets the instance state to transition to after the lifecycle actions are
-- complete. Default is @Stopped@.
--
-- 'minSize', 'putWarmPool_minSize' - Specifies the minimum number of instances to maintain in the warm pool.
-- This helps you to ensure that there is always a certain number of warmed
-- instances available to handle traffic spikes. Defaults to 0 if not
-- specified.
--
-- 'instanceReusePolicy', 'putWarmPool_instanceReusePolicy' - Indicates whether instances in the Auto Scaling group can be returned to
-- the warm pool on scale in. The default is to terminate instances in the
-- Auto Scaling group when the group scales in.
--
-- 'maxGroupPreparedCapacity', 'putWarmPool_maxGroupPreparedCapacity' - Specifies the maximum number of instances that are allowed to be in the
-- warm pool or in any state except @Terminated@ for the Auto Scaling
-- group. This is an optional property. Specify it only if you do not want
-- the warm pool size to be determined by the difference between the
-- group\'s maximum capacity and its desired capacity.
--
-- If a value for @MaxGroupPreparedCapacity@ is not specified, Amazon EC2
-- Auto Scaling launches and maintains the difference between the group\'s
-- maximum capacity and its desired capacity. If you specify a value for
-- @MaxGroupPreparedCapacity@, Amazon EC2 Auto Scaling uses the difference
-- between the @MaxGroupPreparedCapacity@ and the desired capacity instead.
--
-- The size of the warm pool is dynamic. Only when
-- @MaxGroupPreparedCapacity@ and @MinSize@ are set to the same value does
-- the warm pool have an absolute size.
--
-- If the desired capacity of the Auto Scaling group is higher than the
-- @MaxGroupPreparedCapacity@, the capacity of the warm pool is 0, unless
-- you specify a value for @MinSize@. To remove a value that you previously
-- set, include the property but specify -1 for the value.
--
-- 'autoScalingGroupName', 'putWarmPool_autoScalingGroupName' - The name of the Auto Scaling group.
newPutWarmPool ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  PutWarmPool
newPutWarmPool pAutoScalingGroupName_ =
  PutWarmPool'
    { poolState = Prelude.Nothing,
      minSize = Prelude.Nothing,
      instanceReusePolicy = Prelude.Nothing,
      maxGroupPreparedCapacity = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Sets the instance state to transition to after the lifecycle actions are
-- complete. Default is @Stopped@.
putWarmPool_poolState :: Lens.Lens' PutWarmPool (Prelude.Maybe WarmPoolState)
putWarmPool_poolState = Lens.lens (\PutWarmPool' {poolState} -> poolState) (\s@PutWarmPool' {} a -> s {poolState = a} :: PutWarmPool)

-- | Specifies the minimum number of instances to maintain in the warm pool.
-- This helps you to ensure that there is always a certain number of warmed
-- instances available to handle traffic spikes. Defaults to 0 if not
-- specified.
putWarmPool_minSize :: Lens.Lens' PutWarmPool (Prelude.Maybe Prelude.Natural)
putWarmPool_minSize = Lens.lens (\PutWarmPool' {minSize} -> minSize) (\s@PutWarmPool' {} a -> s {minSize = a} :: PutWarmPool)

-- | Indicates whether instances in the Auto Scaling group can be returned to
-- the warm pool on scale in. The default is to terminate instances in the
-- Auto Scaling group when the group scales in.
putWarmPool_instanceReusePolicy :: Lens.Lens' PutWarmPool (Prelude.Maybe InstanceReusePolicy)
putWarmPool_instanceReusePolicy = Lens.lens (\PutWarmPool' {instanceReusePolicy} -> instanceReusePolicy) (\s@PutWarmPool' {} a -> s {instanceReusePolicy = a} :: PutWarmPool)

-- | Specifies the maximum number of instances that are allowed to be in the
-- warm pool or in any state except @Terminated@ for the Auto Scaling
-- group. This is an optional property. Specify it only if you do not want
-- the warm pool size to be determined by the difference between the
-- group\'s maximum capacity and its desired capacity.
--
-- If a value for @MaxGroupPreparedCapacity@ is not specified, Amazon EC2
-- Auto Scaling launches and maintains the difference between the group\'s
-- maximum capacity and its desired capacity. If you specify a value for
-- @MaxGroupPreparedCapacity@, Amazon EC2 Auto Scaling uses the difference
-- between the @MaxGroupPreparedCapacity@ and the desired capacity instead.
--
-- The size of the warm pool is dynamic. Only when
-- @MaxGroupPreparedCapacity@ and @MinSize@ are set to the same value does
-- the warm pool have an absolute size.
--
-- If the desired capacity of the Auto Scaling group is higher than the
-- @MaxGroupPreparedCapacity@, the capacity of the warm pool is 0, unless
-- you specify a value for @MinSize@. To remove a value that you previously
-- set, include the property but specify -1 for the value.
putWarmPool_maxGroupPreparedCapacity :: Lens.Lens' PutWarmPool (Prelude.Maybe Prelude.Int)
putWarmPool_maxGroupPreparedCapacity = Lens.lens (\PutWarmPool' {maxGroupPreparedCapacity} -> maxGroupPreparedCapacity) (\s@PutWarmPool' {} a -> s {maxGroupPreparedCapacity = a} :: PutWarmPool)

-- | The name of the Auto Scaling group.
putWarmPool_autoScalingGroupName :: Lens.Lens' PutWarmPool Prelude.Text
putWarmPool_autoScalingGroupName = Lens.lens (\PutWarmPool' {autoScalingGroupName} -> autoScalingGroupName) (\s@PutWarmPool' {} a -> s {autoScalingGroupName = a} :: PutWarmPool)

instance Core.AWSRequest PutWarmPool where
  type AWSResponse PutWarmPool = PutWarmPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "PutWarmPoolResult"
      ( \s h x ->
          PutWarmPoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutWarmPool where
  hashWithSalt _salt PutWarmPool' {..} =
    _salt `Prelude.hashWithSalt` poolState
      `Prelude.hashWithSalt` minSize
      `Prelude.hashWithSalt` instanceReusePolicy
      `Prelude.hashWithSalt` maxGroupPreparedCapacity
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData PutWarmPool where
  rnf PutWarmPool' {..} =
    Prelude.rnf poolState
      `Prelude.seq` Prelude.rnf minSize
      `Prelude.seq` Prelude.rnf instanceReusePolicy
      `Prelude.seq` Prelude.rnf maxGroupPreparedCapacity
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders PutWarmPool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutWarmPool where
  toPath = Prelude.const "/"

instance Data.ToQuery PutWarmPool where
  toQuery PutWarmPool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("PutWarmPool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "PoolState" Data.=: poolState,
        "MinSize" Data.=: minSize,
        "InstanceReusePolicy" Data.=: instanceReusePolicy,
        "MaxGroupPreparedCapacity"
          Data.=: maxGroupPreparedCapacity,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newPutWarmPoolResponse' smart constructor.
data PutWarmPoolResponse = PutWarmPoolResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutWarmPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putWarmPoolResponse_httpStatus' - The response's http status code.
newPutWarmPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutWarmPoolResponse
newPutWarmPoolResponse pHttpStatus_ =
  PutWarmPoolResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putWarmPoolResponse_httpStatus :: Lens.Lens' PutWarmPoolResponse Prelude.Int
putWarmPoolResponse_httpStatus = Lens.lens (\PutWarmPoolResponse' {httpStatus} -> httpStatus) (\s@PutWarmPoolResponse' {} a -> s {httpStatus = a} :: PutWarmPoolResponse)

instance Prelude.NFData PutWarmPoolResponse where
  rnf PutWarmPoolResponse' {..} = Prelude.rnf httpStatus

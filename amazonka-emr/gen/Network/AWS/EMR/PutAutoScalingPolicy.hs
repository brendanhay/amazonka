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
-- Module      : Network.AWS.EMR.PutAutoScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an automatic scaling policy for a core instance group
-- or task instance group in an Amazon EMR cluster. The automatic scaling
-- policy defines how an instance group dynamically adds and terminates EC2
-- instances in response to the value of a CloudWatch metric.
module Network.AWS.EMR.PutAutoScalingPolicy
  ( -- * Creating a Request
    PutAutoScalingPolicy (..),
    newPutAutoScalingPolicy,

    -- * Request Lenses
    putAutoScalingPolicy_clusterId,
    putAutoScalingPolicy_instanceGroupId,
    putAutoScalingPolicy_autoScalingPolicy,

    -- * Destructuring the Response
    PutAutoScalingPolicyResponse (..),
    newPutAutoScalingPolicyResponse,

    -- * Response Lenses
    putAutoScalingPolicyResponse_clusterArn,
    putAutoScalingPolicyResponse_clusterId,
    putAutoScalingPolicyResponse_instanceGroupId,
    putAutoScalingPolicyResponse_autoScalingPolicy,
    putAutoScalingPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAutoScalingPolicy' smart constructor.
data PutAutoScalingPolicy = PutAutoScalingPolicy'
  { -- | Specifies the ID of a cluster. The instance group to which the automatic
    -- scaling policy is applied is within this cluster.
    clusterId :: Prelude.Text,
    -- | Specifies the ID of the instance group to which the automatic scaling
    -- policy is applied.
    instanceGroupId :: Prelude.Text,
    -- | Specifies the definition of the automatic scaling policy.
    autoScalingPolicy :: AutoScalingPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAutoScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'putAutoScalingPolicy_clusterId' - Specifies the ID of a cluster. The instance group to which the automatic
-- scaling policy is applied is within this cluster.
--
-- 'instanceGroupId', 'putAutoScalingPolicy_instanceGroupId' - Specifies the ID of the instance group to which the automatic scaling
-- policy is applied.
--
-- 'autoScalingPolicy', 'putAutoScalingPolicy_autoScalingPolicy' - Specifies the definition of the automatic scaling policy.
newPutAutoScalingPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'instanceGroupId'
  Prelude.Text ->
  -- | 'autoScalingPolicy'
  AutoScalingPolicy ->
  PutAutoScalingPolicy
newPutAutoScalingPolicy
  pClusterId_
  pInstanceGroupId_
  pAutoScalingPolicy_ =
    PutAutoScalingPolicy'
      { clusterId = pClusterId_,
        instanceGroupId = pInstanceGroupId_,
        autoScalingPolicy = pAutoScalingPolicy_
      }

-- | Specifies the ID of a cluster. The instance group to which the automatic
-- scaling policy is applied is within this cluster.
putAutoScalingPolicy_clusterId :: Lens.Lens' PutAutoScalingPolicy Prelude.Text
putAutoScalingPolicy_clusterId = Lens.lens (\PutAutoScalingPolicy' {clusterId} -> clusterId) (\s@PutAutoScalingPolicy' {} a -> s {clusterId = a} :: PutAutoScalingPolicy)

-- | Specifies the ID of the instance group to which the automatic scaling
-- policy is applied.
putAutoScalingPolicy_instanceGroupId :: Lens.Lens' PutAutoScalingPolicy Prelude.Text
putAutoScalingPolicy_instanceGroupId = Lens.lens (\PutAutoScalingPolicy' {instanceGroupId} -> instanceGroupId) (\s@PutAutoScalingPolicy' {} a -> s {instanceGroupId = a} :: PutAutoScalingPolicy)

-- | Specifies the definition of the automatic scaling policy.
putAutoScalingPolicy_autoScalingPolicy :: Lens.Lens' PutAutoScalingPolicy AutoScalingPolicy
putAutoScalingPolicy_autoScalingPolicy = Lens.lens (\PutAutoScalingPolicy' {autoScalingPolicy} -> autoScalingPolicy) (\s@PutAutoScalingPolicy' {} a -> s {autoScalingPolicy = a} :: PutAutoScalingPolicy)

instance Core.AWSRequest PutAutoScalingPolicy where
  type
    AWSResponse PutAutoScalingPolicy =
      PutAutoScalingPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutAutoScalingPolicyResponse'
            Prelude.<$> (x Core..?> "ClusterArn")
            Prelude.<*> (x Core..?> "ClusterId")
            Prelude.<*> (x Core..?> "InstanceGroupId")
            Prelude.<*> (x Core..?> "AutoScalingPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAutoScalingPolicy

instance Prelude.NFData PutAutoScalingPolicy

instance Core.ToHeaders PutAutoScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.PutAutoScalingPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAutoScalingPolicy where
  toJSON PutAutoScalingPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Core..= clusterId),
            Prelude.Just
              ("InstanceGroupId" Core..= instanceGroupId),
            Prelude.Just
              ("AutoScalingPolicy" Core..= autoScalingPolicy)
          ]
      )

instance Core.ToPath PutAutoScalingPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery PutAutoScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAutoScalingPolicyResponse' smart constructor.
data PutAutoScalingPolicyResponse = PutAutoScalingPolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of a cluster. The instance group to which the automatic
    -- scaling policy is applied is within this cluster.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID of the instance group to which the scaling policy is
    -- applied.
    instanceGroupId :: Prelude.Maybe Prelude.Text,
    -- | The automatic scaling policy definition.
    autoScalingPolicy :: Prelude.Maybe AutoScalingPolicyDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAutoScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'putAutoScalingPolicyResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterId', 'putAutoScalingPolicyResponse_clusterId' - Specifies the ID of a cluster. The instance group to which the automatic
-- scaling policy is applied is within this cluster.
--
-- 'instanceGroupId', 'putAutoScalingPolicyResponse_instanceGroupId' - Specifies the ID of the instance group to which the scaling policy is
-- applied.
--
-- 'autoScalingPolicy', 'putAutoScalingPolicyResponse_autoScalingPolicy' - The automatic scaling policy definition.
--
-- 'httpStatus', 'putAutoScalingPolicyResponse_httpStatus' - The response's http status code.
newPutAutoScalingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAutoScalingPolicyResponse
newPutAutoScalingPolicyResponse pHttpStatus_ =
  PutAutoScalingPolicyResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterId = Prelude.Nothing,
      instanceGroupId = Prelude.Nothing,
      autoScalingPolicy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
putAutoScalingPolicyResponse_clusterArn :: Lens.Lens' PutAutoScalingPolicyResponse (Prelude.Maybe Prelude.Text)
putAutoScalingPolicyResponse_clusterArn = Lens.lens (\PutAutoScalingPolicyResponse' {clusterArn} -> clusterArn) (\s@PutAutoScalingPolicyResponse' {} a -> s {clusterArn = a} :: PutAutoScalingPolicyResponse)

-- | Specifies the ID of a cluster. The instance group to which the automatic
-- scaling policy is applied is within this cluster.
putAutoScalingPolicyResponse_clusterId :: Lens.Lens' PutAutoScalingPolicyResponse (Prelude.Maybe Prelude.Text)
putAutoScalingPolicyResponse_clusterId = Lens.lens (\PutAutoScalingPolicyResponse' {clusterId} -> clusterId) (\s@PutAutoScalingPolicyResponse' {} a -> s {clusterId = a} :: PutAutoScalingPolicyResponse)

-- | Specifies the ID of the instance group to which the scaling policy is
-- applied.
putAutoScalingPolicyResponse_instanceGroupId :: Lens.Lens' PutAutoScalingPolicyResponse (Prelude.Maybe Prelude.Text)
putAutoScalingPolicyResponse_instanceGroupId = Lens.lens (\PutAutoScalingPolicyResponse' {instanceGroupId} -> instanceGroupId) (\s@PutAutoScalingPolicyResponse' {} a -> s {instanceGroupId = a} :: PutAutoScalingPolicyResponse)

-- | The automatic scaling policy definition.
putAutoScalingPolicyResponse_autoScalingPolicy :: Lens.Lens' PutAutoScalingPolicyResponse (Prelude.Maybe AutoScalingPolicyDescription)
putAutoScalingPolicyResponse_autoScalingPolicy = Lens.lens (\PutAutoScalingPolicyResponse' {autoScalingPolicy} -> autoScalingPolicy) (\s@PutAutoScalingPolicyResponse' {} a -> s {autoScalingPolicy = a} :: PutAutoScalingPolicyResponse)

-- | The response's http status code.
putAutoScalingPolicyResponse_httpStatus :: Lens.Lens' PutAutoScalingPolicyResponse Prelude.Int
putAutoScalingPolicyResponse_httpStatus = Lens.lens (\PutAutoScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@PutAutoScalingPolicyResponse' {} a -> s {httpStatus = a} :: PutAutoScalingPolicyResponse)

instance Prelude.NFData PutAutoScalingPolicyResponse

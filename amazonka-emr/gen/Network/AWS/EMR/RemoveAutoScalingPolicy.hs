{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EMR.RemoveAutoScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an automatic scaling policy from a specified instance group
-- within an EMR cluster.
module Network.AWS.EMR.RemoveAutoScalingPolicy
  ( -- * Creating a Request
    RemoveAutoScalingPolicy (..),
    newRemoveAutoScalingPolicy,

    -- * Request Lenses
    removeAutoScalingPolicy_clusterId,
    removeAutoScalingPolicy_instanceGroupId,

    -- * Destructuring the Response
    RemoveAutoScalingPolicyResponse (..),
    newRemoveAutoScalingPolicyResponse,

    -- * Response Lenses
    removeAutoScalingPolicyResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveAutoScalingPolicy' smart constructor.
data RemoveAutoScalingPolicy = RemoveAutoScalingPolicy'
  { -- | Specifies the ID of a cluster. The instance group to which the automatic
    -- scaling policy is applied is within this cluster.
    clusterId :: Prelude.Text,
    -- | Specifies the ID of the instance group to which the scaling policy is
    -- applied.
    instanceGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveAutoScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'removeAutoScalingPolicy_clusterId' - Specifies the ID of a cluster. The instance group to which the automatic
-- scaling policy is applied is within this cluster.
--
-- 'instanceGroupId', 'removeAutoScalingPolicy_instanceGroupId' - Specifies the ID of the instance group to which the scaling policy is
-- applied.
newRemoveAutoScalingPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'instanceGroupId'
  Prelude.Text ->
  RemoveAutoScalingPolicy
newRemoveAutoScalingPolicy
  pClusterId_
  pInstanceGroupId_ =
    RemoveAutoScalingPolicy'
      { clusterId = pClusterId_,
        instanceGroupId = pInstanceGroupId_
      }

-- | Specifies the ID of a cluster. The instance group to which the automatic
-- scaling policy is applied is within this cluster.
removeAutoScalingPolicy_clusterId :: Lens.Lens' RemoveAutoScalingPolicy Prelude.Text
removeAutoScalingPolicy_clusterId = Lens.lens (\RemoveAutoScalingPolicy' {clusterId} -> clusterId) (\s@RemoveAutoScalingPolicy' {} a -> s {clusterId = a} :: RemoveAutoScalingPolicy)

-- | Specifies the ID of the instance group to which the scaling policy is
-- applied.
removeAutoScalingPolicy_instanceGroupId :: Lens.Lens' RemoveAutoScalingPolicy Prelude.Text
removeAutoScalingPolicy_instanceGroupId = Lens.lens (\RemoveAutoScalingPolicy' {instanceGroupId} -> instanceGroupId) (\s@RemoveAutoScalingPolicy' {} a -> s {instanceGroupId = a} :: RemoveAutoScalingPolicy)

instance Prelude.AWSRequest RemoveAutoScalingPolicy where
  type
    Rs RemoveAutoScalingPolicy =
      RemoveAutoScalingPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveAutoScalingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveAutoScalingPolicy

instance Prelude.NFData RemoveAutoScalingPolicy

instance Prelude.ToHeaders RemoveAutoScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.RemoveAutoScalingPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RemoveAutoScalingPolicy where
  toJSON RemoveAutoScalingPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Prelude..= clusterId),
            Prelude.Just
              ("InstanceGroupId" Prelude..= instanceGroupId)
          ]
      )

instance Prelude.ToPath RemoveAutoScalingPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemoveAutoScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveAutoScalingPolicyResponse' smart constructor.
data RemoveAutoScalingPolicyResponse = RemoveAutoScalingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveAutoScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeAutoScalingPolicyResponse_httpStatus' - The response's http status code.
newRemoveAutoScalingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveAutoScalingPolicyResponse
newRemoveAutoScalingPolicyResponse pHttpStatus_ =
  RemoveAutoScalingPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeAutoScalingPolicyResponse_httpStatus :: Lens.Lens' RemoveAutoScalingPolicyResponse Prelude.Int
removeAutoScalingPolicyResponse_httpStatus = Lens.lens (\RemoveAutoScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@RemoveAutoScalingPolicyResponse' {} a -> s {httpStatus = a} :: RemoveAutoScalingPolicyResponse)

instance
  Prelude.NFData
    RemoveAutoScalingPolicyResponse

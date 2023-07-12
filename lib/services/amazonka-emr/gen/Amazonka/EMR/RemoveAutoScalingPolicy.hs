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
-- Module      : Amazonka.EMR.RemoveAutoScalingPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an automatic scaling policy from a specified instance group
-- within an EMR cluster.
module Amazonka.EMR.RemoveAutoScalingPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveAutoScalingPolicy' smart constructor.
data RemoveAutoScalingPolicy = RemoveAutoScalingPolicy'
  { -- | Specifies the ID of a cluster. The instance group to which the automatic
    -- scaling policy is applied is within this cluster.
    clusterId :: Prelude.Text,
    -- | Specifies the ID of the instance group to which the scaling policy is
    -- applied.
    instanceGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest RemoveAutoScalingPolicy where
  type
    AWSResponse RemoveAutoScalingPolicy =
      RemoveAutoScalingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveAutoScalingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveAutoScalingPolicy where
  hashWithSalt _salt RemoveAutoScalingPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` instanceGroupId

instance Prelude.NFData RemoveAutoScalingPolicy where
  rnf RemoveAutoScalingPolicy' {..} =
    Prelude.rnf clusterId
      `Prelude.seq` Prelude.rnf instanceGroupId

instance Data.ToHeaders RemoveAutoScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.RemoveAutoScalingPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveAutoScalingPolicy where
  toJSON RemoveAutoScalingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Data..= clusterId),
            Prelude.Just
              ("InstanceGroupId" Data..= instanceGroupId)
          ]
      )

instance Data.ToPath RemoveAutoScalingPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveAutoScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveAutoScalingPolicyResponse' smart constructor.
data RemoveAutoScalingPolicyResponse = RemoveAutoScalingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf RemoveAutoScalingPolicyResponse' {..} =
    Prelude.rnf httpStatus

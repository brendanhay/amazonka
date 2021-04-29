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
-- Module      : Network.AWS.EMR.RemoveManagedScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a managed scaling policy from a specified EMR cluster.
module Network.AWS.EMR.RemoveManagedScalingPolicy
  ( -- * Creating a Request
    RemoveManagedScalingPolicy (..),
    newRemoveManagedScalingPolicy,

    -- * Request Lenses
    removeManagedScalingPolicy_clusterId,

    -- * Destructuring the Response
    RemoveManagedScalingPolicyResponse (..),
    newRemoveManagedScalingPolicyResponse,

    -- * Response Lenses
    removeManagedScalingPolicyResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveManagedScalingPolicy' smart constructor.
data RemoveManagedScalingPolicy = RemoveManagedScalingPolicy'
  { -- | Specifies the ID of the cluster from which the managed scaling policy
    -- will be removed.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveManagedScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'removeManagedScalingPolicy_clusterId' - Specifies the ID of the cluster from which the managed scaling policy
-- will be removed.
newRemoveManagedScalingPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  RemoveManagedScalingPolicy
newRemoveManagedScalingPolicy pClusterId_ =
  RemoveManagedScalingPolicy'
    { clusterId =
        pClusterId_
    }

-- | Specifies the ID of the cluster from which the managed scaling policy
-- will be removed.
removeManagedScalingPolicy_clusterId :: Lens.Lens' RemoveManagedScalingPolicy Prelude.Text
removeManagedScalingPolicy_clusterId = Lens.lens (\RemoveManagedScalingPolicy' {clusterId} -> clusterId) (\s@RemoveManagedScalingPolicy' {} a -> s {clusterId = a} :: RemoveManagedScalingPolicy)

instance
  Prelude.AWSRequest
    RemoveManagedScalingPolicy
  where
  type
    Rs RemoveManagedScalingPolicy =
      RemoveManagedScalingPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveManagedScalingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveManagedScalingPolicy

instance Prelude.NFData RemoveManagedScalingPolicy

instance Prelude.ToHeaders RemoveManagedScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.RemoveManagedScalingPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RemoveManagedScalingPolicy where
  toJSON RemoveManagedScalingPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Prelude..= clusterId)]
      )

instance Prelude.ToPath RemoveManagedScalingPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemoveManagedScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveManagedScalingPolicyResponse' smart constructor.
data RemoveManagedScalingPolicyResponse = RemoveManagedScalingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveManagedScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeManagedScalingPolicyResponse_httpStatus' - The response's http status code.
newRemoveManagedScalingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveManagedScalingPolicyResponse
newRemoveManagedScalingPolicyResponse pHttpStatus_ =
  RemoveManagedScalingPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeManagedScalingPolicyResponse_httpStatus :: Lens.Lens' RemoveManagedScalingPolicyResponse Prelude.Int
removeManagedScalingPolicyResponse_httpStatus = Lens.lens (\RemoveManagedScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@RemoveManagedScalingPolicyResponse' {} a -> s {httpStatus = a} :: RemoveManagedScalingPolicyResponse)

instance
  Prelude.NFData
    RemoveManagedScalingPolicyResponse

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
-- Module      : Network.AWS.EMR.PutManagedScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a managed scaling policy for an Amazon EMR cluster.
-- The managed scaling policy defines the limits for resources, such as EC2
-- instances that can be added or terminated from a cluster. The policy
-- only applies to the core and task nodes. The master node cannot be
-- scaled after initial configuration.
module Network.AWS.EMR.PutManagedScalingPolicy
  ( -- * Creating a Request
    PutManagedScalingPolicy (..),
    newPutManagedScalingPolicy,

    -- * Request Lenses
    putManagedScalingPolicy_clusterId,
    putManagedScalingPolicy_managedScalingPolicy,

    -- * Destructuring the Response
    PutManagedScalingPolicyResponse (..),
    newPutManagedScalingPolicyResponse,

    -- * Response Lenses
    putManagedScalingPolicyResponse_httpStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutManagedScalingPolicy' smart constructor.
data PutManagedScalingPolicy = PutManagedScalingPolicy'
  { -- | Specifies the ID of an EMR cluster where the managed scaling policy is
    -- attached.
    clusterId :: Prelude.Text,
    -- | Specifies the constraints for the managed scaling policy.
    managedScalingPolicy :: ManagedScalingPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutManagedScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'putManagedScalingPolicy_clusterId' - Specifies the ID of an EMR cluster where the managed scaling policy is
-- attached.
--
-- 'managedScalingPolicy', 'putManagedScalingPolicy_managedScalingPolicy' - Specifies the constraints for the managed scaling policy.
newPutManagedScalingPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  -- | 'managedScalingPolicy'
  ManagedScalingPolicy ->
  PutManagedScalingPolicy
newPutManagedScalingPolicy
  pClusterId_
  pManagedScalingPolicy_ =
    PutManagedScalingPolicy'
      { clusterId = pClusterId_,
        managedScalingPolicy = pManagedScalingPolicy_
      }

-- | Specifies the ID of an EMR cluster where the managed scaling policy is
-- attached.
putManagedScalingPolicy_clusterId :: Lens.Lens' PutManagedScalingPolicy Prelude.Text
putManagedScalingPolicy_clusterId = Lens.lens (\PutManagedScalingPolicy' {clusterId} -> clusterId) (\s@PutManagedScalingPolicy' {} a -> s {clusterId = a} :: PutManagedScalingPolicy)

-- | Specifies the constraints for the managed scaling policy.
putManagedScalingPolicy_managedScalingPolicy :: Lens.Lens' PutManagedScalingPolicy ManagedScalingPolicy
putManagedScalingPolicy_managedScalingPolicy = Lens.lens (\PutManagedScalingPolicy' {managedScalingPolicy} -> managedScalingPolicy) (\s@PutManagedScalingPolicy' {} a -> s {managedScalingPolicy = a} :: PutManagedScalingPolicy)

instance Prelude.AWSRequest PutManagedScalingPolicy where
  type
    Rs PutManagedScalingPolicy =
      PutManagedScalingPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutManagedScalingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutManagedScalingPolicy

instance Prelude.NFData PutManagedScalingPolicy

instance Prelude.ToHeaders PutManagedScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.PutManagedScalingPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutManagedScalingPolicy where
  toJSON PutManagedScalingPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClusterId" Prelude..= clusterId),
            Prelude.Just
              ( "ManagedScalingPolicy"
                  Prelude..= managedScalingPolicy
              )
          ]
      )

instance Prelude.ToPath PutManagedScalingPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutManagedScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutManagedScalingPolicyResponse' smart constructor.
data PutManagedScalingPolicyResponse = PutManagedScalingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutManagedScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putManagedScalingPolicyResponse_httpStatus' - The response's http status code.
newPutManagedScalingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutManagedScalingPolicyResponse
newPutManagedScalingPolicyResponse pHttpStatus_ =
  PutManagedScalingPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putManagedScalingPolicyResponse_httpStatus :: Lens.Lens' PutManagedScalingPolicyResponse Prelude.Int
putManagedScalingPolicyResponse_httpStatus = Lens.lens (\PutManagedScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@PutManagedScalingPolicyResponse' {} a -> s {httpStatus = a} :: PutManagedScalingPolicyResponse)

instance
  Prelude.NFData
    PutManagedScalingPolicyResponse

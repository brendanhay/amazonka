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
-- Module      : Network.AWS.EMR.RemoveAutoTerminationPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an auto-termination policy from an Amazon EMR cluster.
module Network.AWS.EMR.RemoveAutoTerminationPolicy
  ( -- * Creating a Request
    RemoveAutoTerminationPolicy (..),
    newRemoveAutoTerminationPolicy,

    -- * Request Lenses
    removeAutoTerminationPolicy_clusterId,

    -- * Destructuring the Response
    RemoveAutoTerminationPolicyResponse (..),
    newRemoveAutoTerminationPolicyResponse,

    -- * Response Lenses
    removeAutoTerminationPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveAutoTerminationPolicy' smart constructor.
data RemoveAutoTerminationPolicy = RemoveAutoTerminationPolicy'
  { -- | Specifies the ID of the Amazon EMR cluster from which the
    -- auto-termination policy will be removed.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveAutoTerminationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterId', 'removeAutoTerminationPolicy_clusterId' - Specifies the ID of the Amazon EMR cluster from which the
-- auto-termination policy will be removed.
newRemoveAutoTerminationPolicy ::
  -- | 'clusterId'
  Prelude.Text ->
  RemoveAutoTerminationPolicy
newRemoveAutoTerminationPolicy pClusterId_ =
  RemoveAutoTerminationPolicy'
    { clusterId =
        pClusterId_
    }

-- | Specifies the ID of the Amazon EMR cluster from which the
-- auto-termination policy will be removed.
removeAutoTerminationPolicy_clusterId :: Lens.Lens' RemoveAutoTerminationPolicy Prelude.Text
removeAutoTerminationPolicy_clusterId = Lens.lens (\RemoveAutoTerminationPolicy' {clusterId} -> clusterId) (\s@RemoveAutoTerminationPolicy' {} a -> s {clusterId = a} :: RemoveAutoTerminationPolicy)

instance Core.AWSRequest RemoveAutoTerminationPolicy where
  type
    AWSResponse RemoveAutoTerminationPolicy =
      RemoveAutoTerminationPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveAutoTerminationPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveAutoTerminationPolicy

instance Prelude.NFData RemoveAutoTerminationPolicy

instance Core.ToHeaders RemoveAutoTerminationPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.RemoveAutoTerminationPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveAutoTerminationPolicy where
  toJSON RemoveAutoTerminationPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Core..= clusterId)]
      )

instance Core.ToPath RemoveAutoTerminationPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveAutoTerminationPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveAutoTerminationPolicyResponse' smart constructor.
data RemoveAutoTerminationPolicyResponse = RemoveAutoTerminationPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveAutoTerminationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeAutoTerminationPolicyResponse_httpStatus' - The response's http status code.
newRemoveAutoTerminationPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveAutoTerminationPolicyResponse
newRemoveAutoTerminationPolicyResponse pHttpStatus_ =
  RemoveAutoTerminationPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeAutoTerminationPolicyResponse_httpStatus :: Lens.Lens' RemoveAutoTerminationPolicyResponse Prelude.Int
removeAutoTerminationPolicyResponse_httpStatus = Lens.lens (\RemoveAutoTerminationPolicyResponse' {httpStatus} -> httpStatus) (\s@RemoveAutoTerminationPolicyResponse' {} a -> s {httpStatus = a} :: RemoveAutoTerminationPolicyResponse)

instance
  Prelude.NFData
    RemoveAutoTerminationPolicyResponse

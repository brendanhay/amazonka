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
-- Module      : Amazonka.EMR.RemoveManagedScalingPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a managed scaling policy from a specified Amazon EMR cluster.
module Amazonka.EMR.RemoveManagedScalingPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveManagedScalingPolicy' smart constructor.
data RemoveManagedScalingPolicy = RemoveManagedScalingPolicy'
  { -- | Specifies the ID of the cluster from which the managed scaling policy
    -- will be removed.
    clusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest RemoveManagedScalingPolicy where
  type
    AWSResponse RemoveManagedScalingPolicy =
      RemoveManagedScalingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveManagedScalingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveManagedScalingPolicy where
  hashWithSalt _salt RemoveManagedScalingPolicy' {..} =
    _salt `Prelude.hashWithSalt` clusterId

instance Prelude.NFData RemoveManagedScalingPolicy where
  rnf RemoveManagedScalingPolicy' {..} =
    Prelude.rnf clusterId

instance Data.ToHeaders RemoveManagedScalingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.RemoveManagedScalingPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveManagedScalingPolicy where
  toJSON RemoveManagedScalingPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Data..= clusterId)]
      )

instance Data.ToPath RemoveManagedScalingPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveManagedScalingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveManagedScalingPolicyResponse' smart constructor.
data RemoveManagedScalingPolicyResponse = RemoveManagedScalingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf RemoveManagedScalingPolicyResponse' {..} =
    Prelude.rnf httpStatus

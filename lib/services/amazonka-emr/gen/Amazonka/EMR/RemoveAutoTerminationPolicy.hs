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
-- Module      : Amazonka.EMR.RemoveAutoTerminationPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an auto-termination policy from an Amazon EMR cluster.
module Amazonka.EMR.RemoveAutoTerminationPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveAutoTerminationPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveAutoTerminationPolicy where
  hashWithSalt _salt RemoveAutoTerminationPolicy' {..} =
    _salt `Prelude.hashWithSalt` clusterId

instance Prelude.NFData RemoveAutoTerminationPolicy where
  rnf RemoveAutoTerminationPolicy' {..} =
    Prelude.rnf clusterId

instance Data.ToHeaders RemoveAutoTerminationPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.RemoveAutoTerminationPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveAutoTerminationPolicy where
  toJSON RemoveAutoTerminationPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ClusterId" Data..= clusterId)]
      )

instance Data.ToPath RemoveAutoTerminationPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveAutoTerminationPolicy where
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
  where
  rnf RemoveAutoTerminationPolicyResponse' {..} =
    Prelude.rnf httpStatus

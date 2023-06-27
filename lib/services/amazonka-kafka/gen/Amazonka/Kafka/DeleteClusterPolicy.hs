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
-- Module      : Amazonka.Kafka.DeleteClusterPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the MSK cluster policy specified by the Amazon Resource Name
-- (ARN) in the request.
module Amazonka.Kafka.DeleteClusterPolicy
  ( -- * Creating a Request
    DeleteClusterPolicy (..),
    newDeleteClusterPolicy,

    -- * Request Lenses
    deleteClusterPolicy_clusterArn,

    -- * Destructuring the Response
    DeleteClusterPolicyResponse (..),
    newDeleteClusterPolicyResponse,

    -- * Response Lenses
    deleteClusterPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteClusterPolicy' smart constructor.
data DeleteClusterPolicy = DeleteClusterPolicy'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'deleteClusterPolicy_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
newDeleteClusterPolicy ::
  -- | 'clusterArn'
  Prelude.Text ->
  DeleteClusterPolicy
newDeleteClusterPolicy pClusterArn_ =
  DeleteClusterPolicy' {clusterArn = pClusterArn_}

-- | The Amazon Resource Name (ARN) of the cluster.
deleteClusterPolicy_clusterArn :: Lens.Lens' DeleteClusterPolicy Prelude.Text
deleteClusterPolicy_clusterArn = Lens.lens (\DeleteClusterPolicy' {clusterArn} -> clusterArn) (\s@DeleteClusterPolicy' {} a -> s {clusterArn = a} :: DeleteClusterPolicy)

instance Core.AWSRequest DeleteClusterPolicy where
  type
    AWSResponse DeleteClusterPolicy =
      DeleteClusterPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteClusterPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteClusterPolicy where
  hashWithSalt _salt DeleteClusterPolicy' {..} =
    _salt `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData DeleteClusterPolicy where
  rnf DeleteClusterPolicy' {..} = Prelude.rnf clusterArn

instance Data.ToHeaders DeleteClusterPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteClusterPolicy where
  toPath DeleteClusterPolicy' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Data.toBS clusterArn, "/policy"]

instance Data.ToQuery DeleteClusterPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteClusterPolicyResponse' smart constructor.
data DeleteClusterPolicyResponse = DeleteClusterPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteClusterPolicyResponse_httpStatus' - The response's http status code.
newDeleteClusterPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteClusterPolicyResponse
newDeleteClusterPolicyResponse pHttpStatus_ =
  DeleteClusterPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteClusterPolicyResponse_httpStatus :: Lens.Lens' DeleteClusterPolicyResponse Prelude.Int
deleteClusterPolicyResponse_httpStatus = Lens.lens (\DeleteClusterPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteClusterPolicyResponse' {} a -> s {httpStatus = a} :: DeleteClusterPolicyResponse)

instance Prelude.NFData DeleteClusterPolicyResponse where
  rnf DeleteClusterPolicyResponse' {..} =
    Prelude.rnf httpStatus

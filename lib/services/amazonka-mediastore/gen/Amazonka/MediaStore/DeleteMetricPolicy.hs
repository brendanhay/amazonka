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
-- Module      : Amazonka.MediaStore.DeleteMetricPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the metric policy that is associated with the specified
-- container. If there is no metric policy associated with the container,
-- MediaStore doesn\'t send metrics to CloudWatch.
module Amazonka.MediaStore.DeleteMetricPolicy
  ( -- * Creating a Request
    DeleteMetricPolicy (..),
    newDeleteMetricPolicy,

    -- * Request Lenses
    deleteMetricPolicy_containerName,

    -- * Destructuring the Response
    DeleteMetricPolicyResponse (..),
    newDeleteMetricPolicyResponse,

    -- * Response Lenses
    deleteMetricPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMetricPolicy' smart constructor.
data DeleteMetricPolicy = DeleteMetricPolicy'
  { -- | The name of the container that is associated with the metric policy that
    -- you want to delete.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMetricPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'deleteMetricPolicy_containerName' - The name of the container that is associated with the metric policy that
-- you want to delete.
newDeleteMetricPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  DeleteMetricPolicy
newDeleteMetricPolicy pContainerName_ =
  DeleteMetricPolicy'
    { containerName =
        pContainerName_
    }

-- | The name of the container that is associated with the metric policy that
-- you want to delete.
deleteMetricPolicy_containerName :: Lens.Lens' DeleteMetricPolicy Prelude.Text
deleteMetricPolicy_containerName = Lens.lens (\DeleteMetricPolicy' {containerName} -> containerName) (\s@DeleteMetricPolicy' {} a -> s {containerName = a} :: DeleteMetricPolicy)

instance Core.AWSRequest DeleteMetricPolicy where
  type
    AWSResponse DeleteMetricPolicy =
      DeleteMetricPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMetricPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMetricPolicy where
  hashWithSalt _salt DeleteMetricPolicy' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData DeleteMetricPolicy where
  rnf DeleteMetricPolicy' {..} =
    Prelude.rnf containerName

instance Data.ToHeaders DeleteMetricPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.DeleteMetricPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteMetricPolicy where
  toJSON DeleteMetricPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName)
          ]
      )

instance Data.ToPath DeleteMetricPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteMetricPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMetricPolicyResponse' smart constructor.
data DeleteMetricPolicyResponse = DeleteMetricPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMetricPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMetricPolicyResponse_httpStatus' - The response's http status code.
newDeleteMetricPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMetricPolicyResponse
newDeleteMetricPolicyResponse pHttpStatus_ =
  DeleteMetricPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMetricPolicyResponse_httpStatus :: Lens.Lens' DeleteMetricPolicyResponse Prelude.Int
deleteMetricPolicyResponse_httpStatus = Lens.lens (\DeleteMetricPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteMetricPolicyResponse' {} a -> s {httpStatus = a} :: DeleteMetricPolicyResponse)

instance Prelude.NFData DeleteMetricPolicyResponse where
  rnf DeleteMetricPolicyResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.CodeDeploy.DeleteResourcesByExternalId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes resources linked to an external ID.
module Amazonka.CodeDeploy.DeleteResourcesByExternalId
  ( -- * Creating a Request
    DeleteResourcesByExternalId (..),
    newDeleteResourcesByExternalId,

    -- * Request Lenses
    deleteResourcesByExternalId_externalId,

    -- * Destructuring the Response
    DeleteResourcesByExternalIdResponse (..),
    newDeleteResourcesByExternalIdResponse,

    -- * Response Lenses
    deleteResourcesByExternalIdResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourcesByExternalId' smart constructor.
data DeleteResourcesByExternalId = DeleteResourcesByExternalId'
  { -- | The unique ID of an external resource (for example, a CloudFormation
    -- stack ID) that is linked to one or more CodeDeploy resources.
    externalId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcesByExternalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'deleteResourcesByExternalId_externalId' - The unique ID of an external resource (for example, a CloudFormation
-- stack ID) that is linked to one or more CodeDeploy resources.
newDeleteResourcesByExternalId ::
  DeleteResourcesByExternalId
newDeleteResourcesByExternalId =
  DeleteResourcesByExternalId'
    { externalId =
        Prelude.Nothing
    }

-- | The unique ID of an external resource (for example, a CloudFormation
-- stack ID) that is linked to one or more CodeDeploy resources.
deleteResourcesByExternalId_externalId :: Lens.Lens' DeleteResourcesByExternalId (Prelude.Maybe Prelude.Text)
deleteResourcesByExternalId_externalId = Lens.lens (\DeleteResourcesByExternalId' {externalId} -> externalId) (\s@DeleteResourcesByExternalId' {} a -> s {externalId = a} :: DeleteResourcesByExternalId)

instance Core.AWSRequest DeleteResourcesByExternalId where
  type
    AWSResponse DeleteResourcesByExternalId =
      DeleteResourcesByExternalIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcesByExternalIdResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourcesByExternalId where
  hashWithSalt _salt DeleteResourcesByExternalId' {..} =
    _salt `Prelude.hashWithSalt` externalId

instance Prelude.NFData DeleteResourcesByExternalId where
  rnf DeleteResourcesByExternalId' {..} =
    Prelude.rnf externalId

instance Data.ToHeaders DeleteResourcesByExternalId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.DeleteResourcesByExternalId" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourcesByExternalId where
  toJSON DeleteResourcesByExternalId' {..} =
    Data.object
      ( Prelude.catMaybes
          [("externalId" Data..=) Prelude.<$> externalId]
      )

instance Data.ToPath DeleteResourcesByExternalId where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResourcesByExternalId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourcesByExternalIdResponse' smart constructor.
data DeleteResourcesByExternalIdResponse = DeleteResourcesByExternalIdResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcesByExternalIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourcesByExternalIdResponse_httpStatus' - The response's http status code.
newDeleteResourcesByExternalIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcesByExternalIdResponse
newDeleteResourcesByExternalIdResponse pHttpStatus_ =
  DeleteResourcesByExternalIdResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourcesByExternalIdResponse_httpStatus :: Lens.Lens' DeleteResourcesByExternalIdResponse Prelude.Int
deleteResourcesByExternalIdResponse_httpStatus = Lens.lens (\DeleteResourcesByExternalIdResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcesByExternalIdResponse' {} a -> s {httpStatus = a} :: DeleteResourcesByExternalIdResponse)

instance
  Prelude.NFData
    DeleteResourcesByExternalIdResponse
  where
  rnf DeleteResourcesByExternalIdResponse' {..} =
    Prelude.rnf httpStatus

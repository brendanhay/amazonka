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
-- Module      : Amazonka.ApplicationInsights.DeleteApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified application from monitoring. Does not delete the
-- application.
module Amazonka.ApplicationInsights.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_resourceGroupName,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,

    -- * Response Lenses
    deleteApplicationResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupName', 'deleteApplication_resourceGroupName' - The name of the resource group.
newDeleteApplication ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  DeleteApplication
newDeleteApplication pResourceGroupName_ =
  DeleteApplication'
    { resourceGroupName =
        pResourceGroupName_
    }

-- | The name of the resource group.
deleteApplication_resourceGroupName :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_resourceGroupName = Lens.lens (\DeleteApplication' {resourceGroupName} -> resourceGroupName) (\s@DeleteApplication' {} a -> s {resourceGroupName = a} :: DeleteApplication)

instance Core.AWSRequest DeleteApplication where
  type
    AWSResponse DeleteApplication =
      DeleteApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplication where
  hashWithSalt _salt DeleteApplication' {..} =
    _salt `Prelude.hashWithSalt` resourceGroupName

instance Prelude.NFData DeleteApplication where
  rnf DeleteApplication' {..} =
    Prelude.rnf resourceGroupName

instance Core.ToHeaders DeleteApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "EC2WindowsBarleyService.DeleteApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteApplication where
  toJSON DeleteApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceGroupName" Core..= resourceGroupName)
          ]
      )

instance Core.ToPath DeleteApplication where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationResponse_httpStatus' - The response's http status code.
newDeleteApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationResponse
newDeleteApplicationResponse pHttpStatus_ =
  DeleteApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApplicationResponse_httpStatus :: Lens.Lens' DeleteApplicationResponse Prelude.Int
deleteApplicationResponse_httpStatus = Lens.lens (\DeleteApplicationResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationResponse' {} a -> s {httpStatus = a} :: DeleteApplicationResponse)

instance Prelude.NFData DeleteApplicationResponse where
  rnf DeleteApplicationResponse' {..} =
    Prelude.rnf httpStatus

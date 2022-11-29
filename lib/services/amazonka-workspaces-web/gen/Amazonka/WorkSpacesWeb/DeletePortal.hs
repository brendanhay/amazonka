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
-- Module      : Amazonka.WorkSpacesWeb.DeletePortal
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a web portal.
module Amazonka.WorkSpacesWeb.DeletePortal
  ( -- * Creating a Request
    DeletePortal (..),
    newDeletePortal,

    -- * Request Lenses
    deletePortal_portalArn,

    -- * Destructuring the Response
    DeletePortalResponse (..),
    newDeletePortalResponse,

    -- * Response Lenses
    deletePortalResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeletePortal' smart constructor.
data DeletePortal = DeletePortal'
  { -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portalArn', 'deletePortal_portalArn' - The ARN of the web portal.
newDeletePortal ::
  -- | 'portalArn'
  Prelude.Text ->
  DeletePortal
newDeletePortal pPortalArn_ =
  DeletePortal' {portalArn = pPortalArn_}

-- | The ARN of the web portal.
deletePortal_portalArn :: Lens.Lens' DeletePortal Prelude.Text
deletePortal_portalArn = Lens.lens (\DeletePortal' {portalArn} -> portalArn) (\s@DeletePortal' {} a -> s {portalArn = a} :: DeletePortal)

instance Core.AWSRequest DeletePortal where
  type AWSResponse DeletePortal = DeletePortalResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePortalResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePortal where
  hashWithSalt _salt DeletePortal' {..} =
    _salt `Prelude.hashWithSalt` portalArn

instance Prelude.NFData DeletePortal where
  rnf DeletePortal' {..} = Prelude.rnf portalArn

instance Core.ToHeaders DeletePortal where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeletePortal where
  toPath DeletePortal' {..} =
    Prelude.mconcat ["/portals/", Core.toBS portalArn]

instance Core.ToQuery DeletePortal where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePortalResponse' smart constructor.
data DeletePortalResponse = DeletePortalResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePortalResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePortalResponse_httpStatus' - The response's http status code.
newDeletePortalResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePortalResponse
newDeletePortalResponse pHttpStatus_ =
  DeletePortalResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePortalResponse_httpStatus :: Lens.Lens' DeletePortalResponse Prelude.Int
deletePortalResponse_httpStatus = Lens.lens (\DeletePortalResponse' {httpStatus} -> httpStatus) (\s@DeletePortalResponse' {} a -> s {httpStatus = a} :: DeletePortalResponse)

instance Prelude.NFData DeletePortalResponse where
  rnf DeletePortalResponse' {..} =
    Prelude.rnf httpStatus

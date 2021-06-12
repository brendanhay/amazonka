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
-- Module      : Network.AWS.AppStream.DeleteStack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified stack. After the stack is deleted, the application
-- streaming environment provided by the stack is no longer available to
-- users. Also, any reservations made for application streaming sessions
-- for the stack are released.
module Network.AWS.AppStream.DeleteStack
  ( -- * Creating a Request
    DeleteStack (..),
    newDeleteStack,

    -- * Request Lenses
    deleteStack_name,

    -- * Destructuring the Response
    DeleteStackResponse (..),
    newDeleteStackResponse,

    -- * Response Lenses
    deleteStackResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStack' smart constructor.
data DeleteStack = DeleteStack'
  { -- | The name of the stack.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteStack_name' - The name of the stack.
newDeleteStack ::
  -- | 'name'
  Core.Text ->
  DeleteStack
newDeleteStack pName_ = DeleteStack' {name = pName_}

-- | The name of the stack.
deleteStack_name :: Lens.Lens' DeleteStack Core.Text
deleteStack_name = Lens.lens (\DeleteStack' {name} -> name) (\s@DeleteStack' {} a -> s {name = a} :: DeleteStack)

instance Core.AWSRequest DeleteStack where
  type AWSResponse DeleteStack = DeleteStackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStackResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteStack

instance Core.NFData DeleteStack

instance Core.ToHeaders DeleteStack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteStack" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteStack where
  toJSON DeleteStack' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteStack where
  toPath = Core.const "/"

instance Core.ToQuery DeleteStack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteStackResponse' smart constructor.
data DeleteStackResponse = DeleteStackResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStackResponse_httpStatus' - The response's http status code.
newDeleteStackResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteStackResponse
newDeleteStackResponse pHttpStatus_ =
  DeleteStackResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteStackResponse_httpStatus :: Lens.Lens' DeleteStackResponse Core.Int
deleteStackResponse_httpStatus = Lens.lens (\DeleteStackResponse' {httpStatus} -> httpStatus) (\s@DeleteStackResponse' {} a -> s {httpStatus = a} :: DeleteStackResponse)

instance Core.NFData DeleteStackResponse

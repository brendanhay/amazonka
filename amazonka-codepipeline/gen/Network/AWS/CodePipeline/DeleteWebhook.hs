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
-- Module      : Network.AWS.CodePipeline.DeleteWebhook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created webhook by name. Deleting the webhook stops
-- AWS CodePipeline from starting a pipeline every time an external event
-- occurs. The API returns successfully when trying to delete a webhook
-- that is already deleted. If a deleted webhook is re-created by calling
-- PutWebhook with the same name, it will have a different URL.
module Network.AWS.CodePipeline.DeleteWebhook
  ( -- * Creating a Request
    DeleteWebhook (..),
    newDeleteWebhook,

    -- * Request Lenses
    deleteWebhook_name,

    -- * Destructuring the Response
    DeleteWebhookResponse (..),
    newDeleteWebhookResponse,

    -- * Response Lenses
    deleteWebhookResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWebhook' smart constructor.
data DeleteWebhook = DeleteWebhook'
  { -- | The name of the webhook you want to delete.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWebhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteWebhook_name' - The name of the webhook you want to delete.
newDeleteWebhook ::
  -- | 'name'
  Core.Text ->
  DeleteWebhook
newDeleteWebhook pName_ =
  DeleteWebhook' {name = pName_}

-- | The name of the webhook you want to delete.
deleteWebhook_name :: Lens.Lens' DeleteWebhook Core.Text
deleteWebhook_name = Lens.lens (\DeleteWebhook' {name} -> name) (\s@DeleteWebhook' {} a -> s {name = a} :: DeleteWebhook)

instance Core.AWSRequest DeleteWebhook where
  type
    AWSResponse DeleteWebhook =
      DeleteWebhookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWebhookResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteWebhook

instance Core.NFData DeleteWebhook

instance Core.ToHeaders DeleteWebhook where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.DeleteWebhook" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteWebhook where
  toJSON DeleteWebhook' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("name" Core..= name)])

instance Core.ToPath DeleteWebhook where
  toPath = Core.const "/"

instance Core.ToQuery DeleteWebhook where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteWebhookResponse' smart constructor.
data DeleteWebhookResponse = DeleteWebhookResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWebhookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWebhookResponse_httpStatus' - The response's http status code.
newDeleteWebhookResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteWebhookResponse
newDeleteWebhookResponse pHttpStatus_ =
  DeleteWebhookResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWebhookResponse_httpStatus :: Lens.Lens' DeleteWebhookResponse Core.Int
deleteWebhookResponse_httpStatus = Lens.lens (\DeleteWebhookResponse' {httpStatus} -> httpStatus) (\s@DeleteWebhookResponse' {} a -> s {httpStatus = a} :: DeleteWebhookResponse)

instance Core.NFData DeleteWebhookResponse

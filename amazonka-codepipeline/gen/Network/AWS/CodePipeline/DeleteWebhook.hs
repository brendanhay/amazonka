{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWebhook' smart constructor.
data DeleteWebhook = DeleteWebhook'
  { -- | The name of the webhook you want to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteWebhook
newDeleteWebhook pName_ =
  DeleteWebhook' {name = pName_}

-- | The name of the webhook you want to delete.
deleteWebhook_name :: Lens.Lens' DeleteWebhook Prelude.Text
deleteWebhook_name = Lens.lens (\DeleteWebhook' {name} -> name) (\s@DeleteWebhook' {} a -> s {name = a} :: DeleteWebhook)

instance Prelude.AWSRequest DeleteWebhook where
  type Rs DeleteWebhook = DeleteWebhookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWebhookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWebhook

instance Prelude.NFData DeleteWebhook

instance Prelude.ToHeaders DeleteWebhook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.DeleteWebhook" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteWebhook where
  toJSON DeleteWebhook' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteWebhook where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteWebhook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWebhookResponse' smart constructor.
data DeleteWebhookResponse = DeleteWebhookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteWebhookResponse
newDeleteWebhookResponse pHttpStatus_ =
  DeleteWebhookResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWebhookResponse_httpStatus :: Lens.Lens' DeleteWebhookResponse Prelude.Int
deleteWebhookResponse_httpStatus = Lens.lens (\DeleteWebhookResponse' {httpStatus} -> httpStatus) (\s@DeleteWebhookResponse' {} a -> s {httpStatus = a} :: DeleteWebhookResponse)

instance Prelude.NFData DeleteWebhookResponse

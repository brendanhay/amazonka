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
-- Module      : Amazonka.CodeBuild.DeleteWebhook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For an existing CodeBuild build project that has its source code stored
-- in a GitHub or Bitbucket repository, stops CodeBuild from rebuilding the
-- source code every time a code change is pushed to the repository.
module Amazonka.CodeBuild.DeleteWebhook
  ( -- * Creating a Request
    DeleteWebhook (..),
    newDeleteWebhook,

    -- * Request Lenses
    deleteWebhook_projectName,

    -- * Destructuring the Response
    DeleteWebhookResponse (..),
    newDeleteWebhookResponse,

    -- * Response Lenses
    deleteWebhookResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWebhook' smart constructor.
data DeleteWebhook = DeleteWebhook'
  { -- | The name of the CodeBuild project.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWebhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'deleteWebhook_projectName' - The name of the CodeBuild project.
newDeleteWebhook ::
  -- | 'projectName'
  Prelude.Text ->
  DeleteWebhook
newDeleteWebhook pProjectName_ =
  DeleteWebhook' {projectName = pProjectName_}

-- | The name of the CodeBuild project.
deleteWebhook_projectName :: Lens.Lens' DeleteWebhook Prelude.Text
deleteWebhook_projectName = Lens.lens (\DeleteWebhook' {projectName} -> projectName) (\s@DeleteWebhook' {} a -> s {projectName = a} :: DeleteWebhook)

instance Core.AWSRequest DeleteWebhook where
  type
    AWSResponse DeleteWebhook =
      DeleteWebhookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWebhookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWebhook where
  hashWithSalt _salt DeleteWebhook' {..} =
    _salt `Prelude.hashWithSalt` projectName

instance Prelude.NFData DeleteWebhook where
  rnf DeleteWebhook' {..} = Prelude.rnf projectName

instance Data.ToHeaders DeleteWebhook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.DeleteWebhook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWebhook where
  toJSON DeleteWebhook' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("projectName" Data..= projectName)]
      )

instance Data.ToPath DeleteWebhook where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWebhook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWebhookResponse' smart constructor.
data DeleteWebhookResponse = DeleteWebhookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteWebhookResponse where
  rnf DeleteWebhookResponse' {..} =
    Prelude.rnf httpStatus

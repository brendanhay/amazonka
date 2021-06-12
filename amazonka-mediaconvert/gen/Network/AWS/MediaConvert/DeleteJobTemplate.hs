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
-- Module      : Network.AWS.MediaConvert.DeleteJobTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a job template you have created.
module Network.AWS.MediaConvert.DeleteJobTemplate
  ( -- * Creating a Request
    DeleteJobTemplate (..),
    newDeleteJobTemplate,

    -- * Request Lenses
    deleteJobTemplate_name,

    -- * Destructuring the Response
    DeleteJobTemplateResponse (..),
    newDeleteJobTemplateResponse,

    -- * Response Lenses
    deleteJobTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteJobTemplate' smart constructor.
data DeleteJobTemplate = DeleteJobTemplate'
  { -- | The name of the job template to be deleted.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJobTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteJobTemplate_name' - The name of the job template to be deleted.
newDeleteJobTemplate ::
  -- | 'name'
  Core.Text ->
  DeleteJobTemplate
newDeleteJobTemplate pName_ =
  DeleteJobTemplate' {name = pName_}

-- | The name of the job template to be deleted.
deleteJobTemplate_name :: Lens.Lens' DeleteJobTemplate Core.Text
deleteJobTemplate_name = Lens.lens (\DeleteJobTemplate' {name} -> name) (\s@DeleteJobTemplate' {} a -> s {name = a} :: DeleteJobTemplate)

instance Core.AWSRequest DeleteJobTemplate where
  type
    AWSResponse DeleteJobTemplate =
      DeleteJobTemplateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteJobTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteJobTemplate

instance Core.NFData DeleteJobTemplate

instance Core.ToHeaders DeleteJobTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteJobTemplate where
  toPath DeleteJobTemplate' {..} =
    Core.mconcat
      ["/2017-08-29/jobTemplates/", Core.toBS name]

instance Core.ToQuery DeleteJobTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteJobTemplateResponse' smart constructor.
data DeleteJobTemplateResponse = DeleteJobTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteJobTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteJobTemplateResponse_httpStatus' - The response's http status code.
newDeleteJobTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteJobTemplateResponse
newDeleteJobTemplateResponse pHttpStatus_ =
  DeleteJobTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteJobTemplateResponse_httpStatus :: Lens.Lens' DeleteJobTemplateResponse Core.Int
deleteJobTemplateResponse_httpStatus = Lens.lens (\DeleteJobTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteJobTemplateResponse' {} a -> s {httpStatus = a} :: DeleteJobTemplateResponse)

instance Core.NFData DeleteJobTemplateResponse

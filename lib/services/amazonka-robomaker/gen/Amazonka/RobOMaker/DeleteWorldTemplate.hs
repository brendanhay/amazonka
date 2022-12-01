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
-- Module      : Amazonka.RobOMaker.DeleteWorldTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a world template.
module Amazonka.RobOMaker.DeleteWorldTemplate
  ( -- * Creating a Request
    DeleteWorldTemplate (..),
    newDeleteWorldTemplate,

    -- * Request Lenses
    deleteWorldTemplate_template,

    -- * Destructuring the Response
    DeleteWorldTemplateResponse (..),
    newDeleteWorldTemplateResponse,

    -- * Response Lenses
    deleteWorldTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDeleteWorldTemplate' smart constructor.
data DeleteWorldTemplate = DeleteWorldTemplate'
  { -- | The Amazon Resource Name (arn) of the world template you want to delete.
    template :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorldTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'deleteWorldTemplate_template' - The Amazon Resource Name (arn) of the world template you want to delete.
newDeleteWorldTemplate ::
  -- | 'template'
  Prelude.Text ->
  DeleteWorldTemplate
newDeleteWorldTemplate pTemplate_ =
  DeleteWorldTemplate' {template = pTemplate_}

-- | The Amazon Resource Name (arn) of the world template you want to delete.
deleteWorldTemplate_template :: Lens.Lens' DeleteWorldTemplate Prelude.Text
deleteWorldTemplate_template = Lens.lens (\DeleteWorldTemplate' {template} -> template) (\s@DeleteWorldTemplate' {} a -> s {template = a} :: DeleteWorldTemplate)

instance Core.AWSRequest DeleteWorldTemplate where
  type
    AWSResponse DeleteWorldTemplate =
      DeleteWorldTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorldTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorldTemplate where
  hashWithSalt _salt DeleteWorldTemplate' {..} =
    _salt `Prelude.hashWithSalt` template

instance Prelude.NFData DeleteWorldTemplate where
  rnf DeleteWorldTemplate' {..} = Prelude.rnf template

instance Core.ToHeaders DeleteWorldTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteWorldTemplate where
  toJSON DeleteWorldTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("template" Core..= template)]
      )

instance Core.ToPath DeleteWorldTemplate where
  toPath = Prelude.const "/deleteWorldTemplate"

instance Core.ToQuery DeleteWorldTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorldTemplateResponse' smart constructor.
data DeleteWorldTemplateResponse = DeleteWorldTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorldTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorldTemplateResponse_httpStatus' - The response's http status code.
newDeleteWorldTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorldTemplateResponse
newDeleteWorldTemplateResponse pHttpStatus_ =
  DeleteWorldTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteWorldTemplateResponse_httpStatus :: Lens.Lens' DeleteWorldTemplateResponse Prelude.Int
deleteWorldTemplateResponse_httpStatus = Lens.lens (\DeleteWorldTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteWorldTemplateResponse' {} a -> s {httpStatus = a} :: DeleteWorldTemplateResponse)

instance Prelude.NFData DeleteWorldTemplateResponse where
  rnf DeleteWorldTemplateResponse' {..} =
    Prelude.rnf httpStatus

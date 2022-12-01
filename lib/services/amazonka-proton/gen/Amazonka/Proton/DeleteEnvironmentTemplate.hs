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
-- Module      : Amazonka.Proton.DeleteEnvironmentTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If no other major or minor versions of an environment template exist,
-- delete the environment template.
module Amazonka.Proton.DeleteEnvironmentTemplate
  ( -- * Creating a Request
    DeleteEnvironmentTemplate (..),
    newDeleteEnvironmentTemplate,

    -- * Request Lenses
    deleteEnvironmentTemplate_name,

    -- * Destructuring the Response
    DeleteEnvironmentTemplateResponse (..),
    newDeleteEnvironmentTemplateResponse,

    -- * Response Lenses
    deleteEnvironmentTemplateResponse_environmentTemplate,
    deleteEnvironmentTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEnvironmentTemplate' smart constructor.
data DeleteEnvironmentTemplate = DeleteEnvironmentTemplate'
  { -- | The name of the environment template to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteEnvironmentTemplate_name' - The name of the environment template to delete.
newDeleteEnvironmentTemplate ::
  -- | 'name'
  Prelude.Text ->
  DeleteEnvironmentTemplate
newDeleteEnvironmentTemplate pName_ =
  DeleteEnvironmentTemplate' {name = pName_}

-- | The name of the environment template to delete.
deleteEnvironmentTemplate_name :: Lens.Lens' DeleteEnvironmentTemplate Prelude.Text
deleteEnvironmentTemplate_name = Lens.lens (\DeleteEnvironmentTemplate' {name} -> name) (\s@DeleteEnvironmentTemplate' {} a -> s {name = a} :: DeleteEnvironmentTemplate)

instance Core.AWSRequest DeleteEnvironmentTemplate where
  type
    AWSResponse DeleteEnvironmentTemplate =
      DeleteEnvironmentTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEnvironmentTemplateResponse'
            Prelude.<$> (x Core..?> "environmentTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEnvironmentTemplate where
  hashWithSalt _salt DeleteEnvironmentTemplate' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteEnvironmentTemplate where
  rnf DeleteEnvironmentTemplate' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteEnvironmentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.DeleteEnvironmentTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteEnvironmentTemplate where
  toJSON DeleteEnvironmentTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath DeleteEnvironmentTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteEnvironmentTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentTemplateResponse' smart constructor.
data DeleteEnvironmentTemplateResponse = DeleteEnvironmentTemplateResponse'
  { -- | The detailed data of the environment template being deleted.
    environmentTemplate :: Prelude.Maybe EnvironmentTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentTemplate', 'deleteEnvironmentTemplateResponse_environmentTemplate' - The detailed data of the environment template being deleted.
--
-- 'httpStatus', 'deleteEnvironmentTemplateResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEnvironmentTemplateResponse
newDeleteEnvironmentTemplateResponse pHttpStatus_ =
  DeleteEnvironmentTemplateResponse'
    { environmentTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the environment template being deleted.
deleteEnvironmentTemplateResponse_environmentTemplate :: Lens.Lens' DeleteEnvironmentTemplateResponse (Prelude.Maybe EnvironmentTemplate)
deleteEnvironmentTemplateResponse_environmentTemplate = Lens.lens (\DeleteEnvironmentTemplateResponse' {environmentTemplate} -> environmentTemplate) (\s@DeleteEnvironmentTemplateResponse' {} a -> s {environmentTemplate = a} :: DeleteEnvironmentTemplateResponse)

-- | The response's http status code.
deleteEnvironmentTemplateResponse_httpStatus :: Lens.Lens' DeleteEnvironmentTemplateResponse Prelude.Int
deleteEnvironmentTemplateResponse_httpStatus = Lens.lens (\DeleteEnvironmentTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentTemplateResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentTemplateResponse)

instance
  Prelude.NFData
    DeleteEnvironmentTemplateResponse
  where
  rnf DeleteEnvironmentTemplateResponse' {..} =
    Prelude.rnf environmentTemplate
      `Prelude.seq` Prelude.rnf httpStatus

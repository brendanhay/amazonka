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
-- Module      : Amazonka.Proton.DeleteServiceTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If no other major or minor versions of the service template exist,
-- delete the service template.
module Amazonka.Proton.DeleteServiceTemplate
  ( -- * Creating a Request
    DeleteServiceTemplate (..),
    newDeleteServiceTemplate,

    -- * Request Lenses
    deleteServiceTemplate_name,

    -- * Destructuring the Response
    DeleteServiceTemplateResponse (..),
    newDeleteServiceTemplateResponse,

    -- * Response Lenses
    deleteServiceTemplateResponse_serviceTemplate,
    deleteServiceTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteServiceTemplate' smart constructor.
data DeleteServiceTemplate = DeleteServiceTemplate'
  { -- | The name of the service template to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteServiceTemplate_name' - The name of the service template to delete.
newDeleteServiceTemplate ::
  -- | 'name'
  Prelude.Text ->
  DeleteServiceTemplate
newDeleteServiceTemplate pName_ =
  DeleteServiceTemplate' {name = pName_}

-- | The name of the service template to delete.
deleteServiceTemplate_name :: Lens.Lens' DeleteServiceTemplate Prelude.Text
deleteServiceTemplate_name = Lens.lens (\DeleteServiceTemplate' {name} -> name) (\s@DeleteServiceTemplate' {} a -> s {name = a} :: DeleteServiceTemplate)

instance Core.AWSRequest DeleteServiceTemplate where
  type
    AWSResponse DeleteServiceTemplate =
      DeleteServiceTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceTemplateResponse'
            Prelude.<$> (x Data..?> "serviceTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteServiceTemplate where
  hashWithSalt _salt DeleteServiceTemplate' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteServiceTemplate where
  rnf DeleteServiceTemplate' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteServiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.DeleteServiceTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteServiceTemplate where
  toJSON DeleteServiceTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath DeleteServiceTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteServiceTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceTemplateResponse' smart constructor.
data DeleteServiceTemplateResponse = DeleteServiceTemplateResponse'
  { -- | The detailed data of the service template being deleted.
    serviceTemplate :: Prelude.Maybe ServiceTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceTemplate', 'deleteServiceTemplateResponse_serviceTemplate' - The detailed data of the service template being deleted.
--
-- 'httpStatus', 'deleteServiceTemplateResponse_httpStatus' - The response's http status code.
newDeleteServiceTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceTemplateResponse
newDeleteServiceTemplateResponse pHttpStatus_ =
  DeleteServiceTemplateResponse'
    { serviceTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the service template being deleted.
deleteServiceTemplateResponse_serviceTemplate :: Lens.Lens' DeleteServiceTemplateResponse (Prelude.Maybe ServiceTemplate)
deleteServiceTemplateResponse_serviceTemplate = Lens.lens (\DeleteServiceTemplateResponse' {serviceTemplate} -> serviceTemplate) (\s@DeleteServiceTemplateResponse' {} a -> s {serviceTemplate = a} :: DeleteServiceTemplateResponse)

-- | The response's http status code.
deleteServiceTemplateResponse_httpStatus :: Lens.Lens' DeleteServiceTemplateResponse Prelude.Int
deleteServiceTemplateResponse_httpStatus = Lens.lens (\DeleteServiceTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceTemplateResponse' {} a -> s {httpStatus = a} :: DeleteServiceTemplateResponse)

instance Prelude.NFData DeleteServiceTemplateResponse where
  rnf DeleteServiceTemplateResponse' {..} =
    Prelude.rnf serviceTemplate
      `Prelude.seq` Prelude.rnf httpStatus

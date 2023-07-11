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
-- Module      : Amazonka.IoT.DeleteProvisioningTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a provisioning template.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteProvisioningTemplate>
-- action.
module Amazonka.IoT.DeleteProvisioningTemplate
  ( -- * Creating a Request
    DeleteProvisioningTemplate (..),
    newDeleteProvisioningTemplate,

    -- * Request Lenses
    deleteProvisioningTemplate_templateName,

    -- * Destructuring the Response
    DeleteProvisioningTemplateResponse (..),
    newDeleteProvisioningTemplateResponse,

    -- * Response Lenses
    deleteProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProvisioningTemplate' smart constructor.
data DeleteProvisioningTemplate = DeleteProvisioningTemplate'
  { -- | The name of the fleet provision template to delete.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProvisioningTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'deleteProvisioningTemplate_templateName' - The name of the fleet provision template to delete.
newDeleteProvisioningTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteProvisioningTemplate
newDeleteProvisioningTemplate pTemplateName_ =
  DeleteProvisioningTemplate'
    { templateName =
        pTemplateName_
    }

-- | The name of the fleet provision template to delete.
deleteProvisioningTemplate_templateName :: Lens.Lens' DeleteProvisioningTemplate Prelude.Text
deleteProvisioningTemplate_templateName = Lens.lens (\DeleteProvisioningTemplate' {templateName} -> templateName) (\s@DeleteProvisioningTemplate' {} a -> s {templateName = a} :: DeleteProvisioningTemplate)

instance Core.AWSRequest DeleteProvisioningTemplate where
  type
    AWSResponse DeleteProvisioningTemplate =
      DeleteProvisioningTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProvisioningTemplate where
  hashWithSalt _salt DeleteProvisioningTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteProvisioningTemplate where
  rnf DeleteProvisioningTemplate' {..} =
    Prelude.rnf templateName

instance Data.ToHeaders DeleteProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteProvisioningTemplate where
  toPath DeleteProvisioningTemplate' {..} =
    Prelude.mconcat
      ["/provisioning-templates/", Data.toBS templateName]

instance Data.ToQuery DeleteProvisioningTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProvisioningTemplateResponse' smart constructor.
data DeleteProvisioningTemplateResponse = DeleteProvisioningTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProvisioningTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProvisioningTemplateResponse_httpStatus' - The response's http status code.
newDeleteProvisioningTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProvisioningTemplateResponse
newDeleteProvisioningTemplateResponse pHttpStatus_ =
  DeleteProvisioningTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProvisioningTemplateResponse_httpStatus :: Lens.Lens' DeleteProvisioningTemplateResponse Prelude.Int
deleteProvisioningTemplateResponse_httpStatus = Lens.lens (\DeleteProvisioningTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteProvisioningTemplateResponse' {} a -> s {httpStatus = a} :: DeleteProvisioningTemplateResponse)

instance
  Prelude.NFData
    DeleteProvisioningTemplateResponse
  where
  rnf DeleteProvisioningTemplateResponse' {..} =
    Prelude.rnf httpStatus

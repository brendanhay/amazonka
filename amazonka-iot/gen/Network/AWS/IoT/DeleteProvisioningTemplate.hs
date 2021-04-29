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
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template.
module Network.AWS.IoT.DeleteProvisioningTemplate
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProvisioningTemplate' smart constructor.
data DeleteProvisioningTemplate = DeleteProvisioningTemplate'
  { -- | The name of the fleet provision template to delete.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DeleteProvisioningTemplate
  where
  type
    Rs DeleteProvisioningTemplate =
      DeleteProvisioningTemplateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProvisioningTemplate

instance Prelude.NFData DeleteProvisioningTemplate

instance Prelude.ToHeaders DeleteProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteProvisioningTemplate where
  toPath DeleteProvisioningTemplate' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Prelude.toBS templateName
      ]

instance Prelude.ToQuery DeleteProvisioningTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProvisioningTemplateResponse' smart constructor.
data DeleteProvisioningTemplateResponse = DeleteProvisioningTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

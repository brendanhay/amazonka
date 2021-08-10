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
-- Module      : Network.AWS.SESv2.DeleteEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an email template.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SESv2.DeleteEmailTemplate
  ( -- * Creating a Request
    DeleteEmailTemplate (..),
    newDeleteEmailTemplate,

    -- * Request Lenses
    deleteEmailTemplate_templateName,

    -- * Destructuring the Response
    DeleteEmailTemplateResponse (..),
    newDeleteEmailTemplateResponse,

    -- * Response Lenses
    deleteEmailTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to delete an email template. For more information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteEmailTemplate' smart constructor.
data DeleteEmailTemplate = DeleteEmailTemplate'
  { -- | The name of the template to be deleted.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'deleteEmailTemplate_templateName' - The name of the template to be deleted.
newDeleteEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteEmailTemplate
newDeleteEmailTemplate pTemplateName_ =
  DeleteEmailTemplate' {templateName = pTemplateName_}

-- | The name of the template to be deleted.
deleteEmailTemplate_templateName :: Lens.Lens' DeleteEmailTemplate Prelude.Text
deleteEmailTemplate_templateName = Lens.lens (\DeleteEmailTemplate' {templateName} -> templateName) (\s@DeleteEmailTemplate' {} a -> s {templateName = a} :: DeleteEmailTemplate)

instance Core.AWSRequest DeleteEmailTemplate where
  type
    AWSResponse DeleteEmailTemplate =
      DeleteEmailTemplateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEmailTemplate

instance Prelude.NFData DeleteEmailTemplate

instance Core.ToHeaders DeleteEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteEmailTemplate where
  toPath DeleteEmailTemplate' {..} =
    Prelude.mconcat
      ["/v2/email/templates/", Core.toBS templateName]

instance Core.ToQuery DeleteEmailTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newDeleteEmailTemplateResponse' smart constructor.
data DeleteEmailTemplateResponse = DeleteEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEmailTemplateResponse_httpStatus' - The response's http status code.
newDeleteEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEmailTemplateResponse
newDeleteEmailTemplateResponse pHttpStatus_ =
  DeleteEmailTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEmailTemplateResponse_httpStatus :: Lens.Lens' DeleteEmailTemplateResponse Prelude.Int
deleteEmailTemplateResponse_httpStatus = Lens.lens (\DeleteEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteEmailTemplateResponse' {} a -> s {httpStatus = a} :: DeleteEmailTemplateResponse)

instance Prelude.NFData DeleteEmailTemplateResponse

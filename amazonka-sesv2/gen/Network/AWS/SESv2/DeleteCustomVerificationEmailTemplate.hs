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
-- Module      : Network.AWS.SESv2.DeleteCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing custom verification email template.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/es/latest/DeveloperGuide/send-email-verify-address-custom.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SESv2.DeleteCustomVerificationEmailTemplate
  ( -- * Creating a Request
    DeleteCustomVerificationEmailTemplate (..),
    newDeleteCustomVerificationEmailTemplate,

    -- * Request Lenses
    deleteCustomVerificationEmailTemplate_templateName,

    -- * Destructuring the Response
    DeleteCustomVerificationEmailTemplateResponse (..),
    newDeleteCustomVerificationEmailTemplateResponse,

    -- * Response Lenses
    deleteCustomVerificationEmailTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | Represents a request to delete an existing custom verification email
-- template.
--
-- /See:/ 'newDeleteCustomVerificationEmailTemplate' smart constructor.
data DeleteCustomVerificationEmailTemplate = DeleteCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template that you want to
    -- delete.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomVerificationEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'deleteCustomVerificationEmailTemplate_templateName' - The name of the custom verification email template that you want to
-- delete.
newDeleteCustomVerificationEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteCustomVerificationEmailTemplate
newDeleteCustomVerificationEmailTemplate
  pTemplateName_ =
    DeleteCustomVerificationEmailTemplate'
      { templateName =
          pTemplateName_
      }

-- | The name of the custom verification email template that you want to
-- delete.
deleteCustomVerificationEmailTemplate_templateName :: Lens.Lens' DeleteCustomVerificationEmailTemplate Prelude.Text
deleteCustomVerificationEmailTemplate_templateName = Lens.lens (\DeleteCustomVerificationEmailTemplate' {templateName} -> templateName) (\s@DeleteCustomVerificationEmailTemplate' {} a -> s {templateName = a} :: DeleteCustomVerificationEmailTemplate)

instance
  Core.AWSRequest
    DeleteCustomVerificationEmailTemplate
  where
  type
    AWSResponse
      DeleteCustomVerificationEmailTemplate =
      DeleteCustomVerificationEmailTemplateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomVerificationEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteCustomVerificationEmailTemplate

instance
  Prelude.NFData
    DeleteCustomVerificationEmailTemplate

instance
  Core.ToHeaders
    DeleteCustomVerificationEmailTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    DeleteCustomVerificationEmailTemplate
  where
  toPath DeleteCustomVerificationEmailTemplate' {..} =
    Prelude.mconcat
      [ "/v2/email/custom-verification-email-templates/",
        Core.toBS templateName
      ]

instance
  Core.ToQuery
    DeleteCustomVerificationEmailTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200 response
-- with an empty HTTP body.
--
-- /See:/ 'newDeleteCustomVerificationEmailTemplateResponse' smart constructor.
data DeleteCustomVerificationEmailTemplateResponse = DeleteCustomVerificationEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomVerificationEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCustomVerificationEmailTemplateResponse_httpStatus' - The response's http status code.
newDeleteCustomVerificationEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomVerificationEmailTemplateResponse
newDeleteCustomVerificationEmailTemplateResponse
  pHttpStatus_ =
    DeleteCustomVerificationEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteCustomVerificationEmailTemplateResponse_httpStatus :: Lens.Lens' DeleteCustomVerificationEmailTemplateResponse Prelude.Int
deleteCustomVerificationEmailTemplateResponse_httpStatus = Lens.lens (\DeleteCustomVerificationEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomVerificationEmailTemplateResponse' {} a -> s {httpStatus = a} :: DeleteCustomVerificationEmailTemplateResponse)

instance
  Prelude.NFData
    DeleteCustomVerificationEmailTemplateResponse

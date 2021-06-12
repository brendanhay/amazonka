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
-- Module      : Network.AWS.SES.DeleteCustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing custom verification email template.
--
-- For more information about custom verification email templates, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/custom-verification-emails.html Using Custom Verification Email Templates>
-- in the /Amazon SES Developer Guide/.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteCustomVerificationEmailTemplate
  ( -- * Creating a Request
    DeleteCustomVerificationEmailTemplate (..),
    newDeleteCustomVerificationEmailTemplate,

    -- * Request Lenses
    deleteCustomVerificationEmailTemplate_templateName,

    -- * Destructuring the Response
    DeleteCustomVerificationEmailTemplateResponse (..),
    newDeleteCustomVerificationEmailTemplateResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to delete an existing custom verification email
-- template.
--
-- /See:/ 'newDeleteCustomVerificationEmailTemplate' smart constructor.
data DeleteCustomVerificationEmailTemplate = DeleteCustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template that you want to
    -- delete.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteCustomVerificationEmailTemplate
newDeleteCustomVerificationEmailTemplate
  pTemplateName_ =
    DeleteCustomVerificationEmailTemplate'
      { templateName =
          pTemplateName_
      }

-- | The name of the custom verification email template that you want to
-- delete.
deleteCustomVerificationEmailTemplate_templateName :: Lens.Lens' DeleteCustomVerificationEmailTemplate Core.Text
deleteCustomVerificationEmailTemplate_templateName = Lens.lens (\DeleteCustomVerificationEmailTemplate' {templateName} -> templateName) (\s@DeleteCustomVerificationEmailTemplate' {} a -> s {templateName = a} :: DeleteCustomVerificationEmailTemplate)

instance
  Core.AWSRequest
    DeleteCustomVerificationEmailTemplate
  where
  type
    AWSResponse
      DeleteCustomVerificationEmailTemplate =
      DeleteCustomVerificationEmailTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteCustomVerificationEmailTemplateResponse'

instance
  Core.Hashable
    DeleteCustomVerificationEmailTemplate

instance
  Core.NFData
    DeleteCustomVerificationEmailTemplate

instance
  Core.ToHeaders
    DeleteCustomVerificationEmailTemplate
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteCustomVerificationEmailTemplate
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteCustomVerificationEmailTemplate
  where
  toQuery DeleteCustomVerificationEmailTemplate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DeleteCustomVerificationEmailTemplate" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "TemplateName" Core.=: templateName
      ]

-- | /See:/ 'newDeleteCustomVerificationEmailTemplateResponse' smart constructor.
data DeleteCustomVerificationEmailTemplateResponse = DeleteCustomVerificationEmailTemplateResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCustomVerificationEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomVerificationEmailTemplateResponse ::
  DeleteCustomVerificationEmailTemplateResponse
newDeleteCustomVerificationEmailTemplateResponse =
  DeleteCustomVerificationEmailTemplateResponse'

instance
  Core.NFData
    DeleteCustomVerificationEmailTemplateResponse

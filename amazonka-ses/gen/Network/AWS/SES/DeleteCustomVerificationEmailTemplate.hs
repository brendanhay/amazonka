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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteCustomVerificationEmailTemplate
  where
  type
    Rs DeleteCustomVerificationEmailTemplate =
      DeleteCustomVerificationEmailTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteCustomVerificationEmailTemplateResponse'

instance
  Prelude.Hashable
    DeleteCustomVerificationEmailTemplate

instance
  Prelude.NFData
    DeleteCustomVerificationEmailTemplate

instance
  Prelude.ToHeaders
    DeleteCustomVerificationEmailTemplate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteCustomVerificationEmailTemplate
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteCustomVerificationEmailTemplate
  where
  toQuery DeleteCustomVerificationEmailTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteCustomVerificationEmailTemplate" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Prelude.=: templateName
      ]

-- | /See:/ 'newDeleteCustomVerificationEmailTemplateResponse' smart constructor.
data DeleteCustomVerificationEmailTemplateResponse = DeleteCustomVerificationEmailTemplateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomVerificationEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCustomVerificationEmailTemplateResponse ::
  DeleteCustomVerificationEmailTemplateResponse
newDeleteCustomVerificationEmailTemplateResponse =
  DeleteCustomVerificationEmailTemplateResponse'

instance
  Prelude.NFData
    DeleteCustomVerificationEmailTemplateResponse

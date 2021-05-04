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
-- Module      : Network.AWS.SES.DeleteTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an email template.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.DeleteTemplate
  ( -- * Creating a Request
    DeleteTemplate (..),
    newDeleteTemplate,

    -- * Request Lenses
    deleteTemplate_templateName,

    -- * Destructuring the Response
    DeleteTemplateResponse (..),
    newDeleteTemplateResponse,

    -- * Response Lenses
    deleteTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to delete an email template. For more information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteTemplate' smart constructor.
data DeleteTemplate = DeleteTemplate'
  { -- | The name of the template to be deleted.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'deleteTemplate_templateName' - The name of the template to be deleted.
newDeleteTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteTemplate
newDeleteTemplate pTemplateName_ =
  DeleteTemplate' {templateName = pTemplateName_}

-- | The name of the template to be deleted.
deleteTemplate_templateName :: Lens.Lens' DeleteTemplate Prelude.Text
deleteTemplate_templateName = Lens.lens (\DeleteTemplate' {templateName} -> templateName) (\s@DeleteTemplate' {} a -> s {templateName = a} :: DeleteTemplate)

instance Prelude.AWSRequest DeleteTemplate where
  type Rs DeleteTemplate = DeleteTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteTemplateResult"
      ( \s h x ->
          DeleteTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTemplate

instance Prelude.NFData DeleteTemplate

instance Prelude.ToHeaders DeleteTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTemplate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTemplate where
  toQuery DeleteTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteTemplate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Prelude.=: templateName
      ]

-- | /See:/ 'newDeleteTemplateResponse' smart constructor.
data DeleteTemplateResponse = DeleteTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTemplateResponse_httpStatus' - The response's http status code.
newDeleteTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTemplateResponse
newDeleteTemplateResponse pHttpStatus_ =
  DeleteTemplateResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTemplateResponse_httpStatus :: Lens.Lens' DeleteTemplateResponse Prelude.Int
deleteTemplateResponse_httpStatus = Lens.lens (\DeleteTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteTemplateResponse' {} a -> s {httpStatus = a} :: DeleteTemplateResponse)

instance Prelude.NFData DeleteTemplateResponse

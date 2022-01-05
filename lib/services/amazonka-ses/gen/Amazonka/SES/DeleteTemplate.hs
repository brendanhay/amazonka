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
-- Module      : Amazonka.SES.DeleteTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an email template.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DeleteTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete an email template. For more information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteTemplate' smart constructor.
data DeleteTemplate = DeleteTemplate'
  { -- | The name of the template to be deleted.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteTemplate where
  type
    AWSResponse DeleteTemplate =
      DeleteTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteTemplateResult"
      ( \s h x ->
          DeleteTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTemplate where
  hashWithSalt _salt DeleteTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteTemplate where
  rnf DeleteTemplate' {..} = Prelude.rnf templateName

instance Core.ToHeaders DeleteTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTemplate where
  toQuery DeleteTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteTemplate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "TemplateName" Core.=: templateName
      ]

-- | /See:/ 'newDeleteTemplateResponse' smart constructor.
data DeleteTemplateResponse = DeleteTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteTemplateResponse where
  rnf DeleteTemplateResponse' {..} =
    Prelude.rnf httpStatus

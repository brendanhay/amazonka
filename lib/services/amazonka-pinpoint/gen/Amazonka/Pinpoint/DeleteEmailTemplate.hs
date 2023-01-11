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
-- Module      : Amazonka.Pinpoint.DeleteEmailTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the email
-- channel.
module Amazonka.Pinpoint.DeleteEmailTemplate
  ( -- * Creating a Request
    DeleteEmailTemplate (..),
    newDeleteEmailTemplate,

    -- * Request Lenses
    deleteEmailTemplate_version,
    deleteEmailTemplate_templateName,

    -- * Destructuring the Response
    DeleteEmailTemplateResponse (..),
    newDeleteEmailTemplateResponse,

    -- * Response Lenses
    deleteEmailTemplateResponse_httpStatus,
    deleteEmailTemplateResponse_messageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEmailTemplate' smart constructor.
data DeleteEmailTemplate = DeleteEmailTemplate'
  { -- | The unique identifier for the version of the message template to update,
    -- retrieve information about, or delete. To retrieve identifiers and other
    -- information for all the versions of a template, use the Template
    -- Versions resource.
    --
    -- If specified, this value must match the identifier for an existing
    -- template version. If specified for an update operation, this value must
    -- match the identifier for the latest existing version of the template.
    -- This restriction helps ensure that race conditions don\'t occur.
    --
    -- If you don\'t specify a value for this parameter, Amazon Pinpoint does
    -- the following:
    --
    -- -   For a get operation, retrieves information about the active version
    --     of the template.
    --
    -- -   For an update operation, saves the updates to (overwrites) the
    --     latest existing version of the template, if the create-new-version
    --     parameter isn\'t used or is set to false.
    --
    -- -   For a delete operation, deletes the template, including all versions
    --     of the template.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
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
-- 'version', 'deleteEmailTemplate_version' - The unique identifier for the version of the message template to update,
-- retrieve information about, or delete. To retrieve identifiers and other
-- information for all the versions of a template, use the Template
-- Versions resource.
--
-- If specified, this value must match the identifier for an existing
-- template version. If specified for an update operation, this value must
-- match the identifier for the latest existing version of the template.
-- This restriction helps ensure that race conditions don\'t occur.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint does
-- the following:
--
-- -   For a get operation, retrieves information about the active version
--     of the template.
--
-- -   For an update operation, saves the updates to (overwrites) the
--     latest existing version of the template, if the create-new-version
--     parameter isn\'t used or is set to false.
--
-- -   For a delete operation, deletes the template, including all versions
--     of the template.
--
-- 'templateName', 'deleteEmailTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newDeleteEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteEmailTemplate
newDeleteEmailTemplate pTemplateName_ =
  DeleteEmailTemplate'
    { version = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update,
-- retrieve information about, or delete. To retrieve identifiers and other
-- information for all the versions of a template, use the Template
-- Versions resource.
--
-- If specified, this value must match the identifier for an existing
-- template version. If specified for an update operation, this value must
-- match the identifier for the latest existing version of the template.
-- This restriction helps ensure that race conditions don\'t occur.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint does
-- the following:
--
-- -   For a get operation, retrieves information about the active version
--     of the template.
--
-- -   For an update operation, saves the updates to (overwrites) the
--     latest existing version of the template, if the create-new-version
--     parameter isn\'t used or is set to false.
--
-- -   For a delete operation, deletes the template, including all versions
--     of the template.
deleteEmailTemplate_version :: Lens.Lens' DeleteEmailTemplate (Prelude.Maybe Prelude.Text)
deleteEmailTemplate_version = Lens.lens (\DeleteEmailTemplate' {version} -> version) (\s@DeleteEmailTemplate' {} a -> s {version = a} :: DeleteEmailTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
deleteEmailTemplate_templateName :: Lens.Lens' DeleteEmailTemplate Prelude.Text
deleteEmailTemplate_templateName = Lens.lens (\DeleteEmailTemplate' {templateName} -> templateName) (\s@DeleteEmailTemplate' {} a -> s {templateName = a} :: DeleteEmailTemplate)

instance Core.AWSRequest DeleteEmailTemplate where
  type
    AWSResponse DeleteEmailTemplate =
      DeleteEmailTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteEmailTemplate where
  hashWithSalt _salt DeleteEmailTemplate' {..} =
    _salt `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteEmailTemplate where
  rnf DeleteEmailTemplate' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders DeleteEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEmailTemplate where
  toPath DeleteEmailTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Data.toBS templateName, "/email"]

instance Data.ToQuery DeleteEmailTemplate where
  toQuery DeleteEmailTemplate' {..} =
    Prelude.mconcat ["version" Data.=: version]

-- | /See:/ 'newDeleteEmailTemplateResponse' smart constructor.
data DeleteEmailTemplateResponse = DeleteEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
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
--
-- 'messageBody', 'deleteEmailTemplateResponse_messageBody' - Undocumented member.
newDeleteEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  DeleteEmailTemplateResponse
newDeleteEmailTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    DeleteEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
deleteEmailTemplateResponse_httpStatus :: Lens.Lens' DeleteEmailTemplateResponse Prelude.Int
deleteEmailTemplateResponse_httpStatus = Lens.lens (\DeleteEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteEmailTemplateResponse' {} a -> s {httpStatus = a} :: DeleteEmailTemplateResponse)

-- | Undocumented member.
deleteEmailTemplateResponse_messageBody :: Lens.Lens' DeleteEmailTemplateResponse MessageBody
deleteEmailTemplateResponse_messageBody = Lens.lens (\DeleteEmailTemplateResponse' {messageBody} -> messageBody) (\s@DeleteEmailTemplateResponse' {} a -> s {messageBody = a} :: DeleteEmailTemplateResponse)

instance Prelude.NFData DeleteEmailTemplateResponse where
  rnf DeleteEmailTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messageBody

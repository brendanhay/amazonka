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
-- Module      : Amazonka.Pinpoint.DeleteSmsTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the SMS
-- channel.
module Amazonka.Pinpoint.DeleteSmsTemplate
  ( -- * Creating a Request
    DeleteSmsTemplate (..),
    newDeleteSmsTemplate,

    -- * Request Lenses
    deleteSmsTemplate_version,
    deleteSmsTemplate_templateName,

    -- * Destructuring the Response
    DeleteSmsTemplateResponse (..),
    newDeleteSmsTemplateResponse,

    -- * Response Lenses
    deleteSmsTemplateResponse_httpStatus,
    deleteSmsTemplateResponse_messageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSmsTemplate' smart constructor.
data DeleteSmsTemplate = DeleteSmsTemplate'
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
-- Create a value of 'DeleteSmsTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'deleteSmsTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'deleteSmsTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newDeleteSmsTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteSmsTemplate
newDeleteSmsTemplate pTemplateName_ =
  DeleteSmsTemplate'
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
deleteSmsTemplate_version :: Lens.Lens' DeleteSmsTemplate (Prelude.Maybe Prelude.Text)
deleteSmsTemplate_version = Lens.lens (\DeleteSmsTemplate' {version} -> version) (\s@DeleteSmsTemplate' {} a -> s {version = a} :: DeleteSmsTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
deleteSmsTemplate_templateName :: Lens.Lens' DeleteSmsTemplate Prelude.Text
deleteSmsTemplate_templateName = Lens.lens (\DeleteSmsTemplate' {templateName} -> templateName) (\s@DeleteSmsTemplate' {} a -> s {templateName = a} :: DeleteSmsTemplate)

instance Core.AWSRequest DeleteSmsTemplate where
  type
    AWSResponse DeleteSmsTemplate =
      DeleteSmsTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSmsTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteSmsTemplate where
  hashWithSalt _salt DeleteSmsTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteSmsTemplate where
  rnf DeleteSmsTemplate' {..} =
    Prelude.rnf version `Prelude.seq`
      Prelude.rnf templateName

instance Data.ToHeaders DeleteSmsTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSmsTemplate where
  toPath DeleteSmsTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Data.toBS templateName, "/sms"]

instance Data.ToQuery DeleteSmsTemplate where
  toQuery DeleteSmsTemplate' {..} =
    Prelude.mconcat ["version" Data.=: version]

-- | /See:/ 'newDeleteSmsTemplateResponse' smart constructor.
data DeleteSmsTemplateResponse = DeleteSmsTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSmsTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSmsTemplateResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'deleteSmsTemplateResponse_messageBody' - Undocumented member.
newDeleteSmsTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  DeleteSmsTemplateResponse
newDeleteSmsTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    DeleteSmsTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
deleteSmsTemplateResponse_httpStatus :: Lens.Lens' DeleteSmsTemplateResponse Prelude.Int
deleteSmsTemplateResponse_httpStatus = Lens.lens (\DeleteSmsTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteSmsTemplateResponse' {} a -> s {httpStatus = a} :: DeleteSmsTemplateResponse)

-- | Undocumented member.
deleteSmsTemplateResponse_messageBody :: Lens.Lens' DeleteSmsTemplateResponse MessageBody
deleteSmsTemplateResponse_messageBody = Lens.lens (\DeleteSmsTemplateResponse' {messageBody} -> messageBody) (\s@DeleteSmsTemplateResponse' {} a -> s {messageBody = a} :: DeleteSmsTemplateResponse)

instance Prelude.NFData DeleteSmsTemplateResponse where
  rnf DeleteSmsTemplateResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf messageBody

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
-- Module      : Network.AWS.Pinpoint.DeleteSmsTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the SMS
-- channel.
module Network.AWS.Pinpoint.DeleteSmsTemplate
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSmsTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteSmsTemplate

instance Prelude.NFData DeleteSmsTemplate

instance Core.ToHeaders DeleteSmsTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteSmsTemplate where
  toPath DeleteSmsTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/sms"]

instance Core.ToQuery DeleteSmsTemplate where
  toQuery DeleteSmsTemplate' {..} =
    Prelude.mconcat ["version" Core.=: version]

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

instance Prelude.NFData DeleteSmsTemplateResponse

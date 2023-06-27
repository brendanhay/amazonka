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
-- Module      : Amazonka.Pinpoint.DeleteVoiceTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the voice
-- channel.
module Amazonka.Pinpoint.DeleteVoiceTemplate
  ( -- * Creating a Request
    DeleteVoiceTemplate (..),
    newDeleteVoiceTemplate,

    -- * Request Lenses
    deleteVoiceTemplate_version,
    deleteVoiceTemplate_templateName,

    -- * Destructuring the Response
    DeleteVoiceTemplateResponse (..),
    newDeleteVoiceTemplateResponse,

    -- * Response Lenses
    deleteVoiceTemplateResponse_httpStatus,
    deleteVoiceTemplateResponse_messageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVoiceTemplate' smart constructor.
data DeleteVoiceTemplate = DeleteVoiceTemplate'
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
-- Create a value of 'DeleteVoiceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'deleteVoiceTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'deleteVoiceTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newDeleteVoiceTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteVoiceTemplate
newDeleteVoiceTemplate pTemplateName_ =
  DeleteVoiceTemplate'
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
deleteVoiceTemplate_version :: Lens.Lens' DeleteVoiceTemplate (Prelude.Maybe Prelude.Text)
deleteVoiceTemplate_version = Lens.lens (\DeleteVoiceTemplate' {version} -> version) (\s@DeleteVoiceTemplate' {} a -> s {version = a} :: DeleteVoiceTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
deleteVoiceTemplate_templateName :: Lens.Lens' DeleteVoiceTemplate Prelude.Text
deleteVoiceTemplate_templateName = Lens.lens (\DeleteVoiceTemplate' {templateName} -> templateName) (\s@DeleteVoiceTemplate' {} a -> s {templateName = a} :: DeleteVoiceTemplate)

instance Core.AWSRequest DeleteVoiceTemplate where
  type
    AWSResponse DeleteVoiceTemplate =
      DeleteVoiceTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteVoiceTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteVoiceTemplate where
  hashWithSalt _salt DeleteVoiceTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteVoiceTemplate where
  rnf DeleteVoiceTemplate' {..} =
    Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToHeaders DeleteVoiceTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVoiceTemplate where
  toPath DeleteVoiceTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Data.toBS templateName, "/voice"]

instance Data.ToQuery DeleteVoiceTemplate where
  toQuery DeleteVoiceTemplate' {..} =
    Prelude.mconcat ["version" Data.=: version]

-- | /See:/ 'newDeleteVoiceTemplateResponse' smart constructor.
data DeleteVoiceTemplateResponse = DeleteVoiceTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVoiceTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVoiceTemplateResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'deleteVoiceTemplateResponse_messageBody' - Undocumented member.
newDeleteVoiceTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  DeleteVoiceTemplateResponse
newDeleteVoiceTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    DeleteVoiceTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
deleteVoiceTemplateResponse_httpStatus :: Lens.Lens' DeleteVoiceTemplateResponse Prelude.Int
deleteVoiceTemplateResponse_httpStatus = Lens.lens (\DeleteVoiceTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteVoiceTemplateResponse' {} a -> s {httpStatus = a} :: DeleteVoiceTemplateResponse)

-- | Undocumented member.
deleteVoiceTemplateResponse_messageBody :: Lens.Lens' DeleteVoiceTemplateResponse MessageBody
deleteVoiceTemplateResponse_messageBody = Lens.lens (\DeleteVoiceTemplateResponse' {messageBody} -> messageBody) (\s@DeleteVoiceTemplateResponse' {} a -> s {messageBody = a} :: DeleteVoiceTemplateResponse)

instance Prelude.NFData DeleteVoiceTemplateResponse where
  rnf DeleteVoiceTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messageBody

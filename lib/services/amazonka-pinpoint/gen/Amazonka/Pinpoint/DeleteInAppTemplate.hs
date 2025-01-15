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
-- Module      : Amazonka.Pinpoint.DeleteInAppTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages sent using the in-app message
-- channel.
module Amazonka.Pinpoint.DeleteInAppTemplate
  ( -- * Creating a Request
    DeleteInAppTemplate (..),
    newDeleteInAppTemplate,

    -- * Request Lenses
    deleteInAppTemplate_version,
    deleteInAppTemplate_templateName,

    -- * Destructuring the Response
    DeleteInAppTemplateResponse (..),
    newDeleteInAppTemplateResponse,

    -- * Response Lenses
    deleteInAppTemplateResponse_httpStatus,
    deleteInAppTemplateResponse_messageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInAppTemplate' smart constructor.
data DeleteInAppTemplate = DeleteInAppTemplate'
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
-- Create a value of 'DeleteInAppTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'deleteInAppTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'deleteInAppTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
newDeleteInAppTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DeleteInAppTemplate
newDeleteInAppTemplate pTemplateName_ =
  DeleteInAppTemplate'
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
deleteInAppTemplate_version :: Lens.Lens' DeleteInAppTemplate (Prelude.Maybe Prelude.Text)
deleteInAppTemplate_version = Lens.lens (\DeleteInAppTemplate' {version} -> version) (\s@DeleteInAppTemplate' {} a -> s {version = a} :: DeleteInAppTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
deleteInAppTemplate_templateName :: Lens.Lens' DeleteInAppTemplate Prelude.Text
deleteInAppTemplate_templateName = Lens.lens (\DeleteInAppTemplate' {templateName} -> templateName) (\s@DeleteInAppTemplate' {} a -> s {templateName = a} :: DeleteInAppTemplate)

instance Core.AWSRequest DeleteInAppTemplate where
  type
    AWSResponse DeleteInAppTemplate =
      DeleteInAppTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInAppTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteInAppTemplate where
  hashWithSalt _salt DeleteInAppTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteInAppTemplate where
  rnf DeleteInAppTemplate' {..} =
    Prelude.rnf version `Prelude.seq`
      Prelude.rnf templateName

instance Data.ToHeaders DeleteInAppTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteInAppTemplate where
  toPath DeleteInAppTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Data.toBS templateName, "/inapp"]

instance Data.ToQuery DeleteInAppTemplate where
  toQuery DeleteInAppTemplate' {..} =
    Prelude.mconcat ["version" Data.=: version]

-- | /See:/ 'newDeleteInAppTemplateResponse' smart constructor.
data DeleteInAppTemplateResponse = DeleteInAppTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInAppTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteInAppTemplateResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'deleteInAppTemplateResponse_messageBody' - Undocumented member.
newDeleteInAppTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  DeleteInAppTemplateResponse
newDeleteInAppTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    DeleteInAppTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
deleteInAppTemplateResponse_httpStatus :: Lens.Lens' DeleteInAppTemplateResponse Prelude.Int
deleteInAppTemplateResponse_httpStatus = Lens.lens (\DeleteInAppTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteInAppTemplateResponse' {} a -> s {httpStatus = a} :: DeleteInAppTemplateResponse)

-- | Undocumented member.
deleteInAppTemplateResponse_messageBody :: Lens.Lens' DeleteInAppTemplateResponse MessageBody
deleteInAppTemplateResponse_messageBody = Lens.lens (\DeleteInAppTemplateResponse' {messageBody} -> messageBody) (\s@DeleteInAppTemplateResponse' {} a -> s {messageBody = a} :: DeleteInAppTemplateResponse)

instance Prelude.NFData DeleteInAppTemplateResponse where
  rnf DeleteInAppTemplateResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf messageBody

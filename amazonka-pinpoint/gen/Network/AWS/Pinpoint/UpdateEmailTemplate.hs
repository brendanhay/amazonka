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
-- Module      : Network.AWS.Pinpoint.UpdateEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through
-- the email channel.
module Network.AWS.Pinpoint.UpdateEmailTemplate
  ( -- * Creating a Request
    UpdateEmailTemplate (..),
    newUpdateEmailTemplate,

    -- * Request Lenses
    updateEmailTemplate_version,
    updateEmailTemplate_createNewVersion,
    updateEmailTemplate_templateName,
    updateEmailTemplate_emailTemplateRequest,

    -- * Destructuring the Response
    UpdateEmailTemplateResponse (..),
    newUpdateEmailTemplateResponse,

    -- * Response Lenses
    updateEmailTemplateResponse_httpStatus,
    updateEmailTemplateResponse_messageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEmailTemplate' smart constructor.
data UpdateEmailTemplate = UpdateEmailTemplate'
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
    -- | Specifies whether to save the updates as a new version of the message
    -- template. Valid values are: true, save the updates as a new version;
    -- and, false, save the updates to (overwrite) the latest existing version
    -- of the template.
    --
    -- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
    -- the updates to (overwrites) the latest existing version of the template.
    -- If you specify a value of true for this parameter, don\'t specify a
    -- value for the version parameter. Otherwise, an error will occur.
    createNewVersion :: Prelude.Maybe Prelude.Bool,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    emailTemplateRequest :: EmailTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'updateEmailTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'createNewVersion', 'updateEmailTemplate_createNewVersion' - Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
--
-- 'templateName', 'updateEmailTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'emailTemplateRequest', 'updateEmailTemplate_emailTemplateRequest' - Undocumented member.
newUpdateEmailTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'emailTemplateRequest'
  EmailTemplateRequest ->
  UpdateEmailTemplate
newUpdateEmailTemplate
  pTemplateName_
  pEmailTemplateRequest_ =
    UpdateEmailTemplate'
      { version = Prelude.Nothing,
        createNewVersion = Prelude.Nothing,
        templateName = pTemplateName_,
        emailTemplateRequest = pEmailTemplateRequest_
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
updateEmailTemplate_version :: Lens.Lens' UpdateEmailTemplate (Prelude.Maybe Prelude.Text)
updateEmailTemplate_version = Lens.lens (\UpdateEmailTemplate' {version} -> version) (\s@UpdateEmailTemplate' {} a -> s {version = a} :: UpdateEmailTemplate)

-- | Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
updateEmailTemplate_createNewVersion :: Lens.Lens' UpdateEmailTemplate (Prelude.Maybe Prelude.Bool)
updateEmailTemplate_createNewVersion = Lens.lens (\UpdateEmailTemplate' {createNewVersion} -> createNewVersion) (\s@UpdateEmailTemplate' {} a -> s {createNewVersion = a} :: UpdateEmailTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
updateEmailTemplate_templateName :: Lens.Lens' UpdateEmailTemplate Prelude.Text
updateEmailTemplate_templateName = Lens.lens (\UpdateEmailTemplate' {templateName} -> templateName) (\s@UpdateEmailTemplate' {} a -> s {templateName = a} :: UpdateEmailTemplate)

-- | Undocumented member.
updateEmailTemplate_emailTemplateRequest :: Lens.Lens' UpdateEmailTemplate EmailTemplateRequest
updateEmailTemplate_emailTemplateRequest = Lens.lens (\UpdateEmailTemplate' {emailTemplateRequest} -> emailTemplateRequest) (\s@UpdateEmailTemplate' {} a -> s {emailTemplateRequest = a} :: UpdateEmailTemplate)

instance Prelude.AWSRequest UpdateEmailTemplate where
  type
    Rs UpdateEmailTemplate =
      UpdateEmailTemplateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEmailTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateEmailTemplate

instance Prelude.NFData UpdateEmailTemplate

instance Prelude.ToHeaders UpdateEmailTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateEmailTemplate where
  toJSON UpdateEmailTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EmailTemplateRequest"
                  Prelude..= emailTemplateRequest
              )
          ]
      )

instance Prelude.ToPath UpdateEmailTemplate where
  toPath UpdateEmailTemplate' {..} =
    Prelude.mconcat
      [ "/v1/templates/",
        Prelude.toBS templateName,
        "/email"
      ]

instance Prelude.ToQuery UpdateEmailTemplate where
  toQuery UpdateEmailTemplate' {..} =
    Prelude.mconcat
      [ "version" Prelude.=: version,
        "create-new-version" Prelude.=: createNewVersion
      ]

-- | /See:/ 'newUpdateEmailTemplateResponse' smart constructor.
data UpdateEmailTemplateResponse = UpdateEmailTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmailTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEmailTemplateResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'updateEmailTemplateResponse_messageBody' - Undocumented member.
newUpdateEmailTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateEmailTemplateResponse
newUpdateEmailTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    UpdateEmailTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
updateEmailTemplateResponse_httpStatus :: Lens.Lens' UpdateEmailTemplateResponse Prelude.Int
updateEmailTemplateResponse_httpStatus = Lens.lens (\UpdateEmailTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateEmailTemplateResponse' {} a -> s {httpStatus = a} :: UpdateEmailTemplateResponse)

-- | Undocumented member.
updateEmailTemplateResponse_messageBody :: Lens.Lens' UpdateEmailTemplateResponse MessageBody
updateEmailTemplateResponse_messageBody = Lens.lens (\UpdateEmailTemplateResponse' {messageBody} -> messageBody) (\s@UpdateEmailTemplateResponse' {} a -> s {messageBody = a} :: UpdateEmailTemplateResponse)

instance Prelude.NFData UpdateEmailTemplateResponse

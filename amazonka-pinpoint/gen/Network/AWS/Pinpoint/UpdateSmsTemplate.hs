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
-- Module      : Network.AWS.Pinpoint.UpdateSmsTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through
-- the SMS channel.
module Network.AWS.Pinpoint.UpdateSmsTemplate
  ( -- * Creating a Request
    UpdateSmsTemplate (..),
    newUpdateSmsTemplate,

    -- * Request Lenses
    updateSmsTemplate_version,
    updateSmsTemplate_createNewVersion,
    updateSmsTemplate_templateName,
    updateSmsTemplate_sMSTemplateRequest,

    -- * Destructuring the Response
    UpdateSmsTemplateResponse (..),
    newUpdateSmsTemplateResponse,

    -- * Response Lenses
    updateSmsTemplateResponse_httpStatus,
    updateSmsTemplateResponse_messageBody,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSmsTemplate' smart constructor.
data UpdateSmsTemplate = UpdateSmsTemplate'
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
    sMSTemplateRequest :: SMSTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSmsTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'updateSmsTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'createNewVersion', 'updateSmsTemplate_createNewVersion' - Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
--
-- 'templateName', 'updateSmsTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'sMSTemplateRequest', 'updateSmsTemplate_sMSTemplateRequest' - Undocumented member.
newUpdateSmsTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'sMSTemplateRequest'
  SMSTemplateRequest ->
  UpdateSmsTemplate
newUpdateSmsTemplate
  pTemplateName_
  pSMSTemplateRequest_ =
    UpdateSmsTemplate'
      { version = Prelude.Nothing,
        createNewVersion = Prelude.Nothing,
        templateName = pTemplateName_,
        sMSTemplateRequest = pSMSTemplateRequest_
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
updateSmsTemplate_version :: Lens.Lens' UpdateSmsTemplate (Prelude.Maybe Prelude.Text)
updateSmsTemplate_version = Lens.lens (\UpdateSmsTemplate' {version} -> version) (\s@UpdateSmsTemplate' {} a -> s {version = a} :: UpdateSmsTemplate)

-- | Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
updateSmsTemplate_createNewVersion :: Lens.Lens' UpdateSmsTemplate (Prelude.Maybe Prelude.Bool)
updateSmsTemplate_createNewVersion = Lens.lens (\UpdateSmsTemplate' {createNewVersion} -> createNewVersion) (\s@UpdateSmsTemplate' {} a -> s {createNewVersion = a} :: UpdateSmsTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
updateSmsTemplate_templateName :: Lens.Lens' UpdateSmsTemplate Prelude.Text
updateSmsTemplate_templateName = Lens.lens (\UpdateSmsTemplate' {templateName} -> templateName) (\s@UpdateSmsTemplate' {} a -> s {templateName = a} :: UpdateSmsTemplate)

-- | Undocumented member.
updateSmsTemplate_sMSTemplateRequest :: Lens.Lens' UpdateSmsTemplate SMSTemplateRequest
updateSmsTemplate_sMSTemplateRequest = Lens.lens (\UpdateSmsTemplate' {sMSTemplateRequest} -> sMSTemplateRequest) (\s@UpdateSmsTemplate' {} a -> s {sMSTemplateRequest = a} :: UpdateSmsTemplate)

instance Prelude.AWSRequest UpdateSmsTemplate where
  type Rs UpdateSmsTemplate = UpdateSmsTemplateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSmsTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateSmsTemplate

instance Prelude.NFData UpdateSmsTemplate

instance Prelude.ToHeaders UpdateSmsTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateSmsTemplate where
  toJSON UpdateSmsTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SMSTemplateRequest"
                  Prelude..= sMSTemplateRequest
              )
          ]
      )

instance Prelude.ToPath UpdateSmsTemplate where
  toPath UpdateSmsTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Prelude.toBS templateName, "/sms"]

instance Prelude.ToQuery UpdateSmsTemplate where
  toQuery UpdateSmsTemplate' {..} =
    Prelude.mconcat
      [ "version" Prelude.=: version,
        "create-new-version" Prelude.=: createNewVersion
      ]

-- | /See:/ 'newUpdateSmsTemplateResponse' smart constructor.
data UpdateSmsTemplateResponse = UpdateSmsTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSmsTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSmsTemplateResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'updateSmsTemplateResponse_messageBody' - Undocumented member.
newUpdateSmsTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateSmsTemplateResponse
newUpdateSmsTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    UpdateSmsTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
updateSmsTemplateResponse_httpStatus :: Lens.Lens' UpdateSmsTemplateResponse Prelude.Int
updateSmsTemplateResponse_httpStatus = Lens.lens (\UpdateSmsTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateSmsTemplateResponse' {} a -> s {httpStatus = a} :: UpdateSmsTemplateResponse)

-- | Undocumented member.
updateSmsTemplateResponse_messageBody :: Lens.Lens' UpdateSmsTemplateResponse MessageBody
updateSmsTemplateResponse_messageBody = Lens.lens (\UpdateSmsTemplateResponse' {messageBody} -> messageBody) (\s@UpdateSmsTemplateResponse' {} a -> s {messageBody = a} :: UpdateSmsTemplateResponse)

instance Prelude.NFData UpdateSmsTemplateResponse

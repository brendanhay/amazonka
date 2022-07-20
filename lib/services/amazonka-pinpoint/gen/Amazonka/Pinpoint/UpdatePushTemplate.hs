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
-- Module      : Amazonka.Pinpoint.UpdatePushTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through
-- a push notification channel.
module Amazonka.Pinpoint.UpdatePushTemplate
  ( -- * Creating a Request
    UpdatePushTemplate (..),
    newUpdatePushTemplate,

    -- * Request Lenses
    updatePushTemplate_createNewVersion,
    updatePushTemplate_version,
    updatePushTemplate_templateName,
    updatePushTemplate_pushNotificationTemplateRequest,

    -- * Destructuring the Response
    UpdatePushTemplateResponse (..),
    newUpdatePushTemplateResponse,

    -- * Response Lenses
    updatePushTemplateResponse_httpStatus,
    updatePushTemplateResponse_messageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePushTemplate' smart constructor.
data UpdatePushTemplate = UpdatePushTemplate'
  { -- | Specifies whether to save the updates as a new version of the message
    -- template. Valid values are: true, save the updates as a new version;
    -- and, false, save the updates to (overwrite) the latest existing version
    -- of the template.
    --
    -- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
    -- the updates to (overwrites) the latest existing version of the template.
    -- If you specify a value of true for this parameter, don\'t specify a
    -- value for the version parameter. Otherwise, an error will occur.
    createNewVersion :: Prelude.Maybe Prelude.Bool,
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
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    pushNotificationTemplateRequest :: PushNotificationTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePushTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createNewVersion', 'updatePushTemplate_createNewVersion' - Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
--
-- 'version', 'updatePushTemplate_version' - The unique identifier for the version of the message template to update,
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
-- 'templateName', 'updatePushTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'pushNotificationTemplateRequest', 'updatePushTemplate_pushNotificationTemplateRequest' - Undocumented member.
newUpdatePushTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'pushNotificationTemplateRequest'
  PushNotificationTemplateRequest ->
  UpdatePushTemplate
newUpdatePushTemplate
  pTemplateName_
  pPushNotificationTemplateRequest_ =
    UpdatePushTemplate'
      { createNewVersion =
          Prelude.Nothing,
        version = Prelude.Nothing,
        templateName = pTemplateName_,
        pushNotificationTemplateRequest =
          pPushNotificationTemplateRequest_
      }

-- | Specifies whether to save the updates as a new version of the message
-- template. Valid values are: true, save the updates as a new version;
-- and, false, save the updates to (overwrite) the latest existing version
-- of the template.
--
-- If you don\'t specify a value for this parameter, Amazon Pinpoint saves
-- the updates to (overwrites) the latest existing version of the template.
-- If you specify a value of true for this parameter, don\'t specify a
-- value for the version parameter. Otherwise, an error will occur.
updatePushTemplate_createNewVersion :: Lens.Lens' UpdatePushTemplate (Prelude.Maybe Prelude.Bool)
updatePushTemplate_createNewVersion = Lens.lens (\UpdatePushTemplate' {createNewVersion} -> createNewVersion) (\s@UpdatePushTemplate' {} a -> s {createNewVersion = a} :: UpdatePushTemplate)

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
updatePushTemplate_version :: Lens.Lens' UpdatePushTemplate (Prelude.Maybe Prelude.Text)
updatePushTemplate_version = Lens.lens (\UpdatePushTemplate' {version} -> version) (\s@UpdatePushTemplate' {} a -> s {version = a} :: UpdatePushTemplate)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
updatePushTemplate_templateName :: Lens.Lens' UpdatePushTemplate Prelude.Text
updatePushTemplate_templateName = Lens.lens (\UpdatePushTemplate' {templateName} -> templateName) (\s@UpdatePushTemplate' {} a -> s {templateName = a} :: UpdatePushTemplate)

-- | Undocumented member.
updatePushTemplate_pushNotificationTemplateRequest :: Lens.Lens' UpdatePushTemplate PushNotificationTemplateRequest
updatePushTemplate_pushNotificationTemplateRequest = Lens.lens (\UpdatePushTemplate' {pushNotificationTemplateRequest} -> pushNotificationTemplateRequest) (\s@UpdatePushTemplate' {} a -> s {pushNotificationTemplateRequest = a} :: UpdatePushTemplate)

instance Core.AWSRequest UpdatePushTemplate where
  type
    AWSResponse UpdatePushTemplate =
      UpdatePushTemplateResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePushTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdatePushTemplate where
  hashWithSalt _salt UpdatePushTemplate' {..} =
    _salt `Prelude.hashWithSalt` createNewVersion
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` pushNotificationTemplateRequest

instance Prelude.NFData UpdatePushTemplate where
  rnf UpdatePushTemplate' {..} =
    Prelude.rnf createNewVersion
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf pushNotificationTemplateRequest

instance Core.ToHeaders UpdatePushTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePushTemplate where
  toJSON UpdatePushTemplate' {..} =
    Core.toJSON pushNotificationTemplateRequest

instance Core.ToPath UpdatePushTemplate where
  toPath UpdatePushTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/push"]

instance Core.ToQuery UpdatePushTemplate where
  toQuery UpdatePushTemplate' {..} =
    Prelude.mconcat
      [ "create-new-version" Core.=: createNewVersion,
        "version" Core.=: version
      ]

-- | /See:/ 'newUpdatePushTemplateResponse' smart constructor.
data UpdatePushTemplateResponse = UpdatePushTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePushTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePushTemplateResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'updatePushTemplateResponse_messageBody' - Undocumented member.
newUpdatePushTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdatePushTemplateResponse
newUpdatePushTemplateResponse
  pHttpStatus_
  pMessageBody_ =
    UpdatePushTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
updatePushTemplateResponse_httpStatus :: Lens.Lens' UpdatePushTemplateResponse Prelude.Int
updatePushTemplateResponse_httpStatus = Lens.lens (\UpdatePushTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdatePushTemplateResponse' {} a -> s {httpStatus = a} :: UpdatePushTemplateResponse)

-- | Undocumented member.
updatePushTemplateResponse_messageBody :: Lens.Lens' UpdatePushTemplateResponse MessageBody
updatePushTemplateResponse_messageBody = Lens.lens (\UpdatePushTemplateResponse' {messageBody} -> messageBody) (\s@UpdatePushTemplateResponse' {} a -> s {messageBody = a} :: UpdatePushTemplateResponse)

instance Prelude.NFData UpdatePushTemplateResponse where
  rnf UpdatePushTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messageBody

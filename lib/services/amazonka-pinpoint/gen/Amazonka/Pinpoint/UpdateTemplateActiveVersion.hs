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
-- Module      : Amazonka.Pinpoint.UpdateTemplateActiveVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of a specific version of a message template to
-- /active/.
module Amazonka.Pinpoint.UpdateTemplateActiveVersion
  ( -- * Creating a Request
    UpdateTemplateActiveVersion (..),
    newUpdateTemplateActiveVersion,

    -- * Request Lenses
    updateTemplateActiveVersion_templateName,
    updateTemplateActiveVersion_templateType,
    updateTemplateActiveVersion_templateActiveVersionRequest,

    -- * Destructuring the Response
    UpdateTemplateActiveVersionResponse (..),
    newUpdateTemplateActiveVersionResponse,

    -- * Response Lenses
    updateTemplateActiveVersionResponse_httpStatus,
    updateTemplateActiveVersionResponse_messageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTemplateActiveVersion' smart constructor.
data UpdateTemplateActiveVersion = UpdateTemplateActiveVersion'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    -- | The type of channel that the message template is designed for. Valid
    -- values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Prelude.Text,
    templateActiveVersionRequest :: TemplateActiveVersionRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplateActiveVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'updateTemplateActiveVersion_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'templateType', 'updateTemplateActiveVersion_templateType' - The type of channel that the message template is designed for. Valid
-- values are: EMAIL, PUSH, SMS, and VOICE.
--
-- 'templateActiveVersionRequest', 'updateTemplateActiveVersion_templateActiveVersionRequest' - Undocumented member.
newUpdateTemplateActiveVersion ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  Prelude.Text ->
  -- | 'templateActiveVersionRequest'
  TemplateActiveVersionRequest ->
  UpdateTemplateActiveVersion
newUpdateTemplateActiveVersion
  pTemplateName_
  pTemplateType_
  pTemplateActiveVersionRequest_ =
    UpdateTemplateActiveVersion'
      { templateName =
          pTemplateName_,
        templateType = pTemplateType_,
        templateActiveVersionRequest =
          pTemplateActiveVersionRequest_
      }

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
updateTemplateActiveVersion_templateName :: Lens.Lens' UpdateTemplateActiveVersion Prelude.Text
updateTemplateActiveVersion_templateName = Lens.lens (\UpdateTemplateActiveVersion' {templateName} -> templateName) (\s@UpdateTemplateActiveVersion' {} a -> s {templateName = a} :: UpdateTemplateActiveVersion)

-- | The type of channel that the message template is designed for. Valid
-- values are: EMAIL, PUSH, SMS, and VOICE.
updateTemplateActiveVersion_templateType :: Lens.Lens' UpdateTemplateActiveVersion Prelude.Text
updateTemplateActiveVersion_templateType = Lens.lens (\UpdateTemplateActiveVersion' {templateType} -> templateType) (\s@UpdateTemplateActiveVersion' {} a -> s {templateType = a} :: UpdateTemplateActiveVersion)

-- | Undocumented member.
updateTemplateActiveVersion_templateActiveVersionRequest :: Lens.Lens' UpdateTemplateActiveVersion TemplateActiveVersionRequest
updateTemplateActiveVersion_templateActiveVersionRequest = Lens.lens (\UpdateTemplateActiveVersion' {templateActiveVersionRequest} -> templateActiveVersionRequest) (\s@UpdateTemplateActiveVersion' {} a -> s {templateActiveVersionRequest = a} :: UpdateTemplateActiveVersion)

instance Core.AWSRequest UpdateTemplateActiveVersion where
  type
    AWSResponse UpdateTemplateActiveVersion =
      UpdateTemplateActiveVersionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTemplateActiveVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateTemplateActiveVersion where
  hashWithSalt _salt UpdateTemplateActiveVersion' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType
      `Prelude.hashWithSalt` templateActiveVersionRequest

instance Prelude.NFData UpdateTemplateActiveVersion where
  rnf UpdateTemplateActiveVersion' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType
      `Prelude.seq` Prelude.rnf templateActiveVersionRequest

instance Core.ToHeaders UpdateTemplateActiveVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTemplateActiveVersion where
  toJSON UpdateTemplateActiveVersion' {..} =
    Core.toJSON templateActiveVersionRequest

instance Core.ToPath UpdateTemplateActiveVersion where
  toPath UpdateTemplateActiveVersion' {..} =
    Prelude.mconcat
      [ "/v1/templates/",
        Core.toBS templateName,
        "/",
        Core.toBS templateType,
        "/active-version"
      ]

instance Core.ToQuery UpdateTemplateActiveVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTemplateActiveVersionResponse' smart constructor.
data UpdateTemplateActiveVersionResponse = UpdateTemplateActiveVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    messageBody :: MessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplateActiveVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTemplateActiveVersionResponse_httpStatus' - The response's http status code.
--
-- 'messageBody', 'updateTemplateActiveVersionResponse_messageBody' - Undocumented member.
newUpdateTemplateActiveVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'messageBody'
  MessageBody ->
  UpdateTemplateActiveVersionResponse
newUpdateTemplateActiveVersionResponse
  pHttpStatus_
  pMessageBody_ =
    UpdateTemplateActiveVersionResponse'
      { httpStatus =
          pHttpStatus_,
        messageBody = pMessageBody_
      }

-- | The response's http status code.
updateTemplateActiveVersionResponse_httpStatus :: Lens.Lens' UpdateTemplateActiveVersionResponse Prelude.Int
updateTemplateActiveVersionResponse_httpStatus = Lens.lens (\UpdateTemplateActiveVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateTemplateActiveVersionResponse' {} a -> s {httpStatus = a} :: UpdateTemplateActiveVersionResponse)

-- | Undocumented member.
updateTemplateActiveVersionResponse_messageBody :: Lens.Lens' UpdateTemplateActiveVersionResponse MessageBody
updateTemplateActiveVersionResponse_messageBody = Lens.lens (\UpdateTemplateActiveVersionResponse' {messageBody} -> messageBody) (\s@UpdateTemplateActiveVersionResponse' {} a -> s {messageBody = a} :: UpdateTemplateActiveVersionResponse)

instance
  Prelude.NFData
    UpdateTemplateActiveVersionResponse
  where
  rnf UpdateTemplateActiveVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messageBody

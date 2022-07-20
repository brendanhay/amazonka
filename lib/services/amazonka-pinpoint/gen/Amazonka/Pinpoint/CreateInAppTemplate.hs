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
-- Module      : Amazonka.Pinpoint.CreateInAppTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new message template for messages using the in-app message
-- channel.
module Amazonka.Pinpoint.CreateInAppTemplate
  ( -- * Creating a Request
    CreateInAppTemplate (..),
    newCreateInAppTemplate,

    -- * Request Lenses
    createInAppTemplate_templateName,
    createInAppTemplate_inAppTemplateRequest,

    -- * Destructuring the Response
    CreateInAppTemplateResponse (..),
    newCreateInAppTemplateResponse,

    -- * Response Lenses
    createInAppTemplateResponse_httpStatus,
    createInAppTemplateResponse_templateCreateMessageBody,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInAppTemplate' smart constructor.
data CreateInAppTemplate = CreateInAppTemplate'
  { -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    inAppTemplateRequest :: InAppTemplateRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInAppTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createInAppTemplate_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'inAppTemplateRequest', 'createInAppTemplate_inAppTemplateRequest' - Undocumented member.
newCreateInAppTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'inAppTemplateRequest'
  InAppTemplateRequest ->
  CreateInAppTemplate
newCreateInAppTemplate
  pTemplateName_
  pInAppTemplateRequest_ =
    CreateInAppTemplate'
      { templateName = pTemplateName_,
        inAppTemplateRequest = pInAppTemplateRequest_
      }

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
createInAppTemplate_templateName :: Lens.Lens' CreateInAppTemplate Prelude.Text
createInAppTemplate_templateName = Lens.lens (\CreateInAppTemplate' {templateName} -> templateName) (\s@CreateInAppTemplate' {} a -> s {templateName = a} :: CreateInAppTemplate)

-- | Undocumented member.
createInAppTemplate_inAppTemplateRequest :: Lens.Lens' CreateInAppTemplate InAppTemplateRequest
createInAppTemplate_inAppTemplateRequest = Lens.lens (\CreateInAppTemplate' {inAppTemplateRequest} -> inAppTemplateRequest) (\s@CreateInAppTemplate' {} a -> s {inAppTemplateRequest = a} :: CreateInAppTemplate)

instance Core.AWSRequest CreateInAppTemplate where
  type
    AWSResponse CreateInAppTemplate =
      CreateInAppTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInAppTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreateInAppTemplate where
  hashWithSalt _salt CreateInAppTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` inAppTemplateRequest

instance Prelude.NFData CreateInAppTemplate where
  rnf CreateInAppTemplate' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf inAppTemplateRequest

instance Core.ToHeaders CreateInAppTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateInAppTemplate where
  toJSON CreateInAppTemplate' {..} =
    Core.toJSON inAppTemplateRequest

instance Core.ToPath CreateInAppTemplate where
  toPath CreateInAppTemplate' {..} =
    Prelude.mconcat
      ["/v1/templates/", Core.toBS templateName, "/inapp"]

instance Core.ToQuery CreateInAppTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInAppTemplateResponse' smart constructor.
data CreateInAppTemplateResponse = CreateInAppTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    templateCreateMessageBody :: TemplateCreateMessageBody
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInAppTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createInAppTemplateResponse_httpStatus' - The response's http status code.
--
-- 'templateCreateMessageBody', 'createInAppTemplateResponse_templateCreateMessageBody' - Undocumented member.
newCreateInAppTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'templateCreateMessageBody'
  TemplateCreateMessageBody ->
  CreateInAppTemplateResponse
newCreateInAppTemplateResponse
  pHttpStatus_
  pTemplateCreateMessageBody_ =
    CreateInAppTemplateResponse'
      { httpStatus =
          pHttpStatus_,
        templateCreateMessageBody =
          pTemplateCreateMessageBody_
      }

-- | The response's http status code.
createInAppTemplateResponse_httpStatus :: Lens.Lens' CreateInAppTemplateResponse Prelude.Int
createInAppTemplateResponse_httpStatus = Lens.lens (\CreateInAppTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateInAppTemplateResponse' {} a -> s {httpStatus = a} :: CreateInAppTemplateResponse)

-- | Undocumented member.
createInAppTemplateResponse_templateCreateMessageBody :: Lens.Lens' CreateInAppTemplateResponse TemplateCreateMessageBody
createInAppTemplateResponse_templateCreateMessageBody = Lens.lens (\CreateInAppTemplateResponse' {templateCreateMessageBody} -> templateCreateMessageBody) (\s@CreateInAppTemplateResponse' {} a -> s {templateCreateMessageBody = a} :: CreateInAppTemplateResponse)

instance Prelude.NFData CreateInAppTemplateResponse where
  rnf CreateInAppTemplateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templateCreateMessageBody

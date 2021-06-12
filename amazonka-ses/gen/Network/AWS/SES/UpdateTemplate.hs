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
-- Module      : Network.AWS.SES.UpdateTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an email template. Email templates enable you to send
-- personalized email to one or more destinations in a single API
-- operation. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.UpdateTemplate
  ( -- * Creating a Request
    UpdateTemplate (..),
    newUpdateTemplate,

    -- * Request Lenses
    updateTemplate_template,

    -- * Destructuring the Response
    UpdateTemplateResponse (..),
    newUpdateTemplateResponse,

    -- * Response Lenses
    updateTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | /See:/ 'newUpdateTemplate' smart constructor.
data UpdateTemplate = UpdateTemplate'
  { template :: Template
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'updateTemplate_template' - Undocumented member.
newUpdateTemplate ::
  -- | 'template'
  Template ->
  UpdateTemplate
newUpdateTemplate pTemplate_ =
  UpdateTemplate' {template = pTemplate_}

-- | Undocumented member.
updateTemplate_template :: Lens.Lens' UpdateTemplate Template
updateTemplate_template = Lens.lens (\UpdateTemplate' {template} -> template) (\s@UpdateTemplate' {} a -> s {template = a} :: UpdateTemplate)

instance Core.AWSRequest UpdateTemplate where
  type
    AWSResponse UpdateTemplate =
      UpdateTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateTemplateResult"
      ( \s h x ->
          UpdateTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTemplate

instance Core.NFData UpdateTemplate

instance Core.ToHeaders UpdateTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateTemplate where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTemplate where
  toQuery UpdateTemplate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateTemplate" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "Template" Core.=: template
      ]

-- | /See:/ 'newUpdateTemplateResponse' smart constructor.
data UpdateTemplateResponse = UpdateTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTemplateResponse_httpStatus' - The response's http status code.
newUpdateTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTemplateResponse
newUpdateTemplateResponse pHttpStatus_ =
  UpdateTemplateResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateTemplateResponse_httpStatus :: Lens.Lens' UpdateTemplateResponse Core.Int
updateTemplateResponse_httpStatus = Lens.lens (\UpdateTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateTemplateResponse' {} a -> s {httpStatus = a} :: UpdateTemplateResponse)

instance Core.NFData UpdateTemplateResponse

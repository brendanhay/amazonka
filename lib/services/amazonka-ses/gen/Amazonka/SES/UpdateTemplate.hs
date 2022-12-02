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
-- Module      : Amazonka.SES.UpdateTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SES.UpdateTemplate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | /See:/ 'newUpdateTemplate' smart constructor.
data UpdateTemplate = UpdateTemplate'
  { template :: Template
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateTemplateResult"
      ( \s h x ->
          UpdateTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTemplate where
  hashWithSalt _salt UpdateTemplate' {..} =
    _salt `Prelude.hashWithSalt` template

instance Prelude.NFData UpdateTemplate where
  rnf UpdateTemplate' {..} = Prelude.rnf template

instance Data.ToHeaders UpdateTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTemplate where
  toQuery UpdateTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdateTemplate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Template" Data.=: template
      ]

-- | /See:/ 'newUpdateTemplateResponse' smart constructor.
data UpdateTemplateResponse = UpdateTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTemplateResponse
newUpdateTemplateResponse pHttpStatus_ =
  UpdateTemplateResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateTemplateResponse_httpStatus :: Lens.Lens' UpdateTemplateResponse Prelude.Int
updateTemplateResponse_httpStatus = Lens.lens (\UpdateTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateTemplateResponse' {} a -> s {httpStatus = a} :: UpdateTemplateResponse)

instance Prelude.NFData UpdateTemplateResponse where
  rnf UpdateTemplateResponse' {..} =
    Prelude.rnf httpStatus

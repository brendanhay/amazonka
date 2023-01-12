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
-- Module      : Amazonka.SES.CreateTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email template. Email templates enable you to send
-- personalized email to one or more destinations in a single API
-- operation. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.CreateTemplate
  ( -- * Creating a Request
    CreateTemplate (..),
    newCreateTemplate,

    -- * Request Lenses
    createTemplate_template,

    -- * Destructuring the Response
    CreateTemplateResponse (..),
    newCreateTemplateResponse,

    -- * Response Lenses
    createTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to create an email template. For more information,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/send-personalized-email-api.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateTemplate' smart constructor.
data CreateTemplate = CreateTemplate'
  { -- | The content of the email, composed of a subject line, an HTML part, and
    -- a text-only part.
    template :: Template
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'createTemplate_template' - The content of the email, composed of a subject line, an HTML part, and
-- a text-only part.
newCreateTemplate ::
  -- | 'template'
  Template ->
  CreateTemplate
newCreateTemplate pTemplate_ =
  CreateTemplate' {template = pTemplate_}

-- | The content of the email, composed of a subject line, an HTML part, and
-- a text-only part.
createTemplate_template :: Lens.Lens' CreateTemplate Template
createTemplate_template = Lens.lens (\CreateTemplate' {template} -> template) (\s@CreateTemplate' {} a -> s {template = a} :: CreateTemplate)

instance Core.AWSRequest CreateTemplate where
  type
    AWSResponse CreateTemplate =
      CreateTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateTemplateResult"
      ( \s h x ->
          CreateTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTemplate where
  hashWithSalt _salt CreateTemplate' {..} =
    _salt `Prelude.hashWithSalt` template

instance Prelude.NFData CreateTemplate where
  rnf CreateTemplate' {..} = Prelude.rnf template

instance Data.ToHeaders CreateTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTemplate where
  toQuery CreateTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateTemplate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Template" Data.=: template
      ]

-- | /See:/ 'newCreateTemplateResponse' smart constructor.
data CreateTemplateResponse = CreateTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTemplateResponse_httpStatus' - The response's http status code.
newCreateTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTemplateResponse
newCreateTemplateResponse pHttpStatus_ =
  CreateTemplateResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createTemplateResponse_httpStatus :: Lens.Lens' CreateTemplateResponse Prelude.Int
createTemplateResponse_httpStatus = Lens.lens (\CreateTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateTemplateResponse' {} a -> s {httpStatus = a} :: CreateTemplateResponse)

instance Prelude.NFData CreateTemplateResponse where
  rnf CreateTemplateResponse' {..} =
    Prelude.rnf httpStatus

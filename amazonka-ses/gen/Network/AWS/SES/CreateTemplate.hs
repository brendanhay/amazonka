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
-- Module      : Network.AWS.SES.CreateTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SES.CreateTemplate
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CreateTemplate where
  type Rs CreateTemplate = CreateTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateTemplateResult"
      ( \s h x ->
          CreateTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTemplate

instance Prelude.NFData CreateTemplate

instance Prelude.ToHeaders CreateTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateTemplate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTemplate where
  toQuery CreateTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateTemplate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "Template" Prelude.=: template
      ]

-- | /See:/ 'newCreateTemplateResponse' smart constructor.
data CreateTemplateResponse = CreateTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateTemplateResponse

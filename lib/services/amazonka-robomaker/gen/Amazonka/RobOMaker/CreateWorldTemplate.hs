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
-- Module      : Amazonka.RobOMaker.CreateWorldTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a world template.
module Amazonka.RobOMaker.CreateWorldTemplate
  ( -- * Creating a Request
    CreateWorldTemplate (..),
    newCreateWorldTemplate,

    -- * Request Lenses
    createWorldTemplate_tags,
    createWorldTemplate_name,
    createWorldTemplate_clientRequestToken,
    createWorldTemplate_templateBody,
    createWorldTemplate_templateLocation,

    -- * Destructuring the Response
    CreateWorldTemplateResponse (..),
    newCreateWorldTemplateResponse,

    -- * Response Lenses
    createWorldTemplateResponse_tags,
    createWorldTemplateResponse_name,
    createWorldTemplateResponse_clientRequestToken,
    createWorldTemplateResponse_arn,
    createWorldTemplateResponse_createdAt,
    createWorldTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateWorldTemplate' smart constructor.
data CreateWorldTemplate = CreateWorldTemplate'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- world template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the world template.
    name :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The world template body.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The location of the world template.
    templateLocation :: Prelude.Maybe TemplateLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorldTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorldTemplate_tags' - A map that contains tag keys and tag values that are attached to the
-- world template.
--
-- 'name', 'createWorldTemplate_name' - The name of the world template.
--
-- 'clientRequestToken', 'createWorldTemplate_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'templateBody', 'createWorldTemplate_templateBody' - The world template body.
--
-- 'templateLocation', 'createWorldTemplate_templateLocation' - The location of the world template.
newCreateWorldTemplate ::
  CreateWorldTemplate
newCreateWorldTemplate =
  CreateWorldTemplate'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      templateLocation = Prelude.Nothing
    }

-- | A map that contains tag keys and tag values that are attached to the
-- world template.
createWorldTemplate_tags :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldTemplate_tags = Lens.lens (\CreateWorldTemplate' {tags} -> tags) (\s@CreateWorldTemplate' {} a -> s {tags = a} :: CreateWorldTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The name of the world template.
createWorldTemplate_name :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe Prelude.Text)
createWorldTemplate_name = Lens.lens (\CreateWorldTemplate' {name} -> name) (\s@CreateWorldTemplate' {} a -> s {name = a} :: CreateWorldTemplate)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldTemplate_clientRequestToken :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe Prelude.Text)
createWorldTemplate_clientRequestToken = Lens.lens (\CreateWorldTemplate' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldTemplate' {} a -> s {clientRequestToken = a} :: CreateWorldTemplate)

-- | The world template body.
createWorldTemplate_templateBody :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe Prelude.Text)
createWorldTemplate_templateBody = Lens.lens (\CreateWorldTemplate' {templateBody} -> templateBody) (\s@CreateWorldTemplate' {} a -> s {templateBody = a} :: CreateWorldTemplate)

-- | The location of the world template.
createWorldTemplate_templateLocation :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe TemplateLocation)
createWorldTemplate_templateLocation = Lens.lens (\CreateWorldTemplate' {templateLocation} -> templateLocation) (\s@CreateWorldTemplate' {} a -> s {templateLocation = a} :: CreateWorldTemplate)

instance Core.AWSRequest CreateWorldTemplate where
  type
    AWSResponse CreateWorldTemplate =
      CreateWorldTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorldTemplateResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "clientRequestToken")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorldTemplate where
  hashWithSalt _salt CreateWorldTemplate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` templateLocation

instance Prelude.NFData CreateWorldTemplate where
  rnf CreateWorldTemplate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateLocation

instance Core.ToHeaders CreateWorldTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorldTemplate where
  toJSON CreateWorldTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("name" Core..=) Prelude.<$> name,
            ("clientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("templateBody" Core..=) Prelude.<$> templateBody,
            ("templateLocation" Core..=)
              Prelude.<$> templateLocation
          ]
      )

instance Core.ToPath CreateWorldTemplate where
  toPath = Prelude.const "/createWorldTemplate"

instance Core.ToQuery CreateWorldTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorldTemplateResponse' smart constructor.
data CreateWorldTemplateResponse = CreateWorldTemplateResponse'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- world template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the world template.
    name :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the world template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world template was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorldTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorldTemplateResponse_tags' - A map that contains tag keys and tag values that are attached to the
-- world template.
--
-- 'name', 'createWorldTemplateResponse_name' - The name of the world template.
--
-- 'clientRequestToken', 'createWorldTemplateResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'arn', 'createWorldTemplateResponse_arn' - The Amazon Resource Name (ARN) of the world template.
--
-- 'createdAt', 'createWorldTemplateResponse_createdAt' - The time, in milliseconds since the epoch, when the world template was
-- created.
--
-- 'httpStatus', 'createWorldTemplateResponse_httpStatus' - The response's http status code.
newCreateWorldTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorldTemplateResponse
newCreateWorldTemplateResponse pHttpStatus_ =
  CreateWorldTemplateResponse'
    { tags =
        Prelude.Nothing,
      name = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map that contains tag keys and tag values that are attached to the
-- world template.
createWorldTemplateResponse_tags :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldTemplateResponse_tags = Lens.lens (\CreateWorldTemplateResponse' {tags} -> tags) (\s@CreateWorldTemplateResponse' {} a -> s {tags = a} :: CreateWorldTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the world template.
createWorldTemplateResponse_name :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
createWorldTemplateResponse_name = Lens.lens (\CreateWorldTemplateResponse' {name} -> name) (\s@CreateWorldTemplateResponse' {} a -> s {name = a} :: CreateWorldTemplateResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldTemplateResponse_clientRequestToken :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
createWorldTemplateResponse_clientRequestToken = Lens.lens (\CreateWorldTemplateResponse' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldTemplateResponse' {} a -> s {clientRequestToken = a} :: CreateWorldTemplateResponse)

-- | The Amazon Resource Name (ARN) of the world template.
createWorldTemplateResponse_arn :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
createWorldTemplateResponse_arn = Lens.lens (\CreateWorldTemplateResponse' {arn} -> arn) (\s@CreateWorldTemplateResponse' {} a -> s {arn = a} :: CreateWorldTemplateResponse)

-- | The time, in milliseconds since the epoch, when the world template was
-- created.
createWorldTemplateResponse_createdAt :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.UTCTime)
createWorldTemplateResponse_createdAt = Lens.lens (\CreateWorldTemplateResponse' {createdAt} -> createdAt) (\s@CreateWorldTemplateResponse' {} a -> s {createdAt = a} :: CreateWorldTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
createWorldTemplateResponse_httpStatus :: Lens.Lens' CreateWorldTemplateResponse Prelude.Int
createWorldTemplateResponse_httpStatus = Lens.lens (\CreateWorldTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateWorldTemplateResponse' {} a -> s {httpStatus = a} :: CreateWorldTemplateResponse)

instance Prelude.NFData CreateWorldTemplateResponse where
  rnf CreateWorldTemplateResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a world template.
module Amazonka.RobOMaker.CreateWorldTemplate
  ( -- * Creating a Request
    CreateWorldTemplate (..),
    newCreateWorldTemplate,

    -- * Request Lenses
    createWorldTemplate_clientRequestToken,
    createWorldTemplate_name,
    createWorldTemplate_tags,
    createWorldTemplate_templateBody,
    createWorldTemplate_templateLocation,

    -- * Destructuring the Response
    CreateWorldTemplateResponse (..),
    newCreateWorldTemplateResponse,

    -- * Response Lenses
    createWorldTemplateResponse_arn,
    createWorldTemplateResponse_clientRequestToken,
    createWorldTemplateResponse_createdAt,
    createWorldTemplateResponse_name,
    createWorldTemplateResponse_tags,
    createWorldTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateWorldTemplate' smart constructor.
data CreateWorldTemplate = CreateWorldTemplate'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the world template.
    name :: Prelude.Maybe Prelude.Text,
    -- | A map that contains tag keys and tag values that are attached to the
    -- world template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'clientRequestToken', 'createWorldTemplate_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'name', 'createWorldTemplate_name' - The name of the world template.
--
-- 'tags', 'createWorldTemplate_tags' - A map that contains tag keys and tag values that are attached to the
-- world template.
--
-- 'templateBody', 'createWorldTemplate_templateBody' - The world template body.
--
-- 'templateLocation', 'createWorldTemplate_templateLocation' - The location of the world template.
newCreateWorldTemplate ::
  CreateWorldTemplate
newCreateWorldTemplate =
  CreateWorldTemplate'
    { clientRequestToken =
        Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      templateLocation = Prelude.Nothing
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldTemplate_clientRequestToken :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe Prelude.Text)
createWorldTemplate_clientRequestToken = Lens.lens (\CreateWorldTemplate' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldTemplate' {} a -> s {clientRequestToken = a} :: CreateWorldTemplate)

-- | The name of the world template.
createWorldTemplate_name :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe Prelude.Text)
createWorldTemplate_name = Lens.lens (\CreateWorldTemplate' {name} -> name) (\s@CreateWorldTemplate' {} a -> s {name = a} :: CreateWorldTemplate)

-- | A map that contains tag keys and tag values that are attached to the
-- world template.
createWorldTemplate_tags :: Lens.Lens' CreateWorldTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldTemplate_tags = Lens.lens (\CreateWorldTemplate' {tags} -> tags) (\s@CreateWorldTemplate' {} a -> s {tags = a} :: CreateWorldTemplate) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "clientRequestToken")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorldTemplate where
  hashWithSalt _salt CreateWorldTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` templateLocation

instance Prelude.NFData CreateWorldTemplate where
  rnf CreateWorldTemplate' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateLocation

instance Data.ToHeaders CreateWorldTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorldTemplate where
  toJSON CreateWorldTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags,
            ("templateBody" Data..=) Prelude.<$> templateBody,
            ("templateLocation" Data..=)
              Prelude.<$> templateLocation
          ]
      )

instance Data.ToPath CreateWorldTemplate where
  toPath = Prelude.const "/createWorldTemplate"

instance Data.ToQuery CreateWorldTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorldTemplateResponse' smart constructor.
data CreateWorldTemplateResponse = CreateWorldTemplateResponse'
  { -- | The Amazon Resource Name (ARN) of the world template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world template was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the world template.
    name :: Prelude.Maybe Prelude.Text,
    -- | A map that contains tag keys and tag values that are attached to the
    -- world template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'arn', 'createWorldTemplateResponse_arn' - The Amazon Resource Name (ARN) of the world template.
--
-- 'clientRequestToken', 'createWorldTemplateResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'createdAt', 'createWorldTemplateResponse_createdAt' - The time, in milliseconds since the epoch, when the world template was
-- created.
--
-- 'name', 'createWorldTemplateResponse_name' - The name of the world template.
--
-- 'tags', 'createWorldTemplateResponse_tags' - A map that contains tag keys and tag values that are attached to the
-- world template.
--
-- 'httpStatus', 'createWorldTemplateResponse_httpStatus' - The response's http status code.
newCreateWorldTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorldTemplateResponse
newCreateWorldTemplateResponse pHttpStatus_ =
  CreateWorldTemplateResponse'
    { arn = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the world template.
createWorldTemplateResponse_arn :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
createWorldTemplateResponse_arn = Lens.lens (\CreateWorldTemplateResponse' {arn} -> arn) (\s@CreateWorldTemplateResponse' {} a -> s {arn = a} :: CreateWorldTemplateResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldTemplateResponse_clientRequestToken :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
createWorldTemplateResponse_clientRequestToken = Lens.lens (\CreateWorldTemplateResponse' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldTemplateResponse' {} a -> s {clientRequestToken = a} :: CreateWorldTemplateResponse)

-- | The time, in milliseconds since the epoch, when the world template was
-- created.
createWorldTemplateResponse_createdAt :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.UTCTime)
createWorldTemplateResponse_createdAt = Lens.lens (\CreateWorldTemplateResponse' {createdAt} -> createdAt) (\s@CreateWorldTemplateResponse' {} a -> s {createdAt = a} :: CreateWorldTemplateResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the world template.
createWorldTemplateResponse_name :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe Prelude.Text)
createWorldTemplateResponse_name = Lens.lens (\CreateWorldTemplateResponse' {name} -> name) (\s@CreateWorldTemplateResponse' {} a -> s {name = a} :: CreateWorldTemplateResponse)

-- | A map that contains tag keys and tag values that are attached to the
-- world template.
createWorldTemplateResponse_tags :: Lens.Lens' CreateWorldTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldTemplateResponse_tags = Lens.lens (\CreateWorldTemplateResponse' {tags} -> tags) (\s@CreateWorldTemplateResponse' {} a -> s {tags = a} :: CreateWorldTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createWorldTemplateResponse_httpStatus :: Lens.Lens' CreateWorldTemplateResponse Prelude.Int
createWorldTemplateResponse_httpStatus = Lens.lens (\CreateWorldTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateWorldTemplateResponse' {} a -> s {httpStatus = a} :: CreateWorldTemplateResponse)

instance Prelude.NFData CreateWorldTemplateResponse where
  rnf CreateWorldTemplateResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.RobOMaker.DescribeWorldTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a world template.
module Amazonka.RobOMaker.DescribeWorldTemplate
  ( -- * Creating a Request
    DescribeWorldTemplate (..),
    newDescribeWorldTemplate,

    -- * Request Lenses
    describeWorldTemplate_template,

    -- * Destructuring the Response
    DescribeWorldTemplateResponse (..),
    newDescribeWorldTemplateResponse,

    -- * Response Lenses
    describeWorldTemplateResponse_tags,
    describeWorldTemplateResponse_name,
    describeWorldTemplateResponse_clientRequestToken,
    describeWorldTemplateResponse_lastUpdatedAt,
    describeWorldTemplateResponse_arn,
    describeWorldTemplateResponse_createdAt,
    describeWorldTemplateResponse_version,
    describeWorldTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeWorldTemplate' smart constructor.
data DescribeWorldTemplate = DescribeWorldTemplate'
  { -- | The Amazon Resource Name (arn) of the world template you want to
    -- describe.
    template :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorldTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'describeWorldTemplate_template' - The Amazon Resource Name (arn) of the world template you want to
-- describe.
newDescribeWorldTemplate ::
  -- | 'template'
  Prelude.Text ->
  DescribeWorldTemplate
newDescribeWorldTemplate pTemplate_ =
  DescribeWorldTemplate' {template = pTemplate_}

-- | The Amazon Resource Name (arn) of the world template you want to
-- describe.
describeWorldTemplate_template :: Lens.Lens' DescribeWorldTemplate Prelude.Text
describeWorldTemplate_template = Lens.lens (\DescribeWorldTemplate' {template} -> template) (\s@DescribeWorldTemplate' {} a -> s {template = a} :: DescribeWorldTemplate)

instance Core.AWSRequest DescribeWorldTemplate where
  type
    AWSResponse DescribeWorldTemplate =
      DescribeWorldTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorldTemplateResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "clientRequestToken")
            Prelude.<*> (x Core..?> "lastUpdatedAt")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorldTemplate where
  hashWithSalt _salt DescribeWorldTemplate' {..} =
    _salt `Prelude.hashWithSalt` template

instance Prelude.NFData DescribeWorldTemplate where
  rnf DescribeWorldTemplate' {..} = Prelude.rnf template

instance Core.ToHeaders DescribeWorldTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorldTemplate where
  toJSON DescribeWorldTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("template" Core..= template)]
      )

instance Core.ToPath DescribeWorldTemplate where
  toPath = Prelude.const "/describeWorldTemplate"

instance Core.ToQuery DescribeWorldTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorldTemplateResponse' smart constructor.
data DescribeWorldTemplateResponse = DescribeWorldTemplateResponse'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- world template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the world template.
    name :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world template was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the world template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world template was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The version of the world template that you\'re using.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorldTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeWorldTemplateResponse_tags' - A map that contains tag keys and tag values that are attached to the
-- world template.
--
-- 'name', 'describeWorldTemplateResponse_name' - The name of the world template.
--
-- 'clientRequestToken', 'describeWorldTemplateResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'lastUpdatedAt', 'describeWorldTemplateResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the world template was
-- last updated.
--
-- 'arn', 'describeWorldTemplateResponse_arn' - The Amazon Resource Name (ARN) of the world template.
--
-- 'createdAt', 'describeWorldTemplateResponse_createdAt' - The time, in milliseconds since the epoch, when the world template was
-- created.
--
-- 'version', 'describeWorldTemplateResponse_version' - The version of the world template that you\'re using.
--
-- 'httpStatus', 'describeWorldTemplateResponse_httpStatus' - The response's http status code.
newDescribeWorldTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorldTemplateResponse
newDescribeWorldTemplateResponse pHttpStatus_ =
  DescribeWorldTemplateResponse'
    { tags =
        Prelude.Nothing,
      name = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map that contains tag keys and tag values that are attached to the
-- world template.
describeWorldTemplateResponse_tags :: Lens.Lens' DescribeWorldTemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeWorldTemplateResponse_tags = Lens.lens (\DescribeWorldTemplateResponse' {tags} -> tags) (\s@DescribeWorldTemplateResponse' {} a -> s {tags = a} :: DescribeWorldTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the world template.
describeWorldTemplateResponse_name :: Lens.Lens' DescribeWorldTemplateResponse (Prelude.Maybe Prelude.Text)
describeWorldTemplateResponse_name = Lens.lens (\DescribeWorldTemplateResponse' {name} -> name) (\s@DescribeWorldTemplateResponse' {} a -> s {name = a} :: DescribeWorldTemplateResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
describeWorldTemplateResponse_clientRequestToken :: Lens.Lens' DescribeWorldTemplateResponse (Prelude.Maybe Prelude.Text)
describeWorldTemplateResponse_clientRequestToken = Lens.lens (\DescribeWorldTemplateResponse' {clientRequestToken} -> clientRequestToken) (\s@DescribeWorldTemplateResponse' {} a -> s {clientRequestToken = a} :: DescribeWorldTemplateResponse)

-- | The time, in milliseconds since the epoch, when the world template was
-- last updated.
describeWorldTemplateResponse_lastUpdatedAt :: Lens.Lens' DescribeWorldTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeWorldTemplateResponse_lastUpdatedAt = Lens.lens (\DescribeWorldTemplateResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeWorldTemplateResponse' {} a -> s {lastUpdatedAt = a} :: DescribeWorldTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the world template.
describeWorldTemplateResponse_arn :: Lens.Lens' DescribeWorldTemplateResponse (Prelude.Maybe Prelude.Text)
describeWorldTemplateResponse_arn = Lens.lens (\DescribeWorldTemplateResponse' {arn} -> arn) (\s@DescribeWorldTemplateResponse' {} a -> s {arn = a} :: DescribeWorldTemplateResponse)

-- | The time, in milliseconds since the epoch, when the world template was
-- created.
describeWorldTemplateResponse_createdAt :: Lens.Lens' DescribeWorldTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeWorldTemplateResponse_createdAt = Lens.lens (\DescribeWorldTemplateResponse' {createdAt} -> createdAt) (\s@DescribeWorldTemplateResponse' {} a -> s {createdAt = a} :: DescribeWorldTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The version of the world template that you\'re using.
describeWorldTemplateResponse_version :: Lens.Lens' DescribeWorldTemplateResponse (Prelude.Maybe Prelude.Text)
describeWorldTemplateResponse_version = Lens.lens (\DescribeWorldTemplateResponse' {version} -> version) (\s@DescribeWorldTemplateResponse' {} a -> s {version = a} :: DescribeWorldTemplateResponse)

-- | The response's http status code.
describeWorldTemplateResponse_httpStatus :: Lens.Lens' DescribeWorldTemplateResponse Prelude.Int
describeWorldTemplateResponse_httpStatus = Lens.lens (\DescribeWorldTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeWorldTemplateResponse' {} a -> s {httpStatus = a} :: DescribeWorldTemplateResponse)

instance Prelude.NFData DescribeWorldTemplateResponse where
  rnf DescribeWorldTemplateResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
